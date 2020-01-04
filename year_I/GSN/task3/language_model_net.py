import re
import sys
import csv
import random
import string
import argparse
import logging
import datetime
import tempfile

import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import sampler
from torch.nn.utils.rnn import pack_sequence

import numpy as np
import seaborn as sns
import pandas as pd
from sklearn.manifold import TSNE


MAIN_DATA_FILE = "task3_train.txt"
DICTIONARY_FILE = "task3_dictionary.txt"
TRAIN_SAMPLE_FILE = "task3_train_sample.txt"
TRAIN_SAMPLE_SIZE = 100000
VALIDATION_SAMPLE_FILE = "task3_validation_sample.txt"
VALIDATION_SAMPLE_PORTION = 0.2

SENTENCE_EMBED_DIM = 1024
WORD_EMBEDDING_DIM = 128
CHAR_EMBED_DIM = 16
WORD_PADDING = 64

POLISH_SPECIAL_CHARS = "ąćęłńóśźż"
MASK_CHAR = "*"
WORD_PAD_CHAR = " "
PADDING_CHAR = "#"
MASK_WORD = "*" * 8
BREAK = "#" * 8
ALPHABET = WORD_PAD_CHAR + string.ascii_lowercase + POLISH_SPECIAL_CHARS \
           + MASK_CHAR + PADDING_CHAR
CHAR_MAPPING = dict(zip(ALPHABET, range(len(ALPHABET))))


logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
logger.addHandler(logging.StreamHandler())


def pack_sequence_arbitrary(seq):
    seq.sort(key=lambda t: t.size(0), reverse=True)
    return pack_sequence(seq)


def _last_cuda_device():
    return torch.device("cuda:%d" % (torch.cuda.device_count() - 1))


def to_one_hot(word):
    return list(map(lambda c: CHAR_MAPPING[c], word))


def words_to_index_tensor(words):
    words = [s.ljust(64, ' ') for s in words]
    return torch.tensor(list(map(to_one_hot, words)))


class Dictionary:
    def __init__(self):
        self.dictionary = set()
        self.dictionary_list = []

    def update(self, words):
        for w in words:
            if w not in self.dictionary:
                self.dictionary.add(w)
                self.dictionary_list.append(w)

    def save_to_file(self, file_):
        with open(file_, "w+") as h:
            for word in self.dictionary_list:
                h.write(word + '\n')
        return self

    def load_from_file(self, file_):
        with open(file_, "r+") as h:
            for line in h:
                self.dictionary_list.append(line.strip())
        self.dictionary = set(self.dictionary_list)
        return self

    @property
    def as_list(self):
        return self.dictionary_list


class CorpusPreprocessor:

    def __init__(self, dictionary):
        self.dictionary = dictionary

    def transform_text(self, text):
        # /|-|   myślnik na spacje
        # wyrzucać słowa z cyframi
        prep = re.sub('[|\.,-]', ' ', text.strip().lower())
        return ' '.join(re.sub('[^\s{}]'.format(ALPHABET), '',
                               prep).split())

    def mask_text(self, text):
        prep_text = self.transform_text(text).split()

        random_idx = random.randint(0, len(prep_text) - 1)
        original_word = prep_text[random_idx]
        prep_text[random_idx] = MASK_WORD
        masked_sent = ' '.join(prep_text)

        if random.choice([True, False]):
            return masked_sent, original_word, 1
        else:
            return masked_sent, self._random_word(), 0

    def text_to_words(self, text):
        return self.transform_text(text).split()

    def _random_word(self):
        return random.choice(self.dictionary.as_list)


def _flush_buf_to_file(buf, file_):
    if buf:
        file_.write('\n'.join(buf) + '\n')
        buf.clear()


def obtain_sample_datasets(seed=42, buffer_size=2 ** 22):
    random.seed(seed)
    dictionary = Dictionary()
    corpus_preprocessor = CorpusPreprocessor(dictionary)
    total_sentences = 0

    with tempfile.TemporaryFile(mode="w+", buffering=buffer_size) as temp:
        transformed_buf = []

        with open(MAIN_DATA_FILE, "r", buffer_size) as handle:
            for line in handle:
                total_sentences += 1
                transformed = corpus_preprocessor.transform_text(line)
                transformed_buf.append(transformed)

                if len(transformed_buf) == 2 ** 16:
                    _flush_buf_to_file(transformed_buf, temp)
                    logger.info("Total sentences done: {}".format(total_sentences))
                dictionary.update(transformed.split())

        _flush_buf_to_file(transformed_buf, temp)
        del transformed_buf

        if total_sentences < TRAIN_SAMPLE_SIZE:
            raise ValueError("Cannot create larger dataset than data")

        chosen_sentences = set(random.sample(range(0, total_sentences),
                                             TRAIN_SAMPLE_SIZE))

        logger.info("Creating train and validation data")
        with open(TRAIN_SAMPLE_FILE, "w+") as handle_train,\
                open(VALIDATION_SAMPLE_FILE, "w+") as handle_validate:

            temp.seek(0)
            for i, text in enumerate(temp):
                if i not in chosen_sentences:
                    continue

                masked = ','.join(
                    map(str, corpus_preprocessor.mask_text(text))) + '\n'
                if random.uniform(0, 1) > VALIDATION_SAMPLE_PORTION:
                    handle_train.write(masked)
                else:
                    handle_validate.write(masked)


class LanguageModelDataset(torch.utils.data.Dataset):

    def __init__(self, data_file):
        self.data = []
        with open(data_file, "r+") as handle:
            csv_reader = csv.reader(handle, delimiter=',')
            for row in csv_reader:
                self.data.append(row)

    def __len__(self):
        return len(self.data)

    def __getitem__(self, index):
        return self._row_to_data(self.data[index])

    def _row_to_data(self, row):
        sentence, word, bit = row
        words = sentence.split()
        words.extend([BREAK, word])

        indices = words_to_index_tensor(words)
        bit_tensor = self.bit_to_similarity(bit)

        return indices, bit_tensor

    @classmethod
    def bit_to_similarity(cls, bit):
        return torch.tensor(1 if int(bit) == 1 else 0, dtype=torch.long)


class WordEmbedder(nn.Module):
    LSTM_LAYERS = 2

    def __init__(self, char_emb_size, word_emb_size):
        super(WordEmbedder, self).__init__()

        assert word_emb_size % 4 == 0

        self.word_embedding_size = word_emb_size
        self.char_emb = nn.Embedding(len(ALPHABET), char_emb_size)
        self.lstm = nn.LSTM(char_emb_size, word_emb_size // 4,
                            num_layers=self.LSTM_LAYERS,
                            dropout=0.25, bidirectional=True)

    def forward(self, indices):
        x = self.char_emb(indices)
        x = x.permute(1, 0, 2)
        out, (state, x) = self.lstm(x)
        x = x.permute(1, 0, 2)
        x = x.contiguous()
        x = x.view(-1, 1, self.word_embedding_size)

        return x


class LanguageModel(nn.Module):
    LSTM_LAYERS = 2

    def __init__(self, word_embedder, sentence_emb_size):
        super(LanguageModel, self).__init__()
        self.word_emb = word_embedder
        self.lstm = nn.LSTM(self.word_emb.word_embedding_size,
                            sentence_emb_size, num_layers=self.LSTM_LAYERS,
                            dropout=0.25, bidirectional=True)
        self.fc = nn.Linear(4 * sentence_emb_size, 2)

    def forward(self, x):
        x = x.view(x.shape[1], x.shape[2])
        x = self.word_emb(x)

        out, (state, x) = self.lstm(x)
        x = x.view(-1)

        x = self.fc(x)

        return x.view(1, -1)


class LanguageModelTrainer:
    DEFAULT_BATCH_SIZE = 1
    LOGGER_TEMPLATE = '[{}] Accuracy of the network on the {} {} ' \
                      'words: {:.8f} %'

    def __init__(self, net, train_file, validation_file,
                 batch_size=None, use_cuda=False):
        self.use_cuda = use_cuda
        self.net = net

        if use_cuda:
            net = net.cuda(device=_last_cuda_device())

        common_options = {"num_workers": 4}
        if batch_size is None:
            batch_size = 1

        self.trainset = LanguageModelDataset(train_file)
        self.testset = LanguageModelDataset(validation_file)
        self.embedder = net.word_emb

        self.trainloader = torch.utils.data.DataLoader(
            self.trainset, batch_size=batch_size,
            shuffle=True, **common_options)
        self.testloader = torch.utils.data.DataLoader(
            self.testset, batch_size=batch_size, shuffle=False,
            **common_options)

    def train(self, epochs=5):
        criterion = nn.CrossEntropyLoss(reduction='mean')
        optimizer = optim.SGD(self.net.parameters(), lr=0.001, momentum=0.9)

        for epoch in range(epochs):
            self.net.train()
            running_loss = 0.0

            for i, data in enumerate(self.trainloader, 0):
                inputs, labels = data

                if self.use_cuda:
                    inputs, labels = inputs.cuda(device=_last_cuda_device()),\
                                     labels.cuda(device=_last_cuda_device())

                if i % self.DEFAULT_BATCH_SIZE == 0:
                    optimizer.zero_grad()
                outputs = self.net(inputs)
                loss = criterion(outputs, labels)
                loss.backward()
                if i % self.DEFAULT_BATCH_SIZE == 0:
                    optimizer.step()

                running_loss += loss.item()
                printing = 500
                if i % printing == (printing - 1):
                    logger.info('[%s][%d, %5d] loss: %.8f' %
                                (datetime.datetime.now(),
                                 epoch + 1, i + 1, running_loss / printing))
                    running_loss = 0.0

            self.validate()
            torch.save(self.net.state_dict(),
                       "checkpoint-language-model-epoch-%d.net" % epoch)

        return self.net

    def validate(self):
        correct = 0
        total = 0
        self.net.eval()

        with torch.no_grad():
            for data in self.testloader:
                inputs, labels = data

                if self.use_cuda:
                    inputs, labels = inputs.cuda(device=_last_cuda_device()),\
                                     labels.cuda(device=_last_cuda_device())
                total += labels.size(0)

                outputs = self.net(inputs)
                _, predicted = torch.max(outputs, 1)
                correct += (predicted == labels).sum().item()

        accuracy = 100 * correct / total
        logger.info(self.LOGGER_TEMPLATE.format(
            datetime.datetime.now(), total, "test", accuracy))

        return total, accuracy


def batch_gen(data, batch_size):
    for i in range(0, len(data), batch_size):
            yield data[i:i+batch_size]


def numpy_cos_sim(x, y):
    return np.dot(x, y) / (np.sqrt(np.dot(x, x)) * np.sqrt(np.dot(y, y)))


def visualize(words_file, embedder, vis_path, batch_embed=512):
    embedder.cpu()

    words = []
    embeds = []
    with open(words_file, "r+") as h:
        for word in h:
            words.append(word.strip())

    for words_b in batch_gen(words, batch_embed):
        embeds.append(np.squeeze(embedder(
            words_to_index_tensor(words_b)).detach().numpy(), axis=1))

    embeds = np.concatenate(embeds)
    data_embedded = TSNE(perplexity=10.).fit_transform(embeds)
    x, y = data_embedded.T

    df = pd.DataFrame({
        "x": x,
        "y": y
    })
    ax = sns.scatterplot(data=df, x="x", y="y")\

    # code below is from SO, do not have link now
    ymin, ymax = ax.get_ylim()
    color = "#3498db"  # choose a color
    bonus = (ymax - ymin) / 50  # still hard coded bonus but scales with the data
    for x, y, name in zip(x, y, words):
        ax.text(x, y + bonus, name, color=color)

    ax.get_figure().savefig(vis_path)


def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("--split-dataset", dest="split_dataset",
                        action='store_true',
                        help="Perform new dataset split")
    parser.set_defaults(split_dataset=False)

    parser.add_argument("--out-embedder",
                        help="Path where to save trained embedder")

    parser.add_argument("--out-language-model",
                        help="Path where to save trained model")

    parser.add_argument(
        "--in-embedder", help="Path from file from which load embedder")

    parser.add_argument(
        "--in-language-model", help="Path from file from which load model")

    parser.add_argument('--use-cuda', dest='cuda',
                        action='store_true', help="Use cude in training")
    parser.add_argument('--no-use-cuda', dest='cuda',
                        action='store_false', help="Do not use cuda")
    parser.set_defaults(cuda=False)

    parser.add_argument('--train', dest='train',
                        action='store_true', help="Perform training")
    parser.add_argument('--no-train', dest='train',
                        action='store_false', help="Do not train")
    parser.set_defaults(train=True)

    parser.add_argument('--epochs', type=int, default=5,
                        help='Number of epochs of training')

    parser.add_argument(
        "--visualise", nargs=2, type=str,
        metavar=("WORDS_FILE_PATH", "VISUALIZATION_FILE"),
        help="Path to dictionary on which to visualize prediction"
    )

    return parser.parse_args()


def main():
    word_embedder = WordEmbedder(CHAR_EMBED_DIM, WORD_EMBEDDING_DIM)
    language_model = LanguageModel(word_embedder, SENTENCE_EMBED_DIM)
    args = parse_args()

    if args.split_dataset:
        obtain_sample_datasets()

    if args.in_embedder:
        word_embedder.load_state_dict(torch.load(args.in_embedder))

    if args.in_language_model:
        language_model.load_state_dict(torch.load(args.in_language_model, map_location='cpu'))
        word_embedder = language_model.word_emb

    if not torch.cuda.is_available() and args.cuda:
        logger.error("Cuda not supported on host")
        return 1

    if args.train:
        trainer = LanguageModelTrainer(
            language_model, TRAIN_SAMPLE_FILE, VALIDATION_SAMPLE_FILE, use_cuda=args.cuda)
        trainer.train(epochs=args.epochs)

    if args.out_embedder:
        torch.save(word_embedder.cpu().state_dict(), args.out_embedder)

    if args.out_language_model:
        torch.save(language_model.cpu().state_dict(),
                   args.out_language_model)

    if args.visualise:
        visualize(args.visualise[0], word_embedder, args.visualise[1])

    return 0


if __name__ == "__main__":
    sys.exit(main())
