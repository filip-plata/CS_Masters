import os
import re
import sys
import glob
import random
import shutil
import argparse
import logging
import datetime

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as f
import torch.optim as optim
from torch.utils.data import sampler
import torchvision.transforms as transforms
from PIL import Image


logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
logger.addHandler(logging.StreamHandler())


digits = re.compile(r'(\d+)')


def _tokenize(filename):
    return tuple(int(token) if match else token
                 for token, match in
                 ((fragment, digits.search(fragment))
                  for fragment in digits.split(filename)))


def _images_in_dir(dir_):
    return sorted(glob.glob(os.path.join(dir_, "*.png")), key=_tokenize)


#################
# Global data   #
#################
BASE_DIR = os.path.dirname(os.path.realpath(__file__))
RAW_IMAGES_DIR = os.path.join(BASE_DIR, "cityscapes")
IMAGES_DIR = os.path.join(BASE_DIR, "cityscapes-split")

DEFAULT_BATCH_SIZE = 12

IMAGE_WIDTH = 256
IMAGE_HEIGHT = 256
RGB_N = 3
_IMAGE_LABELS = [[116, 17, 36],
     [152, 43,150],
     [106,141, 34],
     [ 69, 69, 69],
     [  2,  1,  3],
     [127, 63,126],
     [222, 52,211],
     [  2,  1,140],
     [ 93,117,119],
     [180,228,182],
     [213,202, 43],
     [ 79,  2, 80],
     [188,151,155],
     [  9,  5, 91],
     [106, 75, 13],
     [215, 20, 53],
     [110,134, 62],
     [  8, 68, 98],
     [244,171,170],
     [171, 43, 74],
     [104, 96,155],
     [ 72,130,177],
     [242, 35,231],
     [147,149,149],
     [ 35, 25, 34],
     [155,247,151],
     [ 85, 68, 99],
     [ 71, 81, 43],
     [195, 64,182],
     [146,133, 92]]
IMAGE_CLASSES = len(_IMAGE_LABELS)
IMAGE_LABELS = dict(zip(map(tuple, _IMAGE_LABELS), range(len(_IMAGE_LABELS))))

IMAGE_FILES = _images_in_dir(RAW_IMAGES_DIR)

CPU_DEVICE = "cpu"
VALIDATION_SHARE = 0.15

#################
# Utils         #
#################


def split_data_randomly(seed=42):
    dataset_size = len(IMAGE_FILES)
    indices = list(range(dataset_size))
    split = int(np.floor(VALIDATION_SHARE * dataset_size))

    np.random.seed(seed)
    np.random.shuffle(indices)
    train_indices, val_indices = indices[split:], indices[:split]

    return {"Training": list(map(lambda x: IMAGE_FILES[x], train_indices)),
            "Test": list(map(lambda x: IMAGE_FILES[x], val_indices))}


def setup_data_split():
    try:
        shutil.rmtree(IMAGES_DIR)
    except FileNotFoundError:
        pass
    os.mkdir(IMAGES_DIR)
    split = split_data_randomly()

    for key, files in split.items():
        dir_path = os.path.join(IMAGES_DIR, key)
        os.mkdir(dir_path)
        for file_ in files:
            shutil.copy(file_, dir_path)


def horizontal_flip(img):
    return img.transpose(Image.FLIP_LEFT_RIGHT)


def random_horizontal_flip(img, p=0.5):
    return img if random.uniform(0, 1) > p else horizontal_flip(img)


def _image_to_classes(img):
    classes = map(lambda pixel: IMAGE_LABELS[pixel], img.getdata())

    res = np.fromiter(classes, dtype=np.int)\
        .reshape((IMAGE_HEIGHT, IMAGE_WIDTH))
    return res


def images_to_classes(imgs):
    # tuple of images or one image
    if not isinstance(imgs, tuple):
        return _image_to_classes(imgs)
    return np.array(list(map(_image_to_classes, imgs)))


def classes_to_image(array):
    height, width = array.shape
    result = [color for class_ in array.flatten()
              for color in _IMAGE_LABELS[class_]]

    return transforms.ToPILImage()(
        np.array(result, np.uint8).reshape(
            (height, width, RGB_N)
        ))


def split_to_image_and_label(img_path):
    full_image = Image.open(img_path)

    image = full_image.crop((0, 0, IMAGE_WIDTH, IMAGE_HEIGHT))
    label = full_image.crop(
        (IMAGE_WIDTH, 0, 2 * IMAGE_WIDTH, IMAGE_HEIGHT))
    image.load()
    label.load()

    return image, label


class SegmentationDataset(torch.utils.data.Dataset):
    def __init__(self, images_files, transform=None, label_tr=None):
        self.image_files = images_files
        self.transform = transform
        self.label_tr = label_tr

    def __len__(self):
        return len(self.image_files)

    def __getitem__(self, item):
        image, label = split_to_image_and_label(self.image_files[item])

        seed = np.random.randint(2147483647)
        if self.transform is not None:

            random.seed(seed)
            image = self.transform(image)

        if self.label_tr is not None:
            random.seed(seed)
            label = self.label_tr(label)

        return image, label


class ConvReluBatchPack(nn.Module):
    def __init__(self, input_channels, target_channels):
        super(ConvReluBatchPack, self).__init__()

        self.conv_tran = nn.ConvTranspose2d(
            input_channels, target_channels,
            stride=2, kernel_size=2)

        self.conv = nn.Conv2d(input_channels, target_channels,
                              kernel_size=3, stride=1, padding=1)
        self.batch_norm = nn.BatchNorm2d(target_channels)

    def forward(self, x):
        x = self.conv(x)
        x = f.relu(x)
        x = self.batch_norm(x)

        return x


class DoubleConvPack(nn.Module):
    def __init__(self, input_channels, target_channels):
        super(DoubleConvPack, self).__init__()

        self.conv = nn.Sequential(
            ConvReluBatchPack(input_channels, target_channels),
            ConvReluBatchPack(target_channels, target_channels)
        )

    def forward(self, x):
        x = self.conv(x)
        return x


class UNetUp(nn.Module):
    def __init__(self, input_channels, target_channels):
        super(UNetUp, self).__init__()

        self.conv_tran = nn.ConvTranspose2d(
            input_channels, target_channels,
            stride=2, kernel_size=2)

        self.conv_pack = DoubleConvPack(input_channels, target_channels)

    def forward(self, x1, x2):
        # x1 is copied, x2 goes from down
        x2 = self.conv_tran(x2)

        x = torch.cat((x1, x2), dim=1)

        x = self.conv_pack(x)
        return x


class UNetDown(nn.Module):
    def __init__(self, input_channels, target_channels):
        super(UNetDown, self).__init__()

        self.conv_pack = DoubleConvPack(input_channels, target_channels)

        self.pool = nn.MaxPool2d(kernel_size=2, stride=2, padding=0)

    def forward(self, x):
        x = self.pool(x)
        x = self.conv_pack(x)
        return x


class SegmentationNet(nn.Module):
    def __init__(self):
        super(SegmentationNet, self).__init__()
        self.initial_conv = DoubleConvPack(3, 64)
        self.down1 = UNetDown(64, 128)
        self.down2 = UNetDown(128, 256)
        self.down3 = UNetDown(256, 512)

        self.bottom_conv_pipe = DoubleConvPack(512, 512)

        self.up1 = UNetUp(512, 256)
        self.up2 = UNetUp(256, 128)
        self.up3 = UNetUp(128, 64)
        self.out_conv = nn.Conv2d(64, IMAGE_CLASSES, kernel_size=1, stride=1)

    def forward(self, x):
        x1 = self.initial_conv(x)

        x2 = self.down1(x1)
        x3 = self.down2(x2)
        x = self.down3(x3)

        x = self.bottom_conv_pipe(x)

        x = self.up1(x3, x)
        x = self.up2(x2, x)
        x = self.up3(x1, x)

        x = self.out_conv(x)
        return x


class Trainer:
    LOGGER_TEMPLATE = '[{}] Accuracy of the network on the {} {} ' \
                      'images: {:.8f} %'

    @staticmethod
    def augment_trans(img):
        return img, horizontal_flip(img.copy())

    @staticmethod
    def augments_to_tensor(imgs):
        to_tensor = transforms.ToTensor()
        return torch.stack(tuple(map(to_tensor, imgs)))

    def __init__(self, image_root, batch_size=DEFAULT_BATCH_SIZE):
        common_options = {"shuffle": True, "num_workers": 2}
        train_folder = os.path.join(image_root, 'Training')
        test_folder = os.path.join(image_root, 'Test')

        # this random horizontal flip is the same
        # for image and label
        transform = transforms.Compose([
            random_horizontal_flip,
            transforms.ToTensor()
        ])

        label_tr = transforms.Compose([
            random_horizontal_flip,
            images_to_classes,
            torch.tensor
        ])

        transform_test = transforms.Compose([
            self.augment_trans, self.augments_to_tensor
        ])

        label_tr_test = transforms.Compose([
            self.augment_trans, images_to_classes, torch.tensor
        ])

        self.trainset = SegmentationDataset(
            _images_in_dir(train_folder),
            transform=transform,
            label_tr=label_tr)
        self.testset = SegmentationDataset(
            _images_in_dir(test_folder),
            transform=transform_test,
            label_tr=label_tr_test)

        self.trainloader = torch.utils.data.DataLoader(
            self.trainset, batch_size=batch_size, **common_options)
        self.testloader = torch.utils.data.DataLoader(
            self.testset, batch_size=batch_size, **common_options
        )

    def train(self, net, epochs=5, use_cuda=False):
        criterion = nn.CrossEntropyLoss()
        optimizer = optim.SGD(
            net.parameters(), lr=0.1, momentum=0.9, weight_decay=0.0005)

        for epoch in range(epochs):
            net.train()
            running_loss = 0.0

            for i, data in enumerate(self.trainloader, 0):
                inputs, labels = data
                if use_cuda:
                    # Data is on cpu by default
                    inputs, labels = inputs.cuda(), labels.cuda()

                optimizer.zero_grad()
                outputs = net(inputs)
                loss = criterion(outputs, labels)
                loss.backward()
                optimizer.step()

                running_loss += loss.item()
                if i % 20 == 19:
                    logger.info('[%s][%d, %5d] loss: %.8f' %
                                (datetime.datetime.now(),
                                 epoch + 1, i + 1, running_loss / 100))
                    running_loss = 0.0

            net.eval()
            self.validate_test(net, use_cuda=use_cuda)
            self.validate_train(net, use_cuda=use_cuda)

        return net

    def validate_train(self, net, use_cuda):
        total, accuracy = self.validate(net, use_cuda, self.trainloader)
        logger.info(self.LOGGER_TEMPLATE.format(
            datetime.datetime.now(), total, "train", accuracy))

    def validate_test(self, net, use_cuda):
        total, accuracy = self.validate(
            net, use_cuda, self.testloader, augments_mean=True)
        logger.info(self.LOGGER_TEMPLATE.format(
            datetime.datetime.now(), total, "test", accuracy))

    @staticmethod
    def validate(net, use_cuda, loader, augments_mean=False):
        correct = 0
        total = 0

        with torch.no_grad():
            for data in loader:
                images, labels = data
                total += labels.size(0)

                if augments_mean:
                    augments_n = images.size(1)

                if use_cuda:
                    images, labels = images.cuda(), labels.cuda()

                if augments_mean:
                    images = images.view(-1, RGB_N, IMAGE_HEIGHT, IMAGE_WIDTH)

                outputs = net(images)
                _, predicted = torch.max(outputs, dim=1)
                if augments_mean:
                    predicted = predicted.view(
                        -1, augments_n, IMAGE_HEIGHT, IMAGE_WIDTH)

                # Calculate average over image dimensions
                results = (predicted == labels).float().mean(-1).mean(-1)

                if augments_mean:
                    # Average over augments
                    results = results.mean(-1)
                correct += results.sum().item()

        accuracy = 100 * correct / total

        return total, accuracy


################################
# Visualization                #
################################


def all_equal(l):
    if not l:
        return True
    return l.count(l[0]) == len(l)


def merge_images(*imgs):
    n = len(imgs)

    if n == 0:
        return None
    if n == 1:
        return imgs[0].copy()

    heights = list(map(lambda img: img.size[1], imgs))
    if not all_equal(heights):
        raise ValueError("Images have to have the same height")

    height = heights[0]
    total_width = sum(map(lambda img: img.size[0], imgs))

    result = Image.new('RGB', (total_width, height))

    x_offset = 0
    for img in imgs:
        result.paste(img, (x_offset, 0))
        x_offset += img.size[0]

    return result


def visualize(net, in_image_path, out_image_path):
    image, label = SegmentationDataset([in_image_path])[0]
    # add dummy batch dimension
    image_tensor = transforms.ToTensor()(image)
    image_tensor.unsqueeze_(0)

    out = net(image_tensor).max(dim=1)[1].squeeze_(0)
    out = classes_to_image(out.numpy())

    merge_images(image, label, out).save(out_image_path)


################################
# Command line interface       #
################################


def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("--split-dataset", dest="split_dataset",
                        action='store_true',
                        help="Perform new dataset split")
    parser.set_defaults(split_dataset=False)

    parser.add_argument("--out-network",
                        help="Path where to save trained network")

    parser.add_argument(
        "--in-network", help="Path from file from which load network. "
                             "This skips training")

    parser.add_argument('--train', dest='train',
                        action='store_true', help="Perform training")
    parser.add_argument('--no-train', dest='train',
                        action='store_false', help="Do not train")
    parser.set_defaults(train=True)

    parser.add_argument('--use-cuda', dest='cuda',
                        action='store_true', help="Use cude in training")
    parser.add_argument('--no-use-cuda', dest='cuda',
                        action='store_false', help="Do not use cuda")
    parser.set_defaults(cuda=False)

    parser.add_argument('--validate', dest='validate',
                        action='store_true', help='Run one validation phase')
    parser.set_defaults(validate=False)

    parser.add_argument('--epochs', type=int, default=5,
                        help='Number of epochs of training')

    parser.add_argument(
        "--visualise", nargs=2, type=str,
        metavar=("IMAGE_PATH", "OUT_PATH"),
        help="Path to image on which to visualize prediction"
    )

    return parser.parse_args()


def main():
    args = parse_args()

    net = SegmentationNet()

    if not torch.cuda.is_available() and args.cuda:
        logger.error("Cuda not supported on host")
        return 1

    if args.cuda:
        net.cuda()

    if args.split_dataset:
        setup_data_split()

    if args.in_network:
        net.load_state_dict(torch.load(args.in_network))
        net.eval()

    if args.train:
        trainer = Trainer(IMAGES_DIR)
        trainer.train(net, use_cuda=args.cuda, epochs=args.epochs)

    if args.validate:
        # batch_size one fits on my laptop
        trainer = Trainer(IMAGES_DIR, batch_size=2)
        trainer.validate_test(net, args.cuda)

    if args.visualise:
        net.cpu()
        visualize(net, args.visualise[0], args.visualise[1])

    if args.out_network:
        torch.save(net.cpu().state_dict(), args.out_network)


if __name__ == "__main__":
    sys.exit(main())
