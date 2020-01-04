import os
import math
import argparse
import logging
import datetime
import sys
from enum import Enum

import numpy as np
import seaborn as sns
import pandas as pd

import torch
import torch.nn as nn
import torch.nn.functional as f
import torch.optim as optim
from torch.nn.modules import Module
from torch.nn.parameter import Parameter
from torch.nn import init
import torchvision
import torchvision.transforms as transforms
from PIL import Image

BASE_DIR = os.path.dirname(os.path.realpath(__file__))
IMAGES_DIR = os.path.join(BASE_DIR, "fruits-360")

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
logger.addHandler(logging.StreamHandler())

DEFAULT_OCCLUSION = (255, 255, 255)  # white
IMAGE_WIDTH = 100
IMAGE_HEIGHT = 100
FRUIT_CLASSES = 96

CPU_DEVICE = "cpu"


def build_labels():
    dir_ = os.path.join(IMAGES_DIR, "Training")
    classes = [d for d in os.listdir(dir_)
               if os.path.isdir(os.path.join(dir_, d))]
    classes.sort()
    class_to_idx = {classes[i]: i for i in range(len(classes))}
    return classes, class_to_idx


def get_image_class(image_path):
    return os.path.basename(os.path.dirname(image_path))


_, CLASS_TO_IDX = build_labels()


class Layer(Enum):
    fully_connected = 1
    convolution = 2
    batch_norm2d = 3
    max_pool = 4
    relu = 5
    batch_norm1d = 6


def name_for_enum(layer_type):
    if layer_type == Layer.convolution.value:
        return "conv"
    elif layer_type == Layer.max_pool.value:
        return "pool"
    elif layer_type == Layer.batch_norm2d.value:
        return "bn"
    elif layer_type == Layer.fully_connected.value:
        return "fc"
    elif layer_type == Layer.relu.value:
        return "relu"
    else:
        raise ValueError("Unknown argument type")


def kernel_desc(k_s=None, s=None, p=None):
    if None in (k_s, s, p):
        raise ValueError("Set all kernel parameters")
    return {"kernel_size": k_s, "stride": s, "padding": p}


def dimension_change(dim, kernel_size=None, stride=None, padding=None):
    return math.floor((dim - kernel_size + 2 * padding) / stride) + 1


class _BatchNorm(Module):
    def __init__(self, num_features, dim, eps=1e-6, momentum=0.9):
        if dim not in (2, 4):
            raise ValueError("Unsuppoprted dimensions")
        super(_BatchNorm, self).__init__()
        self.beta = Parameter(torch.zeros(num_features))
        self.gamma = Parameter(torch.ones(num_features))

        self.eps = eps
        self.momentum = momentum
        self.dim = dim

        self.register_buffer('running_mean', torch.zeros(num_features))
        self.register_buffer('running_var', torch.ones(num_features))

        self.reset_parameters()

    def reset_parameters(self):
        init.zeros_(self.beta)
        init.normal_(self.gamma)

    def normalize(self, x, mean, std):
        return (x - mean) / (std + self.eps).sqrt()

    def running_avg(self, val, new):
        return val * self.momentum + (1. - self.momentum) * new.data

    def _get_dims(self):
        dims = [0]
        dims.extend(range(2, self.dim))
        return tuple(dims)

    def _get_mean(self, x):
        if self.dim == 2:
            return x.mean(0)
        if self.dim == 4:
            return x.mean(0).mean(1).mean(1)

    def _get_var(self, mean, x):
        if self.dim == 2:
            return ((x - mean) ** 2).mean(0)
        if self.dim == 4:
            return ((x - mean) ** 2).mean(0).mean(1).mean(1)

    def _reshape(self, sizes, vec):
        if self.dim == 4:
            sizes = list(sizes)[1:]
            vec = vec.reshape(vec.size()[0], 1, 1)
            return vec.expand(sizes)
        return vec

    def forward(self, x):
        if self.training:
            mean = self._get_mean(x)
            self.running_mean = self.running_avg(self.running_mean, mean)
            mean = self._reshape(x.size(), mean)
            std = self._get_var(mean, x)
            self.running_var = self.running_avg(self.running_var, std)
            std = self._reshape(x.size(), std)

            out = self.normalize(x, mean, std)
        else:
            out = self.normalize(
                x,
                self._reshape(x.size(), self.running_mean),
                self._reshape(x.size(), self.running_var))

        return out * self._reshape(x.size(), self.gamma) \
            + self._reshape(x.size(), self.beta)


class _BatchNorm2d(_BatchNorm):
    """Batch norm for images"""
    def __init__(self, num_features):
        super(_BatchNorm2d, self).__init__(num_features, 4)


class _BatchNorm1d(_BatchNorm):
    def __init__(self, num_features):
        super(_BatchNorm1d, self).__init__(num_features, 2)


BatchNormClass2d = _BatchNorm2d
BatchNormClass1d = _BatchNorm1d


def build_arch_description(
        conv_layers, fully_connected,
        channels=3, width=IMAGE_WIDTH, height=IMAGE_HEIGHT):
    counter = {e.value: 0 for e in Layer}
    result = []

    for layer in conv_layers:
        enum_t = layer[0].value
        name = name_for_enum(enum_t) + str(counter[enum_t])

        if enum_t == Layer.convolution.value:
            obj = torch.nn.Conv2d(
                channels, layer[1], **layer[2])
            result.append((name, obj, obj.__call__))
            channels = layer[1]

            width = dimension_change(width, **layer[2])
            height = dimension_change(height, **layer[2])
        elif enum_t == Layer.max_pool.value:
            obj = torch.nn.MaxPool2d(**layer[1])
            result.append((name, obj, obj.__call__))

            width = dimension_change(width, **layer[1])
            height = dimension_change(height, **layer[1])
        elif enum_t == Layer.batch_norm2d.value:
            obj = BatchNormClass2d(channels)
            result.append((name, obj, obj.__call__))
        elif enum_t == Layer.relu.value:
            result.append((name, None, f.relu))
        else:
            raise ValueError("Use convolution, batch norm or pooling only")

        counter[enum_t] += 1

    size = channels * width * height
    cast_size = size
    result.append((None, None, lambda x: x.view(x.size(0), cast_size)))

    for layer in fully_connected:
        enum_t = layer[0].value
        name = name_for_enum(enum_t) + str(counter[enum_t])

        if enum_t == Layer.fully_connected.value:
            obj = nn.Linear(size, layer[1])
            result.append(
                (name, obj, obj.__call__))
            size = layer[1]
        elif enum_t == Layer.relu.value:
            result.append((name, None, f.relu))
        elif enum_t == Layer.batch_norm2d.value:
            obj = BatchNormClass1d(size)
            result.append((name, obj, obj.__call__))
        else:
            raise ValueError("Use only fully connected layers")

        counter[enum_t] += 1

    return result


class ImageNet(nn.Module):
    # conv -> relu -> pool according to internet. BN after relu
    IMAGE_NET_CONV = [
        (Layer.convolution, 32, kernel_desc(k_s=3, s=1, p=1)),
        (Layer.relu,),
        (Layer.batch_norm2d,),
        (Layer.convolution, 32, kernel_desc(k_s=3, s=1, p=1)),
        (Layer.relu,),
        (Layer.batch_norm2d,),
        (Layer.max_pool, kernel_desc(k_s=2, s=2, p=0)),
        (Layer.convolution, 64, kernel_desc(k_s=3, s=1, p=1)),
        (Layer.relu,),
        (Layer.batch_norm2d,),
        (Layer.max_pool, kernel_desc(k_s=3, s=3, p=0)),
        (Layer.convolution, 32, kernel_desc(k_s=3, s=1, p=1)),
        (Layer.relu,),
        (Layer.batch_norm2d,),
        (Layer.max_pool, kernel_desc(k_s=2, s=2, p=0)),
    ]
    IMAGE_NET_FULLY_CONNECTED = [
        (Layer.fully_connected, 256),
        (Layer.relu,),
        (Layer.batch_norm2d,),
        (Layer.fully_connected, FRUIT_CLASSES)
    ]

    def __init__(self):
        super(ImageNet, self).__init__()
        self.arch = build_arch_description(
            self.IMAGE_NET_CONV, self.IMAGE_NET_FULLY_CONNECTED)
        for name, obj, _ in self.arch:
            if obj is not None:
                setattr(self, name, obj)

    def forward(self, x):
        for _, _, transform in self.arch:
            x = transform(x)
        return x


class Trainer:
    def __init__(self, image_root, batch_size_train=64, augments=None):
        common_options = {"shuffle": True, "num_workers": 2}
        train_folder = os.path.join(image_root, 'Training')
        test_folder = os.path.join(image_root, 'Test')

        transform = transforms.ToTensor()
        if augments is not None:
            transform = transforms.Compose([augments, transform])

        self.trainset = torchvision.datasets.ImageFolder(
            train_folder, transform=transform)
        self.testset = torchvision.datasets.ImageFolder(
            test_folder, transform=transform)

        self.trainloader = torch.utils.data.DataLoader(
            self.trainset, batch_size=batch_size_train, **common_options)
        self.testloader = torch.utils.data.DataLoader(
            self.testset, batch_size=1, **common_options
        )

    def train(self, net, epochs=5, device=CPU_DEVICE):
        if device != CPU_DEVICE:
            net.cuda()
        criterion = nn.CrossEntropyLoss()
        optimizer = optim.SGD(net.parameters(), lr=0.1, momentum=0.9)

        for epoch in range(epochs):
            net.train()
            running_loss = 0.0

            for i, data in enumerate(self.trainloader, 0):
                inputs, labels = data
                if device != CPU_DEVICE:
                    # Data is on cpu by default
                    inputs, labels = inputs.cuda(), labels.cuda()

                optimizer.zero_grad()
                outputs = net(inputs)
                loss = criterion(outputs, labels)
                loss.backward()
                optimizer.step()

                running_loss += loss.item()
                if i % 100 == 99:
                    logger.info('[%s][%d, %5d] loss: %.8f' %
                                (datetime.datetime.now(),
                                 epoch + 1, i + 1, running_loss / 100))
                    running_loss = 0.0

            net.eval()
            self.validate(net, device)

        return net

    def validate(self, net, device):
        correct = 0
        total = 0

        with torch.no_grad():
            for data in self.testloader:
                images, labels = data
                if device != CPU_DEVICE:
                    images, labels = images.cuda(), labels.cuda()
                outputs = net(images)
                _, predicted = torch.max(outputs.data, 1)
                total += labels.size(0)
                correct += (predicted == labels).sum().item()

        logger.info('[{}] Accuracy of the network on the {} test images: {} %'
                    .format(datetime.datetime.now(), total,
                            100 * correct / total))


class Occlusion(torch.utils.data.Dataset):
    """Represents an occlusion dataset for image. On demand
    creates image with pixels set to constant value"""

    def __init__(self, image, occlusion_=DEFAULT_OCCLUSION, kernel=4):
        self.image = image
        self.occlusion = occlusion_
        self.kernel = kernel

    def __getitem__(self, item):
        height, width = int(item / self._width()), item % self._width()
        img = self.image.copy()

        for i in range(-self.kernel, self.kernel):
            for j in range(-self.kernel, self.kernel):
                h = height + i
                w = width + j
                if h < 0 or h >= self._height() or\
                   w < 0 or w >= self._width():
                    continue
                img.putpixel((w, h), self.occlusion)

        return transforms.ToTensor()(img)

    def __len__(self):
        return self._width() * self._height()

    def _width(self):
        return self.image.size[0]

    def _height(self):
        return self.image.size[1]

# Visualization methods


def occlusion(net, image_path, batch_size=128):
    label = torch.tensor([CLASS_TO_IDX[get_image_class(image_path)]])
    image = Image.open(os.path.join(BASE_DIR, image_path))
    criterion = nn.CrossEntropyLoss(reduction='none')

    occlusions = Occlusion(image)
    loader = torch.utils.data.DataLoader(
        occlusions, batch_size=batch_size)
    losses = []

    with torch.no_grad():
        for data in loader:
            outputs = net(data)
            label_ = label.repeat(data.size(0))
            losses.extend(criterion(outputs, label_).tolist())

    df = pd.DataFrame(np.array(losses).reshape((IMAGE_HEIGHT, IMAGE_WIDTH)))

    return sns.heatmap(df, cmap='viridis').get_figure()


def pixel_gradient(net, image_path):
    label = torch.tensor([CLASS_TO_IDX[get_image_class(image_path)]])
    image = Image.open(os.path.join(BASE_DIR, image_path))
    criterion = nn.CrossEntropyLoss()

    inputs = transforms.ToTensor()(image).unsqueeze_(0)\
        .requires_grad_(requires_grad=True)

    outputs = net(inputs)
    loss = criterion(outputs, label)
    loss.backward()
    df = pd.DataFrame(inputs.grad.squeeze_(0).mean(0).numpy())
    df_norm_col = (df - df.mean()) / df.std()

    return sns.heatmap(df_norm_col, cmap='viridis').get_figure()


def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("--out-network",
                        help="Path where to save trained network")
    parser.add_argument(
        "--in-network", help="Path from file from which load network. "
                             "This skips training")
    parser.add_argument(
        "--occlusion", nargs=2, type=str,
        metavar=("IMAGE_PATH", "OUT_PATH"),
        help="Relative path to file on which"
             "perform occlusion analysis")
    parser.add_argument(
        "--pixel-grad", nargs=2, type=str,
        metavar=("IMAGE_PATH", "OUT_PATH"),
        help="Path to image on which to calculate pixelwise gradient"
    )
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

    return parser.parse_args()


def main():
    args = parse_args()

    net = ImageNet()
    if args.in_network:
        net.load_state_dict(torch.load(args.in_network))
        net.eval()

    if args.train:
        if torch.cuda.is_available() and args.cuda:
            device = "cuda:0"
        else:
            if args.cuda:
                logger.error("Cuda not supported on host")
                return 1
            device = "cpu"

        trainer = Trainer(IMAGES_DIR)
        trainer.train(net, device=device, epochs=args.epochs)

    if args.validate:
        trainer = Trainer(IMAGES_DIR)
        trainer.validate(net, CPU_DEVICE)

    if args.occlusion:
        occlusion(net, args.occlusion[0]).savefig(args.occlusion[1])

    if args.pixel_grad:
        pixel_gradient(net, args.pixel_grad[0]).savefig(args.pixel_grad[1])

    if args.out_network:
        torch.save(net.cpu().state_dict(), args.out_network)


if __name__ == "__main__":
    sys.exit(main())
