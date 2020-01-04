import math
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.nn.modules import Dropout
import torch.optim as optim
from torch.nn.parameter import Parameter
from torch.nn import init
import torchvision
import torchvision.transforms as transforms

''''
Tasks:
1. Check that the given implementation reaches 95% test accuracy for
   architecture input-64-64-10 in a few thousand batches.

2. Improve initialization and check that the network learns much faster
   and reaches over 97% test accuracy.

3. Check, that with proper initialization we can train architecture
   input-64-64-64-64-64-10, while with bad initialization it does
   not even get off the ground.

4. Add dropout implemented in pytorch

5. Check that with 10 hidden layers (64 units each) even with proper
    initialization the network has a hard time to start learning.

6. Implement batch normalization (use train mode also for testing
       - it should perform well enough):
    * compute batch mean and variance
    * add new variables beta and gamma
    * check that the networks learns much faster for 5 layers
    * check that the network learns even for 10 hidden layers.

Bonus task:

Design and implement in pytorch (by using pytorch functions)
   a simple convnet and achieve 99% test accuracy.

Note:
This is an exemplary exercise. MNIST dataset is very simple and we are using
it here to get resuts quickly.
To get more meaningful experience with training convnets use the CIFAR dataset.
'''


def truncated_normal_(tensor, mean=0, std=1):
    tensor.data.normal_(mean=mean, std=std)


class MyDropout(nn.Module):
    def __init__(self, p: float = 0.5):
        super(MyDropout, self).__init__()
        if p < 0 or p > 1:
            raise ValueError("dropout probability has to be between 0 and 1, " "but got {}".format(p))
        self.p = p

    def forward(self, X):
        if self.training:
            binomial = torch.distributions.binomial.Binomial(probs=1-self.p)
            return X * (binomial.sample(X.size()) * (1.0/(1-self.p))).cuda()
        return X


class Linear(torch.nn.Module):
    def __init__(self, in_features, out_features):
        super(Linear, self).__init__()
        self.in_features = in_features
        self.out_features = out_features
        self.weight = Parameter(torch.Tensor(out_features, in_features))
        self.bias = Parameter(torch.Tensor(out_features))
        self.beta = Parameter(torch.rand(1), requires_grad=True)
        self.gamma = Parameter(torch.rand(1), requires_grad=True)

        self.reset_parameters()

    def reset_parameters(self):
        truncated_normal_(self.weight, std=2/(sum(self.weight.size())))
        init.zeros_(self.bias)
        init.zeros_(self.beta)
        init.ones_(self.gamma)

    def forward(self, x):
        m = x.mean()
        std = x.norm() / math.sqrt(x.nelement())

        r = (x - m) / std
        r *= self.gamma
        r += self.beta
        # droput - macierz 0-1 i mnożymy, tylko podczas trening
        r = r.matmul(self.weight.t())
        r += self.bias

        return r


class Net(nn.Module):
    def __init__(self):
        super(Net, self).__init__()
        self.fc1 = Linear(784, 64)
        self.fc2 = Linear(64, 64)
        self.fc3 = Linear(64, 64)
        self.fc4 = Linear(64, 64)
        self.fc5 = Linear(64, 64)
        self.fc6 = Linear(64, 10)

    def forward(self, x):
        x = x.view(-1, 28 * 28)
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = F.relu(self.fc3(x))
        x = F.relu(self.fc4(x))
        x = F.relu(self.fc5(x))
        x = self.fc6(x)
        return x


MB_SIZE = 128


class MnistTrainer(object):
    def __init__(self):
        transform = transforms.Compose(
                [transforms.ToTensor()])
        self.trainset = torchvision.datasets.MNIST(
            root='./data',
            download=True,
            train=True,
            transform=transform)
        self.trainloader = torch.utils.data.DataLoader(
            self.trainset, batch_size=MB_SIZE, shuffle=True, num_workers=4,
            pin_memory=True)

        self.testset = torchvision.datasets.MNIST(
            root='./data',
            train=False,
            download=True, transform=transform)
        self.testloader = torch.utils.data.DataLoader(
            self.testset, batch_size=1, shuffle=False, num_workers=4)

    def train(self):
        net = Net()

        criterion = nn.CrossEntropyLoss()
        optimizer = optim.SGD(net.parameters(), lr=0.05, momentum=0.9)

        device = "cuda:0"

        net.to(device)

        j = 0

        for epoch in range(50):
            running_loss = 0.0
            for i, data in enumerate(self.trainloader, 0):
                inputs, labels = data
                inputs, labels = inputs.to(device), labels.to(device)
                optimizer.zero_grad()

                outputs = net(inputs)
                loss = criterion(outputs, labels)
                loss.backward()
                optimizer.step()

                running_loss += loss.item()
                if i % 100 == 99:
                    print('[%d, %5d] loss: %.3f' %
                          (epoch + 1, i + 1, running_loss / 100))
                    running_loss = 0.0
            j += 1
            if j % 10 == 0:
                correct = 0
                total = 0
                with torch.no_grad():
                    for data in self.testloader:
                        images, labels = data
                        images, labels = images.to(device), labels.to(device)
                        outputs = net(images)
                        _, predicted = torch.max(outputs.data, 1)
                        total += labels.size(0)
                        correct += (predicted == labels).sum().item()

                print('Accuracy of the network on the {} test images: {} %'.format(
                    total, 100 * correct / total))


def main():
    trainer = MnistTrainer()
    trainer.train()


if __name__ == '__main__':
    main()
