import numpy as np


# Let's read the mnist dataset

def load_mnist(path='mnist.npz'):
    with np.load(path) as f:
        x_train, _y_train = f['x_train'], f['y_train']
        x_test, _y_test = f['x_test'], f['y_test']

    x_train = x_train.reshape(-1, 28 * 28) / 255.
    x_test = x_test.reshape(-1, 28 * 28) / 255.

    y_train = np.zeros((_y_train.shape[0], 10))
    y_train[np.arange(_y_train.shape[0]), _y_train] = 1

    y_test = np.zeros((_y_test.shape[0], 10))
    y_test[np.arange(_y_test.shape[0]), _y_test] = 1

    return (x_train, y_train), (x_test, y_test)


(x_train, y_train), (x_test, y_test) = load_mnist()


def sigmoid(z):
    return 1.0 / (1.0 + np.exp(-z))


def sigmoid_prime(z):
    # Derivative of the sigmoid
    return sigmoid(z) * (1 - sigmoid(z))


class Network(object):
    def __init__(self, sizes):
        # initialize biases and weights with random normal distr.
        # weights are indexed by target node first
        self.num_layers = len(sizes)
        self.sizes = sizes
        self.biases = [np.random.randn(y, 1) for y in sizes[1:]]
        self.weights = [np.random.randn(y, x)
                        for x, y in zip(sizes[:-1], sizes[1:])]

    def feedforward(self, a):
        # Run the network on a single case
        l = [a]
        for b, w in zip(self.biases, self.weights):
            a = sigmoid(np.dot(w, a) + b)
            l.append(a)
        return l

    def update_mini_batch(self, x_mini_batch, y_mini_batch, eta):
        # Update networks weights and biases by applying a single step
        # of gradient descent using backpropagation to compute the gradient.
        # The gradient is computed for a mini_batch.
        # eta is the learning rate
        nabla_b, nabla_w = self.backprop(x_mini_batch.T, y_mini_batch.T)

        self.weights = [w - (eta / len(x_mini_batch)) * nw
                        for w, nw in zip(self.weights, nabla_w)]
        self.biases = [b - (eta / len(x_mini_batch)) * nb
                       for b, nb in zip(self.biases, nabla_b)]

    def backprop(self, x, y):
        # For a single input (x,y) return a tuple of lists.
        # First contains gradients over biases, second over weights.

        # First initialize the list of gradient arrays
        delta_nabla_b = [0, 0]
        delta_nabla_w = [0, 0]

        # Then go forward remembering all values before and after activations
        # in two other array lists
        gs = self.feedforward(x)

        # Now go backward from the final cost applying backpropagation

        delta = (gs[-1] - y) * sigmoid_prime(gs[-1])

        for i in reversed(range(1, len(gs))):
            delta_nabla_w[i-1] = np.dot(delta, gs[i-1].T)
            delta_nabla_b[i-1] = delta
            if i == 1:
                break
            delta = np.dot(self.weights[i-1].T, delta) * sigmoid_prime(gs[i-1])

        return delta_nabla_b, delta_nabla_w

    def evaluate(self, x_test_data, y_test_data):
        # Count the number of correct answers for test_data
        test_results = [(np.argmax(self.feedforward(x_test_data[i].reshape(784, 1))[-1]), np.argmax(y_test_data[i]))
                        for i in range(len(x_test_data))]
        # return accuracy
        return np.mean([int(x == y) for (x, y) in test_results])

    def cost_derivative(self, output_activations, y):
        return (output_activations - y)

    def SGD(self, training_data, epochs, mini_batch_size, eta, test_data=None):
        x_train, y_train = training_data
        if test_data:
            x_test, y_test = test_data
        for j in range(epochs):
            for i in range(x_train.shape[0] // mini_batch_size):
                x_mini_batch = x_train[i * mini_batch_size:(i * mini_batch_size + mini_batch_size)]
                y_mini_batch = y_train[i * mini_batch_size:(i * mini_batch_size + mini_batch_size)]
                self.update_mini_batch(x_mini_batch, y_mini_batch, eta)
            if test_data:
                print("Epoch: {0}, Accuracy: {1}".format(j, self.evaluate(x_test, y_test)))
            else:
                print("Epoch: {0}".format(j))


network = Network([784, 30, 10])
network.SGD((x_train, y_train), epochs=50, mini_batch_size=100, eta=5., test_data=(x_test, y_test))


