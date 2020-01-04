import pandas
import numpy
import numpy as np
from statistics import mean
from math import log, exp

data_file = "mieszkania.csv"


def create_features(filename):
    df = pandas.read_csv(filename)
    averages = df.groupby(["dzielnica"]).mean()["cena"]
    neighs = list(set(df["dzielnica"]))

    xs = []
    ys = []

    for i in range(len(df)):
        x = []
        for col in df:
            if col == "dzielnica":
                neighs_l = [0] * len(neighs)
                neighs_l[neighs.index(df[col][i])] = 1
                x.extend(neighs_l)
                continue
            if col == "cena":
                ys.append(df[col][i])
                continue
            x.append(df[col][i])
        x.append(1)
        xs.append(x)

    return np.array(xs), np.array(ys)

def engineer_features(xs):
    result = []
    for x in xs:
        n = len(x)
        l = list(x)
        for i in range(1, 5):
            # multiplying by area
            l[i] *= x[0]
        l.append(x[0] if x[5] >= 3 else 0) # rooms for sleeping
        l.append(x[0] if x[6] >= 2 else 0) # bathrooms
        l.append(x[0] if x[5] >= 2 else 0)
        result.append(l)
    return np.array(result)

xs, ys = create_features(data_file)
xs = engineer_features(xs)
ys = np.fromiter(map(lambda y: log(y + 1), ys), dtype=np.float)
N = len(xs)
print(xs[0])

lr = 0.0000005 # step size

xsT = np.transpose(xs)
n_epochs = 8851 # number of passes over the training data
vec = np.zeros(len(xs[0]))
constant = np.dot(xsT, ys)

def predict(vec, xM=xs):
    return np.dot(xM, vec)

def evaluate(vec, xM=xs, ys=ys):
    prediction = predict(vec)
    diff = ys - prediction
    return np.dot(np.transpose(diff), diff) / len(xs)

losses = []

for i in range(n_epochs):
    loss = evaluate(vec)
    losses.append(loss)
    gradient = (np.dot(xsT, predict(vec)) - constant) / N
    vec -= lr * gradient

    if i % (int(n_epochs / 10)) == 0:
        print('Iter: {:>3} Loss: {:8.8f} vec: '.format(i, loss))


test_file = "mieszkania_test.csv"

testXs, testYs = create_features(test_file)
testXs = engineer_features(testXs)
testYs = np.fromiter(map(lambda y: log(y + 1), testYs), dtype=np.float)

def error(ps, ys=testYs):
    diff = ps - ys
    return np.dot(np.transpose(diff), diff) / len(ys)

predictions = predict(vec, xM=testXs)

mean_error = error([mean(ys)] * len(ys), ys)
regression_error = evaluate(vec, xM=testXs)
print(mean_error)
print(regression_error)

for i in range(len(predictions)):
    pass
    #print(exp(predictions[i]), exp(testYs[i]))
