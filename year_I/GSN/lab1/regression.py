import pandas
import random
import numpy
import numpy as np
from statistics import mean
from math import log, exp
from scipy.stats import skew

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
                ys.append(np.log1p(df[col][i]))
                continue
            if col == "m2":
                x.append(df[col][i])
                continue
            if col == "rok_budowy":
                x.append(df[col][i])
                continue
            x.append(df[col][i])
        xs.append(x)

    return np.array(xs), np.array(ys)

def engineer_features(xs, ys):
    result = []
    for x in xs:
        n = len(x)
        l = list(x)
        for i in range(1, 5):
            pass
            l[i] *= x[0]
        l[7] = 0
        #l[8] = 0
        #l.append(1 if x[7] >= 1982 else 0)
        #l.append(x[0] * x[5] * x[6])
        result.append(l)
    return np.array(result)

xs, ys = create_features(data_file)
xs = engineer_features(xs, ys)
N = len(xs)
print(xs[0])

lr = 0.5 # step size

xsT = np.transpose(xs)
n_epochs = 81 # number of passes over the training data
vec = np.zeros(len(xs[0]))
constant = np.dot(xsT, ys)

def predict(vec, xM=xs):
    return np.fromiter(map(lambda x: np.log1p(x), np.dot(xM, vec)), dtype=float)

def mean_msle(ys, m):
    diff = [m] * len(ys) - ys
    return np.dot(np.transpose(diff), diff) / len(ys)

def evaluate(vec, xM=xs, ys=ys):
    prediction = predict(vec)
    diff = ys - prediction
    return np.dot(np.transpose(diff), diff) / len(xM)

losses = []

for i in range(n_epochs):
    loss = evaluate(vec)
    losses.append(loss)
    gradient = 2 * (np.dot(xsT, predict(vec)) - constant) / N
    vec -= lr * gradient

    if i % (int(n_epochs / 10)) == 0:
        print('Iter: {:>3} Loss: {:8.8f}'.format(i, loss))

print(vec)
test_file = "mieszkania_test.csv"

testXs, testYs = create_features(test_file)
testXs = engineer_features(testXs, testYs)

predictions = predict(vec, xM=testXs)

m = mean(ys)
mean_error = mean_msle(testYs, m)
regression_error = evaluate(vec, xM=testXs, ys=testYs)
print(mean_error)
print(regression_error)

for i in range(len(predictions)):
    pass
    #print(int(exp(predictions[i])), int(exp(testYs[i])))
