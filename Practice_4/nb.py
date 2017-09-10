import csv
import random
import math
import operator
import numpy as np
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis as LDA


random.seed(123)


# loadDataset and create training set and test set
def load_data(filename, split, trainingSet, testSet):
    with open(filename, 'rb') as csvfile:
        lines = csv.reader(csvfile)
        dataset = list(lines)
        for x in range(len(dataset) - 1):
            for y in range(9):
                dataset[x][y] = float(dataset[x][y])
            if random.random() < split:
                trainingSet.append(dataset[x])
            else:
                testSet.append(dataset[x])


# separate class of chd for '0' & '1'
def separate(dataset):
    separated = {}
    for i in range(len(dataset)):
        vector = dataset[i]
        if (vector[-1] not in separated):
            separated[vector[-1]] = []
        separated[vector[-1]].append(vector)
    return separated


# find euclidean distance between two instances (rows)
def get_distance(instance1, instance2, length):
    distance = 0
    for x in range(length):
        distance += pow((instance1[x] - instance2[x]), 2)
    return math.sqrt(distance)


# find prior probability for each class
def priorProb(dict, dataset):
    total = len(dataset)
    prior = {}
    for k in dict:
        if (k == '1'):
            class1 = len(dict[k])
            prior1 = float(class1) / float(total)
            prior[k] = prior1
            print(prior1)
        else:
            class0 = len(dict[k])
            prior0 = float(class0) / float(total)
            prior[k] = prior0
            print(prior0)
    return prior


# get the most similar neighbors for a test instance
def get_neighbours(train, test, width):
    distances = []
    length = len(test) - 1
    for x in range(len(train)):
        dist = get_distance(train[x], test, length)
        distances.append((train[x], dist))

    distances.sort(key=operator.itemgetter(1))
    # print(distances)
    neighbours = []

    for x in range(len(distances)):
        if (distances[x][1] < width):
            neighbours.append(distances[x][0])

    return neighbours


# Gaussian product kernel
def kernel(neighborsDic, testInstance, width):
    difference1 = []
    difference0 = []
    length = len(testInstance) - 1
    n1 = 0
    n0 = 0
    for k in neighborsDic:
        if (k == "1"):
            for neighbor in neighborsDic[k]:
                diff1 = get_distance(neighbor, testInstance, length)
                difference1.append((neighbor, diff1))
                n1 += 1
            print("difference is 1" + "\n")
            print(difference1)
            print(n1)
        else:
            for neighbor in neighborsDic[k]:
                diff0 = get_distance(neighbor, testInstance, length)
                difference0.append((neighbor, diff0))
                n0 += 1
            print("difference is 0" + "\n")
            print(difference0)
            print(n0)

    sum1 = 0
    sum0 = 0
    for d in difference1:
        sum1 += math.exp(-(math.pow(d[1] / width, 2)) / 2)
    print("sum1 is: " + "\n")
    print(sum1)

    for d in difference0:
        sum0 += math.exp(-(math.pow(d[1] / width, 2)) / 2)
    print("sum0 is: " + "\n")
    print(sum0)

    width2 = math.pow(width, 2)
    print("width2 is: ", width2)
    nValue1 = n1 * math.pow(2 * width2 * math.pi, length / 2)
    print("nValue1 is: ", nValue1)
    if (nValue1 == 0):
        product1 = 0
    else:
        product1 = 1 / nValue1 * sum1

    nValue0 = n0 * math.pow(2 * width2 * math.pi, length / 2)
    print("nValue0 is: ", nValue0)
    if (nValue0 == 0):
        product0 = 0
    else:
        product0 = 1 / nValue0 * sum0

    print(product1)
    print(product0)

    prob = {}
    prob["1"] = product1
    prob["0"] = product0

    return prob


# make prediction for the test set
def get_response(prior, prob, testInstance):
    r = []
    prob1 = prior["1"] * prob["1"]
    prob0 = prior["0"] * prob["0"]

    totalProb = prob1 + prob0
    if (totalProb == 0):
        freq1 = prob1
        freq0 = prob0
    else:
        freq1 = prob1 / totalProb
        freq0 = prob0 / totalProb

    print(freq1)
    print(freq0)
    if (freq1 <= freq0):
        response = "0"
        r.append((testInstance, response))

    else:
        response = "1"
        r.append((testInstance, response))

    print(r)
    return r


if __name__ == "__main__":
    trainingSet = []
    testSet = []
    split = 0.5
    # data.csv is the dataset "South African Heart Disease";
    # however, it doesn't contain a header line and non-continous value such as famhist
    load_data('data.csv', split, trainingSet, testSet)
    print 'Train set: ' + repr(len(trainingSet))
    print 'Test set: ' + repr(len(testSet))
    # print trainingSet

    separatedClass = separate(trainingSet)
    # print('separatedClass instances: {0}').format(separatedClass)
    prior = priorProb(separatedClass, trainingSet)

    width = 50

    # make prediction for each test set
    responses = []
    correct = 0
    print(testSet)
    for x in range(len(testSet)):
        neighbors = get_neighbours(trainingSet, testSet[x], width)
        print("neighbors" + "\n")
        print(neighbors)

        neighborsClass = separate(neighbors)
        print("neighborsClass" + "\n")
        print(neighborsClass)

        prob = kernel(neighborsClass, testSet[x], width)

        response = get_response(prior, prob, testSet[x])
        print("testSet[x] is:")
        print(testSet[x])

        if (testSet[x][-1] == response[0][1]):
            correct += 1
        responses.append(response)
        print("responses is " + "\n")
        print(responses)

    accuracy = (correct / float(len(testSet))) * 100.0

    # LDA analysis
    trainingSetX = []
    trainingSetY = []
    testSetX = []
    testSetY = []
    for trainx in trainingSet:
        trainy = trainx.pop()
        trainingSetX.append(trainx)
        trainingSetY.append(trainy)
    for testx in testSet:
        testy = testx.pop()
        testSetX.append(testx)
        testSetY.append(testy)
    X = np.array(trainingSetX)
    Y = np.array(trainingSetY)
    lda = LDA()
    lda.fit(X, Y)
    predict = lda.predict(testSetX)
    print(predict)
    testY = np.array(testSetY)
    print(testY)
    accuracyLDA = np.sum(predict == testY) / float(len(testSet)) * 100.0

    print "Naive Bayes Accuracy: " + str(accuracy)
    print "LDA Accuracy: " + str(accuracyLDA)
