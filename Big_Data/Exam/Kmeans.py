# _*_ coding: utf-8 _*_
from pyspark import SparkContext

textfile = "kmeans_test.csv"
sc = SparkContext("local", "Test_app")
rawData = sc.textFile(textfile).cache()
data = rawData.map(lambda line: line.split(',')).map(lambda x: (float(x[0]), float(x[1])))

K = 4
centers = data.takeSample(withReplacement=False, num=K, seed=None)

def dist(x1, y1, x2, y2):
  return (x1 - x2)**2 + (y1 - y2)**2

def cluster(x, y, centers):
  d = [dist(x, y, X, Y) for (X, Y) in centers]
  mD = d[0]
  mI = 0
  for i in range(len(d)):
    if (d[i] < mD):
      mD = d[i]
      mI = i
  return mI

def kmeansIter(data, centers):
  return data.map(lambda x: (cluster(x[0], x[1], centers), (1, x))).reduceByKey(lambda (n1, (x1, y1)), (n2, (x2, y2)): (n1+n2, (x1+x2, y1+y2))).map(lambda (k, (n, (x, y))): (x/n, y/n)).collect()

def kmeans(data, centers):
  delta = 1
  while (delta > 0.01):
    centers1 = kmeansIter(data, centers)
    d = [dist(x, y, X, Y) for ((x, y), (X, Y)) in zip(centers, centers1)]
    delta = max(d)
    centers = centers1
  return centers

print kmeans(data, centers)