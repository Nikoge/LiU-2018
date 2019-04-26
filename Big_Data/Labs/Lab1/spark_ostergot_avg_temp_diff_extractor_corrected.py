import pyspark
path = '/user/x_hecpl/data/'
sc = pyspark.SparkContext(appName="assignment04")

# Loading the file.
preci_data = sc.textFile(path + "precipitation-readings.csv")

stations = sc.textFile(path + "stations-Ostergotland.csv")
stations = stations.map(lambda x: x.split(";"))
stations = stations.map(lambda line: line[0])
stations = stations.coalesce(1)
stations = stations.collect()
stations = sc.broadcast(stations)

preci = preci_data.map(lambda x: x.split(";"))
preci = preci.map(lambda x: ((x[0], x[1][0:4], x[1][5:7]), (float(x[3]))))
preci = preci.filter(lambda x: x[0][0] in stations.value)

preci_2016 = preci.filter(lambda x: int(x[0][1]) >= 1993 and int(x[0][1]) <= 2016)
preci_2016 = preci_2016.reduceByKey(lambda x, y: x + y)
preci_2016 = preci_2016.map(lambda line: ((line[0][1], line[0][2]), (line[1], 1)))
preci_2016 = preci_2016.reduceByKey(lambda x, y: (x[0] + y[0], x[1] + y[1]))
preci_2016 = preci_2016.map(lambda line: (line[0][0], line[0][1], (line[1][0] / line[1][1])))
preci_2016 = preci_2016.coalesce(1)
preci_2016.saveAsTextFile("res/assignment05")
