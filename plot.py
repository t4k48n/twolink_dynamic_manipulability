import numpy
import matplotlib
matplotlib.use("Qt5Agg")
import matplotlib.pyplot as pyplot

data = numpy.loadtxt("./result.csv", delimiter=",", skiprows=1)

data[:,0] *= 180 / numpy.pi
data[:,1] *= 180 / numpy.pi

pyplot.figure()
pyplot.subplot(311)
pyplot.plot(data[:,2])
pyplot.subplot(312)
pyplot.plot(data[:,3])
pyplot.subplot(313)
pyplot.plot(data[:,4])
pyplot.figure()
pyplot.scatter(data[:,0], data[:,1], c=data[:,2], marker='s')
pyplot.colorbar()
pyplot.gca().set_aspect(1)
pyplot.figure()
pyplot.scatter(data[:,0], data[:,1], c=data[:,3], marker='s')
pyplot.colorbar()
pyplot.gca().set_aspect(1)
pyplot.figure()
pyplot.scatter(data[:,0], data[:,1], c=data[:,4], marker='s')
pyplot.colorbar()
pyplot.gca().set_aspect(1)
pyplot.show()
