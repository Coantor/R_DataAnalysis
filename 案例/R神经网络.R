library(neuralnet)

ind<-sample(2,nrow(iris),replace = T,prob = c(0.7,0.3))
trainset<-iris[ind==1,]
testset<-iris[ind==2,]

trainset$setosa<-trainset$Species=="setosa"
trainset$virginica<-trainset$Species=="virginica"
trainset$versicolor<-trainset$Species=="versicolor"


network<-neuralnet(versicolor+virginica+setosa~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                   trainset,hidden = 3)
plot(network)

net.predict<-compute(network,testset[-5])$net.result
net.prediction<-c("versicolor","virginica","setosa")[apply(net.predict,1,which.max)]
predict.table<-table(testset$Species,net.prediction)
predict.table
