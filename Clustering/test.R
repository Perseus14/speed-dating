library(cluster)
mydata=read.csv("filtered_data.csv")
data<-cbind(mydata[51],mydata[57])
data<- na.omit(data)
data<- sapply(data,as.numeric)
data<- scale(data)
fit<-kmeans(data,4)
dissE<-daisy(data)
dE2 <- dissE^2
sk2 <-silhouette(fit$cl,dE2)
plot(sk2)

x<-mydata$attr3_1
x<-na.omit(x)
x<-sapply(x,as.numeric)
clusplot(data, fit$cluster,color=TRUE,shade=TRUE,lines=0,col.p=(x>7)+1)
print (head(fit))



