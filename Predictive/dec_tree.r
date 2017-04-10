library(rpart)
#setwd('/home/karthik/DA/Classification')
mydata=read.csv("female_male.csv")
mydata[,"train"] <- ifelse(runif(nrow(mydata))<0.80,1,0)
trainColNum <- grep("train",names(mydata))
traindata <- mydata[mydata$train==1,-trainColNum]
testdata <- mydata[mydata$train==0,-trainColNum]

# grow tree
#Data <- data.frame(t,can2p,code) 
fit <- rpart(dec_o ~ attr1_1+sinc1_1+intel1_1+fun1_1+amb1_1+shar1_1+attr3_1+sinc3_1+fun3_1+intel3_1+amb3_1+attr1_1+sinc1_1+intel1_1+fun1_1+amb1_1+shar1_1+attr3_1+sinc3_1+fun3_1+intel3_1+amb3_1+sports+tvsports+exercise+dining+museums+art+hiking+gaming+clubbing+reading+tv+theater+movies+concerts+music+shopping+yoga
,method="class", data=traindata,control=rpart.control(minsplit=2, minbucket=1, cp=0.0025))

#printcp(fit) # display the results
#plotcp(fit) # visualize cross-validation results
#summary(fit) # detailed summary of splits

setEPS()
postscript("male.eps")
plot(fit, uniform=TRUE,main="Match for Males")
text(fit, cex=.8)
dev.off()
labels(fit)
