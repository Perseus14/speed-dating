library(rpart)
library(party)

mydata_male=read.csv("male_female.csv")
mydata_female=read.csv("female_male.csv")
# grow tree
mydata_male=mydata_male[3:25]
mydata_female=mydata_female[3:25]

fit <- rpart(as.factor(dec_o) ~ attr1_1 + sinc1_1	+ intel1_1 + fun1_1 + amb1_1 + shar1_1 + attr3_1 + sinc3_1 + fun3_1 + intel3_1 + amb3_1 + attr1_1 +	sinc1_1 + intel1_1 + fun1_1 + amb1_1 + shar1_1 + attr3_1 + sinc3_1 + fun3_1 + intel3_1 + amb3_1,method="class", data=mydata_male,control=rpart.control(cp=0.003))

output.tree <- ctree(
  as.factor(dec_o) ~ attr1_1 + sinc1_1	+ intel1_1 + fun1_1 + amb1_1 + shar1_1 + attr3_1 + sinc3_1 + fun3_1 + intel3_1 + amb3_1 + attr1_1 +	sinc1_1 + intel1_1 + fun1_1 + amb1_1 + shar1_1 + attr3_1 + sinc3_1 + fun3_1 + intel3_1 + amb3_1, 
  data = mydata_male)

plot(output.tree)
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,main="Classification Tree for Kyphosis")
text(fit,  all=TRUE, cex=.8)

pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
plot(pfit, uniform=TRUE,main="Pruned Classification Tree for Kyphosis")
text(pfit, all=TRUE, cex=.8)
