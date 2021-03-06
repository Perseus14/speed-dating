library("e1071")

mydata_male=read.csv("male_female.csv")
mydata_female=read.csv("female_male.csv")


mydata_male=mydata_male[1:800,]
mydata_female=mydata_female[1:800,]


mydata_male=(mydata_male[3:59])
mydata_female=(mydata_female[3:59])

head(mydata_male)
dim(mydata_male)



nb_multiple_runs <- function(train_fraction,n)
{
	fraction_correct_male <- rep(NA,n)
	fraction_correct_female <- rep(NA,n)
	for (i in 1:n)
		{
			mydata_male[,"train"] <- ifelse(runif(nrow(mydata_male))<0.80,1,0)
			trainColNum_male <- grep("train",names(mydata_male))
			traindata_male <- mydata_male[mydata_male$train==1,-trainColNum_male]
			testdata_male <- mydata_male[mydata_male$train==0,-trainColNum_male]
			head(traindata_male)
			dim(traindata_male)

			mydata_female[,"train"] <- ifelse(runif(nrow(mydata_female))<0.80,1,0)
			trainColNum_female <- grep("train",names(mydata_female))
			traindata_female <- mydata_female[mydata_female$train==1,-trainColNum_female]
			testdata_female <- mydata_female[mydata_female$train==0,-trainColNum_female]

			svm_tune_male <- tune(svm, train.x=traindata_male[1:56], train.y=traindata_male$dec_o,kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2))) #TODO

			svm_tune_female <- tune(svm, train.x=traindata_female[1:56], train.y=traindata_female$dec_o,kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

			svm_model_male <- svm(as.factor(dec_o) ~ .,data=traindata_male, kernel="radial", cost=svm_tune_male$best.parameters$cost, gamma=svm_tune_male$best.parameters$gamma)

			svm_model_female <- svm(as.factor(dec_o) ~ .,data=traindata_female, kernel="radial", cost=svm_tune_female$best.parameters$cost, gamma=svm_tune_female$best.parameters$gamma)
	
			pred_male <- predict(svm_model_male,testdata_male[1:56]) 
			pred_female <- predict(svm_model_female,testdata_female[1:56]) 

			fraction_correct_male[i] <- mean(pred_male==testdata_male$dec_o) 
			fraction_correct_female[i] <- mean(pred_female==testdata_female$dec_o) 
		}
	arg<-list("male" = fraction_correct_male, "female" = fraction_correct_female)
	return(arg)
}

arg <- nb_multiple_runs(0.8,1)
fraction_correct_predictions_male <- arg$male
fraction_correct_predictions_female <- arg$female

print("Average Male")
mean(fraction_correct_predictions_male)

print("Average Female")
mean(fraction_correct_predictions_female)

print("Summary of Male with Standard Deviation")
summary(fraction_correct_predictions_male)
sd(fraction_correct_predictions_male)
#function to create, run and record model results

#warnings()
