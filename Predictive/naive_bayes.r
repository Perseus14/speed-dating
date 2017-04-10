library(e1071)
mydata_male=read.csv("male_female.csv")
mydata_female=read.csv("female_male.csv")


mydata_male=mydata_male[1:3000,]
mydata_female=mydata_male[1:3000,]

mydata_male=(mydata_male[3:60])
mydata_female=(mydata_female[3:60])

if(FALSE)
{
mydata_male[,"train"] <- ifelse(runif(nrow(mydata_male))<0.83,1,0)
trainColNum_male <- grep("train",names(mydata_male))
traindata_male <- mydata_male[mydata_male$train==1,-trainColNum_male]
testdata_male <- mydata_male[mydata_male$train==0,-trainColNum_male]

mydata_female[,"train"] <- ifelse(runif(nrow(mydata_female))<0.80,1,0)
trainColNum_female <- grep("train",names(mydata_female))
traindata_female <- mydata_female[mydata_female$train==1,-trainColNum_female]
testdata_female <- mydata_female[mydata_female$train==0,-trainColNum_female]


nb_model_male <- naiveBayes(as.factor(dec_o) ~ .,data = traindata_male) 
nb_model_female <- naiveBayes(as.factor(dec_o) ~ .,data = traindata_female) 

#summary(nb_model)
#str(nb_model)

nb_test_predict_male <- predict(nb_model_male,testdata_male[1:57]) 
nb_test_predict_female <- predict(nb_model_female,testdata_female[1:57]) 

table(pred=nb_test_predict_male,true=testdata_male$dec_o) 
mean(nb_test_predict_male==testdata_male$dec_o)

table(pred=nb_test_predict_female,true=testdata_female$dec_o) 
mean(nb_test_predict_female==testdata_female$dec_o)
}
#function to create, run and record model results
nb_multiple_runs <- function(train_fraction,n)
{
	fraction_correct_male <- rep(NA,n)
	fraction_correct_female <- rep(NA,n)
	for (i in 1:n)
		{
			mydata_male[,"train"] <- ifelse(runif(nrow(mydata_male))<0.70,1,0)
			trainColNum_male <- grep("train",names(mydata_male))
			traindata_male <- mydata_male[mydata_male$train==1,-trainColNum_male]
			testdata_male <- mydata_male[mydata_male$train==0,-trainColNum_male]

			mydata_female[,"train"] <- ifelse(runif(nrow(mydata_female))<0.70,1,0)
			trainColNum_female <- grep("train",names(mydata_female))
			traindata_female <- mydata_female[mydata_female$train==1,-trainColNum_female]
			testdata_female <- mydata_female[mydata_female$train==0,-trainColNum_female]

			nb_model_male <- naiveBayes(as.factor(dec_o) ~ .,data = traindata_male) 
			nb_model_female <- naiveBayes(as.factor(dec_o) ~ .,data = traindata_female)
	
			nb_test_predict_male <- predict(nb_model_male,testdata_male[1:57]) 
			nb_test_predict_female <- predict(nb_model_female,testdata_female[1:57]) 

			fraction_correct_male[i] <- mean(nb_test_predict_male==testdata_male$dec_o) #TODO
			fraction_correct_female[i] <- mean(nb_test_predict_female==testdata_female$dec_o) #TODO
		}
	arg<-list("male" = fraction_correct_male, "female" = fraction_correct_female)
	return(arg)
}

arg <- nb_multiple_runs(0.8,20)
fraction_correct_predictions_male <- arg$male
fraction_correct_predictions_female <- arg$female

print("Average Male")
mean(fraction_correct_predictions_male)

print("Average Female")
mean(fraction_correct_predictions_female)

print("Summary of Male with Standard Deviation")
summary(fraction_correct_predictions_male)
sd(fraction_correct_predictions_male)

print("Summary of Female with Standard Deviation")
summary(fraction_correct_predictions_female)
sd(fraction_correct_predictions_female)
