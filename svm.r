spam = read.csv("spam.csv", header = TRUE)
fix(spam)
attach(spam)
#Sampling 85% data as training data and 15% data as the test data.
set.seed(1);
ind = sample(nrow(spam), 0.85*nrow(spam))
train_data = spam[ind,]
test_data = spam[-ind,]

#Taking 500 samples from the training data for tuning.
set.seed(2)
tune_ind = sample(nrow(train_data), 500)
tune_data = train_data[tune_ind,]

install.packages("e1071", repos = "http://cran.us.r-project.org")
library(e1071);
tune.out = tune(svm, type~., data = tune_data, kernel="radial", 
                ranges = list(cost = c(seq(10,100,8)), gamma = c(seq(0.000001, 0.001, length.out=8))))

summary(tune.out)
#We get the best parameters : cost = 82 and gamma = 0.0005718571
#Using the above tuned parameters to train the SVM.

svmfit = svm(type~., data = train_data, kernel="radial", cost=82, gamma=0.0005718571)
summary(svmfit)
#We get that number of support vectors = 844; (415 429) -> (spam, nonspam)

conf_mat = table(true=test_data$type, pred=predict(svmfit, test_data))
accuracy = sum(diag(conf_mat))/sum(conf_mat)
#We get accuracy of 93.63% on the test data 

