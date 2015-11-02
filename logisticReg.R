credit = read.table("credit_data.txt", header = TRUE)
dim(credit)
credit[1,]
credit[885,]
set.seed(1)
ind = sample(nrow(credit), 0.8*nrow(credit))
train_data = credit[ind,]
test_data = credit[-ind,]
table(train_data$Fail)
table(test_data$Fail)
attach(credit)

#1st arg is the function which is binomial (or class) term ~(v/s) attributes to consider. We skip ID.
#Select the attributes with high coefficients as changing a unit of them, will affect the model maximum.
#Here - Liquid, CumulProfit, Links, CapStruct are 4 main apart from Intercept
#The signs of attributes are correct. They are positive indicating that if those attributes' values are high, the company
#is more likely to get bankrupt and hence will give higher probability and vice versa.
lrfit = glm(Fail ~ Leverage + CumulProfit + Liquid + OverDueDebt + WorkCap + OperProfit + ShortDebt + GuarDebt +
            StateLag + FiscalLag + InFinan + Links + CapStruct, family = binomial, data = train_data)

#plot(lrfit)

#predict returns the corresponding probability of each test data on applying logistic regression
#We classify the samples as TRUE if the probability >=0.5. This makes a vector of TRUE/FALSE or (0/1) values
#as in the Fail attribute of credit. Thus when table, get the confusion matrix.
conf_mat = table(test_data$Fail, predict(lrfit, test_data, type = "response") >= 0.5, dnn = c("Predicted", "Observed"))

#install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)

mat_train = data.matrix(train_data)
mat_train = mat_train[,-1:-2]
y = factor(train_data$Fail, levels=c(0,1), labels=c(-1,1))

reglrfit = cv.glmnet(mat_train, y, family="binomial")
plot(reglrfit)
#https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html

#It includes the cross-validation curve (red dotted line), and upper and lower standard deviation curves 
#along the lambda sequence (error bars). Two selected lambda's are indicated by the vertical dotted lines.
#First is lambda.min - is the value of lambda that gives minimum mean cross-validated error
#Second is lambda.1se - which gives the most regularized model such that error is within one standard error of the minimum

coef(reglrfit, s = "lambda.1se")

mat_test = data.matrix(test_data)
mat_test= mat_test[,-1:-2]
y_test = factor(test_data$Fail, levels=c(0,1), labels=c(-1,1))
pred = predict(reglrfit, newx = mat_test, s = "lambda.min", type="class")
conf_mat2 = table(pred, y_test)
test_accuracy1 = sum(diag(conf_mat))/sum(conf_mat)
test_accuracy2 = sum(diag(conf_mat2))/sum(conf_mat2)
