alphabet = read.table("az-5000.txt", header=TRUE);
ind = sample(nrow(alphabet), 0.8*nrow(alphabet));
train_data = alphabet[ind,]
test_data = alphabet[-ind,]
dim(alphabet)
attach(alphabet)

mat_train = matrix(0, nrow=4000, ncol=26)
mat_test = matrix(0, nrow = 1000, ncol=26)
for(i in 1:4000) {
  if(i <= 1000)
    mat_test[i, as.numeric(test_data$char[i])] = 1
  
  mat_train[i, as.numeric(train_data$char[i])] = 1
}
sum(mat_train == 1)
sum(mat_test == 1)
library(nnet)
set.seed(1)

error_train = c(0)
error_test = c(0)
prev_err_test = 100

for(i in 1:20) {
  model_nnet = nnet(char~., train_data, maxit = 1000, size = i)
  error_train = cbind(error_train, 1/4000 * sum((mat_train - model_nnet$fitted.values)^2))
  error_test = cbind(error_test, 1/4000 * sum((mat_test - predict(model_nnet, test_data))^2))
  if(error_test[i+1] < prev_err_test) {
    best_model = model_nnet
    prev_err_test = error_test[i+1]
  }
}
error_train = error_train[-1]
error_test = error_test[-1]
plot(error_train, type='l', col="red", ylim=c(0,0.4), ylab="Error")
lines(error_test, type='l', col="blue", ylim=c(0,0.4), ylab="Error")
points(which(error_test==prev_err_test), prev_err_test, pch=4, col="black")
legend("topright", c("Train Error", "Test Error"), lty=c(1,1), lwd=c(2,2), col = c("red", "blue"))

conf_mat_train = table(train_data$char, predict(best_model, train_data, type="class"))
conf_mat_test = table(test_data$char, predict(best_model, test_data, type="class"))

accuracy_train = sum(diag(conf_mat_train))/sum(conf_mat_train)
accuracy_test = sum(diag(conf_mat_test))/sum(conf_mat_test)
