alphabet = read.table("az-5000.txt", header = TRUE);
alphabet[1,]
alphabet[5000,]
dim(alphabet)
set.seed(1);
ind = sample(nrow(alphabet), 4000);
train_data = alphabet[ind,]
fix(train_data)
test_data = alphabet[-ind,]
table(train_data$char)
priors = rep(c(1/26), times = 26)
library(MASS)
fit = lda(char~., train_data, prior = priors);
conf_mat = table(test_data$char, predict(fit, test_data)$class, dnn = c("Predicted", "Observed"))
#true_pos = apply(conf,2,max)
true_pos = diag(conf_mat)
which.max(true_pos/table(test_data$char))
which.min(true_pos/table(test_data$char))
conf_mat_train = table(train_data$char, predict(fit, train_data)$class, dnn = c("Predicted", "Observed"))
train_accuracy = sum(diag(conf_mat_train))/nrow(train_data)
test_accuracy = sum(true_pos)/nrow(test_data)
