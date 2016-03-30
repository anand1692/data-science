install.packages("gdata")
library(gdata)
diamond = read.xls("Diamond_data/Diamond_Data.xls", sheet = 1, header = TRUE)
attach(diamond)

#removing the ID column
diamond = diamond[,-1]

diamond[,"Cut"] = as.factor(diamond[,"Cut"])
diamond[,"Color"] = as.factor(diamond[,"Color"])
diamond[,"Clarity"] = as.factor(diamond[,"Clarity"])
diamond[,"Polish"] = as.factor(diamond[,"Polish"])
diamond[,"Symmetry"] = as.factor(diamond[,"Symmetry"])
diamond[,"Report"] = as.factor(diamond[,"Report"])

plot(diamond[,"Cut"], main="Cut Ratings", ylim = c(0,3000))
plot(diamond[,"Price"])

d <- density(diamond[,"Price"])
plot(d, type="n", main="Price")
polygon(d, col="red", border="gray")

platform = "mac"
rfhome = "~/Documents/DataMining/R/Assign4/Rulefit"
source("~/Documents/DataMining/R/Assign4/Rulefit/rulefit.r")
install.packages("akima", lib=rfhome)
library(akima, lib.loc=rfhome)

set.seed(100)
ind = sample(nrow(diamond), 5000)
train_data = diamond[ind,]
test_data = diamond[-ind,]

#For training attributes, we remove the price column from the training data
#On increasing(doubling) the reps and fract, the test error came down (~740.65)
#When the reps was made 3 times, the error started increasing
cat_vars = c("Cut", "Color", "Clarity", "Polish", "Symmetry", "Report")
rfmodel = rulefit(train_data[,-8], train_data$Price, rfmode="regress", 
                  cat.vars=cat_vars, test.reps=10, test.fract=0.1)
rfmodinfo(rfmodel)
rules(beg=1, end=10)

#The three most important variables with their importance in determining diamond price are: 
#Carat Weight(100), Clarity(46.5241) & Color(43.6308)
vi = varimp(plot=T)
names(train_data)[c(vi$ord[1:3])]

#Average Absolute error ~766.37408
pred = rfpred(test_data[,-8])
avg.abs.error = sum(abs(test_data$Price - pred))/(nrow(test_data))

#Fitting the training data to a decision tree 
library(rpart)
treefit = rpart(Price ~., data = train_data, control = rpart.control(cp=0.0001))
printcp(treefit)
min_cp = treefit$cptable[which.min(treefit$cptable[,"xerror"]),"CP"]

set.seed(101)
ptree = prune(treefit,cp=min_cp)
plot(ptree, uniform=TRUE, main="Price of Diamond", margin=0.1)
text(ptree, all=TRUE, cex=.7, pretty=0)

#Average Absolute error ~1126.71144
predTree = predict(ptree, test_data)
avg.abs.error.tree = sum(abs(test_data$Price - predTree))/(nrow(test_data))

#We clearly observe that the ensemble method predicts with lower error than simple decision tree
