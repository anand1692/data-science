college = read.csv("College.csv")
dim(college)
fix(college)
rownames(college) = college[,1]
fix(college)
college = college[,-1]
fix(college)
summary(college)
pairs(college[,5:10])
attach(college)
plot(Outstate, Private, col="red")
Elite = rep("No", nrow(college))
Elite[Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
plot(Outstate, Elite, col="blue")
par(mfrow=c(2,2));
hist(Apps, col=2, breaks=10)
hist(Top10perc, col=5, breaks = 10)
hist(Accept, col=3)
hist(perc.alumni, col=4)
ind = which(college$Top10perc == max(college$Top10perc, na.rm=TRUE), arr.ind = TRUE)
maxTop10perc = rownames(college)[ind]
ind2 = which(college$Accept == min(college$Accept, na.rm = TRUE), arr.ind = TRUE)
minAccept = rownames(college)[ind2]
