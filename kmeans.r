alphabet = read.table("az-5000.txt", header=TRUE)
View(alphabet)

cost.withinss = c(0);
for(i in 2:26 ){
  cluster = kmeans(alphabet[,-1], centers=i)
  cost = cluster$tot.withinss/i
  cost.withinss = cbind(cost.withinss, cost)
}

cost.withinss = cost.withinss[-1]
plot(cost.withinss, xlab="K-values")
plot(cost.withinss[15:26], xlab="K-values")
