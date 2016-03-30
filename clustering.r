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
plot(15:26,cost.withinss[15:26], type='b', xlab="K-values")

hcluster = hclust(dist(cluster$centers), method="average")
plot(hcluster)
conf_mat = table(alphabet[,1], cluster$cluster)
cluster.labels = rownames(conf_mat)[apply(conf_mat,2,which.max)]
plot(hcluster, labels = cluster.labels, xlab="Clusters")

#j,k missing
cluster.labels[which.max(conf_mat[7,])]
cluster.labels[which.max(conf_mat[11,])]



#On doing the following, we observe that either 3 or 4 clusters makes maximum sense
#This is because, on splitting so, the clusters would have had the maximum distance between them
#at the time of agglomeration, which means they were more distinct as distance corresponds to the dissimilarity
#Thus cluster1 = m:r, cluster2=l,x, cluster3=t:j, cluster4=l:z or c1=m:x, c2=t:j, c3=l:z 
counts = sapply(15:26,function(ncl)table(cutree(hcluster,ncl)))
names(counts) = 15:26
counts
