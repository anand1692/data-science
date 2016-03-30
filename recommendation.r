install.packages("recommenderlab")
library(recommenderlab)
ratings = scan("Movies_data/ratings.txt", what=list(userid=0, movieid=0, rating=0), sep="|")
#ratings.mat = lapply(ratings, as, "sparseMatrix")
#ratings.mat = do.call(cBind, ratings.mat)
#First 5 rows of the sparse matrix formed
#ratings.mat[1:5,]

ylabel = paste("u", 1:10000, sep="")
xlabel = paste("m", 1:7223, sep="")
ratings.mat = sparseMatrix(i=ratings[[1]], j=ratings[[2]], x=ratings[[3]], dimnames=list(ylabel, xlabel))
dim(ratings.mat)
ratings.mat = new("realRatingMatrix", data=ratings.mat)
rec.model = Recommender(ratings.mat, method="UBCF")

# "m3084" "m2242" "m2584" "m2313" "m538" are the top 5 movies for user 10000
rec.movies.u10000 = predict(rec.model, ratings.mat[10000,], n=5)
as(rec.movies.u10000, "list")

# "m4349" - highest rated movie by user 500
rec.movies.u500 = predict(rec.model, ratings.mat[500,], n=1)
as(rec.movies.u500, "list")
