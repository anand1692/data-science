houseType = read.csv("housetype_data.csv")
fix(houseType)
dim(houseType)
houseType[1:5, 1:5]
attributes = names(houseType)
attach(houseType);
attributeHist <- function(attr) {
  if (!attr %in% attributes) {
    return(message("No such attribute in the table"))
  }
  count_na = sum(is.na(houseType[,attr]))
  hist(houseType[,attr], main=paste("Histogram of", attr), xlab=attr, col="red")
  if (count_na > 0)
    message(paste(count_na, "missing values"));
}
par(mfrow=c(1,1));
attributeHist("age")
attributeHist("hello")
attributeHist("eth")

