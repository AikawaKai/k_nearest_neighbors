source("/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors/knn_library.r")

file <- paste(path,"/parsed.csv",sep="")
csv_readed <- read.csv(file=file)
csv_readed <- csv_readed[1:5000,]
myK <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45)
set.seed(1)
S <- matrix(nrow=0, ncol=15)
#system.time(err<-sequentialKnn(S, csv_readed, 7))
system.time(res2 <- parallelExternalCrossValidationWithInnerOptimizationSeq(csv_readed, myK, 5))
print(res2)
