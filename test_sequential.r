source("/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors/knn_library.r")

file <- paste(path,"/parsed.csv",sep="")
csv_readed <- read.csv(file=file)
myK <- c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)
set.seed(1)
system.time(err<-sequentialKnn(csv_readed, 3))
print(err/nrow(as.matrix(csv_readed)))
