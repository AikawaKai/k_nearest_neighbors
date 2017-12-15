source("/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors/knn_library.r")

file <- paste(path,"/parsed.csv",sep="")
csv_readed <- read.csv(file=file)
csv_readed <- csv_readed[1:4000,]
myK <- c(1, 3, 5, 7, 9)
set.seed(1)
system.time(res2 <- parallelExternalCrossValidationWithInnerOptimization(csv_readed, myK, 3))
print(res2)

