source("/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors/knn_library.r")

file <- paste(path,"/parsed.csv",sep="")
csv_readed <- read.csv(file=file)
csv_readed <- csv_readed[1:20000,]
csv_readed <- csv_readed[sample(nrow(csv_readed)),]
myK <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45)
set.seed(1)
S <- matrix(nrow=0, ncol=15)
#system.time(err<-sequentialKnn(S, csv_readed, 7))
system.time(res2 <- parallelExternalCrossValidationWithInnerOptimizationSeq(csv_readed, myK, 5))
print(res2)
system.time(bestK<-selectBestKByCrossValidationSeq(csv_readed, myK, 5))
print(bestK)
myRes <- sequentialKnn(S=matrix(nrow = 0, ncol=15), csv_readed, bestK) #training
train_err <- myRes[[1]] # to plot
S<-myRes[[2]]
nrow(S)
length(train_err)
count_err <- myRes[[3]]
plotMySeqRisk(seq(1, length(train_err)), train_err, "seq_risk_final")
plotMySeqRisk(seq(1,length(train_err)), count_err, "misclassified_final")
