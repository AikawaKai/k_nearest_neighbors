source("/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors/knn_library.r")

file <- paste(path,"/parsed.csv",sep="")
csv_readed <- read.csv(file=file)
csv_readed <- csv_readed[1:200,]
csv_readed <- csv_readed[sample(nrow(csv_readed)),]
myK <- seq(1, 103, by=2)
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
plotMyMisclassified(seq(1,length(train_err)), count_err, "misclassified_final")
