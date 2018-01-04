source("/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors/knn_library.r")

file <- paste(path,"/parsed_normalized.csv",sep="")
csv_readed <- read.csv(file=file)
csv_readed <- csv_readed[1:2000,]
#set.seed(100)
#csv_readed <- csv_readed[sample(nrow(csv_readed)),]
pca <- FALSE
num_feat <- 5
myK <- seq(1, 41, by=2)
S <- matrix(nrow=0, ncol=ncol(csv_readed))
#system.time(err<-sequentialKnn(S, csv_readed, 7))
system.time(res2 <- parallelExternalCrossValidationWithInnerOptimizationSeq(csv_readed, myK, 5, pca=pca, num_feat=num_feat))
print("Expected accuracy with k selection (seq): ")
print(res2)
system.time(bestK<-selectBestKByCrossValidationSeq(csv_readed, myK, 5, pca = pca, num_feat = num_feat))
print("Best final k selected: ")
print(bestK)
myRes <- sequentialKnn(S=matrix(nrow = 0, ncol=ncol(csv_readed)), csv_readed, bestK, pca = pca, num_feat = num_feat) #training
train_err <- myRes[[1]] # to plot
S<-myRes[[2]]
print("final S size: ")
print(nrow(S))
print("S/DATASET = ")
print(nrow(S)/length(train_err))
count_err <- myRes[[3]]
plotMySeqRisk(seq(1, length(train_err)), train_err, "seq_risk_final")
plotMyMisclassified(seq(1,length(train_err)), count_err, "misclassified_final")
