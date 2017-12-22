source("/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors/knn_library.r")

file <- paste(path,"/parsed.csv",sep="")
csv_readed <- read.csv(file=file)
csv_readed <- csv_readed[1:4000,]
pca <- FALSE
num_feat <- 5
#set.seed(1)
myK <- seq(1, 61, by=2)
system.time(res2 <- parallelExternalCrossValidationWithInnerOptimization(csv_readed, myK, 5, pca=pca, num_feat=num_feat))
print("Expected accuracy with k selection: ")
print(res2)
system.time(bestk <- selectBestKByCrossValidation(csv_readed, myK, 5, pca=pca, num_feat=num_feat))
print("Best final k selected: ")
print(bestk)
