source("/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors/knn_library.r")

file <- paste(path,"/parsed_normalized.csv",sep="")
csv_readed <- read.csv(file=file)
features <- csv_readed[,1:88]
pca <- prcomp(features, center = TRUE, scale. = TRUE)
plot(pca$x[,1:2], col=csv_readed[,89])
ncol(pca$x)
