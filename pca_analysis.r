file <- paste(path,"/parsed.csv",sep="")
csv_readed <- read.csv(file=file)
features <- csv_readed[,1:14]
pca <- prcomp(features, center = TRUE, scale. = TRUE)
plot(pca$x[,13:14], col=csv_readed[,15])
ncol(pca$x)
