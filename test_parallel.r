source("/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors/knn_library.r")

file <- paste(path,"/parsed.csv",sep="")
csv_readed <- read.csv(file=file)
csv_readed <- csv_readed[1:400,]
#csv_readed$income <- factor(csv_readed$income)
ret <- getTrainingTestCrossValidation(csv_readed, 5)
trainings <- ret[[1]]
tests <- ret[[2]]
myK <- c(1, 3, 5, 7, 9)
set.seed(1)
system.time(res2 <- parallelExternalCrossValidationWithInnerOptimization(trainings, tests, myK, 5))
print(res2)

