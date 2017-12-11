library(caret)
# View(class::knn)
library(Rcpp)
path <- "/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors"

# hold out from table with split=perc
myHoldOut<-function(csv_readed, perc, seed){
  ## 75% of the sample size
  smp_size <- floor(perc * nrow(csv_readed))
  smp_size
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(csv_readed)), size = smp_size)
  training = csv_readed[train_ind,]
  test = csv_readed[-train_ind,]
  return(list(training, test))
}

# extract from csv training and test with split=perc_train
getTrainingTestHoldOutFromCsv <- function(file_, perc_train){
  csv_readed = read.csv(file=file_)
  #csv_readed$income = factor(csv_readed$income)
  return(myHoldOut(csv_readed, perc_train, 123))
}

# euclidean distance
calculateDistance <- function(x1, x2){
  d = 0
  for(i in c(1:(length(x1)-1) ))
  {
    d = d + (x1[[i]]-x2[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

# check if TP, FP, TN Or FN
checkTpFpTnFn<-function(res, expected){
  if(res==1){
    if(res==expected){
      return(1)
    }else{
      return(2)
    }
  }else{
    if(res==expected){
      return(3)
    }else{
      return(4)
    }
  }
}

# slow implementation
myKnn <- function(training, test, k){
  myconfusionMatrix <- c(0, 0, 0, 0)
  training = training[1:4000,]
  test = test[1:2000,]
  list_ = list()
  for (i in 1:nrow(test)){
    distances <- c()
    classes <- c()
    for(j in 1:nrow(training)){
      d = calculateDistance(test[i,], training[j,])
      distances <- c(distances, d)
      classes <- c(classes, training[j,][[15]])
    }
    df_res = data.frame(distances, classes)
    df_res = df_res[order(df_res$distances),]
    k_classes <- df_res[1:k,2]
    count_1 <- 0
    count_2 <- 0
    for (m in 1:k){
      if(as.integer(k_classes[[m]])==1){
        count_1 <- count_1 + 1
      }else{
        count_2 <- count_2 + 1
      }
    }
    if(count_1>count_2){
      res <- checkTpFpTnFn(as.integer(test[i,15]),1)
    }else{
      res <- checkTpFpTnFn(as.integer(test[i,15]),2)
    }
    myconfusionMatrix[res] = myconfusionMatrix[res]+1
  }  
  return(myconfusionMatrix)
}


# get accuracy from confusion matrix
getAccuracyFromCM<-function(confMatrix){
  true <- confMatrix[1]+confMatrix[3]
  total <- confMatrix[1]+confMatrix[2]+confMatrix[3]+confMatrix[4]
  return(true/total)
}

getPrecisionFromCM<-function(confMatrix, class_){
  if(class_==1){
    index1 = 1
    index2 = 2
  }else{
    index1 = 3
    index2 = 4
  }
  Tp <- confMatrix[index1]
  Fp <- confMatrix[index2]
  return(Tp/(Tp+Fp))
}

# faster implementation with CPP
myKnnWithCpp <- function(training, test, k){
  sourceCpp(paste(path,"/search.cpp", sep=""))
  train = as.matrix(training)
  test = as.matrix(test)
  myRes <- searchCpp(train, test, k)
  return(myRes)
}

getTrainingTestCrossValidation <- function(data, k_){
  folds <- createFolds(factor(data$income), k = k_)
  trainings <- list(k_)
  tests <- list(k_)
  for(i in 1:k_){
    index_test <- unname(unlist(folds[i]))
    test <-data[index_test,]
    training <- data[-index_test,]
    tests[[i]] <- test
    trainings[[i]] <- training
  }
  return(list(trainings, tests))
}

externalCrossValidationWithInnerOptimization<-function(trainings, tests, myK, k_fold){
  for(i in 1:k_fold){
    training <- trainings[[i]]
    test <- tests[[i]]
    for(k in myK){
      ret <- getTrainingTestCrossValidation(training, k_fold)
      trains <- ret[[1]]
      tests <- ret[[2]]
      print("###### Started Inner Cross Validation ######\n")
      for(j in 1:k_fold){
        confusionMatrix <- myKnnWithCpp(trains[[j]], tests[[j]], k)
        acc <- getAccuracyFromCM(confusionMatrix)
        prec1 <- getPrecisionFromCM(confusionMatrix, 1)
        prec2 <- getPrecisionFromCM(confusionMatrix, 2)
        print(acc)
        print(prec1)
        print(prec2)
      }
    }
  }
}
# main
file <- paste(path,"/parsed.csv",sep="")
res <- getTrainingTestHoldOutFromCsv(file, 0.75)
training <- res[[1]]
test <- res[[2]]
confusionMatrix <- myKnnWithCpp(training, test, 9)
getAccuracyFromCM(confusionMatrix)
csv_readed <- read.csv(file=file)
#csv_readed$income <- factor(csv_readed$income)
ret <- getTrainingTestCrossValidation(csv_readed, 5)
trainings <- ret[[1]]
tests <- ret[[2]]
myK <- c(3, 5, 7, 9,11)
res <- externalCrossValidationWithInnerOptimization(trainings, tests, myK, 5)

