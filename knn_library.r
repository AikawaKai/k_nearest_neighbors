library(caret)
library(parallel)
library(Rcpp)
path <- "/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors"
# View(class::knn)
set.seed(1)
# basic hold out
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
  training = training
  test = test
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

#get precision from confusion matrix
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

# get k fold for cross validation
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

# cross validation with inner optimization and validation
externalCrossValidationWithInnerOptimization<-function(trainings, tests, myK, k_fold){
  max_k <- list(k_fold)
  for(i in 1:k_fold){
    training <- trainings[[i]]
    test <- tests[[i]]
    total_res <- list(length(myK))
    l <- 1
    for(k in myK){
      ret <- getTrainingTestCrossValidation(training, k_fold)
      trains <- ret[[1]]
      tests <- ret[[2]]
      res_k <- list(k_fold)
      for(j in 1:k_fold){
        confusionMatrix <- myKnnWithCpp(trains[[j]], tests[[j]], k)
        acc <- getAccuracyFromCM(confusionMatrix)
        res_k[[j]] <- acc
      }
      total_res[[l]] <- mean(as.numeric(res_k), na.rm = TRUE)
      l<-l+1
    }
    max_k[[i]]<-as.numeric(myK[which.max(total_res)])
  }
  return(max_k)
}

# inner cross validation
innerCrossValidation <- function(training, k_fold, myK){
  ret <- getTrainingTestCrossValidation(training, k_fold)
  trains <- ret[[1]]
  tests <- ret[[2]]
  total_res <- list(length(myK))
  l <- 1
  for(k in myK){
    ret <- getTrainingTestCrossValidation(training, k_fold)
    trains <- ret[[1]]
    tests <- ret[[2]]
    res_k <- list(k_fold)
    for(j in 1:k_fold){
      confusionMatrix <- myKnnWithCpp(trains[[j]], tests[[j]], k)
      acc <- getAccuracyFromCM(confusionMatrix)
      res_k[[j]] <- acc
    }
    total_res[[l]] <- mean(as.numeric(res_k), na.rm = TRUE)
    l<-l+1
  }
  return(as.numeric(myK[which.max(total_res)]))
}

# parallel implementation for cross validation with inner optimization
parallelExternalCrossValidationWithInnerOptimization <- function(trainings, tests, myK, k_fold){
  no_cores <- detectCores() -1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("getPrecisionFromCM", "myK", "innerCrossValidation", "getTrainingTestCrossValidation", "createFolds", "myKnnWithCpp", "sourceCpp", "path", "getAccuracyFromCM"))
  results <- list(k_fold)
  res <- parLapply(cl, trainings, function(x) c(innerCrossValidation(x, k_fold, myK)))
  return(res)
}
