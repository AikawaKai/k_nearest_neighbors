library(caret)
library(parallel)
library(Rcpp)
require(stats)
path <- "/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors"

# basic hold out
myHoldOut<-function(csv_readed, perc){
  ## 75% of the sample size
  smp_size <- floor(perc * nrow(csv_readed))
  train_ind <- sample(seq_len(nrow(csv_readed)), size = smp_size)
  training <- csv_readed[train_ind,]
  test <- csv_readed[-train_ind,]
  return(list(training, test))
}

# extract from csv training and test with split=perc_train
getTrainingTestHoldOutFromCsv <- function(file_, perc_train){
  csv_readed <- read.csv(file=file_)
  #csv_readed$income = factor(csv_readed$income)
  return(myHoldOut(csv_readed, perc_train))
}

# euclidean distance
calculateDistance <- function(x1, x2){
  d <- 0
  for(i in c(1:(length(x1)-1) ))
  {
    d <- d + (x1[[i]]-x2[[i]])^2
  }
  d <- sqrt(d)
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
  training <- training
  test <- test
  list_ <- list()
  for (i in 1:nrow(test)){
    distances <- c()
    classes <- c()
    for(j in 1:nrow(training)){
      d <- calculateDistance(test[i,], training[j,])
      distances <- c(distances, d)
      classes <- c(classes, training[j,][[15]])
    }
    df_res <- data.frame(distances, classes)
    df_res <- df_res[order(df_res$distances),]
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
    myconfusionMatrix[res] <- myconfusionMatrix[res]+1
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
    index1 <- 1
    index2 <- 2
  }else{
    index1 <- 3
    index2 <- 4
  }
  Tp <- confMatrix[index1]
  Fp <- confMatrix[index2]
  return(Tp/(Tp+Fp))
}

# faster implementation with CPP
myKnnWithCpp <- function(training, test, k){
  sourceCpp(paste(path,"/search.cpp", sep=""))
  train <- as.matrix(training)
  test <- as.matrix(test)
  myRes <- searchCpp(train, test, k)
  return(myRes)
}

# basic split for crossvalidation 3
getTrainingTestCrossValidation3 <- function(data, k_){
  num_el <- nrow(data)/k_
  start <- 1
  index <- list(k_)
  index[[1]]<-seq(start, num_el, by=1)
  start <- start + num_el
  for (i in seq(2,k_-1, by = 1)){
    prec <- start 
    start <- start + num_el 
    start <- start + 1
    index[[i]] <- seq(prec, start, by=1)
  }
  index[[k_]] <- seq(start+1, nrow(data), by=1)
  trainings <- list(k_)
  tests <- list(k_)
  for(i in 1:k_){
    tests[[i]] <- data[index[[i]],]
    trainings[[i]] <- data[-index[[i]],]
  }
  return(list(trainings, tests))
}

# basic split for crossvalidation 2
getTrainingTestCrossValidation2 <- function(data, k_){
  #Create k_ equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=k_,labels=FALSE)
  trainings <- list(k_)
  tests <- list(k_)
  for(i in 1:k_){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    tests[[i]] <- testData
    trainings[[i]] <- trainData
    #Use the test and train data partitions however you desire...
  }
  return(list(trainings, tests))
}

# get k fold for cross validation with stratified sampling
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
externalCrossValidationWithInnerOptimization<-function(data, myK, k_fold){
  ret <- getTrainingTestCrossValidation3(data, k_fold)
  train_fold <- ret[[1]]
  test_fold <- ret[[2]]
  max_k <- list(k_fold)
  accuracy_x_fold <- list(k_fold)
  for(i in 1:k_fold){
    curr_train <- train_fold[[i]]
    curr_test <- test_fold[[i]]
    ret <- getTrainingTestCrossValidation3(curr_train, k_fold)
    inner_train <- ret[[1]]
    inner_test <- ret[[2]]
    total_res <- list(length(myK))
    l <- 1
    for(k in myK){
      res_k <- list(k_fold)
      for(j in 1:k_fold){
        confusionMatrix <- myKnnWithCpp(inner_train[[j]], inner_test[[j]], k)
        acc <- getAccuracyFromCM(confusionMatrix)
        res_k[[j]] <- acc
      }
      total_res[[l]] <- mean(as.numeric(res_k), na.rm = TRUE)
      l<-l+1
    }
    plotMyResults(myK,total_res, paste("fold_",toString(i), sep=""))
    sel_k <- as.numeric(myK[which.max(total_res)])
    max_k[[i]]<-sel_k
    confusionMatrix <- myKnnWithCpp(curr_train, curr_test, sel_k)
    fold_acc <- getAccuracyFromCM(confusionMatrix)
    accuracy_x_fold[[i]] <- fold_acc
  }
  print(max_k)
  print(accuracy_x_fold)
  return(mean(as.numeric(accuracy_x_fold)))
}

# inner cross validation
innerCrossValidation <- function(training, k_fold, myK, i){
  ret <- getTrainingTestCrossValidation3(training, k_fold)
  trains <- ret[[1]]
  tests <- ret[[2]]
  total_res <- list(length(myK))
  l <- 1
  for(k in myK){
    res_k <- list(k_fold)
    for(j in 1:k_fold){
      confusionMatrix <- myKnnWithCpp(trains[[j]], tests[[j]], k)
      acc <- getAccuracyFromCM(confusionMatrix)
      res_k[[j]] <- acc
    }
    fileConn<-paste(path,"/output2.txt", sep = "")
    lapply((unlist(lapply(res_k, paste, collapse=" "))), cat, file=fileConn, append=TRUE,sep="\n")
    total_res[[l]] <- mean(as.numeric(res_k), na.rm = TRUE)
    l<-l+1
  }
  plotMyResults(myK,total_res,i)
  return(as.numeric(myK[which.max(total_res)]))
}

# parallel implementation for cross validation with inner optimization
parallelExternalCrossValidationWithInnerOptimization <- function(data, myK, k_fold){
  ret <- getTrainingTestCrossValidation3(data, k_fold)
  trainings <- ret[[1]]
  tests <- ret[[2]]
  no_cores <- detectCores() -1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("path", "myK", "plotMyResults", "getPrecisionFromCM", "innerCrossValidation","getTrainingTestCrossValidation3", "createFolds", "myKnnWithCpp", "sourceCpp", "getAccuracyFromCM"))
  res_k <- parLapply(cl, 1:k_fold, function(x) c(innerCrossValidation(trainings[[x]], k_fold, myK, x)))
  fileConn<-paste(path,"/output2.txt", sep = "")
  lapply((unlist(lapply(res_k, paste, collapse=" "))), cat, file=fileConn, append=TRUE,sep="\n")
  final_res <- parLapply(cl, 1:k_fold, function(x) c(myKnnWithCpp(trainings[[x]], tests[[x]], res_k[[x]])))
  final_res <-lapply(final_res, getAccuracyFromCM)
  lapply(unlist(lapply(final_res, paste, collapse=" ")), cat, file=fileConn, append=TRUE, sep="\n")
  stopCluster(cl)
  return(mean(as.numeric(final_res)))
}

# plotting data
plotMyResults <- function(x, y, name){
  png(filename = paste(name,".png", sep = ""), res = 100, height = length(y)*25, width = length(x)*25*2)
  plot(x = x, y = y, type="o", xaxt = "n", yaxt="n",  xlab = "K", 
       ylab = "test_error")
  points(x = x, y = y, col = "dark red")
  axis(tck=-0.08, cex.axis=0.7, at = x, side = 1)
  axis(tck=-0.08, side = 2, cex.axis=0.7)
  dev.off()
}