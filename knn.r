library(caret)
path <- getwd()
csv_readed = read.csv(file=paste(path,"/parsed.csv",sep = ""))
csv_readed$income = factor(csv_readed$income)

crossValidationByCaret <- function(x)  {
  train_control <- trainControl(method="cv", number=x)
  model <- train(data=csv_readed,income~.,  method = "knn", trControl = train_control)
  return(model)
}

crossValidationByCaret(20)

classifyKnn <- function(model, input, k) {
  for(row in model[1:14]){
    print(row)
  }
}

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

calculateDistance <- function(x1, x2){
  d = 0
  for(i in c(1:(length(x1)-1) ))
  {
    d = d + (x1[[i]]-x2[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

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

getAccuracyFromCM<-function(confMatrix){
  true <- confMatrix[1]+confMatrix[3]
  total <- confMatrix[1]+confMatrix[2]+confMatrix[3]+confMatrix[4]
  return(true/total)
}
myKnn <- function(training, test, k){
  myconfusionMatrix <- c(0, 0, 0, 0)
  training = training[1:2000,]
  test = test[1:1000,]
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

res <- myHoldOut(csv_readed, 0.75, 123)
training <- res[[1]]
test <- res[[2]]
confMatrix <- myKnn(training, test,9)
accuracy <- getAccuracyFromCM(confMatrix)
accuracy
