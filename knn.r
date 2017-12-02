library(caret)
path <- getwd()
csv_readed = read.csv(file=paste(path,"/parsed.csv",sep = ""))
csv_readed$income = factor(csv_readed$income)

crossValidationByCaret <- function(x)  {
  train_control <- trainControl(method="cv", number=10)
  model <- train(data=csv_readed,income~.,  method = "knn", trControl = train_control)
  return(model)
}

crossValidationByCaret()

