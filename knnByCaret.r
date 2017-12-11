library(caret)
path <- "/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors"
crossValidationByCaret <- function(data_set, class, fold)  {
  train_control <- trainControl(method="cv", number=fold)
  model <- train(data=data_set,income~.,  method = "knn", trControl = train_control)
  return(model)
}

classifyKnn <- function(model, input, k) {
  for(row in model[1:14]){
    print(row);
  }
}

file_ <- paste(path,"/parsed.csv",sep="")
csv_readed <- read.csv(file=file_)
csv_readed$income <- factor(csv_readed$income)
model <- crossValidationByCaret(csv_readed, income, 20)
model
