library(caret)
path <- "/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors"
crossValidationByCaret <- function(data_set, class, fold)  {
  train_control <- trainControl(method="cv", number=fold, returnResamp = "all")
  model <- train(data=data_set,income~.,  method = "knn", trControl = train_control, tuneLength = 26)
  return(model)
}

file_ <- paste(path,"/parsed_normalized.csv",sep="")
csv_readed <- read.csv(file=file_)
csv_readed$income <- factor(csv_readed$income)
model <- crossValidationByCaret(csv_readed, income, 5)
model
