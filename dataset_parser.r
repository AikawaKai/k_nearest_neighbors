


filterRowWithUnknow<-function(file){
  h <- head(file[0,])
  for(r in 1:nrow(file)){
    row <- file[r,]
    
  }
}

wd <- getwd()
path <- "/home/kai/Documents/Unimi/MetodiStatisticiApp/k_nearest_neighbors/"
dataset_path <- paste(path,"/adult.csv",sep="")
csv_data <- read.csv(file=dataset_path,sep=",",header = TRUE, stringsAsFactors = TRUE)
filterRowWithUnknow(csv_data)



test <- csv_data$workclass
values <- test[test!='?']
m <- mean(as.numeric(values))
"new_header <- colnames(csv_data[2,])
head(csv_data)
for (i in 1:length(csv_data)){
  csv_data[,i] = as.numeric(csv_data[,i])
}
print(csv_data[,15])"
#write.csv(csv_data, file="./parsed.csv", sep = ',',row.names = FALSE)
