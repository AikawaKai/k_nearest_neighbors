wd <- getwd()
dataset_path <- paste(wd,"/adult.csv",sep="")
csv_data <- read.csv(file=dataset_path,sep=",",header = TRUE, stringsAsFactors = TRUE)
(csv_data[3,3])
for (i in 1:length(csv_data)){
  csv_data[,i] = as.numeric(csv_data[,i])
  print(i)
}
print(csv_data[,3][48842])
