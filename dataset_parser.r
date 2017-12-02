wd <- getwd()
dataset_path <- paste(wd,"/adult.csv",sep="")
csv_data <- read.csv(file=dataset_path,sep=",",header = TRUE, stringsAsFactors = TRUE)
new_header <- colnames(csv_data[2,])
head(csv_data)
for (i in 1:length(csv_data)){
  csv_data[,i] = as.numeric(csv_data[,i])
}
print(csv_data[,15])
write.csv(csv_data, file="./parsed.csv", sep = ',',row.names = FALSE)
