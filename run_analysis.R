setwd("C:\\All Work\\Box Sync\\2015\\Analytics Engineer Course\\Getting and Cleaning Data\\Course Project")
namedata = read.table(
  "getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\features.txt", 
  sep=" ")
xtrain = read.table(
  "getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\X_train.txt", 
                sep="",fill=TRUE)
names(xtrain) <- namedata[,2]
ytrain = read.table(
  "getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\Y_train.txt", 
  sep=" ")
subjtrain = read.table(
  "getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\subject_train.txt", 
  sep=" ")
datasettrain <- cbind(ytrain,subjtrain,xtrain)
names(datasettrain)[1:2]=c("Activity Code","Subject")

xtest = read.table(
  "getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\X_test.txt", 
  sep="",fill=TRUE)
names(xtest) <- namedata[,2]
ytest = read.table(
  "getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\Y_test.txt", 
  sep=" ")
subjtest = read.table(
  "getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\subject_test.txt", 
  sep=" ")
datasettest <- cbind(ytest,subjtest,xtest)
names(datasettest)[1:2]=c("Activity Code","Subject")

dataset <- rbind(datasettrain, datasettest)

meancols <- namedata[grep('mean\\(\\)',names(dataset)),1]
stdcols <- namedata[grep('std\\(\\)',names(dataset)),1]

finaldataset <- cbind(dataset[,1:2],dataset[,meancols], dataset[,stdcols])

labels = read.table(
  "getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\activity_labels.txt", 
  sep=" ")


finaldataset['Activity Label'] <- factor(finaldataset$`Activity Code`, labels=labels[,2])
variables <- names(dataset)[c(meancols, stdcols)]
activities <- as.vector(labels[,2])
subjects <- as.vector(data.frame(table(finaldataset$Subject))[,1])
datalength <- length(activities)*length(subjects)*length(variables)
submitdata <- data.frame(Variable=rep(variables,length(subjects)*length(activities)), 
                         Subject=rep(subjects, length(variables)*length(activities)), 
                         Activity=rep(activities, length(subjects)*length(variables)),
                         Average=rep(0,datalength)
                         )

submitdata$Average <- 0
num = nrow(submitdata)

for (i in 1:num){
  col = as.vector(submitdata[,'Variable'])[i]
  datat = finaldataset[finaldataset$`Activity Label`== submitdata[i,'Activity'] & finaldataset$`Subject`== submitdata[i,'Subject'],col]
  nr <- length(datat)
  mn <- mean(datat)
  submitdata[i,'Average'] <-mn
}
write.table(submitdata, 'submit.txt',row.name=FALSE)
head(submitdata)

