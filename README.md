# getcleandata
The script run_analysis.R reads the data from the different text files and calculates the averages as required in the question and stores them in the file submit.txt
Working directory must be set as the one where the Samsung data folder resides.
The file submit.txt has four columns, the first row indicates the variable names and the subsequent rows have the values.
The variables names are described in the file codebook.txt

#Working of the Script

##Reading the names of the variables
namedata = read.table(
  "getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\features.txt", 
  sep=" ")
  
##Reading the Training Data set  
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
  
##Adding the columns on Acitivty and Subject for the Training Data
datasettrain <- cbind(ytrain,subjtrain,xtrain)
names(datasettrain)[1:2]=c("Activity Code","Subject")

##Reading the Training Data set  
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
  
##Adding the columns on Acitivty and Subject for the Training Data
datasettest <- cbind(ytest,subjtest,xtest)
names(datasettest)[1:2]=c("Activity Code","Subject")

##Combine the Training Data and the Test Data
dataset <- rbind(datasettrain, datasettest)

##Get the variables which relate to mean and standard deviation
meancols <- namedata[grep('mean\\(\\)',names(dataset)),1]
stdcols <- namedata[grep('std\\(\\)',names(dataset)),1]

finaldataset <- cbind(dataset[,1:2],dataset[,meancols], dataset[,stdcols])

##Get the Activity Labels
labels = read.table(
  "getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\activity_labels.txt", 
  sep=" ")

##Create the dataset with the required variables and the subject and the activity labels
finaldataset['Activity Label'] <- factor(finaldataset$`Activity Code`, labels=labels[,2])
variables <- names(dataset)[c(meancols, stdcols)]
activities <- as.vector(labels[,2])
subjects <- as.vector(data.frame(table(finaldataset$Subject))[,1])
datalength <- length(activities)*length(subjects)*length(variables)

##Create a Data Frame for the Final Tidy Data
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

