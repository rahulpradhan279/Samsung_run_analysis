run_analysis <- function()
{
  
  setwd<-"C:/Users/RAHUL/Documents/R/UCI HAR Dataset"
  #Reading Train and Test Data
  
  xtest<-read.table(file="./test/X_test.txt",header = FALSE)
  ytest<-read.table("./test/y_test.txt",header = FALSE)
  subjecttest<-read.table("./test/subject_test.txt",header = FALSE)
 
  xtrain<-read.table("./train/X_train.txt",header=FALSE)
  ytrain<-read.table("./train/y_train.txt",header = FALSE)
  subjecttrain<-read.table("./train/subject_train.txt",header = FALSE)
  
  #merging Train and Test data
  
  xdata<-rbind(xtest,xtrain)
  ydata<-rbind(ytest,ytrain)
  subjectdata<-rbind(subjecttest,subjecttrain)
  dim(subjectdata)
  
  #reading feature table for description 
  feature<-read.table("./features.txt",header = FALSE)
  
  #extracting only those variable having mean and std in their names
  xdatameanstd<-xdata[,grep("-(mean|std)\\(\\)",feature[,2])]
  
  #assigning colum names of test variable
  names(xdatameanstd)<-feature[grep("-(mean|std)\\(\\)",feature[,2]),2]
  
  #assigning Acitivity description from activity file to y file
  ydata[,1]<-read.table("./activity_labels.txt",header=FALSE)[ydata[,1],2]
  names(ydata)<-"Activity"
  
  # assiging default column name to subject data 
  names(subjectdata)<-"SubjectData" 
  
  #creating a single data file having mean and std variables and adding
  #column Activity and Subject
  singleData<-cbind(xdatameanstd,ydata,subjectdata)
  
  ## Tidying data by replacing full form for accroynm
  
  names(singleData) <- gsub('Acc',"Acceleration",names(singleData))
  names(singleData) <- gsub('GyroJerk',"AngularAcceleration",names(singleData))
  names(singleData) <- gsub('Gyro',"AngularSpeed",names(singleData))
  names(singleData) <- gsub('Mag',"Magnitude",names(singleData))
  names(singleData) <- gsub('^t',"TimeDomain.",names(singleData))
  names(singleData) <- gsub('^f',"FrequencyDomain.",names(singleData))
  names(singleData) <- gsub('-mean',".Mean",names(singleData))
  names(singleData) <- gsub('-std',".StandardDeviation",names(singleData))
  names(singleData) <- gsub('Freq\\.',"Frequency.",names(singleData))
  names(singleData) <- gsub('Freq$',"Frequency",names(singleData))
  
  ## creating a second file containig mean for each variable 
  ## base on Activity and Subject
  
  newdata<-aggregate(.~Activity+SubjectData,singleData,mean)
  newdata<-newdata[order(newdata$SubjectData,newdata$Activity),]
  
  ## Creating a TXT file for the new data
  write.table(newdata, file = "tidydata.txt",row.name=FALSE)
  
  }