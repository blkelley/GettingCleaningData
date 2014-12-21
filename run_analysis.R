## run_analysis.R
## use dplyr package

## activityLabels and features are common to both the test set and train set.
## read the test and train data sets and append train to test to produce a combined
## data set. this is the one to clean up and operate on.

activityLabels <- read.table(file="./activity_labels.txt")
features       <- read.table(file="./features.txt")

testLabels     <- read.table(file="./test/y_test.txt")
testSubjects   <- read.table(file="./test/subject_test.txt")
testSet        <- read.table(file="./test/X_test.txt")

trainLabels    <- read.table(file="./train/y_train.txt")
trainSubjects  <- read.table(file="./train/subject_train.txt")
trainSet       <- read.table(file="./train/X_train.txt")

# combine the test and training data sets

combinedLabels   <- rbind(testLabels,trainLabels)
combinedSubjects <- rbind(testSubjects,trainSubjects)
combinedSet      <- rbind(testSet,trainSet)
# assign features names to testSet column names

colnames(combinedSet) <- features[,"V2"]

# join activity labels to test labels and name columns

combinedLabelsJoin<- left_join(combinedLabels, activityLabels)
colnames(combinedLabelsJoin) <- c("ActivityCode", "ActivityName")

# now put together the testSubjects and their respective activitys

combinedSubjectdf<- tbl_df(cbind(combinedSubjects,combinedLabelsJoin[,2]))
colnames(combinedSubjectdf)<- c("Subject", "ActivityName")

# extract the mean and standard deviation columns from testSet- these are the data of interest

combinedcolnames<- c(grep("[Mm]ean",names(combinedSet),value=TRUE),grep(("std"),names(combinedSet),value=TRUE))

# subset the test set and set up for dplyr

combinedSetsubset<- combinedSet[,combinedcolnames]
combineddf<- tbl_df(cbind(combinedSubjectdf, combinedSetsubset))

# now we have the structure (df) of interest for the test set, let's clean up the column names

x<- colnames(combineddf)
x<- gsub("-","",x)
x<- gsub("\\()","",x)
x<- sub("^t","time",x)
x<- sub("^f","freq",x)
colnames(combineddf)<- x

combineddf<- arrange(combineddf,Subject)
tidyresult<- data.frame()
combinedSubjectsindex<- c(sort(combinedSubjects$V1))
dataindex<- grep(("[Mm]ean|std"),colnames(combineddf))
rowresult<- vector()


#get all subjects
for (k in unique(combinedSubjectsindex)){
        thisSubject<- k 
        #get each activity

        for (j in activityLabels$V1) { 
                thisActivity= as.character(activityLabels[j,2])
                z<- filter(combineddf, Subject==thisSubject, ActivityName==thisActivity)
                rowresult<- cbind(thisSubject, thisActivity)
                for (i in dataindex)  {
                        rowresult<- cbind(rowresult,mean(unlist(select(z,i))))
                }
                
                tidyresult<- rbind(tidyresult,rowresult)
   
        }
}
colnames(tidyresult)<- colnames(combineddf)
s<- colnames(tidyresult); s<- c(s[1:2],paste("Mean",s[3:length(s)]))
colnames(tidyresult)<- s
tidyresult<- tbl_df(tidyresult)
write.table(tidyresult,file="./myTidyResult.txt", row.name=FALSE)
