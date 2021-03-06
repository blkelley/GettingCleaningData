---
title: "Readme"
author: "BKelley"
date: "December 21, 2014"
output: html_document  
---
 
  
myTidyResult.txt is the file that contains the results of cleaning and reorganizing the data
for Coursera's Getting and Cleaning Data project course requirements.

This is a tidy data set since:

* each measured variable is in one column
* each different observation of the variables is in a different row
* there is one table for each kind of variable. for this particular case it is the mean of all means and standard deviations in the raw data for each acvitity type and subject
* the variable names are above each column. The names are cleanup up orignal names from the raw data with Mean prefixed to the variable names.

The script, run_analysis.R, first reads the activity labels and features files. Then the subject file, the subject activity file, and the measured data file are read. Similar for the train data sets. The train and test data sets are then combined into a single data set.

The column names are assigned to the combined data set from the features file data read in the beginning. Column names are cleaning of extra characters that tend to cluttering reading them (e.g. -,() characters). The columns of data of interest are extracted by finding the character string patterns Mean, mean, or std. 

The script now has a data set it can operate on to obtain the mean values of the means and standard deviations for each subject and activity. It loops though all subjects, then all activities, and then each column (set of measurements). The inner loop is the mean of all measurements for a particular activity (for example, all the WALKING measurement means) and computes the mean of these data as required by the rubric. The tidy data set is accummulated into a data frame using row binding of all the computed mean values of row column data for each subject/activity pair.

Finally, the tidy data set is written to myTidyResult.txt.
This result is easily viewed using:

data<- read.table(file-path, header=TRUE)
View(data). Thanks to CTA David Hood for providing this good idea.

The column names of the myTidyResult are Subject and ActivityName followed by the orignal features name with Mean prefixed, indicating the operation on the data and are as follows:

"Subject"                                   "ActivityName"                             
 [3] "Mean timeBodyAccmeanX"                     "Mean timeBodyAccmeanY"                    
 [5] "Mean timeBodyAccmeanZ"                     "Mean timeGravityAccmeanX"                 
 [7] "Mean timeGravityAccmeanY"                  "Mean timeGravityAccmeanZ"                 
 [9] "Mean timeBodyAccJerkmeanX"                 "Mean timeBodyAccJerkmeanY"                
[11] "Mean timeBodyAccJerkmeanZ"                 "Mean timeBodyGyromeanX"                   
[13] "Mean timeBodyGyromeanY"                    "Mean timeBodyGyromeanZ"                   
[15] "Mean timeBodyGyroJerkmeanX"                "Mean timeBodyGyroJerkmeanY"               
[17] "Mean timeBodyGyroJerkmeanZ"                "Mean timeBodyAccMagmean"                  
[19] "Mean timeGravityAccMagmean"                "Mean timeBodyAccJerkMagmean"              
[21] "Mean timeBodyGyroMagmean"                  "Mean timeBodyGyroJerkMagmean"             
[23] "Mean freqBodyAccmeanX"                     "Mean freqBodyAccmeanY"                    
[25] "Mean freqBodyAccmeanZ"                     "Mean freqBodyAccmeanFreqX"                
[27] "Mean freqBodyAccmeanFreqY"                 "Mean freqBodyAccmeanFreqZ"                
[29] "Mean freqBodyAccJerkmeanX"                 "Mean freqBodyAccJerkmeanY"                
[31] "Mean freqBodyAccJerkmeanZ"                 "Mean freqBodyAccJerkmeanFreqX"            
[33] "Mean freqBodyAccJerkmeanFreqY"             "Mean freqBodyAccJerkmeanFreqZ"            
[35] "Mean freqBodyGyromeanX"                    "Mean freqBodyGyromeanY"                   
[37] "Mean freqBodyGyromeanZ"                    "Mean freqBodyGyromeanFreqX"               
[39] "Mean freqBodyGyromeanFreqY"                "Mean freqBodyGyromeanFreqZ"               
[41] "Mean freqBodyAccMagmean"                   "Mean freqBodyAccMagmeanFreq"              
[43] "Mean freqBodyBodyAccJerkMagmean"           "Mean freqBodyBodyAccJerkMagmeanFreq"      
[45] "Mean freqBodyBodyGyroMagmean"              "Mean freqBodyBodyGyroMagmeanFreq"         
[47] "Mean freqBodyBodyGyroJerkMagmean"          "Mean freqBodyBodyGyroJerkMagmeanFreq"     
[49] "Mean angle(tBodyAccMean,gravity)"          "Mean angle(tBodyAccJerkMean),gravityMean)"
[51] "Mean angle(tBodyGyroMean,gravityMean)"     "Mean angle(tBodyGyroJerkMean,gravityMean)"
[53] "Mean angle(X,gravityMean)"                 "Mean angle(Y,gravityMean)"                
[55] "Mean angle(Z,gravityMean)"                 "Mean timeBodyAccstdX"                     
[57] "Mean timeBodyAccstdY"                      "Mean timeBodyAccstdZ"                     
[59] "Mean timeGravityAccstdX"                   "Mean timeGravityAccstdY"                  
[61] "Mean timeGravityAccstdZ"                   "Mean timeBodyAccJerkstdX"                 
[63] "Mean timeBodyAccJerkstdY"                  "Mean timeBodyAccJerkstdZ"                 
[65] "Mean timeBodyGyrostdX"                     "Mean timeBodyGyrostdY"                    
[67] "Mean timeBodyGyrostdZ"                     "Mean timeBodyGyroJerkstdX"                
[69] "Mean timeBodyGyroJerkstdY"                 "Mean timeBodyGyroJerkstdZ"                
[71] "Mean timeBodyAccMagstd"                    "Mean timeGravityAccMagstd"                
[73] "Mean timeBodyAccJerkMagstd"                "Mean timeBodyGyroMagstd"                  
[75] "Mean timeBodyGyroJerkMagstd"               "Mean freqBodyAccstdX"                     
[77] "Mean freqBodyAccstdY"                      "Mean freqBodyAccstdZ"                     
[79] "Mean freqBodyAccJerkstdX"                  "Mean freqBodyAccJerkstdY"                 
[81] "Mean freqBodyAccJerkstdZ"                  "Mean freqBodyGyrostdX"                    
[83] "Mean freqBodyGyrostdY"                     "Mean freqBodyGyrostdZ"                    
[85] "Mean freqBodyAccMagstd"                    "Mean freqBodyBodyAccJerkMagstd"           
[87] "Mean freqBodyBodyGyroMagstd"               "Mean freqBodyBodyGyroJerkMagstd"          



