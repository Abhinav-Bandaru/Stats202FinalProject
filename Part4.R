#Option of averaging P, N and G with logistic regression

library(dplyr)
library(matrixStats)
library(ggplot2)
library(plyr)
library(glmnet)
library(olsrr)
library(caret)
# import data
studya <- read.csv("/Users/andreabeltran/Study_A.csv")
studyb <- read.csv("/Users/andreabeltran/Study_B.csv")
studyc <- read.csv("/Users/andreabeltran/Study_C.csv")
studyd <- read.csv("/Users/andreabeltran/Study_D.csv")
studye <- read.csv("/Users/andreabeltran/Study_E.csv")
# append studies A - D
studiesad <- rbind(studya, studyb, studyc, studyd)

# remove duplicates from Patient ID in VisitDay CHECK
studiesad<- studiesad[!duplicated(select(studiesad, PatientID, VisitDay)),]

#Removing Variables Assessment ID, Patient ID,, Panss total and Study
studiesad = subset(studiesad, select = -c(AssessmentID) )
studiesad = subset(studiesad, select = -c(PatientID) )
studiesad = subset(studiesad, select = -c(PANSS_Total) )
#studiesad = subset(studiesad, select = -c(Study) )
studiesad = subset(studiesad, select = -c(Country) )
studiesad = subset(studiesad, select = -c(RaterID) )
studiesad = subset(studiesad, select = -c(SiteID) )

#Try rater ID with Abhinavs matrix to have many variables in a dummy for Country
dim(studiesad)

# convert to dummy variable TxGroup
studiesad$TxGroup=as.factor(studiesad$TxGroup)
contrasts(studiesad$TxGroup)
typeof(studiesad$TxGroup)
class(studiesad$TxGroup)

FlaggedOrAssign2 = factor(studiesad$LeadStatus == "Flagged" | studiesad$LeadStatus == "Assign to CS")
studiesad$FlaggedOrAssign <- FlaggedOrAssign2
studiesad = subset(studiesad, select = -c(LeadStatus) )
dim(studiesad)

#Average P1-P7 into Pmean
studiesad$Pmean <- rowMeans(studiesad[4:10])
studiesad$Nmean <- rowMeans(studiesad[11:17])
studiesad$Gmean <- rowMeans(studiesad[18:33])

#Create std columns
Pmatstudies<-as.matrix(studiesad[4:10])
Nmatstudies<-as.matrix(studiesad[11:17])
Gmatstudies<-as.matrix(studiesad[18:33])

studiesad$Psds <- rowSds(Pmatstudies)
studiesad$Nsds <- rowSds(Nmatstudies)
studiesad$Gsds <- rowSds(Gmatstudies)
names(studiesad)
studiesad

# logistic regression

# divide data into train and test
set.seed(1)

#Divide training and test data using crossvalidation
smp_size <- floor(0.5 * nrow(studiesad))
train_indices2 = sample(nrow(studiesad), size=smp_size)
dftrain2 = studiesad[train_indices2, ]
dftest2 = studiesad[-train_indices2, ]

#After fitting the model using Nsds we see that all the variables have a P value of 
#>0.001 except for Nsds which =0.67. Removing variable to see how the model behaves.
glm.fit2.2 <- glm(FlaggedOrAssign2~ Psds + Gsds + Pmean + Nmean + Gmean + VisitDay + TxGroup, data=studiesad, subset=train_indices2, family=binomial)
summary(glm.fit2.2)

# evaluate model on the test set
glm.probs2.2 = predict(glm.fit2.2, dftest2, type="response")
# generate the confusion marix
threshold = 0.5
glm.pred2.2 = rep(FALSE, nrow(dftest2))
glm.pred2.2[glm.probs2.2 > threshold] = TRUE
glm.pred2.2<-factor(glm.pred2.2,levels=c(FALSE,TRUE))
ConfMat<-confusionMatrix(glm.pred2.2,FlaggedOrAssign2[-train_indices2])
ConfMat

#Evaluate on all data studies A-D
glm.fit2.2 <- glm(FlaggedOrAssign2~ Psds + Gsds + Pmean + Nmean + Gmean + VisitDay + TxGroup, data=studiesad, family=binomial)
summary(glm.fit2.2)

#Average P1-P7 into Pmean
names(studye)
studye$Pmean <- rowMeans(studye[9:15])
studye$Nmean <- rowMeans(studye[16:22])
studye$Gmean <- rowMeans(studye[23:38])

#SDS P1-P7 into Psds and Gsds
Pmate<-as.matrix(studye[9:15])
Gmate<-as.matrix(studye[28:38])

studye$Psds <- rowSds(Pmate)
studye$Gsds <- rowSds(Gmate)


#Evaluate model on study E
glm.e2.2 = predict(glm.fit2.2, studye,type="response")

LeadStatus<-glm.e2.2
AssessmentID<-studye$AssessmentID
model2.2<-data.frame(AssessmentID,LeadStatus)
model2.2
#EXPORT MODEL TO CSV
write.csv(model2.2,"/Users/andreabeltran/Stanford/STATS202/Final_Project/Model2.2.csv", row.names = FALSE)
