#Part 2 Unsupervised learning with means of P, N and G no study E

#Dropped study e because it doesn't have the Lead Status column
library(dplyr)
library(devtools)
library(ggbiplot)
library(ggplot2)
library(factoextra)

# import data
studya <- read.csv("/Users/andreabeltran/Study_A.csv")
studyb <- read.csv("/Users/andreabeltran/Study_B.csv")
studyc <- read.csv("/Users/andreabeltran/Study_C.csv")
studyd <- read.csv("/Users/andreabeltran/Study_D.csv")

# append studies A - D
studiesad <- rbind(studya,studyb, studyc,studyd)

#Keep only min value of  Visit Day
studiesad<-studiesad %>% group_by(PatientID) %>%slice(which.min(VisitDay))
dim(studiesad)
max(studiesad$VisitDay)

#Remove flagged and CS data
studiesad=subset(studiesad, studiesad$LeadStatus != "Flagged") 
dim(studiesad)
studiesad=subset(studiesad, studiesad$LeadStatus != "Assign to CS")  
dim(studiesad)

# remove duplicates from Patient ID in VisitDay if any
studiesad<- studiesad[!duplicated(select(studiesad, PatientID, VisitDay)),]
studies<- studiesad[!duplicated(select(studiesad, PatientID, AssessmentID)),]

#Removing Variables Assessment ID, Patient ID,, Panss total and Study
studiesad = subset(studiesad, select = -c(AssessmentID) )
studiesad = subset(studiesad, select = -c(PANSS_Total) )
studiesad = subset(studiesad, select = -c(Study) )
studiesad = subset(studiesad, select = -c(Country) ) 
studiesad = subset(studiesad, select = -c(RaterID) )
studiesad = subset(studiesad, select = -c(SiteID) ) 
studiesad = subset(studiesad, select = -c(TxGroup) )
studiesad = subset(studiesad, select = -c(VisitDay) )
studiesad = subset(studiesad, select = -c(PatientID) )
studiesad = subset(studiesad, select = -c(LeadStatus) )

#Convert to mean
names(studiesad)
studiesad$Pmean <- rowMeans(studiesad[1:7])
studiesad$Nmean <- rowMeans(studiesad[8:14])
studiesad$Gmean <- rowMeans(studiesad[15:30])
names(studiesad)

#Removing Ps, Ns and Gs
studiesad = subset(studiesad, select = -c(P1,P2,P3,P4,P5,P6,P7,N1,N2,N3,N4,N5,N6,N7,G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12,G13,G14,G15,G16) )
names(studiesad)

#Scaling data
studiesad<-scale(studiesad)
min(studiesad)
max(studiesad)

#PCA
studies.pca <- prcomp(studiesad, center = TRUE,scale. = TRUE)
print(studies.pca)

biplot(studies.pca, scale = 0,cex = c(0.2, 0.5))
summary(studies.pca)

#Silhouetter method 
fviz_nbclust(studiesad, kmeans, method = "silhouette")

#K means clustering
set.seed(2)
kmod<-kmeans(studiesad, 2, iter.max = 50, nstart = 20)
fviz_cluster(kmod, studiesad,pointsize=0.5,show.clust.cent=TRUE,labelsize=0)
kmod
