library(dplyr)

# Study A

# Control Group

A <- read.csv("Study_A.csv")

# Filtering out only the accepted assessments
A_passed <- dplyr::filter(A, LeadStatus == "Passed")

# Using only the PatientID, TxGroup and PANSS_Total
A_passed <- A_passed[c(3,7,39)]

# Separating the data into 2 data frames
A_passed_control <- dplyr::filter(A_passed, TxGroup == "Control")
A_passed_treatment <- dplyr::filter(A_passed, TxGroup == "Treatment")


# Control Group

# Finding the difference between first and last PANSS_Total Scores
i <- 1
control_score_difference <- data.frame(matrix(ncol = 3, nrow = 0))
while(i < nrow(A_passed_control)){
  
  final_score <- -1
  if(A_passed_control$PatientID[i]==A_passed_control$PatientID[i+1]){
    initial_score <- A_passed_control$PANSS_Total[i]
    while(A_passed_control$PatientID[i]==A_passed_control$PatientID[i+1]){
      i <- i + 1
      final_score <- A_passed_control$PANSS_Total[i]
      if(i>=nrow(A_passed_control))
        break
    }
    
    if(final_score!=-1){
      control_score_difference[nrow(control_score_difference)+1,] <- c(A_passed_control$PatientID[i], 
                                                                       A_passed_control$TxGroup[i],
                                                                       initial_score-final_score)
    }
  }
  i <- i + 1
}

colnames(control_score_difference) <- c('PatientID', 'TxGroup', 'PANSS_Total')



# Treatment Group

# Finding the difference between first and last PANSS_Total Scores
i <- 1
treatment_score_difference <- data.frame(matrix(ncol = 3, nrow = 0))
while(i < nrow(A_passed_treatment)){
  
  final_score <- -1
  if(A_passed_treatment$PatientID[i]==A_passed_treatment$PatientID[i+1]){
    initial_score <- A_passed_treatment$PANSS_Total[i]
    while(A_passed_treatment$PatientID[i]==A_passed_treatment$PatientID[i+1]){
      i <- i + 1
      final_score <- A_passed_treatment$PANSS_Total[i]
      if(i>=nrow(A_passed_treatment))
        break
    }
    
    if(final_score!=-1){
      treatment_score_difference[nrow(treatment_score_difference)+1,] <- c(A_passed_treatment$PatientID[i], 
                                                                       A_passed_treatment$TxGroup[i],
                                                                       initial_score-final_score)
    }
  }
  i <- i + 1
}

colnames(treatment_score_difference) <- c('PatientID', 'TxGroup', 'PANSS_Total')

# Converting the PANSS scores to numeric class
treatment_score_difference$PANSS_Total <- as.numeric(treatment_score_difference$PANSS_Total)
control_score_difference$PANSS_Total <- as.numeric(control_score_difference$PANSS_Total)


# Checking the T-Test Assumptions

var(treatment_score_difference$PANSS_Total)
var(control_score_difference$PANSS_Total)

hist(treatment_score_difference$PANSS_Total)
hist(control_score_difference$PANSS_Total)

A_T_Test <- rbind(control_score_difference,treatment_score_difference)
aov(A_T_Test$PANSS_Total ~ A_T_Test$TxGroup)
summary(aov(A_T_Test$PANSS_Total ~ A_T_Test$TxGroup))
