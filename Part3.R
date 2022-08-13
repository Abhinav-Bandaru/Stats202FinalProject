library(neuralnet)
library(dplyr)

E <- read.csv("Study_E.csv")
E_dup <- E
E <- E[,-c(1,2,4,5,6,7,39)]
Predicted_PANSS_Scores <- c()

i <- 1
while(i<nrow(E)){
  
  # Separating every patient into a small data frame called more_than_one
  
  more_than_one <- data.frame(matrix(ncol = 32, nrow = 0))
  colnames(more_than_one) <- colnames(E)[-c(1)]
  if(E$PatientID[i]==E$PatientID[i+1]){
    while(E$PatientID[i]==E$PatientID[i+1]){
      each_patient <- E[i,-c(1)]
      more_than_one[nrow(more_than_one) + 1,] <- each_patient
      i <- i + 1
      if(i>=nrow(E))
        break
    }
  }
  each_patient <- E[i,-c(1)]
  more_than_one[nrow(more_than_one) + 1,] <- each_patient
  i <- i + 1
}

# Applying regression on individual variables P1-P7, N1-N7, G1-G16 for every patient

  answer <- 0
  for(j in 2:ncol(more_than_one)){
    nn <- neuralnet(more_than_one[[colnames(more_than_one)[j]]]~VisitDay, data = more_than_one, hidden = c(5, 3), linear.output = TRUE)
    ans <- compute(nn, data.frame(VisitDay=c(18)))
    answer <- answer + ans$net.result
  }
  print(answer[[1]])
  Predicted_PANSS_Scores <- c(Predicted_PANSS_Scores, answer[[1]])
  
}

Predicted_PANSS_Scores

Final_Predictions = data.frame(unique(E_dup$PatientID), Predicted_PANSS_Scores)
colnames(Final_Predictions) <- c("PatientID", "PANSS_Total")
write.table(Final_Predictions, file='......data.txt', sep=',',
            row.names = F)
