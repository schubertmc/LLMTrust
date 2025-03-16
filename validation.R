sel <- read.csv("../sel.csv")
sel$X <- NULL
colnames(sel) <- c("LLM", "rater1", "rater2", "rater3")

# Function to calculate sensitivity and specificity for each pair of raters
calculate_sensitivity_specificity <- function(rater1, rater2) {
  # Create confusion matrix
  TP <- sum(rater1 == 1 & rater2 == 1)
  TN <- sum(rater1 == 0 & rater2 == 0)
  FP <- sum(rater1 == 1 & rater2 == 0)
  FN <- sum(rater1 == 0 & rater2 == 1)
  
  # Calculate sensitivity and specificity
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  # Precision
  precision <- TP / (TP + FP)
  
  # Recall
  recall <- TP / (TP + FN)
  
  # F1-Score
  F1 <- 2 * (precision * recall) / (precision + recall)
  
  return(c(Sensitivity = sensitivity, Specificity = specificity, Precision = precision, Recall = recall, F1 = F1))
}

# List of rater column names
raters <- colnames(sel)

# Create an empty data frame to store results
result <- data.frame(
  Rater1 = character(),
  Rater2 = character(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  stringsAsFactors = FALSE,
  RaterType = character()
)

# Step 1: Identify non-LLM columns
non_LLM_raters <- setdiff(colnames(sel), "LLM")

# Step 2: Create a new column with the majority vote of non-LLM raters
sel$Majority_Human <- apply(
  sel[, non_LLM_raters], 
  1,  # Apply function across rows
  function(row) {
    ifelse(mean(row) > 0.5, 1, 0)  # Majority vote (1 if average > 0.5, otherwise 0)
  }
)

LLM <- sel[,"LLM"]
majority <- sel[,"Majority_Human"]
metrics <- calculate_sensitivity_specificity(LLM, majority)
metrics


library(irr)

# Example dataframe with three raters and binary measures (200 rows)

ratings <- sel[,c("rater1", "rater2", "rater3")]

# Calculate Fleiss' Kappa
fleiss_kappa <- kappam.fleiss(as.matrix(ratings))

# Output the results
print(fleiss_kappa)