library(ggplot2)
library(data.table)
library(plyr)
library(here)

# Parameters: student name, errors per student data table
# Returns: string, error class representing most common error for student, or vector of strings in case of ties
Most_Common_Error <- function(student, eps) {
  errors <- eps[which(eps$Student == student & eps$Class != "No syntax errors"), ]
  winner <- errors[which(errors$freq == max(errors$freq)), ]
  return(winner$Class)
}

# Parameters: student name, errors per student data table
# Returns: numeric, erroneous attempts / total attempts
Relative_Error_Frequency <- function(student, eps) {
  errors <- eps[which(eps$Student == student), ]
  no_errors <- errors[which(errors$Class == "No syntax errors"), ]
  # Check for emtpy data frame, return 100% if true
  if (empty(no_errors)) { 
    return(1)
  } else {
    total <- sum(errors$freq)
    final <- 1 - (no_errors$freq / total)
    print(total)
    return(final)
  }
}

# Parameters: errors per student data table
# Returns: data frame of the number of students for whom e is the most common error class
Most_Common_Count <- function(eps) {
  # For each student, find most common error(s)
  MCES <- sapply(unique(eps$Student), Most_Common_Error, eps)
  # For each most common error class, count occurences
  # (Duplicated student names shouldn't be an issue as we are only interested in error class occurences)
  MCES_Freq <- count(unlist(MCES))
  df <- as.data.frame(c(MCES_Freq[1], MCES_Freq[2]))
  colnames(df) <- c("Class", "Count")
  return(df)
}
  
  
# Read in data
records <- read.csv(here("records.csv"), header = TRUE)
student_errors <- data.table(Student=records$Student, Class=records$Error.Class, Problem=records$Problem)
for (x in c("FoodChain", "Family", "Total")){
  if (x == "FoodChain") {
    DT <- student_errors["Food Chain", on = "Problem"]
  } else if (x == "Family") {
    DT <- student_errors["Family", on = "Problem"]
  } else {
    DT <- student_errors
  }
  
  student_counts <- DT[, .(rowCount = .N), by = Student]
  class_counts <- DT[, .(rowCount = .N), by = Class]
  errors_per_student <- count(DT, c("Student", "Class"))

  # Relative error frequency per student
  if (x == "FoodChain") {
    food_refs <- sapply(student_counts$Student, Relative_Error_Frequency, errors_per_student)
  } else if (x =="Family") {
    family_refs <- sapply(student_counts$Student, Relative_Error_Frequency, errors_per_student)
  } else {
    total_refs <- sapply(student_counts$Student, Relative_Error_Frequency, errors_per_student)
  }
  
  # Most common counts for each class
  mces <- Most_Common_Count(errors_per_student)
  
  #(x)
  #print(summary(refs))
  print(mces)
}

boxplot(food_refs, family_refs, total_refs,
        main = "Relative error frequency (per student)",
        at = c(1,3,5),
        names = c("Food\nChain", "Family", "Total"),
        las = 2,
        col = "orange",
        border = "brown",
        horizontal = TRUE
)

sbc <- ggplot(data = mces, aes(x = Class)) + geom_bar(aes(fill = Count), position = position_stack(reverse = TRUE)) + coord_flip() + theme(legend.position = "top")

