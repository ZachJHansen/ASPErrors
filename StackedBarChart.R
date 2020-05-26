library(ggplot2)
library(data.table)
library(plyr)
library(here)

# Replace the name of each student with an anonymous identifier
# (Index of student name in sorted student list)
anonymize <- function(name, students){
  indx <- match(name, students)
  return(paste0("S", indx))
}

# Read in data 
records <- read.csv(here("records.csv"), header = TRUE)

# Create anonymous ids
students <- sort(unique(records$Student))
ids <- sapply(records$Student, anonymize, students)
student_errors <- data.table(Student=ids, Class=records$Error.Class, Problem=records$Problem)

# Generate all graphs
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
  
  # Specify graph destination
  pic_name = paste0("Student_Errors_", x, ".png")
  
  # Plot and save images
  sbc <- ggplot(data = DT, aes(x = Student)) + geom_bar(aes(fill = Class), position = position_stack(reverse = TRUE)) + coord_flip() + theme(legend.position = "top")
  ggsave(filename = here("Images", pic_name), sbc, width = 10, height = 8)
}

