library(ggplot2)
library(data.table)
library(plyr)

# Set the working directory independent of OS (records.csv must be saved in working directory)
setwd(file.path("C:", "Users", "ZACH", "Desktop", "ASPErrors"))
records <- read.csv(file.path(getwd(), "records.csv"), header = TRUE)
student_errors <- data.table(Student=records$Student, Class=records$Error.Class, Problem=records$Problem)

x = "Family"
#x = "Total"

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
#directory = paste0(BASE_DIRECTORY, "/ASPErrors/")
pic_name = paste("StudentErrors", x, sep = "_")
png(paste0(BASE_DIRECTORY, pic_name), width = 1400, height = 800)

# Plot
sbc <- ggplot(data = DT, aes(x = Student))
sbc + geom_bar(aes(fill = Class), position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme(legend.position = "top")
