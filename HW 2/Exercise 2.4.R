# Load Data
gpa <- read.table("CH01PR19.txt", header = FALSE, sep = "")
colnames(gpa) <-c("Y","X")
gpa <- as.data.frame(gpa)

# Summarize Data
model <- lm(formula = Y ~ X, data = gpa)
summary(model)