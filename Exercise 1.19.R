# Load Data
gpa <- read.table("CH01PR19.txt", header = FALSE, sep = "")
colnames(gpa) <-c("Y","X")
gpa <- as.data.frame(gpa)

# View Data
head(gpa)


