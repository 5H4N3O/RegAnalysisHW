# Load data
my.data <- read.table("../datasets/CH06PR09.txt", header = FALSE, sep = "")
head(my.data)
View(my.data)

colnames(my.data) <-c("Y","X1","X2","X3")
my.data <- as.data.frame(my.data)
n <- nrow(my.data)

#Scatter Plot
plot(my.data)

#Correlation Matrix
cor(my.data)