# Load data
my.data <- read.table("../datasets/CH06PR05.txt", header = FALSE, sep = "")
head(my.data)
#View(my.data)

colnames(my.data) <-c("Y","X1","X2")
my.data <- as.data.frame(my.data)
n <- nrow(my.data)

my.data$X1C <- my.data$X1 - mean(my.data$X1)
my.data$X2C <- my.data$X2 - mean(my.data$X2)

#Fit MLR
model <- lm(formula = Y ~ X1 + X2 + I(X1*X2), data = my.data)
summary(model)

qt(.975,12)