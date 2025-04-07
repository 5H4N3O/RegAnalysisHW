# Load data
mydata <- read.table("../datasets/CH06PR05.txt", header = FALSE, sep = "")
head(mydata)
# View(mydata)

colnames(mydata) <-c("Y","X1","X2")
my.data <- as.data.frame(mydata)
n <- nrow(my.data)

#Fit MLR
model <- lm(formula = Y ~ X1+X2, data = my.data)
summary(model)

anova(model)
qf(.99,1,13)