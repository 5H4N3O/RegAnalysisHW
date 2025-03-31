# Load data
mydata <- read.table("../datasets/CH06PR09.txt", header = FALSE, sep = "")
head(mydata)

colnames(mydata) <-c("Y","X1","X2","X3")
my.data <- as.data.frame(mydata)
n <- nrow(my.data)

#Fit MLR
model <- lm(formula = Y ~ X1+X2+X3, data = my.data)
summary(model)

#Bonferroni CI for beta1 and beta2
n <- nrow(my.data)
B <- qt(1-(0.05/(2*2)), n-4)
B

#ANOVA
anova(model)