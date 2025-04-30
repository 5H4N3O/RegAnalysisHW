# Load data
mydata <- read.table("../datasets/CH06PR09.txt", header = FALSE, sep = "")
head(mydata)

colnames(mydata) <-c("Y","X1","X2","X3")
my.data <- as.data.frame(mydata)
n <- nrow(my.data)

#Fit MLR
model1 <- lm(formula =  Y~X1+X2+X3, data = my.data)
#summary(model)

model2 <- lm(formula =  Y~X1+X3, data = my.data)
#summary(model)


anova(model1)
anova(model2)
summary(model1)
vcov(model1)