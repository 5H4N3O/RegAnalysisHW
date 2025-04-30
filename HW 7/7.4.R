# Load data
mydata <- read.table("../datasets/CH06PR09.txt", header = FALSE, sep = "")
head(mydata)

colnames(mydata) <-c("Y","X1","X2","X3")
my.data <- as.data.frame(mydata)
n <- nrow(my.data)

#Fit MLR
model <- lm(formula = Y ~ X1+X3+X2, data = my.data)
summary(model)
model <- lm(formula = Y ~ X2+X1+X3, data = my.data)


anova(model)
qf(.95,1,48)
#P-value of the test:
#Pr(>F) of Variable we are contemplating removing