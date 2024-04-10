data= read.csv("C:\\Users\\ANJALI\\Desktop\\FDATA.csv");data
head(data)
dim(data)
str(data)
names(data)
library(psych)
data(ToothGrowth)
describe(ToothGrowth)
data1 = subset(data, select = -c(1))
data
library(corrplot)
datamatrix=cor(data1)
corrplot(datamatrix,method="number")
library(ppcor)
pcor(data1,method="pearson")
model0 = lm(Satisfaction~., data1);model0
summary(model0)
library(car)
vif(model0)
data2=data1[,-12]
data
datamatrix=cor(data2)
datamatrix
KMO(r=datamatrix)
print(cortest.bartlett(datamatrix,nrow(data1)))
parallel = fa.parallel(data2, fm ="minres",fa="fa")
ev=eigen(cor(data2))
ev$values
part.fa=ev$values/sum(ev$values)*100
part.fa
#Plot a Scree plot using base plot:
Factor = c(1,2,3,4,5,6,7,8,9,10,11)
Eigen_Values <-ev$values
Scree <- data.frame(Factor, Eigen_Values)
plot(Scree, main = "Scree Plot", col= "Blue",ylim=c(0,4))
lines(Scree,col='Red')
abline(h = 1, col="Green")
Factor = c(1,2,3,4,5,6,7,8,9,10,11)
Eigen_Values <-ev$values
Scree <- data.frame(Factor, Eigen_Values)
plot(Scree, main = "Scree Plot", col= "Blue",ylim=c(0,4))
lines(Scree,col='Red')
abline(h = 1, col="Green")
library(ggplot2)
ggplot(data = Scree,mapping = aes(x=Factor,y=Eigen_Values))+
  geom_point()+
  geom_line()+
  scale_y_continuous(name="Eigen Values",limits = c(0,4))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  theme(panel.grid.major.y = element_line(colour = "skyblue"))+
  ggtitle("Scree Plot")
nfactors <- 4
fit <- factanal(data2, nfactors, scores = c("regression"),rotation = "none")
print(fit)
fit1 <-factanal(data2,nfactors,scores = c("regression"),rotation = "varimax")
print(fit1)
fanone <-  fa(r=data2, nfactors = 4, rotate="none",fm="pa")
print(fanone)
fa.diagram(fanone)
fanone$loading
fa1<- fa(r=data2, nfactors = 4, rotate="varimax",fm="pa")
print(fa1)
fa1$loadings
fa.diagram(fa1)
head(fa1$scores)
regdata <- cbind(data1[12], fa1$scores)
names(regdata) <- c("Satisfaction", "Purchase", "Marketing","Post_purchase", "Prod_positioning")
head(regdata)
set.seed(100)
indices= sample(1:nrow(regdata), 0.7*nrow(regdata))
train=regdata[indices,]
test = regdata[-indices,]
model1 = lm(Satisfaction~., train)
summary(model1)
vif(model1)
library(Metrics)
pred_test1 <- predict(model1, newdata = test, type = "response")
pred_test1
test$Satisfaction_Predicted <- pred_test1
head(test[c(1,6)], 10)
test_r2 <- cor(test$Satisfaction, test$Satisfaction_Predicted) ^2

mse_test1 <- mse(test$Satisfaction, pred_test1)
rmse_test1 <- sqrt(mse(test$Satisfaction, pred_test1))
mape_test1 <- mape(test$Satisfaction, pred_test1)
model1_metrics <- cbind(mse_test1,rmse_test1,mape_test1,test_r2)
print(model1_metrics, 3)
model2 <- lm(Satisfaction ~ Purchase+ Marketing+ 
                Prod_positioning, data= train)
summary(model2)
pred_test2 <- predict(model2, newdata = test, type = "response")
pred_test2
test$Satisfaction_Predicted2 <- pred_test2
head(test[c(1,7)], 10)
test_r22 <- cor(test$Satisfaction, test$Satisfaction_Predicted2) ^2
mse_test2 <- mse(test$Satisfaction, pred_test2)
rmse_test2 <- sqrt(mse(test$Satisfaction, pred_test2))
mape_test2 <- mape(test$Satisfaction, pred_test2)

model2_metrics <- cbind(mse_test2,rmse_test2,mape_test2,test_r22)
model2_metrics
Overall <- rbind(model1_metrics,model2_metrics)
row.names(Overall) <- c("Test1", "Test2")
colnames(Overall) <- c("MSE", "RMSE", "MAPE", "R-squared")
print(Overall,3)
names(regdata)
model3 <- lm(lm(Satisfaction ~ Purchase+ Marketing+ Post_purchase+
                 Prod_positioning+ Purchase*Post_purchase+
                  Marketing*Prod_positioning+ Purchase* Marketing+
                  Purchase* Prod_positioning*Marketing,data=train ))
summary(model3)
pred_int_train = predict(model3, data = train, type = "response")
pred_int_test = predict(model3, newdata = test, type = "response")

mse_train_int <- mse(train$Satisfaction, pred_int_train)
mse_test_int <- mse(test$Satisfaction, pred_int_test)

rmse_train_int <- sqrt(mse(train$Satisfaction, pred_int_train))
rmse_test_int <- sqrt(mse(test$Satisfaction, pred_int_test))

mape_train_int <- mape(train$Satisfaction, pred_int_train)
mape_test_int <- mape(test$Satisfaction, pred_int_test)

r2_train <- cor(train$Satisfaction, pred_int_train) ^2
r2_test <- cor(test$Satisfaction, pred_int_test) ^2

model3_metrics_train <- cbind(mse_train_int,rmse_train_int,mape_train_int,r2_train)
model3_metrics_test <- cbind(mse_test_int,rmse_test_int,mape_test_int,r2_test)

interact_train_test <- rbind(model3_metrics_train,model3_metrics_test)

row.names(interact_train_test) <- c("Train","Test")
colnames(interact_train_test) <- c("MSE","RMSE","MAPE","R-squared")

print(interact_train_test,digits = 3)
plot(model3)
