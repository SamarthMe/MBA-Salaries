data.df <-read.csv(paste("C:/Users/USER/Downloads/MBA Starting Salaries Data.csv", sep=""))#Loading the Data set
attach(data.df)#Attaching the Data set
#Replacing the 998 and 999
data.df$salary[data.df$salary == 998] <- 3
data.df$salary[data.df$salary == 999]<- 4
View(data.df)#General view of the entire Data frme

#1. Loading required package: ggplot2
#Seggregating according to gender 
library(ggplot2)
ggplot(data.df, aes(x = sex, fill = sex)) + geom_bar()

#Seggregating according to gmat scores
ggplot(data.df, aes(x = gmat_tot)) + geom_density()

#2. Correlation plots for Gmat totals and the three components of the same

ggplot(data.df, aes(x=gmat_tot,y=gmat_qpc)) +
  geom_point(position=position_jitter(w=0.1,h=0)) +
  geom_smooth()
ggplot(data.df, aes(x=gmat_tot,y=gmat_vpc)) +
  geom_point(position=position_jitter(w=0.1,h=0)) +
  geom_smooth()
ggplot(data.df, aes(x=gmat_tot,y=gmat_tpc)) +
  geom_point(position=position_jitter(w=0.1,h=0)) +
  geom_smooth()

#Altering the males and the females
data.df$sex[data.df$sex == 1]<- "male"
data.df$sex[data.df$sex == 2]<- "female"

#3.Seggregating salaries according to gender
library(lattice)
histogram(~salary | sex, data=data.df, 
          type="count", 
          layout=c(4,1), 
          col=c("burlywood", "darkolivegreen"))

#Density plot showing the salaries earned by gender
ggplot(data.df, aes(x = salary, color = sex)) + geom_density()


#Replacing the 998 and 999
data.df$satis[data.df$satis == 998] <- 0
data.df$satis[data.df$satis == 999]<- 1

#4.Calculating the satisfaction level of employees, 0 representing not available
ggplot(data.df, aes(x = satis, fill = satis)) + geom_bar()

#5. Is the salary of an individual have to do anything with his/her gender?
library(vcd) 
mytable <- xtabs(~salary+sex, data=data.df)
chisq.test(mytable)


#6. checking for correlation between different sets of variables
library(corrplot)
library(gplots)      # for color interpolation
par(mfrow=c(1, 1))
corrplot.mixed(corr=cor(data.df[ , c(3:10,12,13)], use="complete.obs"), upper="ellipse", tl.pos="lt", 
               col = colorpanel(50, "red", "gray60", "blue4"))

#7. using the tableplot to analyze the different data values
library(tabplot)
tableplot(data.df, select = c(3:10,12,13))


#Data frame of the student who got placed
placed <- data.df[ which(data.df$salary > 4), ]
placed
View(placed)

#8. checking which factors are necessary to predict the salary
library(Boruta)
set.seed(1234) # for code reproducibility
response <- placed$salary
bor.results <- Boruta(placed,response,maxRuns=101,doTrace=0)
plot(bor.results)


#Dividing the Data set into Test and Training Data ste
ratio = sample(1:nrow(placed), size = 0.25*nrow(placed))
Test = placed[ratio,] #Test dataset 25% of total
Training = placed[-ratio,] #Train dataset 75% of total
dim(Training)
dim(Test)

#9. applying multi variable linear regression model
linear.mod<- lm(salary ~ age + work_yrs + quarter + s_avg + f_avg + gmat_tot, data = Training)
summary(linear.mod)
#the t value of age, work_yrs and quarter are positive suggesting their assosication with the salary.
#The model's, p-value: < 2.2e-16 is also lower than the statistical significance level of 0.05, this indicates that we can safely reject the null hypothesis that the value for the coefficient is zero 
#(or in other words, the predictor variable has no explanatory relationship with the response variable).
#the p value of work_yrs is less, hence we can say it is statistically significant


#10. Implementing Random Forest Algorithm
library(rpart)
library(randomForest)
model.forest <- randomForest(salary ~ age + work_yrs + quarter + s_avg + f_avg + gmat_tot, data = Training, method = "anova", 
                             ntree = 300,
                             mtry = 2, #mtry is sqrt(6)
                             replace = F,
                             nodesize = 1,
                             importance = T)

varImpPlot(model.forest)
#From the VIF plot we see that age, g_mat total and work_yrs are most important in deriving the salary

#We test the model using Random Forest
prediction <- predict(model.forest,Test)
rmse <- sqrt(mean((log(prediction)-log(Test$salary))^2))
round(rmse, digits = 3)

#12. Evaluation metric function
#A custom root mean Square Function to evaluate the performance of our model
RMSE <- function(x,y)
{
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

#Implementing the Regression Tree Model 
model <- rpart(salary ~ age + work_yrs + quarter + s_avg + f_avg + gmat_tot, data = Training, method = "anova")
predict <- predict(model, Test)
RMSE1 <- RMSE(predict, Test$salary)
RMSE1 <- round(RMSE1, digits = 3)
RMSE1

#PREDICTING THE ACCURACY OF THE MODEL
predict<- predict(linear.mod, Test)
actuals_preds <- data.frame(cbind(actuals=Test$salary, predicteds=predict)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

#CALCULATING THE MIN MAX ACCURACY AND MAPE
min_max_accuracy <- mean (apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

#Calculations for Logistic Regression 
new = data.df
new$salary[new$salary == 4] <- 0
new$salary[new$salary == 3] <- 0
new$salary[new$salary > 0] <- 1
View(new)

#Dividing the Data set into Test and Training Data ste
ratio = sample(1:nrow(new), size = 0.25*nrow(new))
Test = new[ratio,] #Test dataset 25% of total
Training = new[-ratio,] #Train dataset 75% of total
dim(Training)
dim(Test)

#12. Applying Logisitc Regression
model <- glm(salary ~age + work_yrs + quarter + s_avg + f_avg + gmat_tot,family=binomial(link='logit'),data=Training)
summary(model)

#Fitting the Anova model
anova(model, test="Chisq")

library(pscl)
pR2(model) 

#Calculating the Accuracy of the model
fitted.results <- predict(model,newdata=subset(Test,select=c(1,3,7,8,9,10)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != Test$salary)
print(paste('Accuracy',1-misClasificError))

#Plotting the ROC
library(ROCR)
p <- predict(model, newdata=subset(Test,select=c(1,3,7,8,9,10)), type="response")
pr <- prediction(p, Test$salary)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#Calculating the AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
      