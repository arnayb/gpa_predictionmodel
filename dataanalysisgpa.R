#Loading the data
#sleepdata <- read.csv("/Users/arnaybisht/Downloads/sleepdata.csv")
#Making the 80 percent subset
#N <- dim(sleepdata)[1]
#sap <- sample(1:N, 200)
#sleepsubset <- sleepdata[sap,]


sleepsubset <- sta108_my_sleepsubset
#Scatterplots for each of the predictor variables
data_path <- "data/sleepdata.csv"
plot_path <- "plots/"
output_path <- "output/"

par(mfrow = c(1, 1))

plot(sleepsubset$ClassesMissed, sleepsubset$GPA, main = "GPA vs Classes Missed", xlab = "Classes Missed", ylab = "GPA")
lm(GPA~ClassesMissed, data = sleepsubset)
abline(lm(GPA~ClassesMissed, data = sleepsubset), col = 'red', lwd = 2)

plot(sleepsubset$CognitionZscore, sleepsubset$GPA, main = "GPA vs Z-Score on Cognitive Test", xlab = "Cognitive Score", ylab = "GPA")
lm(GPA~CognitionZscore, data = sleepsubset)
abline(lm(GPA~CognitionZscore, data = sleepsubset), col = 'red', lwd = 2)

plot(sleepsubset$PoorSleepQuality, sleepsubset$GPA, main = "GPA vs Measure of Sleep Quality", xlab = "Sleep Score", ylab = "GPA")
lm(GPA~PoorSleepQuality, data = sleepsubset)
abline(lm(GPA~PoorSleepQuality, data = sleepsubset), col = 'red', lwd = 2)

plot(sleepsubset$DepressionScore, sleepsubset$GPA, main = "GPA vs Depression Score", xlab = "Depression Score", ylab = "GPA")
lm(GPA~DepressionScore, data = sleepsubset)
abline(lm(GPA~DepressionScore, data = sleepsubset), col = 'red', lwd = 2)

plot(sleepsubset$AnxietyScore, sleepsubset$GPA, main = "GPA vs Anxiety Score", xlab = "Anxiety Score", ylab = "GPA")
lm(GPA~AnxietyScore, data = sleepsubset)
abline(lm(GPA~AnxietyScore, data = sleepsubset), col = 'red', lwd = 2)

plot(sleepsubset$StressScore, sleepsubset$GPA, main = "GPA vs Stress Score", xlab = "Stress Score", ylab = "GPA")
lm(GPA~StressScore, data = sleepsubset)
abline(lm(GPA~StressScore, data = sleepsubset), col = 'red', lwd = 2)

plot(sleepsubset$DASScore, sleepsubset$GPA, main = "GPA vs DAS Score", xlab = "DAS Score", ylab = "GPA")
lm(GPA~DASScore, data = sleepsubset)
abline(lm(GPA~DASScore, data = sleepsubset), col = 'red', lwd = 2)

plot(sleepsubset$Happiness, sleepsubset$GPA, main = "GPA vs Happiness Score", xlab = "Happiness Score", ylab = "GPA")
lm(GPA~Happiness, data = sleepsubset)
abline(lm(GPA~Happiness, data = sleepsubset), col = 'red', lwd = 2)

plot(sleepsubset$AverageSleep, sleepsubset$GPA, main = "GPA vs Average Sleep", xlab = "Average Sleep", ylab = "GPA")
lm(GPA~AverageSleep, data = sleepsubset)
abline(lm(GPA~AverageSleep, data = sleepsubset), col = 'red', lwd = 2)

plot(sleepsubset$AverageSleep, sleepsubset$GPA, main = "GPA vs Drinks", xlab = "Drinks", ylab = "GPA")
lm(GPA~Drinks, data = sleepsubset)
abline(lm(GPA~Drinks, data = sleepsubset), col = 'red', lwd = 2)

plot(sleepsubset$WeekdaySleep, sleepsubset$GPA, main = "GPA vs Weekday Sleep", xlab = "Weekday Sleep", ylab = "GPA")
lm(GPA~WeekdaySleep, data = sleepsubset)
abline(lm(GPA~WeekdaySleep, data = sleepsubset), col = 'red', lwd = 2)



#Simple linear regressions for each predictor variable
summary(GPA~ClassesMissed, data = sleepsubset)
summary(GPA~CognitionZscore, data = sleepsubset)
summary(GPA~PoorSleepQuality, data = sleepsubset)
summary(GPA~DepressionScore, data = sleepsubset)
summary(GPA~AnxietyScore, data = sleepsubset)
summary(GPA~StressScore, data = sleepsubset)
summary(GPA~DASScore, data = sleepsubset)
summary(GPA~Happiness, data = sleepsubset)
summary(GPA~Drinks, data = sleepsubset)
summary(GPA~AverageSleep, data = sleepsubset)
summary(GPA~WeekdaySleep, data = sleepsubset)




#multicollinearity as DAS score contains depression, anxiety, and stress. Therefore I am chosing to remove it
fit2 <- lm(GPA ~ ClassesMissed + CognitionZscore + PoorSleepQuality + DepressionScore + AnxietyScore + StressScore + Happiness + Drinks + AverageSleep + WeekdaySleep, data = sleepsubset)
summary(fit2)

#forward selection
reg1 = regsubsets(GPA ~ ClassesMissed + CognitionZscore + PoorSleepQuality + DepressionScore + AnxietyScore + StressScore + Happiness + Drinks + AverageSleep + WeekdaySleep, data = sleepsubset, method = "forward")
summary(reg1)

#backward selection
reg2 = regsubsets(GPA ~ ClassesMissed + CognitionZscore + PoorSleepQuality + DepressionScore + AnxietyScore + StressScore + Happiness + Drinks + AverageSleep + WeekdaySleep, data = sleepsubset, method = "backward")
summary(reg2)

#adjusted r^2
X = cbind(sleepsubset$ClassesMissed,sleepsubset$CognitionZscore, sleepsubset$PoorSleepQuality, sleepsubset$DepressionScore, sleepsubset$AnxietyScore, sleepsubset$StressScore, sleepsubset$Happiness,sleepsubset$Drinks, sleepsubset$AverageSleep, sleepsubset$WeekdaySleep)
lep <- leaps(X, sleepsubset$GPA,method = 'adjr2', names = c('ClassesMissed', 'CognitionZScore', 'PoorSleepQuality', 'DepressionScore', 'AnxietyScore', 'StressScore', 'Happiness', 'Drinks', 'AverageSleep', 'WeekdaySleep'))
results <- cbind(lep$which, "adjr2" = lep$adjr2)
print(results)

#mallows cp
X = cbind(sleepsubset$ClassesMissed,sleepsubset$CognitionZscore, sleepsubset$PoorSleepQuality, sleepsubset$DepressionScore, sleepsubset$AnxietyScore, sleepsubset$StressScore, sleepsubset$Happiness,sleepsubset$Drinks, sleepsubset$AverageSleep, sleepsubset$WeekdaySleep)
lep <- leaps(X,sleepsubset$GPA,method = 'Cp') 

best_cp_index <- which.min(lep$Cp)
best_model <- lep$which[best_cp_index, ]
best_cp_value <- lep$Cp[best_cp_index]
print(best_model)
print(best_cp_value)

#FINAL MODEL
#Classes Missed, Cognition Zscore, Anxiety Score, Stress, Drinks, Average Sleep are predictors based on lowest cp mallwos
final_model <- lm(GPA ~ CognitionZscore + AnxietyScore + Stress + Drinks + AverageSleep + ClassesMissed, data = sleepsubset)
#residual plot
plot(final_model, which = 1)
#QQ Plot
plot(final_model, which = 2)
