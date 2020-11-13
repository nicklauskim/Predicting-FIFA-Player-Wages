# setwd("C:/Users/lihua/Desktop/UCLA/Winter 2020/Stats 101A/Project")
rm(list=ls())

# library(ggplot2)
library(dplyr)
library(car)

train <- read.csv("train_clean_imp2.csv")
test <- read.csv("test_clean_imp2.csv")

############### variable creation

# par(mfrow=c(1,1))
# hist(log(train$WageNew), breaks = seq(1, 14, 0.2))
train$WageNew <- pmax(train$WageNew, exp(7.2))
train$logWageNew <- log(train$WageNew)

train$logClubAvg <- as.vector(tapply(log(train$WageNew), train$Club, mean)[train$Club])
test$logClubAvg <- as.vector(tapply(log(train$WageNew), train$Club, mean)[test$Club])

ClubInfo <- aggregate(select_if(train, is.numeric), list(Club = train$Club), mean)
Club_lm <- lm(logWageNew ~ Overall * I(Overall < 62.6) * I(logWageNew < 7.5), data = ClubInfo)
# summary(Club_lm)
# par(mfrow=c(2,2))
# plot(Club_lm)
# vif(Club_lm)
train$ClubFactor <- Club_lm$residuals[train$Club]
test$ClubFactor <- Club_lm$residuals[test$Club]

# summary(ClubInfo$logWageNew)
# par(mfrow=c(1,1))
# hist(ClubInfo$logWageNew, breaks = seq(7, 12, 0.1))
# summary(ClubInfo$logWageNew)
train$PoorClub <- as.numeric(train$logClubAvg < 7.5)
test$PoorClub <- as.numeric(test$logClubAvg < 7.5)

############# model and diagnostics

model4 <- lm(log(WageNew) ~ Overall + I(Overall < 62) + (Overall:I(Overall < 62) + Overall:I(Overall < 62):PoorClub + PositionNew_2) * Potential +
               logClubAvg + ClubFactor, data = train)
# summary(model4)
# anova(model4)
# par(mfrow=c(2,2))
# plot(model4)
# vif(lm(WageNew ~ Overall + Special + Potential + logClubAvg + ClubFactor, data = train))

########## model validation

cross_val <- function(model, n = 100) {
  mean(replicate(n = n, {
    split <- sample(nrow(train), 0.8*nrow(train))
    train_80 <- train[split,]
    train_20 <- train[-split,]
    cor(exp(predict(update(model, data = train_80), train_20)), train_20$WageNew)^2
  }))
}
# cross_val(model4, n=1000)

# backward/forward AIC/BIC
# step(model4, direction = "backward", k=2)
# step(model4, direction = "backward", k=log(nobs(model3)))
# step(lm(log(WageNew) ~ 1, data = train), scope = formula(model4), direction = "forward", k=2)
# step(lm(log(WageNew) ~ 1, data = train), scope = formula(model4), direction = "forward", k=log(nobs(model3)))


########### model prediction

submission <- data.frame(Ob = test$Ob, WageNew = exp(predict(model4, test)))
# which(is.na(predict(model4, test)))
# write.csv(submission, file = "Kaggle_submission4.csv", row.names = FALSE)


