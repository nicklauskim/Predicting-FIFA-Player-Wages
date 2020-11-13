rm(list=ls())

library(ggplot2)
library(dplyr)
library(car)
library(corrplot)
library(caret)

train <- read.csv("train_clean_imp2.csv")
test <- read.csv("test_clean_imp2.csv")

train$WageNew <- pmax(train$WageNew, 1000)
train$logWageNew <- log(train$WageNew)



# count NAs per column
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

 
# correlation of every numeric variable with WageNew
corrplot(cor(select_if(train, is.numeric), use = "complete"))
sort(sapply(train, function(x) {cor(as.numeric(x), log(train$WageNew), use = "complete")}), decreasing = T)

# experimenting with Overall

model2 <- lm(log(WageNew) ~ I(Overall^2) + Overall + I(Potential^0.7) + I(Special^3) + International.Reputation + Club, data = train)
summary(model2)
rm(model2)

model2 <- lm(log(WageNew) ~ (poly(Overall,2) + poly(Age,2) + poly(Potential,2))^3, data = train)
summary(model2)

vif(lm(WageNew ~ Overall + Age + I(Potential-Overall), data = train))
 
powerTransform(I(Potential - Overall + 1) ~ 1, data = train)
with(train, hist(log(Potential-Overall+1)))
 
View(train[train$Potential == train$Overall,])
with(train, hist(log(Potential - Overall + 1)))
with(train, hist(Potential / Overall))

atPotential <- train$Potential == train$Overall
model2 <- lm(log(WageNew) ~ Overall + atPotential + Age, data = train)
summary(model2)



# creating new factors 

# Club
ClubInfo <- aggregate(select_if(train, is.numeric), list(Club = train$Club), mean)
ClubInfo$LEAGUE <- train$LEAGUE[!duplicated(train$Club)]
ClubInfo$CONTINENT <- train$CONTINENT[!duplicated(train$Club)]

Club_lm <- lm(logWageNew ~ Overall * I(Overall < 62.6) * I(logWageNew < 7.5) + International.Reputation, data = ClubInfo)
summary(Club_lm)
par(mfrow=c(2,2))
plot(Club_lm)
vif(Club_lm)

summary(ClubInfo$Overall)
summary(ClubInfo$logWageNew)

View(cbind(Club_lm$residuals, ClubInfo))
View(cbind(rstandard(Club_lm), ClubInfo))

train$NoClub <- train$Club == "No Club"

Club_lm$residuals
train$Club

train$PoorClub <- train$logClubAvg < 7.5
test$PoorClub <- test$logClubAvg < 7.5

train$ClubFactor <- Club_lm$residuals[train$Club]
test$ClubFactor <- Club_lm$residuals[test$Club]


# League
LeagueInfo <- aggregate(select_if(train, is.numeric), list(LEAGUE = train$LEAGUE), mean)
League_lm <- lm(logWageNew ~ Overall + International.Reputation, data = LeagueInfo)
summary(League_lm)

train$logLeagueAvg <- tapply(log(train$WageNew), train$LEAGUE, mean)[train$LEAGUE]
test$logLeagueAvg <- tapply(log(train$WageNew), train$LEAGUE, mean)[test$LEAGUE]

train$LeagueFactor <- League_lm$residuals[train$LEAGUE]
test$LeagueFactor <- Club_lm$residuals[test$LEAGUE]


# Position
PositionInfo <- aggregate(select_if(train, is.numeric), list(PositionNew = train$PositionNew_2), mean)

Position_lm <- lm(logWageNew ~ Overall, data = PositionInfo)
summary(Position_lm)
vif(Position_lm)

par(mfrow=c(2,2))
plot(Club_lm)
plot(Position_lm)

train$PositionFactor <- 100*Position_lm$residuals[train$PositionNew_2]
test$PositionFactor <- 100*Position_lm$residuals[test$PositionNew_2]



# actual model

model3 <- lm(log(WageNew) ~ Overall * I(Overall < 62) * PoorClub + I(Potential^0.7) + I(Special^3) +
               International.Reputation + logClubAvg + ClubFactor + PositionNew_2, data = train)
summary(model3)
anova(model3)
par(mfrow=c(2,2))
plot(model3)

vif(lm(WageNew ~ Overall + Special + International.Reputation + Potential + International.Reputation + logClubAvg + ClubFactor + PositionFactor, data = train))

boxplot(model3$residuals ~ train$CONTINENT, las=2)

par(mfrow=c(1,1))
View(cbind(model3$residuals, train))
interaction.plot(train$International.Reputation, train$CONTINENT == "Rest Of World", model3$residuals)



# cross validation

mean(replicate(n = 100, {
  split <- sample(nrow(train), 0.8*nrow(train))
  train_80 <- train[split,]
  train_20 <- train[-split,]
  cor(exp(predict(update(model3, data = train_80), train_20)), train_20$WageNew)^2
}))



# back AIC/BIC
step(model3, direction = "backward", k=2)



# make predictions
predictions <- data.frame(Ob = test$Ob, WageNew = exp(predict(model3, test)))

which(is.na(predict(model3, test)))




