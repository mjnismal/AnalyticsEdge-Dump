wine_data = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/834e8a91ad31bfcdf317bf6356b808f2/asset-v1:MITx+15.071x+2T2017+type@asset+block/wine.csv")
  
lm_wine = lm(Price~WinterRain+HarvestRain,data=wine_data)
summary(lm_wine)


lm_wine2 = lm(Price~.,data=wine_data)
summary(lm_wine2)


cor(wine_data$HarvestRain,wine_data$WinterRain)
OBP = 0.361 
SLG =  0.500
OOBP = 0.297
OSLG = 0.370
-837.38+2913.6*OOBP+1514.29*OSLG

-804.63+2737.77*OBP+1584.91*SLG

teamRank = c(1,2,3,3,4,4,4,4,5,5)

wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)
cor(teamRank,wins2013)


NBA_train = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/70b87da0aabdde7b8b4ce73380821e84/asset-v1:MITx+15.071x+2T2017+type@asset+block/NBA_train.csv")
NBA_test = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/59c34f97ad892fb75b757f15812da228/asset-v1:MITx+15.071x+2T2017+type@asset+block/NBA_test.csv")
str(NBA_train)
table(NBA_train$W,NBA_train$Playoffs)



NBA_train$PTSdiff = NBA_train$PTS - NBA_train$oppPTS
plot(NBA_train$PTSdiff,NBA_train$W)


winsreg = lm(W~PTSdiff,data =NBA_train)
summary(winsreg)


Ptsreg=lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK,data = NBA_train)
summary(Ptsreg)


RMSE = sqrt(SSEptsreg/nrow(NBA_train))

Ptsreg=lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL,data = NBA_train)
summary(Ptsreg)

SSEptsreg = sum(Ptsreg$residuals^2)
RMSE = sqrt(SSEptsreg/nrow(NBA_train))

pts_pred = predict(Ptsreg,newdata=NBA_test)


SSE_pred = sum((pts_pred-NBA_test$PTS)^2)

SST_pred = sum((mean(NBA_train$PTS)-NBA_test$PTS)^2)

R2 = 1-SSE_pred/SST_pred

RMSE = sqrt(SSE_pred/nrow(NBA_test))

