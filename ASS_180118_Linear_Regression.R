
# HW1 Climate Change
cc_data = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/f43727842620758fda4e204cc0d7d558/asset-v1:MITx+15.071x+2T2017+type@asset+block/climate_change.csv")
traincc=subset(cc_data,Year<=2006)
testcc=subset(cc_data,Year>2006)

cc_reg= lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data=traincc)

summary(cc_reg)

summary(lm(Temp~MEI+N2O+TSI+Aerosols,data=traincc))

cc_reg_2 =step(cc_reg)

summary(cc_reg_2)

test =predict(cc_reg_2, newdata = testcc)

SSE_test = sum((testcc$Temp-test)^2)

SST_pred = sum((mean(traincc$Temp)-testcc$Temp)^2)

R2 = 1-SSE_test/SST_pred


#HW2 Reading Test Scores



RMSE = function(lm){
  N = nrow(lm$model)
  SSE = sum((lm$residuals)^2)
  
  RMSE = sqrt(SSE/N)
  
  return(RMSE)
  
}  

tscore_trn = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/d2c0eaf1a7b1aeee047a2be54e0c6173/asset-v1:MITx+15.071x+2T2017+type@asset+block/pisa2009train.csv")

tscore_tst = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/b1cf56f09e79ee9b71b38143560252be/asset-v1:MITx+15.071x+2T2017+type@asset+block/pisa2009test.csv")

summary(tscore_trn)

tapply(tscore_trn$readingScore,tscore_trn$male,mean)


tscore_trn = na.omit(tscore_trn)
tscore_tst = na.omit(tscore_tst)

tscore_trn$raceeth = relevel(tscore_trn$raceeth,"White")
tscore_tst$raceeth = relevel(tscore_tst$raceeth,"White")

lmScr = lm(readingScore~.,data = tscore_trn)
summary(lmScr)
  RMSE(lmScr)

  
pred_tst = predict(lmScr,newdata = tscore_tst)  
SSE_2 = sum((tscore_tst$readingScore-pred_tst)^2)
SSE_2 #test SSE
RMSE = sqrt(SSE_2/nrow(tscore_tst)) #test RMSE

#baseline model
blm = mean(tscore_trn$readingScore)
SST = sum((tscore_tst$readingScore-mean(tscore_trn$readingScore))^2)

1-SSE_2/SST

#HW3 Detecting Flu Epidemics via Search Engine Query Data
FluTrain = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/df331a605387ca8382972c88d2853ddf/asset-v1:MITx+15.071x+2T2017+type@asset+block/FluTrain.csv")

summary(FluTrain)
subset(FluTrain,ILI == max(ILI))
subset(FluTrain,Queries == max(Queries))


hist(FluTrain$ILI)

plot(log(FluTrain$ILI),FluTrain$Queries)
FluTrain$logILI = log(FluTrain$ILI)
FluTrnd1 = lm(log(ILI)~Queries,FluTrain)  
summary(FluTrnd1)


# exp(-0.5*cor(log(FluTrain$ILI),FluTrain$Queries))
# log(1/cor((FluTrain$ILI),FluTrain$Queries))

cor(log(FluTrain$ILI),FluTrain$Queries)^2

FluTest = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/b09d1c001a63a540e853c5250f43d6a5/asset-v1:MITx+15.071x+2T2017+type@asset+block/FluTest.csv")

PredTest1 = exp(predict(FluTrnd1, newdata=FluTest))
PredTest1[11]

(FluTest$ILI[11]-PredTest1[11])/FluTest$ILI[11]
SSE_tst = sum((FluTest$ILI-PredTest1)^2)
RMSE = sqrt(SSE_tst/nrow(FluTest)) 
RMSE



# install.packages("zoo")

library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)

summary(FluTrain$ILILag2)

plot(FluTrain$ILI,FluTrain$ILILag2)

FluTrnd2 = lm(log(ILI)~Queries+log(ILILag2),data = FluTrain)

summary(FluTrnd2)


ILILag3 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)

FluTest$ILILag2 = coredata(ILILag3)
summary(FluTest)
FluTest$ILILag2[1:2] = FluTrain$ILI[416:417]
FluTest$ILILag2[1:2]

pred2 = exp(predict(FluTrnd2,newdata = FluTest))

SSE_tst = sum((FluTest$ILI-pred2)^2)
RMSE = sqrt(SSE_tst/nrow(FluTest)) 
RMSE

#HW 4 State Data (Optional)

state_data = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/8b670b06ca2b35c86d9b2d23b3bfca03/asset-v1:MITx+15.071x+2T2017+type@asset+block/statedata.csv")
  
plot(state_data$x,state_data$y)

summary(state_data)
str(state_data)


tapply(state_data$HS.Grad,state_data$state.region,mean)

boxplot(state_data$Murder~state_data$state.region)


state_lm= lm(Life.Exp~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area,data=state_data)
summary(state_lm)


plot(state_data$Income, state_data$Life.Exp)


state_lm2 = step(state_lm)
summary(state_lm2)

state_pred = predict(state_lm2)

state_data$pred_lifeexp = state_pred

state_data$res = abs(state_lm2$residuals)


#HW 5 Forecasting Elantra Sales (OPTIONAL)

elantra_dt = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/78f6bc572ffdf2bca928179d83723fb0/asset-v1:MITx+15.071x+2T2017+type@asset+block/elantra.csv")
# Month = the month of the year for the observation (1 = January, 2 = February, 3 = March, ...).
# Year = the year of the observation.
# ElantraSales = the number of units of the Hyundai Elantra sold in the United States in the given month.
# Unemployment = the estimated unemployment percentage in the United States in the given month.
# Queries = a (normalized) approximation of the number of Google searches for "hyundai elantra" in the given month.
# CPI_energy = the monthly consumer price index (CPI) for energy for the given month.
# CPI_all = the consumer price index (CPI) for all products for the given month; 
# this is a measure of the magnitude of the prices paid by consumer households for goods and services (e.g., food, clothing, electricity, etc.).
summary(elantra_dt)
elantra_tr= subset(elantra_dt,Year<=2012)
elantra_ts=subset(elantra_dt,Year>2012)

elantra_lm = lm(ElantraSales~ Unemployment+CPI_all+CPI_energy+Queries,data = elantra_tr)
summary(elantra_lm)


elantra_tr$Month = as.factor(elantra_tr$Month)

elantra_lm = lm(ElantraSales~ Month+Unemployment+CPI_all+CPI_energy+Queries,data = elantra_tr)
summary(elantra_lm)



elantra_tr$Month = as.numeric(elantra_tr$Month)
cor(elantra_tr)


elantra_lm2 =step(elantra_lm)
summary(elantra_lm2)


elantra_ts$Month = as.factor(elantra_ts$Month)
elantra_predts = predict(elantra_lm2,newdata=elantra_ts)
elantra_ts$predsales = elantra_predts
elantra_ts$se = (elantra_ts$ElantraSales-elantra_ts$predsales)^2
SSE =sum (elantra_ts$se)

bs_tr= mean(elantra_tr$ElantraSales)
bs_tr
SST=sum((elantra_ts$ElantraSales-bs_tr)^2)

R2 = 1-SSE/SST
R2


elantra_ts$abserr =abs(elantra_ts$predsales-elantra_ts$ElantraSales)
elantra_ts[which.max(elantra_ts$abserr),]




