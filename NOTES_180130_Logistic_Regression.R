
quality = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/cbb7baaf6c8a0559bb6a4549ad194ab8/asset-v1:MITx+15.071x+2T2017+type@asset+block/quality.csv")
 
# install.packages("caTools")

library(caTools)

set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)

qualityTrain = subset(quality, split == TRUE)

qualityTest = subset(quality, split == FALSE)

# Then recall that we built a logistic regression model to predict PoorCare using the R command:
  
  # QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
  QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
  
  predictTest = predict(QualityLog, type="response", newdata=qualityTest)
  summary(QualityLog)
  

  
  
  # install.packages("ROCR")
  library(ROCR)
  
  # 1/(1+exp(-predict(QualityLog)))
  
  ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
  
  auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
  
  
  