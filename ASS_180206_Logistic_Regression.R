# HW1 Popularity of Music Records

music_data = read.csv('https://prod-edxapp.edx-cdn.org/assets/courseware/v1/796af11b376799bbe7769dc645564867/asset-v1:MITx+15.071x+2T2017+type@asset+block/songs.csv')


nrow(music_data[music_data$year==2010,])
summary(music_data)
nrow(music_data[toupper(music_data$artistname)=='MICHAEL JACKSON',])
MJ_bb_data = music_data[toupper(music_data$artistname)=='MICHAEL JACKSON' & music_data$Top10 !=0,]

table(music_data$timesignature)
utils::View(music_data[music_data$tempo==max(music_data$tempo),])

SongsTrain = subset(music_data,year <=2009)
SongsTest = subset(music_data,year ==2010)



nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]

SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

cor(SongsTrain$loudness,SongsTrain$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

testpred = predict(SongsLog3,newdata = SongsTest,type ='response')

table(SongsTest$Top10,testpred>.45)

table(SongsTest$Top10)


# HW2 Predicting Parole Violators
# 
# male: 1 if the parolee is male, 0 if female
# race: 1 if the parolee is white, 2 otherwise
# age: the parolee's age (in years) when he or she was released from prison
# state: a code for the parolee's state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, and 1 is any other state. The three states were selected due to having a high representation in the dataset.
# time.served: the number of months the parolee served in prison (limited by the inclusion criteria to not exceed 6 months).
# max.sentence: the maximum sentence length for all charges, in months (limited by the inclusion criteria to not exceed 18 months).
# multiple.offenses: 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
# crime: a code for the parolee's main crime leading to incarceration. 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
# violator: 1 if the parolee violated the parole, and 0 if the parolee completed the parole without violation.

parole_data = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/7dece252db48059c1324e37815b7f882/asset-v1:MITx+15.071x+2T2017+type@asset+block/parole.csv")
nrow(parole_data)
for(i in c(1,2,4,7,8,9))
parole_data[,i]=as.factor(parole_data[,i])

summary(parole_data)


set.seed(144)
library(caTools)
split = sample.split(parole_data$violator, SplitRatio = 0.7)
train = subset(parole_data, split == TRUE)
test = subset(parole_data, split == FALSE)

parml = glm(violator~.,data = train,family = 'binomial')

summary(parml) 

x=c(1,1,0,50,0,0,0,3,12,0,1,0,0)

exp(sum(parml$coefficients*x))

1/(1+exp(-sum(parml$coefficients*x)))

parmpred = predict(parml,newdata = test,type = 'response')
max(parmpred)

table(test$violator,parmpred>0.5)
12/(11+12)#sensitivty
167/(167+12)#specificity
(167+12)/202   #accuracy

table(test$violator)/202
nrow(test[test$violator==0,])/nrow(test)

install.packages("ROCR")
library(ROCR)
ROCRpredtest =prediction(parmpred,test$violator)

auc = as.numeric(performance(ROCRpredtest,'auc')@y.values)
auc


# HW3 Predicting Loan Repayment

loans = read.csv('https://prod-edxapp.edx-cdn.org/assets/courseware/v1/0103daeca0831af08e233ec9b730dd35/asset-v1:MITx+15.071x+2T2017+type@asset+block/loans.csv')
summary(loans)
table(loans$not.fully.paid)/nrow(loans)


na_data = subset(loans,is.na(loans[,c(5,8,10:13)]))
install.packages('mice')
library(mice)

for(i in c(1,2))
  loans[,i]=as.factor(loans[,i])

set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed



set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

loanmdl=glm(not.fully.paid~.,data = train,family='binomial')
summary(loanmdl)

-10*-9.406e-3
exp(-10*-9.406e-3)

testpred = predict(loanmdl,newdata=test,type='response')

table(test$not.fully.paid,testpred>.5)
(2400+3)/2873

table(test$not.fully.paid)

library(ROCR)

ROCRpredtest =prediction(testpred,test$not.fully.paid)
auc = as.numeric(performance(ROCRpredtest,'auc')@y.values)
auc


bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")

summary(bivariate)


testpred_bi = predict(bivariate,newdata = test,type='response')

max(testpred_bi)

table(test$not.fully.paid,testpred_bi>.5)

ROCRpredtest_bi =prediction(testpred_bi,test$not.fully.paid)
aucbi = as.numeric(performance(ROCRpredtest_bi,'auc')@y.values)
aucbi


test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1
test$pred.risk = 
max(test$profit)


high_int = subset(test,int.rate>=0.15)
mean(high_int$profit)

cutoff = sort(high_int$, decreasing=FALSE)[100]
































































