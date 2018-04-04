# UNIT 1
#original source
# data = read.csv(url("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/96f9b8f751467da3a4b8a5be33e32905/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/mvtWeek1.csv")) 

#download first the file from the source
setwd("C:/Users/Popo/Documents/DP-Projects/ANALYTICS EDGE- Edx")
data = read.csv("./Codes & Data/mvtWeek1.csv") 

str(data)

summary(data)
DateConvert = as.Date(strptime(data$Date, "%m/%d/%y %H:%M"))

data$Month = months(DateConvert)

data$Weekday = weekdays(DateConvert)

data$Date = DateConvert
table(data$Month,data$)
hist(data$Date, breaks=100)
boxplot(data$Date~data$Arrest)
table(data[data$Year==2001,]$Arrest)
table(data[data$Year==2007,]$Arrest)
table(data[data$Year==2012,]$Arrest)

sort(table(data$LocationDescription[!data$LocationDescription=="OTHER"]),decreasing = T)
sum(head(sort(table(data$LocationDescription[!data$LocationDescription=="OTHER"]),decreasing = T),n=5))
data2 = data[data$LocationDescription %in% names(head(sort(table(data$LocationDescription[!data$LocationDescription=="OTHER"]),decreasing = T),n=5)),]
data2$LocationDescription= factor(data2$LocationDescription)
prop.table(table(data2$LocationDescription,data2$Arrest),1)
prop.table(table(data2$LocationDescription,data2$Weekday),1)


# UNIT 2
# dat_IBM =   read.csv(url("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/4fc08d10f171aacf2ef61c6b4b5bb4d8/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/IBMStock.csv"))
# dat_GE =   read.csv(url("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/448b8be4693d913c2b5153be0c0e25d6/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/GEStock.csv"))
# dat_PG =   read.csv(url("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/bb6ed54230b5b2e29fb66819ed535da0/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/ProcterGambleStock.csv"))
# dat_CC =   read.csv(url("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/4c30fd7f4f55e537989ca13a6db36289/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/CocaColaStock.csv"))
# dat_BOE =   read.csv(url("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/2e8c9fb294db48e5a999c747b317722d/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/BoeingStock.csv"))



dat_IBM$Date = as.Date(dat_IBM$Date, "%m/%d/%y")

dat_GE$Date = as.Date(dat_GE$Date, "%m/%d/%y")

dat_CC$Date = as.Date(dat_CC$Date, "%m/%d/%y")

dat_PG$Date = as.Date(dat_PG$Date, "%m/%d/%y")

dat_BOE$Date = as.Date(dat_BOE$Date, "%m/%d/%y")
alldata = rbind(dat_IBM,dat_GE,dat_CC,dat_PG,dat_BOE)

summary(alldata)
summary(dat_IBM)
summary(dat_GE)
summary(dat_CC)
summary(dat_PG)
summary(dat_BOE)


sd(dat_PG$StockPrice)

plot(dat_CC$Date,dat_CC$StockPrice,"l",col = "red")
lines(dat_PG$Date, dat_PG$StockPrice)

plot(dat_CC$Date[301:432], dat_CC$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

lines(dat_PG$Date, dat_PG$StockPrice, col = "blue")

lines(dat_IBM$Date, dat_IBM$StockPrice, col = "grey")
lines(dat_BOE$Date, dat_BOE$StockPrice, col = "green")
lines(dat_GE$Date, dat_GE$StockPrice, col = "orange")

abline(v=as.Date(c("2003-03-01")), lwd=2)


plot(dat_CC$Date[301:432], dat_CC$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

lines(dat_PG$Date, dat_PG$StockPrice, col = "blue")

lines(dat_IBM$Date, dat_IBM$StockPrice, col = "grey")
lines(dat_BOE$Date, dat_BOE$StockPrice, col = "green")
lines(dat_GE$Date, dat_GE$StockPrice, col = "orange")

abline(v=as.Date(c("2003-03-01")), lwd=2)


tapply(dat_IBM$StockPrice,months(dat_IBM$Date),mean)
summary(dat_IBM)



tapply(dat_CC$StockPrice,months(dat_CC$Date),mean)
tapply(dat_GE$StockPrice,months(dat_GE$Date),mean)


#UNIT 3
# demo_US = read.csv(url("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/f041f6c100061fc06bc3b6320e6512fa/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/CPSData.csv"))
# Country_CD = read.csv(url("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/763710fa6703caea1cf9c708e31e99a3/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/CountryCodes.csv"))
# Metro_CD = read.csv(url("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/fd88455abc1b5b69112daf70f3bb0c77/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/MetroAreaCodes.csv"))

summary(demo_US)
str(demo_US)


table(demo_US$Citizenship)

table(demo_US$Hispanic,demo_US$Race)

colSums(apply(demo_US,2,is.na))

table(demo_US$Region, is.na(demo_US$Married))

table(demo_US$State, is.na(demo_US$MetroAreaCode))

sum(table(demo_US$State, is.na(demo_US$MetroAreaCode))[,2]==0)

sum(table(demo_US$State, is.na(demo_US$MetroAreaCode))[,1]==0)

table(demo_US$Region, is.na(demo_US$MetroAreaCode))

sort(tapply(is.na(demo_US$MetroAreaCode),demo_US$State,mean),decreasing = T)

demo_US = merge(demo_US, Metro_CD, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)         

str(demo_US)
summary(demo_US)

sort(table(demo_US$MetroArea),decreasing= T)

sum(sort(tapply(demo_US$Race == "Asian",demo_US$MetroArea,mean),decreasing = T)>.2)

sort(tapply(demo_US$Education == "No high school diploma", demo_US$MetroArea, mean,na.rm=T))


demo_US = merge(demo_US, Country_CD, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE) 


str(demo_US)
summary(demo_US)

sort(table(demo_US$Country),decreasing = T)

  
table(demo_US$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", demo_US$Country != "United States")
  
sort(tapply(demo_US$Country =="India" , demo_US$MetroArea, sum,na.rm=T),decreasing = T)

sort(tapply(demo_US$Country =="Brazil" , demo_US$MetroArea, sum,na.rm=T),decreasing = T)

sort(tapply(demo_US$Country =="Somalia" , demo_US$MetroArea, sum,na.rm=T),decreasing = T)


# UNIT 4

poll_dat = read.csv(url("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/f2c3bdcba475ddd1399e001dd98f4fd0/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/AnonymityPoll.csv"))



summary(poll_dat)
str(poll_dat)

table(poll_dat$Smartphone)
summary(poll_dat$Smartphone)


sort(table(poll_dat$Region, poll_dat$State)["Midwest",],decreasing = T)
sort(table(poll_dat$Region, poll_dat$State)["South",],decreasing = T)

table(poll_dat$Smartphone, poll_dat$Internet.Use)

lmtd = subset(poll_dat,Internet.Use ==1 | Smartphone == 1) 

summary(lmtd)
str(l)

table(lmtd$Worry.About.Info)
386/(404+386)

table(lmtd$Anonymity.Possible)
278/(278+475)

table(lmtd$Tried.Masking.Identity)
128/(128+656)


table(lmtd$Privacy.Laws.Effective)
186/(186+541)

hist(lmtd$Age)

max(table(lmtd$Age,lmtd$Info.On.Internet))

plot(jitter(lmtd$Age), jitter(lmtd$Info.On.Internet))


sort(tapply(lmtd$Info.On.Internet, lmtd$Smartphone, mean,na.rm=T),decreasing = T)


sort(tapply(lmtd$Tried.Masking.Identity, lmtd$Smartphone, mean,na.rm=T),decreasing = T)

