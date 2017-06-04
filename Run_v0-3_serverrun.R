
sink("Rlog.txt", append=FALSE, split=TRUE)

source("FunctionList.R")
library(kernlab)


ModelLag = 7

#holding period parameters
hpList <-list()
hpList[[1]] <- 5
hpList[[2]] <- 10
hpList[[3]] <- 15

hpNames <- c("hp1", "hp2", "hp3")
names(hpList) <- hpNames

plTargets <- list()
plTargets[[1]]<- 0.0015
plTargets[[2]]<- 0.0030
plTargets[[3]]<- 0.0050

plNames <- c("pl1", "pl2", "pl3")
names(plTargets) <- plNames

drwDnTgt = 0.9

#download data
data1 <- data.frame()
temp <- tempfile()

for (i in seq(as.Date("2016/10/8"), as.Date("2016/10/15"), 1)) {
  
  if (weekdays(as.Date(i, origin="1970-01-01"))== "Sunday" | 
      weekdays(as.Date(i, origin="1970-01-01"))== "Saturday") next else { 
  # code above generates i as a number rather than a date, need to transform
  date4yr = format(as.Date(i , origin="1970-01-01"), "%Y")
  date2yr = format(as.Date(i , origin="1970-01-01"), "%y")
  
  date2m = format(as.Date(i , origin="1970-01-01"), "%m")
  
  date2day = format(as.Date(i , origin="1970-01-01"), "%d")

  
  download.file(paste("https://www.forexite.com/free_forex_quotes/",date4yr,"/"
      ,date2m,"/",date2day,date2m,date2yr,".zip", sep=""),temp, quiet = TRUE)
  TempData <- read.csv(unz(temp, paste(date2day, date2m, date2yr,".txt",sep="")))
  TempData <- data.frame(TempData,  weekdayNumeric(i))
  data1 <- rbind(data1, TempData)
  unlink(temp)
  
  }
}

print("datadownloaded")

#check deuplicates
anyDuplicated(data1[c(1:3)])



#seperate into currencies
ccyList <- unique(data1[1])

uniqueDateTime <- unique(data1[c(2,3,8)])

ccYKeep <- c("EURUSD", "GBPUSD", "USDCHF", "USDJPY", "USDCAD", "AUDUSD", "AUDJPY", "NZDUSD",
"NZDJPY", "XAUUSD", "XAGUSD", "USDCZK", "USDDKK", "USDRUB", "USDSEK", "USDSGD", "USDZAR", 
"USDHKD", "USDMXN", "USDTRY")


print("Generating data")

DataList <- list()

for (i in 1:length(ccYKeep)) {
  DataList[[i]] <-  merge(data1[data1[1]==ccYKeep[i],], uniqueDateTime, all = TRUE)
  
  DataList[[i]] = DataList[[i]][order(DataList[[i]][1],DataList[[i]][2],DataList[[i]][3]),]

  DataList[[i]][4] = na.lomf(DataList[[i]][4])
  
  DataList[[i]][5:8] = na.lomf(DataList[[i]][5:8])

  #ccyPair <- ccYKeep[i]
  
  #assign(ccyPair, data1[data1[1]==ccYKeep[i],])
  #ccyPair
  #colnames(get(ccyPair)).....
  #above works except for the get part. Something strange due to the syntax. List is a 
  #better way in general
  
  colnames(DataList[[i]]) = c("Date", 
                          "Time",
                          "WKDay", 
                          paste(ccYKeep[i], "Name", sep=""),
                          paste(ccYKeep[i], "Open", sep=""),
                          paste(ccYKeep[i], "High", sep=""),
                          paste(ccYKeep[i], "Low", sep=""),
                          paste(ccYKeep[i], "Close", sep=""))
  
}

names(DataList[i]) = ccYKeep[i]


#create the lagged variables
for(i in 1:length(DataList)) {
  
  
  DataList[[i]]= DataList[[i]][order(DataList[[i]][1], DataList[[i]][2]),]
  
  for(j in 1:ModelLag) {
    
    # c4lagname <- paste(colnames(DataList[[i]][4]), "L_",j, sep="" )
    #   assign(c4lagname , createLagVar(DataList[[i]][4], j))
    # 
    # DataList[[i]] <- data.frame(DataList[[i]], get(c4lagname) = createLagVar(DataList[[i]][4], j))
    DataList[[i]] <- data.frame(DataList[[i]], createLagVar(DataList[[i]][5], j))
    colnames(DataList[[i]])[length(DataList[[i]])] = 
      paste(colnames(DataList[[i]][5]), "L_", j, sep="")
    
    DataList[[i]] <- data.frame(DataList[[i]], createLagVar(DataList[[i]][6], j))
    colnames(DataList[[i]])[length(DataList[[i]])] = 
      paste(colnames(DataList[[i]][6]), "L_", j, sep="")
    
    DataList[[i]] <- data.frame(DataList[[i]], createLagVar(DataList[[i]][7], j))
    colnames(DataList[[i]])[length(DataList[[i]])] = 
      paste(colnames(DataList[[i]][7]), "L_", j, sep="")
    
    DataList[[i]] <- data.frame(DataList[[i]], createLagVar(DataList[[i]][8], j))
    colnames(DataList[[i]])[length(DataList[[i]])] = 
      paste(colnames(DataList[[i]][8]), "L_", j, sep="")
    
    print(paste("i'm still working, i is ",i , sep = ""))
    print(paste("i'm still working, j is ", j, sep = ""))
  }
}



print("datagenerated")

#Generate trading signals
FutShiftLst <- list()
PerctRtnLst <- list()
MinValueList <- list()
MaxValueList <- list()
MinDrwDnList <- list()
MaxDrwDnList <- list()


TSLngList <- list()
TSShtList <- list()

for (i in 1:length(hpList)) {
  FutShiftLst[[i]] <- createLagVar(DataList[[1]][8], hpList[[i]]*-1)
  PerctRtnLst[[i]] <- ( FutShiftLst[[i]] / DataList[[1]][8] ) - 1
  MinValueList[[i]] <- minPeriod(DataList[[1]][7], hpList[[i]]*-1)
  MinDrwDnList[[i]] <- ( MinValueList[[i]] / DataList[[1]][8]) -1
  MaxValueList[[i]] <- maxPeriod(DataList[[1]][6], hpList[[i]]*-1)
  MaxDrwDnList[[i]] <- ( MaxValueList[[i]] / DataList[[1]][8]) -1

  
  # #could consider vectorize
  
  #version 2 onwards looks at max value for long and min value for short
  TSLngList[[i]] <- data.frame(ifelse(MaxDrwDnList[[i]] > plTargets[[1]] &
                                      MinDrwDnList[[i]]> -1 * plTargets[[1]]*drwDnTgt, 1, 0),
                               ifelse(MaxDrwDnList[[i]] > plTargets[[2]] &
                                      MinDrwDnList[[i]]> -1 * plTargets[[2]]*drwDnTgt, 1, 0),
                               ifelse(MaxDrwDnList[[i]] > plTargets[[3]] &
                                        MinDrwDnList[[i]]> -1 * plTargets[[3]]*drwDnTgt, 1, 0)
                               )
  TSShtList[[i]] <- data.frame(ifelse(MinDrwDnList[[i]] < plTargets[[1]] *-1 &
                                        MaxDrwDnList[[i]]< plTargets[[1]]*drwDnTgt, 1, 0),
                               ifelse(MinDrwDnList[[i]] < plTargets[[2]] *-1 &
                                        MaxDrwDnList[[i]]< plTargets[[2]]*drwDnTgt, 1, 0),
                               ifelse(MinDrwDnList[[i]] < plTargets[[3]] * -1 &
                                        MaxDrwDnList[[i]]< plTargets[[3]]*drwDnTgt, 1, 0)
  )

  print(paste("generating signals for i = ", i, sep=""))
  
  }

# #Result Checking
# TempView = 3
# 
# TempLngTS <- data.frame(TSLngList[[TempView]][TSLngList[[TempView]][1] ==1 | 
#                                                 TSLngList[[TempView]][2] ==1 | 
#                                       TSLngList[[TempView]][3] ==1 , ])
# 
# TempShtTS <- data.frame(TSShtList[[TempView]][TSShtList[[TempView]][1] ==1 | 
#                                                 TSShtList[[TempView]][2] ==1 | 
#                                       TSShtList[[TempView]][3] ==1 , ])


#Create Data for ML testing
  #Select Output first and concantenate with source data

MLData <- data.frame(TSLngList[[3]][1], DataList[[1]][-4])
  #Merge with rest of the data
  #***note may lead to some issues. It may be safer to merge and stuff in the beginning in 
  #case there are irregular holidays and stuff. Otherwise lag variables won't make complete
  #sense. should really have calendar and timetable schedule to mark when markets are open 

for (i in 2:length(DataList)) {
  MLData <- merge(MLData, DataList[[i]][-4], all=TRUE, by = c("Date", "Time", "WKDay"), 
                  sort = FALSE)
}

MLData <- MLData[order(MLData[1], MLData[2]),]

#View(data.frame(MLData[1:4], MLData[221:244]))

MLData[2] <- ((MLData[2]%/%10000)*60 + (MLData[2]%%10000)/100 )


#actual sVM testing

ytest<-data.matrix(MLData[4])
ychartest<-data.matrix(as.character(ytest))
xtest<-data.matrix(MLData[-4])

#note encountered a lot of issues. First it didn't seem to work
#when it was numeric values for some reason. Then it kept saying our of bounds or something
#not sure if it was cause of NA values, but realized that interval didn't have any successful
#signlas at all, could have been the source of problem

#also works better if y is a factor (factor classification) instead of regression. I guess
#the nature of the problem is a bit different


#different versions of sVP. we only use classification as regression model rubbish
###svp <- ksvm(xtest, ychartest, kernel = "rbfdot", kpar = "automatic", C = 10, prob.model= TRUE)
#gives an output for parameters in sigmoid function, could be plugged back in to get params
#can leave it in there, might be useful to get it back
#gives same error as normal one. Low error is good

#kfold
###svp <- ksvm(xtest, ychartest, kernel = "rbfdot", kpar = "automatic", C = 10, cross = 4)
#doesn't seem to affect the error
#note error for classification is just percentage of wrongly classified values
#in above case there are 27 missed classifications, total nrow(xtest)-(8-1)-10 NA values

#putting type = "probabilities" to show probability of falling into each class 
#instead of actual output





#rerun with prob model, otherwise gives an error

print("running first svm")

svp <- ksvm(xtest, ychartest, kernel = "rbfdot", kpar = "automatic", C = 10, prob.model= TRUE, cross = 4)

print("done")


ypredprob = predict(svp, xtest[8:nrow(xtest),], type = "probabilities")
ypred = predict(svp, xtest[8:nrow(xtest),])

preddf <-data.frame(ypred, ytest[8:nrow(xtest)], xtest[8:nrow(xtest),])
#View(preddf[preddf[2]=="1"| preddf[1]=="1",])

preddfprob <-data.frame(ypredprob, ytest[8:nrow(xtest)], xtest[8:nrow(xtest),])
#View(preddfprob[preddfprob[3]=="1",])



print("running second svm")
#try linear kernal. According to cookbook when features large maybe just use linear 
svp <- ksvm(xtest, ychartest, kernel = "vanilladot", C = 10, prob.model= TRUE, cross = 4)

print("done")

ypredprob = predict(svp, xtest[8:nrow(xtest),], type = "probabilities")
ypred = predict(svp, xtest[8:nrow(xtest),])

print("notcrashed1")
preddf <-data.frame(ypred, ytest[8:nrow(xtest)], xtest[8:nrow(xtest),])
#View(preddf[preddf[2]=="1"| preddf[1]=="1",])

print("notcrashed2")
preddfprob <-data.frame(ypredprob, ytest[8:nrow(xtest)], xtest[8:nrow(xtest),])
#View(preddfprob[preddfprob[3]=="1",])





#can't figure out how to make plot work...


#test grid search, try for radial model first

# CSearchE <- c(rep.int(-1, 21))
# CSearchC <- c(rep.int(-1, 21))
# 
# for (i in -1:11) {
#   
#   svpCS <- ksvm(xtest, ychartest, kernel = "rbfdot", kpar = "automatic", C = 2^i, 
#                 prob.model= TRUE, cross = 4)
#   
#   CSearchE[i+6] = error(svpCS)
#   CSearchC[i+6] = cross(svpCS)
#   
#   #Both Error and Cross results show that higher C lead to better result. I guess reduce
#   #overfitting? Hard to say with current data however
#   
# }



#########add a graph function ##################
#### extract entries that sandwich a 1 classifier output

chart_hearder= 15
chart_footer = 15



print("notcrashed2")
#hardcoded values to see what is going on. 

jpeg("graphtest.jpg")

print("notcrashed 5")
  plot(preddfprob[3490:5000, 10], type="l")
  par(new = T)
  plot(preddfprob[3490:5000, 3], type="l", axes = F, ylab = '', col = "Gray")
  par(new = T)
  plot(preddfprob[3490:5000, 2], type="l", axes = F, ylab = '', col = "Green")
  axis(side = 4)
  title(main="Lng, HP3, Pl1")

dev.off()


sink()

#try varying holding time, might actually not make sense logically
#need non=tradedate adjutment to accomoate for gap opens



  
  


