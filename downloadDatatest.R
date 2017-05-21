
a = c(1)

for (i in seq.Date("2016/10/1", "2016/10/3", 1)) {
  
  print(i)
  print(class(i))
  # code above generates i as a number rather than a date, need to transform
  z = format(as.Date(i , origin="1899-12-30"), "%Y")

  
  #a = c(a, z)
}


for (i in seq(as.Date("2016/10/1"), as.Date("2016/10/3"), 1)) {
  
  print(i)
  print(class(i))
  # code above generates i as a number rather than a date, need to transform
  z = format(as.Date(i , origin="1899-12-30"), "%Y")
  
  
  #a = c(a, z)
}


a = as.Date(17075)


z = format(as.Date("2016/10/1"), "%Y")




temp <- tempfile()
download.file("https://www.forexite.com/free_forex_quotes/2011/11/011111.zip",temp)
data1 <- read.csv(unz(temp, "011111.txt"))
unlink(temp)



download.file("https://www.forexite.com/free_forex_quotes/2011/11/021111.zip",temp)
data2 <- read.csv(unz(temp, "021111.txt"))
unlink(temp)

#append two data series together
rbindtest <- rbind(data1, data2)


#decreasing True just makes the default order decreasing, so the below two are the same
#rank needs to be used for factor type
rbindorder1 <- rbindtest[order(-rbindtest[2], rbindtest[1]),]
rbindorder2 <- rbindtest[order(rbindtest[2], -rank(rbindtest[1]), decreasing = TRUE),]

#check if two dataframes identical . 
#All.equal returns some output/summary of differences if not identical
identical(ibindorder1, rbindorder2)

rbindorder2 <- rbindtest[order(rbindtest[2], -rank(rbindtest[1])),]
all.equal(rbindorder1, rbindorder2)

rbindorder2Seq <- data.frame(rbindorder2, seq(1, nrow(rbindorder2), 1))

#nrow gives number of row entries, NROW works differently depending on input param
nrpw(rbindorder2)


#the order function thing seems to work differently depending on data type.
#doesn't always default to Dataframe type
dupresults <- duplicated(rbindorder2[1])
dupresultsOrder2 <- data.frame(dupresults[order(dupresults[1]),])

any(duplicated(rbindorder2[1][1:anyDuplicated(rbindorder2[1]),]))
any(duplicated(rbindorder2[1][1:anyDuplicated(rbindorder2[1])-1,]))