## display 10 digits of precision
options(digits=10)

## load raw data
data2011 <- read.csv("https://data.nola.gov/api/views/28ec-c8d6/rows.csv", header = TRUE)
data2012 <- read.csv("https://data.nola.gov/api/views/rv3g-ypg7/rows.csv", header = TRUE)
data2013 <- read.csv("https://data.nola.gov/api/views/5fn8-vtui/rows.csv", header = TRUE)
data2014 <- read.csv("https://data.nola.gov/api/views/jsyu-nz5r/rows.csv", header = TRUE)
data2015 <- read.csv("https://data.nola.gov/api/views/w68y-xmk6/rows.csv", header = TRUE)

## add year to each data set
data2011$year <- 2011
data2012$year <- 2012
data2013$year <- 2013
data2014$year <- 2014
data2015$year <- 2015

## merge all individual data sets
mydat <- rbind(data2011,data2012,data2013,data2014,data2015)
attach(mydat)
Ntotal <- nrow(mydat)

## part 1 ##
## Throughout this question, use "Type_" for call types
max(table(Type_))/Ntotal

## part 2 ##
index.valid.response <- intersect(which(TimeArrive!=""), which(TimeDispatch!=""))
time.arrive <- strptime(TimeArrive[index.valid.response], format = "%m/%d/%Y %r", tz = "UTC") #UTC=New Orleans time zone 
time.dispatch <- strptime(TimeDispatch[index.valid.response], format = "%m/%d/%Y %r", tz="UTC")
response.time <- as.numeric(difftime(time.arrive, time.dispatch, units="secs"))
median(response.time[response.time>=0])

## part 3 ##
response.district <- as.factor(PoliceDistrict[index.valid.response])
average.response.time.district <- vector()
for (district in levels(response.district)){
  index <- response.district==district
  response.time.district <- response.time[index]
  average.response.time.district[district] <- mean(response.time.district[response.time.district>=0])
}
diff(range(average.response.time.district))

## part 4 ##
index.valid.district <- which(PoliceDistrict!=0)
type100 <- names(which(table(Type_)>100))
index.valid.type <- which(Type_ %in% type100)
index.type.district <- intersect(index.valid.district, index.valid.type)
suprising.event.type <- Type_[index.type.district] #NOTE: Type "93" has more than 100 event, but all of its district is "0"
## unconditional probability
uncond.prob <- table(suprising.event.type)/length(suprising.event.type)
## conditional probability given district
suprising.event.district <- as.factor(PoliceDistrict[index.type.district])
cond.prob.mat <- matrix(NA, nrow = length(uncond.prob), ncol = length(levels(suprising.event.district)))
rownames(cond.prob.mat) <- names(uncond.prob)
colnames(cond.prob.mat) <- levels(suprising.event.district)
for (district in levels(suprising.event.district)){
  index <- suprising.event.district==district
  suprising.event.type.district <- suprising.event.type[index]
  cond.prob <- table(suprising.event.type.district)/length(suprising.event.type.district)
  cond.prob.mat[,district] <- cond.prob
}
ratio <- cond.prob.mat/as.vector(uncond.prob)
max(ratio, na.rm = TRUE)

## part 5 ##
vol2011 <- table(data2011$Type_)
vol2015 <- table(data2015$Type_)
type.decrease <- intersect(names(vol2011), names(vol2015))
decrease <- vol2015[type.decrease] - vol2011[type.decrease]
pct.decrease <- decrease/vol2011[type.decrease]
largest.decrease.type <- names(pct.decrease)[pct.decrease==min(pct.decrease)]
decrease[largest.decrease.type]/sum(vol2011)

## part 6 ##
time.create <- strptime(TimeCreate, format = "%m/%d/%Y %r", tz = "UTC")
hour <- time.create$hour
disposition.fraction.mat <- matrix(NA, nrow = length(levels(Disposition)), ncol = length(unique(hour)))
rownames(disposition.fraction.mat) <- levels(Disposition)
colnames(disposition.fraction.mat) <- as.character(sort(unique(hour)))
for (h in sort(unique(hour))){
  index <- hour==h
  disposition.hour <- Disposition[index]
  disposition.fraction.hour <- table(disposition.hour)/length(disposition.hour)
  disposition.fraction.mat[,as.character(h)] <- disposition.fraction.hour
}
change <- apply(disposition.fraction.mat, 1, FUN = function(z){diff(range(z))})
max(change)

## part 7 ##
## ignore anonymized locations, i.e. district "0"
dis <- as.factor(PoliceDistrict[index.valid.district])
loc <- Location[index.valid.district]
l1 <- gsub("\\,.*","",loc)
l1 <- gsub("\\(","",l1)
l1 <- as.numeric(l1)
l2 <- gsub(".*\\,","",loc)
l2 <- gsub("\\)","",l2)
l2 <- as.numeric(l2)
## ignore anonymized locations
index.anonymized <- intersect(which(l1<28|l1>32),which(l2<(-92)|l2>(-88)))
l1 <- l1[-index.anonymized]
l2 <- l2[-index.anonymized]
dis <- dis[-index.anonymized]
## calculate standard deviation
l1.sd.vec <- l2.sd.vec <- vector()
for (district in levels(dis)){
  index <- dis==district
  l1.sd.vec[district] <- sd(l1[index])
  l2.sd.vec[district] <- sd(l2[index])
}
## calculate ellipse area and change the unit to kilometer
max(pi*l1.sd.vec*l2.sd.vec*110.94*85.276)

## part 8 ##
common.priority.fraction <- vector()
for (type in levels(Type_)){
  index <- Type_==type
  common.priority.fraction[type] <- max(table(Priority[index]))/sum(index)
}
min(common.priority.fraction)
