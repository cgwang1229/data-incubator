## load the data
dat <- read.csv("Tweets.csv", header = TRUE)

### new plots for studying the negative reasons ###
library(ggplot2)

## set the levels in order we want
dat2 <- dat[dat$negativereason!="",]
counts2 <- table(dat2$negativereason)
dat2 <- within(dat2, negativereason <- factor(negativereason, levels=names(sort(counts2[names(counts2)!=""], decreasing=TRUE))))
levels(dat2$negativereason)

## overall plot
pdf("negative_reason_overall.pdf", width=6, height=6)
ggplot(dat2, aes(negativereason)) + 
  geom_bar(stat = "count") + 
  labs(x="Negative Reason", y="Overall Count") + 
  ggtitle("Distribution of Negative Reason for All Airlines") +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1),axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

## per airline
pdf("negative_reason_airline.pdf", width=8, height=6)
ggplot(dat2, aes(negativereason)) + 
  geom_freqpoly(aes(group = airline, colour = airline), stat = "count") + 
  labs(x="Negative Reason", y="Count per Airline", colour="Airline") + 
  ggtitle("Distribution of Negative Reason per Airline") +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1),axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

##########################################################################

## old plots
counts <- table(dat$negativereason)
pdf("reason.pdf", width=8, height=6)
par(mar=c(4,10,2,2), oma=c(1.1,0.1,0,0))
barplot(sort(counts[names(counts)!=""]), las=1, cex.names=0.8, horiz=TRUE, xlim=c(0,3000), xlab="Count", main="Distribution of Negative Reasons")
mtext("Reasons ordered by count",cex=0.8, line=0, side=1, adj=0, outer=TRUE)
dev.off()

pdf("sentiment.pdf", width=8, height=6)
ggplot(dat, aes(airline, fill = airline_sentiment)) + 
  geom_bar(position = "fill") + 
  labs(x="Airline", y="Percent", fill="Sentiment", title="Airline Sentiment Proportion")
dev.off()
