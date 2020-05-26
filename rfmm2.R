marketing$Tenure <- marketing$Tenure / 365

marketing$Recency   <- as.numeric(marketing$MaxDate)
marketing$Frequency <- marketing$numberOfVisits / marketing$Tenure
marketing$Monetary  <- marketing$TotalEarned / marketing$Tenure
marketing$Monetary2 <- marketing$Monetary2 /marketing$Tenure

marketing$orderR[order(marketing$Recency, decreasing = FALSE)] <- 1:nrow(marketing)
marketing$orderF[order(marketing$Frequency)] <- 1:nrow(marketing)
marketing$orderM[order(marketing$Monetary)] <- 1:nrow(marketing)
marketing$orderM2[order(marketing$Monetary2)] <- 1:nrow(marketing)


marketing$RScore <- cut(marketing$orderR, 3, labels = F)
marketing$FScore <- cut(marketing$orderF, 3, labels = F)
marketing$MScore <- cut(marketing$orderM, 3, labels = F)
marketing$M2Score <- cut(marketing$orderM2, 3, labels = F)


marketing$RFMM2 <- marketing$RScore*1000 + marketing$FScore*100 + marketing$MScore*10 + marketing$M2Score*1
marketing$RFM <- marketing$RScore*100 + marketing$FScore*10 + marketing$MScore*1


ggplot(marketing, aes(factor(RFM))) + geom_bar() +
  ggtitle('Customer Distribution by RFM') +
  labs(x = "RFM Scores", y = "Number of Customers")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

rfm <- subset(marketing, marketing$RScore == 3 & marketing$FScore == 3 & marketing$MScore == 3)
rfm1 <- subset(marketing, marketing$RScore == 1 & marketing$FScore == 1 & marketing$MScore == 1)


ggplot(rfm,aes(factor(RFMM2))) + geom_bar()

ggplot(rfm1,aes(factor(RFMM2))) + geom_bar()


qplot(x=M2Score, xlab = "M2 Scores", ylab = "Number of Customers", data = rfm) +
  facet_wrap (~factor(RFM)) + scale_x_continuous(breaks=1:3)

qplot(x=M2Score, xlab = "M2 Scores", ylab = "Number of Customers", data = rfm1) +
  facet_wrap (~factor(RFM)) + scale_x_continuous(breaks=1:3)