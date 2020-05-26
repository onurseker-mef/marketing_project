

install.packages("ggplot2")
library(ggpot2)


outlier$Z_visit <- (outlier$Numberofvisits - mean(outlier$Numberofvisits)) / sd(outlier$Numberofvisits) 
outlier$Z_earn  <- (outlier$TotalEarned - mean(outlier$TotalEarned)) / sd(outlier$TotalEarned)
outlier$Z_M2 <- (outlier$Monetary2 - mean(outlier$Monetary2)) / sd(outlier$Monetary2)
outlier$Z_Lasttransaction <- (outlier$Lasttransaction - mean(outlier$Lasttransaction))/sd(outlier$Lasttransaction) 

qplot(x=outlier$Z_visit, data = outlier)
qplot(x=outlier$Z_earn, data = outlier)
qplot(x=outlier$Z_M2, data = outlier)
qplot(x=outlier$Z_Lasttransaction, data = outlier)


outlier1 <- subset(outlier,outlier$Z_visit<3 & outlier1$Z_visit>-3 )
outlier2 <- subset(outlier1,outlier1$Z_earn <3 &  outlier2$Z_earn >-3 )
outlier3 <- subset(outlier2,outlier2$Z_M2 <3 &  outlier3$Z_M2 >-3 )
outlier4 <- subset(outlier3,outlier3$Z_Lasttransaction <3 &  outlier4$Z_Lasttransaction >-3 )


write.table(outlier4, file = "outlier_zscores.csv", sep = ",")
