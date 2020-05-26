library(readxl)
getwd()
install.packages("ggplot2")
library(ggplot2)

###1 r=1 vs others

library(anchors)

genius  <- function(i){
  temp0 <- subset(question292, (question292$UserID %in% outlier$UserID))
  temp  <- subset(temp0, QuestionID == i)
  temp2 <- replace.value(temp, c("R"), 2, as.integer(3))
  table(temp2$R, temp2$Answers_0)
  prop.test(table(temp2$R, temp2$Answers_0), correct = FALSE)
}
genius(292)

names(question292)

###2 r=1 vs r=3

geniusV2 <- function(t) {
  temp0 <- subset(question292, (question292$UserID %in% outlier$UserID))
  temp  <- subset(temp0, QuestionID == t & R !=2)
  table(temp$R, temp$Answers_0)
  prop.test(table(temp$R, temp$Answers_0), correct = FALSE) 
}
geniusV2(292)

names(question292)

###3


geniusV3  <- function(e){
  temp0  <- subset(question292, (question292$UserID %in% outlier$UserID))
  temp   <- subset(temp0, QuestionID == e)
  temp2  <- replace.value(temp,   c("RFMM2"), 1112, as.integer(1111))
  temp3  <- replace.value(temp2,  c("RFMM2"), 1113, as.integer(1111))
  temp4  <- replace.value(temp3,  c("RFMM2"), 1121, as.integer(1111))
  temp5  <- replace.value(temp4,  c("RFMM2"), 1122, as.integer(1111))
  temp6  <- replace.value(temp5,  c("RFMM2"), 1123, as.integer(1111))
  temp7  <- replace.value(temp6,  c("RFMM2"), 1131, as.integer(1111))
  temp8  <- replace.value(temp7,  c("RFMM2"), 1132, as.integer(1111))
  temp9  <- replace.value(temp8,  c("RFMM2"), 1133, as.integer(1111))
  temp10 <- replace.value(temp9,  c("RFMM2"), 1211, as.integer(1111))
  temp11 <- replace.value(temp10, c("RFMM2"), 1212, as.integer(1111))
  temp12 <- replace.value(temp11, c("RFMM2"), 1213, as.integer(1111))
  temp13 <- replace.value(temp12, c("RFMM2"), 1221, as.integer(1111))
  temp14 <- replace.value(temp13, c("RFMM2"), 1222, as.integer(1111))
  temp13 <- replace.value(temp14, c("RFMM2"), 1223, as.integer(1111))
  temp16 <- replace.value(temp13, c("RFMM2"), 1231, as.integer(1111))
  temp17 <- replace.value(temp16, c("RFMM2"), 1232, as.integer(1111))
  temp18 <- replace.value(temp17, c("RFMM2"), 1233, as.integer(1111))
  temp19 <- replace.value(temp18, c("RFMM2"), 1311, as.integer(1111))
  temp20 <- replace.value(temp19, c("RFMM2"), 1312, as.integer(1111))
  temp21 <- replace.value(temp20, c("RFMM2"), 1313, as.integer(1111))
  temp22 <- replace.value(temp21, c("RFMM2"), 1321, as.integer(1111))
  temp23 <- replace.value(temp22, c("RFMM2"), 1322, as.integer(1111))
  temp24 <- replace.value(temp23, c("RFMM2"), 1323, as.integer(1111))
  temp25 <- replace.value(temp24, c("RFMM2"), 1331, as.integer(1111))
  temp26 <- replace.value(temp25, c("RFMM2"), 1332, as.integer(1111))
  temp27 <- replace.value(temp26, c("RFMM2"), 1333, as.integer(1111))
  temp28 <- replace.value(temp27, c("RFMM2"), 2111, as.integer(1111))
  temp29 <- replace.value(temp28, c("RFMM2"), 2112, as.integer(1111))
  temp30 <- replace.value(temp29, c("RFMM2"), 2113, as.integer(1111))
  temp31 <- replace.value(temp30, c("RFMM2"), 2121, as.integer(1111))
  temp32 <- replace.value(temp31, c("RFMM2"), 2122, as.integer(1111))
  temp33 <- replace.value(temp32, c("RFMM2"), 2123, as.integer(1111))
  temp34 <- replace.value(temp33, c("RFMM2"), 2131, as.integer(1111))
  temp35 <- replace.value(temp34, c("RFMM2"), 2132, as.integer(1111))
  temp36 <- replace.value(temp35, c("RFMM2"), 2133, as.integer(1111))
  temp37 <- replace.value(temp36, c("RFMM2"), 2211, as.integer(1111))
  temp38 <- replace.value(temp37, c("RFMM2"), 2212, as.integer(1111))
  temp39 <- replace.value(temp38, c("RFMM2"), 2213, as.integer(1111))
  temp40 <- replace.value(temp39, c("RFMM2"), 2221, as.integer(1111))
  temp41 <- replace.value(temp40, c("RFMM2"), 2222, as.integer(1111))
  temp42 <- replace.value(temp41, c("RFMM2"), 2223, as.integer(1111))
  temp43 <- replace.value(temp42, c("RFMM2"), 2231, as.integer(1111))
  temp44 <- replace.value(temp43, c("RFMM2"), 2232, as.integer(1111))
  temp45 <- replace.value(temp44, c("RFMM2"), 2233, as.integer(1111))
  temp46 <- replace.value(temp45, c("RFMM2"), 2311, as.integer(1111))
  temp47 <- replace.value(temp46, c("RFMM2"), 2312, as.integer(1111))
  temp48 <- replace.value(temp47, c("RFMM2"), 2313, as.integer(1111))
  temp49 <- replace.value(temp48, c("RFMM2"), 2321, as.integer(1111))
  temp50 <- replace.value(temp49, c("RFMM2"), 2322, as.integer(1111))
  temp51 <- replace.value(temp50, c("RFMM2"), 2323, as.integer(1111))
  temp52 <- replace.value(temp51, c("RFMM2"), 2331, as.integer(1111))
  temp53 <- replace.value(temp52, c("RFMM2"), 2332, as.integer(1111))
  temp54 <- replace.value(temp53, c("RFMM2"), 2333, as.integer(1111))
  temp55 <- replace.value(temp54, c("RFMM2"), 3111, as.integer(1111))
  temp56 <- replace.value(temp55, c("RFMM2"), 3112, as.integer(1111))
  temp57 <- replace.value(temp56, c("RFMM2"), 3113, as.integer(1111))
  temp58 <- replace.value(temp57, c("RFMM2"), 3121, as.integer(1111))
  temp59 <- replace.value(temp58, c("RFMM2"), 3122, as.integer(1111))
  temp60 <- replace.value(temp59, c("RFMM2"), 3123, as.integer(1111))
  temp61 <- replace.value(temp60, c("RFMM2"), 3131, as.integer(1111))
  temp62 <- replace.value(temp61, c("RFMM2"), 3132, as.integer(1111))
  temp63 <- replace.value(temp62, c("RFMM2"), 3133, as.integer(1111))
  temp64 <- replace.value(temp63, c("RFMM2"), 3211, as.integer(1111))
  temp65 <- replace.value(temp64, c("RFMM2"), 3212, as.integer(1111))
  temp66 <- replace.value(temp65, c("RFMM2"), 3213, as.integer(1111))
  temp67 <- replace.value(temp66, c("RFMM2"), 3221, as.integer(1111))
  temp68 <- replace.value(temp67, c("RFMM2"), 3222, as.integer(1111))
  temp69 <- replace.value(temp68, c("RFMM2"), 3223, as.integer(1111))
  temp70 <- replace.value(temp69, c("RFMM2"), 3231, as.integer(1111))
  temp71 <- replace.value(temp70, c("RFMM2"), 3232, as.integer(1111))
  temp72 <- replace.value(temp71, c("RFMM2"), 3233, as.integer(1111))
  temp73 <- replace.value(temp72, c("RFMM2"), 3311, as.integer(1111))
  temp74 <- replace.value(temp73, c("RFMM2"), 3312, as.integer(1111))
  temp75 <- replace.value(temp74, c("RFMM2"), 3313, as.integer(1111))
  temp76 <- replace.value(temp75, c("RFMM2"), 3321, as.integer(1111))
  temp77 <- replace.value(temp76, c("RFMM2"), 3322, as.integer(1111))
  temp78 <- replace.value(temp77, c("RFMM2"), 3323, as.integer(1111))
  temp79 <- replace.value(temp78, c("RFMM2"), 3331, as.integer(1111))
  temp80 <- replace.value(temp79, c("RFMM2"), 3332, as.integer(1111))
  
  table(temp2$RFMM2, temp2$Answers_0)
  prop.test(table(temp80$RFMM2, temp80$Answers_0), correct = FALSE)
}
geniusV3(292)

names(question292)




