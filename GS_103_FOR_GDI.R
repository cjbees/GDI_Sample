#getwd()
#setwd("~/Downloads")
#setwd("~/Desktop")

library(xlsx)
library(reshape2)
library(lme4)
library(dplyr)
library(ggplot2)

#Reading in and formatting data
DF_wide <- read.xlsx("[0.3]GS_unnecesary-columns-omitted-scoring.xlsx", sheetIndex = 1, header = TRUE)
	str(DF_wide)
	colnames(DF_wide)
	
#Only good participants	
DF_wide_participants <- subset(DF_wide, ParticipantFilter == 1)	
	colnames(DF_wide_participants)
	dim(DF_wide_participants)
	str(DF_wide_participants)
	
#Check distributions for all variables - summary
   sapply(DF_wide_participants[,24:ncol(DF_wide_participants)], summary, na.rm = TRUE)
#Frequencies for demographics
	hist(DF_wide_participants$qGender, main = "Count by Gender", xlab = "Gender, 1 = Men, 2 = Women")
	hist(DF_wide_participants$qAge, main = "Count by Age", xlab = "Age")
	
	table(DF_wide_participants$qGender)
	table(DF_wide_participants$qAge)
	table(DF_wide_participants$qNation)
	table(DF_wide_participants$qNatOth) #No responses
	table(DF_wide_participants$qFLang)
	table(DF_wide_participants$qFLangOth)
	
#Distributions of questionnaires
install.packages("moments")
		library(moments)
		
#Skewness - Around 0 is best; within |.5| very little skewness; > |1|, concern.
#Kurtosis - Within |3|

	#IRI
	hist(DF_wide_participants$iri.fs_sum) #Good
		skewness(DF_wide_participants$iri.fs_sum)
		kurtosis(DF_wide_participants$iri.fs_sum)
	hist(DF_wide_participants$iri.ec_sum) #A little kurtosis, 3.08
		skewness(DF_wide_participants$iri.ec_sum)
		kurtosis(DF_wide_participants$iri.ec_sum)
	hist(DF_wide_participants$iri.pt_sum) #Good
		skewness(DF_wide_participants$iri.pt_sum)
		kurtosis(DF_wide_participants$iri.pt_sum)
	hist(DF_wide_participants$iri.pd_sum) #Good
		skewness(DF_wide_participants$iri.pd_sum)
		kurtosis(DF_wide_participants$iri.pd_sum)

#AERT
#Final score
hist(DF_wide_participants$ERT_FinalScore) #Good
		skewness(DF_wide_participants$ERT_FinalScore)
		kurtosis(DF_wide_participants$ERT_FinalScore)
#RT - But, do we care? 

#Is anyone unusually fast on all of them?
# No one  subset(DF_wide_participants[,c("Ident", "rtaert.1.1_3", "rtaert.1.2_3", "rtaert.1.3_3")], rtaert.1.1_3 < 3 & rtaert.1.2_3 < 3 & rtaert.1.3_3 < 3)
subset(DF_wide_participants[,c("Ident", "rtaert.1.1_3")], rtaert.1.1_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.1.2_3")], rtaert.1.2_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.1.3_3")], rtaert.1.3_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.1.4_3")], rtaert.1.4_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.2.1_3")], rtaert.2.1_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.2.2_3")], rtaert.2.2_3 < 3.0) #87 has 0
subset(DF_wide_participants[,c("Ident", "rtaert.2.3_3")], rtaert.2.3_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.2.4_3")], rtaert.2.4_3 < 3.0) #85 has 0
subset(DF_wide_participants[,c("Ident", "rtaert.3.1_3")], rtaert.3.1_3 < 3.0) #85 has 0
subset(DF_wide_participants[,c("Ident", "rtaert.3.2_3")], rtaert.3.2_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.3.3_3")], rtaert.3.3_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.3.4_3")], rtaert.3.4_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.4.1_3")], rtaert.4.1_3 < 3.0) 
subset(DF_wide_participants[,c("Ident", "rtaert.4.2_3")], rtaert.4.2_3 < 3.0) #87 and 93
subset(DF_wide_participants[,c("Ident", "rtaert.4.3_3")], rtaert.4.3_3 < 3.0) #87
subset(DF_wide_participants[,c("Ident", "rtaert.4.4_3")], rtaert.4.4_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.5.1_3")], rtaert.5.1_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.5.2_3")], rtaert.5.2_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.5.3_3")], rtaert.5.3_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.5.4_3")], rtaert.5.4_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.6.1_3")], rtaert.6.1_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.6.2_3")], rtaert.6.2_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.6.3_3")], rtaert.6.3_3 < 3.0)
subset(DF_wide_participants[,c("Ident", "rtaert.6.4_3")], rtaert.6.4_3 < 3.0) #87

hist(DF_wide_participants$rtaert.1.1_3) #Bad
	skewness(DF_wide_participants$rtaert.1.1_3)
	kurtosis(DF_wide_participants$rtaert.1.1_3)                   
 subset(DF_wide_participants[,c("Ident", "rtaert.1.1_3")], abs(rtaert.1.1_3 - mean(rtaert.1.1_3)) > 3*sd(rtaert.1.1_3))
hist(DF_wide_participants$rtaert.1.2_3) #Bad
	skewness(DF_wide_participants$rtaert.1.2_3)
	kurtosis(DF_wide_participants$rtaert.1.2_3)
subset(DF_wide_participants[,c("Ident", "rtaert.1.2_3")], abs(rtaert.1.2_3 - mean(rtaert.1.2_3)) > 3*sd(rtaert.1.2_3))	
hist(DF_wide_participants$rtaert.1.3_3) #Bad
	skewness(DF_wide_participants$rtaert.1.3_3)
	kurtosis(DF_wide_participants$rtaert.1.3_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.1.3_3")], abs(rtaert.1.3_3 - mean(rtaert.1.3_3)) > 3*sd(rtaert.1.3_3))	
hist(DF_wide_participants$rtaert.1.4_3) #Bad
	skewness(DF_wide_participants$rtaert.1.4_3)
	kurtosis(DF_wide_participants$rtaert.1.4_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.1.4_3")], abs(rtaert.1.4_3 - mean(rtaert.1.4_3)) > 3*sd(rtaert.1.4_3))	
hist(DF_wide_participants$rtaert.2.1_3) #Bad
	skewness(DF_wide_participants$rtaert.2.1_3)
	kurtosis(DF_wide_participants$rtaert.2.1_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.2.1_3")], abs(rtaert.2.1_3 - mean(rtaert.2.1_3)) > 3*sd(rtaert.2.1_3))
hist(DF_wide_participants$rtaert.2.2_3) #Bad
	skewness(DF_wide_participants$rtaert.2.2_3)
	kurtosis(DF_wide_participants$rtaert.2.2_3)		
subset(DF_wide_participants[,c("Ident", "rtaert.2.2_3")], abs(rtaert.2.2_3 - mean(rtaert.2.2_3)) > 3*sd(rtaert.2.2_3))
hist(DF_wide_participants$rtaert.2.3_3) #Bad
	skewness(DF_wide_participants$rtaert.2.3_3)
	kurtosis(DF_wide_participants$rtaert.2.3_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.2.3_3")], abs(rtaert.2.3_3 - mean(rtaert.2.3_3)) > 3*sd(rtaert.2.3_3))		
hist(DF_wide_participants$rtaert.2.4_3) #Bad
	skewness(DF_wide_participants$rtaert.2.4_3)
	kurtosis(DF_wide_participants$rtaert.2.4_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.2.4_3")], abs(rtaert.2.4_3 - mean(rtaert.2.4_3)) > 3*sd(rtaert.2.4_3))			
hist(DF_wide_participants$rtaert.3.1_3) #Bad
	skewness(DF_wide_participants$rtaert.3.1_3)
	kurtosis(DF_wide_participants$rtaert.3.1_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.3.1_3")], abs(rtaert.3.1_3 - mean(rtaert.3.1_3)) > 3*sd(rtaert.3.1_3))
hist(DF_wide_participants$rtaert.3.2_3) #Bad
	skewness(DF_wide_participants$rtaert.3.2_3)
	kurtosis(DF_wide_participants$rtaert.3.2_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.3.2_3")], abs(rtaert.3.2_3 - mean(rtaert.3.2_3)) > 3*sd(rtaert.3.2_3))	
hist(DF_wide_participants$rtaert.3.3_3) #Bad
	skewness(DF_wide_participants$rtaert.3.3_3)
	kurtosis(DF_wide_participants$rtaert.3.3_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.3.3_3")], abs(rtaert.3.3_3 - mean(rtaert.3.3_3)) > 3*sd(rtaert.3.3_3))	
hist(DF_wide_participants$rtaert.3.4_3) #Bad
	skewness(DF_wide_participants$rtaert.3.4_3)
	kurtosis(DF_wide_participants$rtaert.3.4_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.3.4_3")], abs(rtaert.3.4_3 - mean(rtaert.3.4_3)) > 3*sd(rtaert.3.4_3))
hist(DF_wide_participants$rtaert.4.1_3) #Bad
	skewness(DF_wide_participants$rtaert.4.1_3)
	kurtosis(DF_wide_participants$rtaert.4.1_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.4.1_3")], abs(rtaert.4.1_3 - mean(rtaert.4.1_3)) > 3*sd(rtaert.4.1_3))
hist(DF_wide_participants$rtaert.4.2_3) #Bad
	skewness(DF_wide_participants$rtaert.4.2_3)
	kurtosis(DF_wide_participants$rtaert.4.2_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.4.2_3")], abs(rtaert.4.2_3 - mean(rtaert.4.2_3)) > 3*sd(rtaert.4.2_3))	
hist(DF_wide_participants$rtaert.4.3_3) #Bad
	skewness(DF_wide_participants$rtaert.4.3_3)
	kurtosis(DF_wide_participants$rtaert.4.3_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.4.3_3")], abs(rtaert.4.3_3 - mean(rtaert.4.3_3)) > 3*sd(rtaert.4.3_3))	
hist(DF_wide_participants$rtaert.4.4_3) #Bad
	skewness(DF_wide_participants$rtaert.4.4_3)
	kurtosis(DF_wide_participants$rtaert.4.4_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.4.4_3")], abs(rtaert.4.4_3 - mean(rtaert.4.4_3)) > 3*sd(rtaert.4.4_3))	
hist(DF_wide_participants$rtaert.5.1_3) #Bad
	skewness(DF_wide_participants$rtaert.5.1_3)
	kurtosis(DF_wide_participants$rtaert.5.1_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.5.1_3")], abs(rtaert.5.1_3 - mean(rtaert.5.1_3)) > 3*sd(rtaert.5.1_3))		
hist(DF_wide_participants$rtaert.5.2_3) #Bad
	skewness(DF_wide_participants$rtaert.5.2_3)
	kurtosis(DF_wide_participants$rtaert.5.2_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.5.2_3")], abs(rtaert.5.2_3 - mean(rtaert.5.2_3)) > 3*sd(rtaert.5.2_3))		
hist(DF_wide_participants$rtaert.5.3_3) #Bad
	skewness(DF_wide_participants$rtaert.5.3_3)
	kurtosis(DF_wide_participants$rtaert.5.3_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.5.3_3")], abs(rtaert.5.3_3 - mean(rtaert.5.3_3)) > 3*sd(rtaert.5.3_3))	
hist(DF_wide_participants$rtaert.5.4_3) #Bad
	skewness(DF_wide_participants$rtaert.5.4_3)
	kurtosis(DF_wide_participants$rtaert.5.4_3)
subset(DF_wide_participants[,c("Ident", "rtaert.5.4_3")], abs(rtaert.5.4_3 - mean(rtaert.5.4_3)) > 3*sd(rtaert.5.4_3))	
hist(DF_wide_participants$rtaert.6.1_3) #Bad
	skewness(DF_wide_participants$rtaert.6.1_3)
	kurtosis(DF_wide_participants$rtaert.6.1_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.6.1_3")], abs(rtaert.6.1_3 - mean(rtaert.6.1_3)) > 3*sd(rtaert.6.1_3))
hist(DF_wide_participants$rtaert.6.2_3) #Bad
	skewness(DF_wide_participants$rtaert.6.2_3)
	kurtosis(DF_wide_participants$rtaert.6.2_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.6.2_3")], abs(rtaert.6.2_3 - mean(rtaert.6.2_3)) > 3*sd(rtaert.6.2_3))	
hist(DF_wide_participants$rtaert.6.3_3) #Bad
	skewness(DF_wide_participants$rtaert.6.3_3)
	kurtosis(DF_wide_participants$rtaert.6.3_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.6.3_3")], abs(rtaert.6.3_3 - mean(rtaert.6.3_3)) > 3*sd(rtaert.6.3_3))	
hist(DF_wide_participants$rtaert.6.4_3) #Bad
	skewness(DF_wide_participants$rtaert.6.4_3)
	kurtosis(DF_wide_participants$rtaert.6.4_3)	
subset(DF_wide_participants[,c("Ident", "rtaert.6.4_3")], abs(rtaert.6.4_3 - mean(rtaert.6.4_3)) > 3*sd(rtaert.6.4_3))	

#Frequencies for N1A1 - Int
apply(DF_wide_participants[,25:33], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1, 25:33)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for N1A1 - Emo
apply(DF_wide_participants[,35:44], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1, 35:44)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)


#Frequencies for N2A2 - Int
apply(DF_wide_participants[,47:55], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1, 47:55)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for N2A2 - Emo
apply(DF_wide_participants[,57:66], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1, 57:66)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)


#Frequencies for N3A3 - Int
apply(DF_wide_participants[,70:78], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1, 70:78)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for N3A3 - Emo
apply(DF_wide_participants[,80:89], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1, 80:89)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)


#Frequencies for N4A4 - Int
apply(DF_wide_participants[,93:101], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1, 93:101)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)


#Frequencies for N4A4 - Emo
apply(DF_wide_participants[,103:112], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1, 103:112)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)


#Frequencies for N5A5 - Int
apply(DF_wide_participants[,116:124], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1, 116:124)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for N5A5 - Emo
apply(DF_wide_participants[,126:135], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1, 126:135)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for N6A6 - Int
apply(DF_wide_participants[,139:147], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,139:147)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for N6A6 - Emo
apply(DF_wide_participants[,149:158], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,149:158)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for N7A7 - Int
apply(DF_wide_participants[,162:170], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,162:170)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for N7A7 - Emo
apply(DF_wide_participants[,172:181], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,172:181)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for N8A8 - Int
apply(DF_wide_participants[,185:193], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,185:193)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for N8A8 - Emo
apply(DF_wide_participants[,195:204], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,195:204)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for G1A1 - Int
apply(DF_wide_participants[,208:216], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,208:216)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for G1A1 - Emo
apply(DF_wide_participants[,218:227], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,218:227)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for G2A2 - Int
apply(DF_wide_participants[,231:239], 2, table)
#Row 40 is NA

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,231:239)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for G2A2 - Emo
apply(DF_wide_participants[,241:250], 2, table)
#Row 40 is NA

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,241:250)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for G3A3 - Int
apply(DF_wide_participants[,264:273], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,264:273)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for G3A3 - Emo
apply(DF_wide_participants[,264:273], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,264:273)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for G4A4 - Int
apply(DF_wide_participants[,277:285], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,277:285)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for G4A4 - Emo
apply(DF_wide_participants[,287:296], 2, table)

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,287:296)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for S1A5 - Int
apply(DF_wide_participants[,300:308], 2, table)
#Row 16 NA

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,300:308)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for S1A5 - Emo
apply(DF_wide_participants[,310:319], 2, table)
#Row 16 NA

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,310:319)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for S2A6 - Int
apply(DF_wide_participants[,323:331], 2, table)
#Row 16 NA

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,323:331)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)
	
#Frequencies for S2A6 - Emo
apply(DF_wide_participants[,333:342], 2, table)
#Row 16 NA

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,333:342)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for S3A7 - Int
apply(DF_wide_participants[,346:354], 2, table)
#Row 16 NA

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,346:354)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for S3A7 - Emo
apply(DF_wide_participants[,356:365], 2, table)
#Row 16 NA

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,356:365)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for S4A8 - Int
apply(DF_wide_participants[,369:377], 2, table)
#Row 16 NA

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,369:377)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Frequencies for S4A8 - Emo
apply(DF_wide_participants[,379:388], 2, table)
#Row 16 NA

#Graph them all
ggplot(data = melt(DF_wide_participants[,c(1,379:388)], id = "Ident"), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Graph Attractiveness, Character 2, Character 5
 
ggplot(data = melt(DF_wide_participants[,c(391:393, 395:397, 399:401, 403:405, 407:409, 411:413, 415:417, 419:421)]), aes(x = value)) + geom_histogram() + facet_wrap(~ variable, ncol = 3)

#Look at hist of at all RTs
hist(DF_wide_participants$rtvid.n1a1_3) #800
	table(DF_wide_participants$rtvid.n1a1_3)
hist(DF_wide_participants$rtqIn.n1a1_3)	
	table(DF_wide_participants$rtqIn.n1a1_3)
hist(DF_wide_participants$rtqEm.n1a1_3)
	table(DF_wide_participants$rtqEm.n1a1_3)

hist(DF_wide_participants$rtvid.n2a2_3)
	table(DF_wide_participants$rtvid.n2a2_3)
hist(DF_wide_participants$rtqIn.n2a2_3)
	table(DF_wide_participants$rtqIn.n2a2_3)
hist(DF_wide_participants$rtqEm.n2a2_3)
	table(DF_wide_participants$rtqEm.n2a2_3)

hist(DF_wide_participants$rtvid.n3a3_3)
	table(DF_wide_participants$rtvid.n3a3_3)
hist(DF_wide_participants$rtqIn.n3a3_3)
	table(DF_wide_participants$rtqIn.n3a3_3)
hist(DF_wide_participants$rtqEm.n3a3_3) #500
	table(DF_wide_participants$rtqEm.n3a3_3)

hist(DF_wide_participants$rtvid.n4a4_3)	
	table(DF_wide_participants$rtvid.n4a4_3)
hist(DF_wide_participants$rtqIn.n4a4_3)
	table(DF_wide_participants$rtqIn.n4a4_3)
hist(DF_wide_participants$rtqEm.n4a4_3)
	table(DF_wide_participants$rtqEm.n4a4_3)

hist(DF_wide_participants$rtvid.n5a5_3) #3000
	table(DF_wide_participants$rtvid.n5a5_3)
hist(DF_wide_participants$rtqIn.n5a5_3)
	table(DF_wide_participants$rtqIn.n5a5_3)
hist(DF_wide_participants$rtqEm.n5a5_3)
	table(DF_wide_participants$rtqEm.n5a5_3)

hist(DF_wide_participants$rtvid.n6a6_3)
	table(DF_wide_participants$rtvid.n6a6_3)
hist(DF_wide_participants$rtqIn.n6a6_3)
	table(DF_wide_participants$rtqIn.n6a6_3)
hist(DF_wide_participants$rtqEm.n6a6_3)
	table(DF_wide_participants$rtqEm.n6a6_3)

hist(DF_wide_participants$rtvid.n7a7_3)
	table(DF_wide_participants$rtvid.n7a7_3)
hist(DF_wide_participants$rtqIn.n7a7_3)
	table(DF_wide_participants$rtqIn.n7a7_3)
hist(DF_wide_participants$rtqEm.n7a7_3) #600
	table(DF_wide_participants$rtqEm.n7a7_3)

hist(DF_wide_participants$rtvid.n8a8_3)
	table(DF_wide_participants$rtvid.n8a8_3)
hist(DF_wide_participants$rtqIn.n8a8_3)
	table(DF_wide_participants$rtqIn.n8a8_3)
hist(DF_wide_participants$rtqEm.n8a8_3)
	table(DF_wide_participants$rtqEm.n8a8_3)

hist(DF_wide_participants$rtvid.g1a1_3)
	table(DF_wide_participants$rtvid.g1a1_3)
hist(DF_wide_participants$rtqIn.g1a1_3) #500
	table(DF_wide_participants$rtqIn.g1a1_3)
hist(DF_wide_participants$rtqEm.g1a1_3)
	table(DF_wide_participants$rtqEm.g1a1_3)

hist(DF_wide_participants$rtvid.g2a2_3) #1200
	table(DF_wide_participants$rtvid.g2a2_3)
hist(DF_wide_participants$rtqIn.g2a2_3)
	table(DF_wide_participants$rtqIn.g2a2_3)
hist(DF_wide_participants$rtqEm.g2a2_3)
	table(DF_wide_participants$rtqEm.g2a2_3)

hist(DF_wide_participants$rtvid.g3a3_3)
	table(DF_wide_participants$rtvid.g3a3_3)
hist(DF_wide_participants$rtqIn.g3a3_3)
	table(DF_wide_participants$rtqIn.g3a3_3)
hist(DF_wide_participants$rtqEm.g3a3_3)
	table(DF_wide_participants$rtqEm.g3a3_3)

hist(DF_wide_participants$rtvid.g4a4_3) #1000
	table(DF_wide_participants$rtvid.g4a4_3)
hist(DF_wide_participants$rtqIn.g4a4_3)
	table(DF_wide_participants$rtqIn.g4a4_3)
hist(DF_wide_participants$rtqEm.g4a4_3) #500
	table(DF_wide_participants$rtqEm.g4a4_3)

hist(DF_wide_participants$rtvid.s1a5_3)
	table(DF_wide_participants$rtvid.s1a5_3)
hist(DF_wide_participants$rtqIn.s1a5_3)
	table(DF_wide_participants$rtqIn.s1a5_3)
hist(DF_wide_participants$rtqEm.s1a5_3) #500
	table(DF_wide_participants$rtqEm.s1a5_3)

hist(DF_wide_participants$rtvid.s2a6_3)
	table(DF_wide_participants$rtvid.s2a6_3)
hist(DF_wide_participants$rtqIn.s2a6_3)
	table(DF_wide_participants$rtqIn.s2a6_3)
hist(DF_wide_participants$rtqEm.s2a6_3)
	table(DF_wide_participants$rtqEm.s2a6_3)

hist(DF_wide_participants$rtvid.s3a7_3) #800
	table(DF_wide_participants$rtvid.s3a7_3)
hist(DF_wide_participants$rtqIn.s3a7_3)
	table(DF_wide_participants$rtqIn.s3a7_3)
hist(DF_wide_participants$rtqEm.s3a7_3)
	table(DF_wide_participants$rtqEm.s3a7_3)

hist(DF_wide_participants$rtvid.s4a8_3)
	table(DF_wide_participants$rtvid.s4a8_3)
hist(DF_wide_participants$rtqIn.s4a8_3)
	table(DF_wide_participants$rtqIn.s4a8_3)
hist(DF_wide_participants$rtqEm.s4a8_3)
	table(DF_wide_participants$rtqEm.s4a8_3)

	
#Make aggregated Scores
#For all variables, summing the 4 videos of g, the four videos of s, the four videos of n-g and n-s.
#Int, N, Actors 1-4
DF_wide_participants$qInt.n1a1_4_1 <- rowSums(cbind(DF_wide_participants$qInt.n1a1_1, DF_wide_participants$qInt.n2a2_1, DF_wide_participants$qInt.n3a3_1, DF_wide_participants$qInt.n4a4_1), na.rm=TRUE)

DF_wide_participants$qInt.n1a1_4_2 <- rowSums(cbind(DF_wide_participants$qInt.n1a1_2, DF_wide_participants$qInt.n2a2_2, DF_wide_participants$qInt.n3a3_2, DF_wide_participants$qInt.n4a4_2), na.rm=TRUE)

DF_wide_participants$qInt.n1a1_4_3 <- rowSums(cbind(DF_wide_participants$qInt.n1a1_3, DF_wide_participants$qInt.n2a2_3, DF_wide_participants$qInt.n3a3_3, DF_wide_participants$qInt.n4a4_3), na.rm=TRUE)

DF_wide_participants$qInt.n1a1_4_4 <- rowSums(cbind(DF_wide_participants$qInt.n1a1_4, DF_wide_participants$qInt.n2a2_4, DF_wide_participants$qInt.n3a3_4, DF_wide_participants$qInt.n4a4_4), na.rm=TRUE)

DF_wide_participants$qInt.n1a1_4_5 <- rowSums(cbind(DF_wide_participants$qInt.n1a1_5, DF_wide_participants$qInt.n2a2_5, DF_wide_participants$qInt.n3a3_5, DF_wide_participants$qInt.n4a4_5), na.rm=TRUE)

DF_wide_participants$qInt.n1a1_4_6 <- rowSums(cbind(DF_wide_participants$qInt.n1a1_6, DF_wide_participants$qInt.n2a2_6, DF_wide_participants$qInt.n3a3_6, DF_wide_participants$qInt.n4a4_6), na.rm=TRUE)

DF_wide_participants$qInt.n1a1_4_7 <- rowSums(cbind(DF_wide_participants$qInt.n1a1_7, DF_wide_participants$qInt.n2a2_7, DF_wide_participants$qInt.n3a3_7, DF_wide_participants$qInt.n4a4_7), na.rm=TRUE)

DF_wide_participants$qInt.n1a1_4_8 <- rowSums(cbind(DF_wide_participants$qInt.n1a1_8, DF_wide_participants$qInt.n2a2_8, DF_wide_participants$qInt.n3a3_8, DF_wide_participants$qInt.n4a4_8), na.rm=TRUE)

DF_wide_participants$qInt.n1a1_4_9 <- rowSums(cbind(DF_wide_participants$qInt.n1a1_9, DF_wide_participants$qInt.n2a2_9, DF_wide_participants$qInt.n3a3_9, DF_wide_participants$qInt.n4a4_9), na.rm=TRUE)


#Emo, N, Actors 1-4
DF_wide_participants$qEmo.n1a1_4_1 <- rowSums(cbind(DF_wide_participants$qEmo.n1a1_1, DF_wide_participants$qEmo.n2a2_1, DF_wide_participants$qEmo.n3a3_1, DF_wide_participants$qEmo.n4a4_1), na.rm=TRUE)

DF_wide_participants$qEmo.n1a1_4_2 <- rowSums(cbind(DF_wide_participants$qEmo.n1a1_2, DF_wide_participants$qEmo.n2a2_2, DF_wide_participants$qEmo.n3a3_2, DF_wide_participants$qEmo.n4a4_2), na.rm=TRUE)

DF_wide_participants$qEmo.n1a1_4_3 <- rowSums(cbind(DF_wide_participants$qEmo.n1a1_3, DF_wide_participants$qEmo.n2a2_3, DF_wide_participants$qEmo.n3a3_3, DF_wide_participants$qEmo.n4a4_3), na.rm=TRUE)

DF_wide_participants$qEmo.n1a1_4_4 <- rowSums(cbind(DF_wide_participants$qEmo.n1a1_4, DF_wide_participants$qEmo.n2a2_4, DF_wide_participants$qEmo.n3a3_4, DF_wide_participants$qEmo.n4a4_4), na.rm=TRUE)

DF_wide_participants$qEmo.n1a1_4_5 <- rowSums(cbind(DF_wide_participants$qEmo.n1a1_5, DF_wide_participants$qEmo.n2a2_5, DF_wide_participants$qEmo.n3a3_5, DF_wide_participants$qEmo.n4a4_5), na.rm=TRUE)

#DF_wide_participants$qEmo.n1a1_4_6 <- rowSums(cbind(DF_wide_participants$qEmo.n1a1_6_R, DF_wide_participants$qEmo.n2a2_6_R, DF_wide_participants$qEmo.n3a3_6_R, DF_wide_participants$qEmo.n4a4_6_R), na.rm=TRUE)

DF_wide_participants$qEmo.n1a1_4_6_R <- (40 - DF_wide_participants$qEmo.n1a1_4_6)


DF_wide_participants$qEmo.n1a1_4_7 <- rowSums(cbind(DF_wide_participants$qEmo.n1a1_7, DF_wide_participants$qEmo.n2a2_7, DF_wide_participants$qEmo.n3a3_7, DF_wide_participants$qEmo.n4a4_7), na.rm=TRUE)

#DF_wide_participants$qEmo.n1a1_4_8 <- rowSums(cbind(DF_wide_participants$qEmo.n1a1_8_R, DF_wide_participants$qEmo.n2a2_8_R, DF_wide_participants$qEmo.n3a3_8_R, DF_wide_participants$qEmo.n4a4_8_R), na.rm=TRUE)

DF_wide_participants$qEmo.n1a1_4_8_R <- (40 - DF_wide_participants$qEmo.n1a1_4_8)

#DF_wide_participants$qEmo.n1a1_4_9 <- rowSums(cbind(DF_wide_participants$qEmo.n1a1_9, DF_wide_participants$qEmo.n2a2_9, DF_wide_participants$qEmo.n3a3_9, DF_wide_participants$qEmo.n4a4_9), na.rm=TRUE)

DF_wide_participants$qEmo.n1a1_4_9_R <- (40 - DF_wide_participants$qEmo.n1a1_4_9)

DF_wide_participants$qEmo.n1a1_4_10 <- rowSums(cbind(DF_wide_participants$qEmo.n1a1_10, DF_wide_participants$qEmo.n2a2_10, DF_wide_participants$qEmo.n3a3_10, DF_wide_participants$qEmo.n4a4_10), na.rm=TRUE)

#Int, Neutral, Actors 5-8
DF_wide_participants$qInt.n5a5_8_1 <- rowSums(cbind(DF_wide_participants$qInt.n5a5_1, DF_wide_participants$qInt.n6a6_1, DF_wide_participants$qInt.n7a7_1, DF_wide_participants$qInt.n8a8_1), na.rm=TRUE)

DF_wide_participants$qInt.n5a5_8_2 <- rowSums(cbind(DF_wide_participants$qInt.n5a5_2, DF_wide_participants$qInt.n6a6_2, DF_wide_participants$qInt.n7a7_2, DF_wide_participants$qInt.n8a8_2), na.rm=TRUE)

DF_wide_participants$qInt.n5a5_8_3 <- rowSums(cbind(DF_wide_participants$qInt.n5a5_3, DF_wide_participants$qInt.n6a6_3, DF_wide_participants$qInt.n7a7_3, DF_wide_participants$qInt.n8a8_3), na.rm=TRUE)

DF_wide_participants$qInt.n5a5_8_4 <- rowSums(cbind(DF_wide_participants$qInt.n5a5_4, DF_wide_participants$qInt.n6a6_4, DF_wide_participants$qInt.n7a7_4, DF_wide_participants$qInt.n8a8_4), na.rm=TRUE)

DF_wide_participants$qInt.n5a5_8_5 <- rowSums(cbind(DF_wide_participants$qInt.n5a5_5, DF_wide_participants$qInt.n6a6_5, DF_wide_participants$qInt.n7a7_5, DF_wide_participants$qInt.n8a8_5), na.rm=TRUE)

DF_wide_participants$qInt.n5a5_8_6 <- rowSums(cbind(DF_wide_participants$qInt.n5a5_6, DF_wide_participants$qInt.n6a6_6, DF_wide_participants$qInt.n7a7_6, DF_wide_participants$qInt.n8a8_6), na.rm=TRUE)

DF_wide_participants$qInt.n5a5_8_7 <- rowSums(cbind(DF_wide_participants$qInt.n5a5_7, DF_wide_participants$qInt.n6a6_7, DF_wide_participants$qInt.n7a7_7, DF_wide_participants$qInt.n8a8_7), na.rm=TRUE)

DF_wide_participants$qInt.n5a5_8_8 <- rowSums(cbind(DF_wide_participants$qInt.n5a5_8, DF_wide_participants$qInt.n6a6_8, DF_wide_participants$qInt.n7a7_8, DF_wide_participants$qInt.n8a8_8), na.rm=TRUE)

DF_wide_participants$qInt.n5a5_8_9 <- rowSums(cbind(DF_wide_participants$qInt.n5a5_9, DF_wide_participants$qInt.n6a6_9, DF_wide_participants$qInt.n7a7_9, DF_wide_participants$qInt.n8a8_9), na.rm=TRUE)

#Emo, N, Actors 5-8

DF_wide_participants$qEmo.n5a5_8_1 <- rowSums(cbind(DF_wide_participants$qEmo.n5a5_1, DF_wide_participants$qEmo.n6a6_1, DF_wide_participants$qEmo.n7a7_1, DF_wide_participants$qEmo.n8a8_1), na.rm=TRUE)

DF_wide_participants$qEmo.n5a5_8_2 <- rowSums(cbind(DF_wide_participants$qEmo.n5a5_2, DF_wide_participants$qEmo.n6a6_2, DF_wide_participants$qEmo.n7a7_2, DF_wide_participants$qEmo.n8a8_2), na.rm=TRUE)

DF_wide_participants$qEmo.n5a5_8_3 <- rowSums(cbind(DF_wide_participants$qEmo.n5a5_3, DF_wide_participants$qEmo.n6a6_3, DF_wide_participants$qEmo.n7a7_3, DF_wide_participants$qEmo.n8a8_3), na.rm=TRUE)

DF_wide_participants$qEmo.n5a5_8_4 <- rowSums(cbind(DF_wide_participants$qEmo.n5a5_4, DF_wide_participants$qEmo.n6a6_4, DF_wide_participants$qEmo.n7a7_4, DF_wide_participants$qEmo.n8a8_4), na.rm=TRUE)

DF_wide_participants$qEmo.n5a5_8_5 <- rowSums(cbind(DF_wide_participants$qEmo.n5a5_5, DF_wide_participants$qEmo.n6a6_5, DF_wide_participants$qEmo.n7a7_5, DF_wide_participants$qEmo.n8a8_5), na.rm=TRUE)

#DF_wide_participants$qEmo.n5a5_8_6 <- rowSums(cbind(DF_wide_participants$qEmo.n5a5_6, DF_wide_participants$qEmo.n6a6_6, DF_wide_participants$qEmo.n7a7_6, DF_wide_participants$qEmo.n8a8_6), na.rm=TRUE)

DF_wide_participants$qEmo.n5a5_8_6_R <- (40 - DF_wide_participants$qEmo.n5a5_8_6)

DF_wide_participants$qEmo.n5a5_8_7 <- rowSums(cbind(DF_wide_participants$qEmo.n5a5_7, DF_wide_participants$qEmo.n6a6_7, DF_wide_participants$qEmo.n7a7_7, DF_wide_participants$qEmo.n8a8_7), na.rm=TRUE)

#DF_wide_participants$qEmo.n5a5_8_8 <- rowSums(cbind(DF_wide_participants$qEmo.n5a5_8, DF_wide_participants$qEmo.n6a6_8, DF_wide_participants$qEmo.n7a7_8, DF_wide_participants$qEmo.n8a8_8), na.rm=TRUE)

DF_wide_participants$qEmo.n5a5_8_8_R <- (40 - DF_wide_participants$qEmo.n5a5_8_8)

#DF_wide_participants$qEmo.n5a5_8_9 <- rowSums(cbind(DF_wide_participants$qEmo.n5a5_9, DF_wide_participants$qEmo.n6a6_9, DF_wide_participants$qEmo.n7a7_9, DF_wide_participants$qEmo.n8a8_9), na.rm=TRUE)

DF_wide_participants$qEmo.n5a5_8_9_R <- (40 - DF_wide_participants$qEmo.n5a5_8_9)

DF_wide_participants$qEmo.n5a5_8_10 <- rowSums(cbind(DF_wide_participants$qEmo.n5a5_10, DF_wide_participants$qEmo.n6a6_10, DF_wide_participants$qEmo.n7a7_10, DF_wide_participants$qEmo.n8a8_10), na.rm=TRUE)

#Emotion (G), Int, Actors 1-4

DF_wide_participants$qInt.g1a1_4_1 <- rowSums(cbind(DF_wide_participants$qInt.g1a1_1, DF_wide_participants$qInt.g2a2_1, DF_wide_participants$qInt.g3a3_1, DF_wide_participants$qInt.g4a4_1), na.rm=TRUE)
	
DF_wide_participants$qInt.g1a1_4_2 <- rowSums(cbind(DF_wide_participants$qInt.g1a1_2, DF_wide_participants$qInt.g2a2_2, DF_wide_participants$qInt.g3a3_2, DF_wide_participants$qInt.g4a4_2), na.rm=TRUE)

DF_wide_participants$qInt.g1a1_4_3 <- rowSums(cbind(DF_wide_participants$qInt.g1a1_3, DF_wide_participants$qInt.g2a2_3, DF_wide_participants$qInt.g3a3_3, DF_wide_participants$qInt.g4a4_3), na.rm=TRUE)	

DF_wide_participants$qInt.g1a1_4_4 <- rowSums(cbind(DF_wide_participants$qInt.g1a1_4, DF_wide_participants$qInt.g2a2_4, DF_wide_participants$qInt.g3a3_4, DF_wide_participants$qInt.g4a4_4), na.rm=TRUE)

DF_wide_participants$qInt.g1a1_4_5 <- rowSums(cbind(DF_wide_participants$qInt.g1a1_5, DF_wide_participants$qInt.g2a2_5, DF_wide_participants$qInt.g3a3_5, DF_wide_participants$qInt.g4a4_5), na.rm=TRUE)

DF_wide_participants$qInt.g1a1_4_6 <- rowSums(cbind(DF_wide_participants$qInt.g1a1_6, DF_wide_participants$qInt.g2a2_6, DF_wide_participants$qInt.g3a3_6, DF_wide_participants$qInt.g4a4_6), na.rm=TRUE)

DF_wide_participants$qInt.g1a1_4_7 <- rowSums(cbind(DF_wide_participants$qInt.g1a1_7, DF_wide_participants$qInt.g2a2_7, DF_wide_participants$qInt.g3a3_7, DF_wide_participants$qInt.g4a4_7), na.rm=TRUE)

DF_wide_participants$qInt.g1a1_4_8 <- rowSums(cbind(DF_wide_participants$qInt.g1a1_8, DF_wide_participants$qInt.g2a2_8, DF_wide_participants$qInt.g3a3_8, DF_wide_participants$qInt.g4a4_8), na.rm=TRUE)

DF_wide_participants$qInt.g1a1_4_9 <- rowSums(cbind(DF_wide_participants$qInt.g1a1_9, DF_wide_participants$qInt.g2a2_9, DF_wide_participants$qInt.g3a3_9, DF_wide_participants$qInt.g4a4_9), na.rm=TRUE)

#Emotion (G), Emo, Actors 1-4


DF_wide_participants$qEmo.g1a1_4_1 <- rowSums(cbind(DF_wide_participants$qEmo.g1a1_1, DF_wide_participants$qEmo.g2a2_1, DF_wide_participants$qEmo.g3a3_1, DF_wide_participants$qEmo.g4a4_1), na.rm=TRUE)
	
DF_wide_participants$qEmo.g1a1_4_2 <- rowSums(cbind(DF_wide_participants$qEmo.g1a1_2, DF_wide_participants$qEmo.g2a2_2, DF_wide_participants$qEmo.g3a3_2, DF_wide_participants$qEmo.g4a4_2), na.rm=TRUE)

DF_wide_participants$qEmo.g1a1_4_3 <- rowSums(cbind(DF_wide_participants$qEmo.g1a1_3, DF_wide_participants$qEmo.g2a2_3, DF_wide_participants$qEmo.g3a3_3, DF_wide_participants$qEmo.g4a4_3), na.rm=TRUE)	

DF_wide_participants$qEmo.g1a1_4_4 <- rowSums(cbind(DF_wide_participants$qEmo.g1a1_4, DF_wide_participants$qEmo.g2a2_4, DF_wide_participants$qEmo.g3a3_4, DF_wide_participants$qEmo.g4a4_4), na.rm=TRUE)

DF_wide_participants$qEmo.g1a1_4_5 <- rowSums(cbind(DF_wide_participants$qEmo.g1a1_5, DF_wide_participants$qEmo.g2a2_5, DF_wide_participants$qEmo.g3a3_5, DF_wide_participants$qEmo.g4a4_5), na.rm=TRUE)

#DF_wide_participants$qEmo.g1a1_4_6 <- rowSums(cbind(DF_wide_participants$qEmo.g1a1_6, DF_wide_participants$qEmo.g2a2_6, DF_wide_participants$qEmo.g3a3_6, DF_wide_participants$qEmo.g4a4_6), na.rm=TRUE)

DF_wide_participants$qEmo.g1a1_4_6_R <- (40 - DF_wide_participants$qEmo.g1a1_4_6)

DF_wide_participants$qEmo.g1a1_4_7 <- rowSums(cbind(DF_wide_participants$qEmo.g1a1_7, DF_wide_participants$qEmo.g2a2_7, DF_wide_participants$qEmo.g3a3_7, DF_wide_participants$qEmo.g4a4_7), na.rm=TRUE)

#DF_wide_participants$qEmo.g1a1_4_8 <- rowSums(cbind(DF_wide_participants$qEmo.g1a1_8, DF_wide_participants$qEmo.g2a2_8, DF_wide_participants$qEmo.g3a3_8, DF_wide_participants$qEmo.g4a4_8), na.rm=TRUE)

DF_wide_participants$qEmo.g1a1_4_8_R <- (40 - DF_wide_participants$qEmo.g1a1_4_8)

#DF_wide_participants$qEmo.g1a1_4_9 <- rowSums(cbind(DF_wide_participants$qEmo.g1a1_9, DF_wide_participants$qEmo.g2a2_9, DF_wide_participants$qEmo.g3a3_9, DF_wide_participants$qEmo.g4a4_9), na.rm=TRUE)

DF_wide_participants$qEmo.g1a1_4_9_R <- (40 - DF_wide_participants$qEmo.g1a1_4_9 )

DF_wide_participants$qEmo.g1a1_4_10 <- rowSums(cbind(DF_wide_participants$qEmo.g1a1_10, DF_wide_participants$qEmo.g2a2_10, DF_wide_participants$qEmo.g3a3_10, DF_wide_participants$qEmo.g4a4_10), na.rm=TRUE)

#Emotion (Shame), Int, Actors 5-8

DF_wide_participants$qInt.s1a5_8_1 <- rowSums(cbind(DF_wide_participants$qInt.s1a5_1, DF_wide_participants$qInt.s2a6_1, DF_wide_participants$qInt.s3a7_1, DF_wide_participants$qInt.s4a8_1), na.rm=TRUE)
	
DF_wide_participants$qInt.s1a5_8_2 <- rowSums(cbind(DF_wide_participants$qInt.s1a5_2, DF_wide_participants$qInt.s2a6_2, DF_wide_participants$qInt.s3a7_2, DF_wide_participants$qInt.s4a8_2), na.rm=TRUE)

DF_wide_participants$qInt.s1a5_8_3 <- rowSums(cbind(DF_wide_participants$qInt.s1a5_3, DF_wide_participants$qInt.s2a6_3, DF_wide_participants$qInt.s3a7_3, DF_wide_participants$qInt.s4a8_3), na.rm=TRUE)	

DF_wide_participants$qInt.s1a5_8_4 <- rowSums(cbind(DF_wide_participants$qInt.s1a5_4, DF_wide_participants$qInt.s2a6_4, DF_wide_participants$qInt.s3a7_4, DF_wide_participants$qInt.s4a8_4), na.rm=TRUE)

DF_wide_participants$qInt.s1a5_8_5 <- rowSums(cbind(DF_wide_participants$qInt.s1a5_5, DF_wide_participants$qInt.s2a6_5, DF_wide_participants$qInt.s3a7_5, DF_wide_participants$qInt.s4a8_5), na.rm=TRUE)

DF_wide_participants$qInt.s1a5_8_6 <- rowSums(cbind(DF_wide_participants$qInt.s1a5_6, DF_wide_participants$qInt.s2a6_6, DF_wide_participants$qInt.s3a7_6, DF_wide_participants$qInt.s4a8_6), na.rm=TRUE)

DF_wide_participants$qInt.s1a5_8_7 <- rowSums(cbind(DF_wide_participants$qInt.s1a5_7, DF_wide_participants$qInt.s2a6_7, DF_wide_participants$qInt.s3a7_7, DF_wide_participants$qInt.s4a8_7), na.rm=TRUE)

DF_wide_participants$qInt.s1a5_8_8 <- rowSums(cbind(DF_wide_participants$qInt.s1a5_8, DF_wide_participants$qInt.s2a6_8, DF_wide_participants$qInt.s3a7_8, DF_wide_participants$qInt.s4a8_8), na.rm=TRUE)

DF_wide_participants$qInt.s1a5_8_9 <- rowSums(cbind(DF_wide_participants$qInt.s1a5_9, DF_wide_participants$qInt.s2a6_9, DF_wide_participants$qInt.s3a7_9, DF_wide_participants$qInt.s4a8_9), na.rm=TRUE)

#Emotion (S), Emo, Actors 5-8

DF_wide_participants$qEmo.s1a5_8_1 <- rowSums(cbind(DF_wide_participants$qEmo.s1a5_1, DF_wide_participants$qEmo.s2a6_1, DF_wide_participants$qEmo.s3a7_1, DF_wide_participants$qEmo.s4a8_1), na.rm=TRUE)
	
DF_wide_participants$qEmo.s1a5_8_2 <- rowSums(cbind(DF_wide_participants$qEmo.s1a5_2, DF_wide_participants$qEmo.s2a6_2, DF_wide_participants$qEmo.s3a7_2, DF_wide_participants$qEmo.s4a8_2), na.rm=TRUE)

DF_wide_participants$qEmo.s1a5_8_3 <- rowSums(cbind(DF_wide_participants$qEmo.s1a5_3, DF_wide_participants$qEmo.s2a6_3, DF_wide_participants$qEmo.s3a7_3, DF_wide_participants$qEmo.s4a8_3), na.rm=TRUE)	

DF_wide_participants$qEmo.s1a5_8_4 <- rowSums(cbind(DF_wide_participants$qEmo.s1a5_4, DF_wide_participants$qEmo.s2a6_4, DF_wide_participants$qEmo.s3a7_4, DF_wide_participants$qEmo.s4a8_4), na.rm=TRUE)

DF_wide_participants$qEmo.s1a5_8_5 <- rowSums(cbind(DF_wide_participants$qEmo.s1a5_5, DF_wide_participants$qEmo.s2a6_5, DF_wide_participants$qEmo.s3a7_5, DF_wide_participants$qEmo.s4a8_5), na.rm=TRUE)

#DF_wide_participants$qEmo.s1a5_8_6 <- rowSums(cbind(DF_wide_participants$qEmo.s1a5_6, DF_wide_participants$qEmo.s2a6_6, DF_wide_participants$qEmo.s3a7_6, DF_wide_participants$qEmo.s4a8_6), na.rm=TRUE)

DF_wide_participants$qEmo.s1a5_8_6_R <- (40 - DF_wide_participants$qEmo.s1a5_8_6)

DF_wide_participants$qEmo.s1a5_8_7 <- rowSums(cbind(DF_wide_participants$qEmo.s1a5_7, DF_wide_participants$qEmo.s2a6_7, DF_wide_participants$qEmo.s3a7_7, DF_wide_participants$qEmo.s4a8_7), na.rm=TRUE)

#DF_wide_participants$qEmo.s1a5_8_8 <- rowSums(cbind(DF_wide_participants$qEmo.s1a5_8, DF_wide_participants$qEmo.s2a6_8, DF_wide_participants$qEmo.s3a7_8, DF_wide_participants$qEmo.s4a8_8), na.rm=TRUE)

DF_wide_participants$qEmo.s1a5_8_8_R <- (40 - DF_wide_participants$qEmo.s1a5_8_8)

#DF_wide_participants$qEmo.s1a5_8_9 <- rowSums(cbind(DF_wide_participants$qEmo.s1a5_9, DF_wide_participants$qEmo.s2a6_9, DF_wide_participants$qEmo.s3a7_9, DF_wide_participants$qEmo.s4a8_9), na.rm=TRUE)

DF_wide_participants$qEmo.s1a5_8_9_R <- (40 - DF_wide_participants$qEmo.s1a5_8_9)

DF_wide_participants$qEmo.s1a5_8_10 <- rowSums(cbind(DF_wide_participants$qEmo.s1a5_10, DF_wide_participants$qEmo.s2a6_10, DF_wide_participants$qEmo.s3a7_10, DF_wide_participants$qEmo.s4a8_10), na.rm=TRUE)

write.csv(DF_wide_participants, "[0.4-wide]GS-SumScores.csv")

#3 cronbach’s alpha - from these aggregated scores: behavior approach: 1-3 and #avoidance 4-9 

#Neutral all

NeutralBehavAll <- subset(DF_wide_participants, select = c(qInt.n1a1_4_1, qInt.n1a1_4_2, qInt.n1a1_4_3, qInt.n5a5_8_1, qInt.n5a5_8_2, qInt.n5a5_8_3, qInt.n1a1_4_4, qInt.n1a1_4_5, qInt.n1a1_4_6, qInt.n1a1_4_7, qInt.n1a1_4_8, qInt.n1a1_4_9, qInt.n5a5_8_4, qInt.n5a5_8_5, qInt.n5a5_8_6, qInt.n5a5_8_7, qInt.n5a5_8_8, qInt.n5a5_8_9))
	
	alpha(NeutralBehavAll )
	cor(NeutralBehavAll, method = "pearson")
		#All responses correlated - .18-.87
		cor.test(DF_wide_participants$qInt.n5a5_8_1, DF_wide_participants$qInt.n5a5_8_3)
		cor.test(DF_wide_participants$qInt.n5a5_8_1, DF_wide_participants$qInt.n1a1_4_6)
		
	NeutralBehavFit18 <- principal(NeutralBehavAll , nfactors=18, rotate="varimax")
	plot(NeutralBehavFit18$values, type = "b") #3
	
	NeutralBehavFit3 <- principal(NeutralBehavAll , nfactors=3, rotate="varimax")
	#print.psych(NeutralBehavFit3, cut = 0.3, sort = TRUE)
	
	NeutralBehavFit18O <- principal(NeutralBehavAll , nfactors=18, rotate="oblimin")
	plot(NeutralBehavFit18O$values, type = "b") #3
	NeutralBehavFit3O <- principal(NeutralBehavAll , nfactors=3, rotate="oblimin")
	print.psych(NeutralBehavFit3O, cut = .5, sort = TRUE)

#Neutral, behavior approach items 1-3

NeutralBehavAppr1_4 <- subset(DF_wide_participants, select = c(qInt.n1a1_4_1, qInt.n1a1_4_2, qInt.n1a1_4_3))
	alpha(NeutralBehavAppr1_4)
NeutralBehavAppr5_8 <- subset(DF_wide_participants, select = c(qInt.n5a5_8_1, qInt.n5a5_8_2, qInt.n5a5_8_3))
	alpha(NeutralBehavAppr5_8)
NeutralBehavApprAll <- subset(DF_wide_participants, select = c(qInt.n1a1_4_1, qInt.n1a1_4_2, qInt.n1a1_4_3, qInt.n5a5_8_1, qInt.n5a5_8_2, qInt.n5a5_8_3))
	str(NeutralBehavApprAll)
	alpha(NeutralBehavApprAll, na.rm = TRUE)
	
	cor.test(DF_wide_participants$qInt.n1a1_4_1, DF_wide_participants$qInt.n1a1_4_3 )
	cor.test(DF_wide_participants$qInt.n5a5_8_1, DF_wide_participants$qInt.n5a5_8_3 )
	
	#Drop item 2
	NeutralBehavApprAll_no2 <- subset(DF_wide_participants, select = c(qInt.n1a1_4_1, qInt.n1a1_4_3, qInt.n5a5_8_1, qInt.n5a5_8_3))
		alpha( NeutralBehavApprAll_no2 )
		
	#
	NeutralBehavApprfit6 <- principal(NeutralBehavApprAll, nfactors=6, rotate="varimax")
	plot(NeutralBehavApprfit6$values, type = "b")
	NeutralBehavApprfit2 <- principal(NeutralBehavApprAll, nfactors=2, rotate="varimax")

#Avoid - Remove item 5
NeutralBehaviorAvoid1_4 <- subset(DF_wide_participants, select = c(qInt.n1a1_4_4, qInt.n1a1_4_5, qInt.n1a1_4_6, qInt.n1a1_4_7, qInt.n1a1_4_8, qInt.n1a1_4_9))	
	alpha(NeutralBehaviorAvoid1_4, na.rm = TRUE)
NeutralBehaviorAvoid5_8 <- subset(DF_wide_participants, select = c(qInt.n5a5_8_4, qInt.n5a5_8_5, qInt.n5a5_8_6, qInt.n5a5_8_7, qInt.n5a5_8_8, qInt.n5a5_8_9))
	alpha(NeutralBehaviorAvoid5_8 , na.rm = TRUE)	
		
#Avoid - All
NeutralBehaviorAvoidAll <- subset(DF_wide_participants, select = c(qInt.n1a1_4_4, qInt.n1a1_4_5, qInt.n1a1_4_6, qInt.n1a1_4_7, qInt.n1a1_4_8, qInt.n1a1_4_9, qInt.n5a5_8_4, qInt.n5a5_8_5, qInt.n5a5_8_6, qInt.n5a5_8_7, qInt.n5a5_8_8, qInt.n5a5_8_9))
	alpha(NeutralBehaviorAvoidAll)
	
	NeutralBehavAvoid_PCA <- prcomp(NeutralBehaviorAvoidAll , center = TRUE, scale = TRUE)
	screeplot(NeutralBehavAvoid_PCA) #1
	
#Neutral all actors 1-4
NeutralBehavAll1_4 <- subset(DF_wide_participants, select = c(qInt.n1a1_4_1, qInt.n1a1_4_2, qInt.n1a1_4_3, qInt.n1a1_4_4, qInt.n1a1_4_5, qInt.n1a1_4_6, qInt.n1a1_4_7, qInt.n1a1_4_8, qInt.n1a1_4_9))
	alpha(NeutralBehavAll1_4)
	 NB1_4 <- fa(NeutralBehavAll1_4, nfactors=4, rotate = "varimax")
	 fa.diagram(NB1_4)
	 
#Remove 5
NeutralBehavNo5_1_4 <- subset(DF_wide_participants, select = c(qInt.n1a1_4_1, qInt.n1a1_4_2, qInt.n1a1_4_3, qInt.n1a1_4_4, qInt.n1a1_4_6, qInt.n1a1_4_7, qInt.n1a1_4_8, qInt.n1a1_4_9))
	alpha(NeutralBehavNo5_1_4)
	 NBno5_1_4 <- fa(NeutralBehavNo5_1_4, nfactors=3, rotate = "varimax")
	 fa.diagram( NBno5_1_4 )

	 
#Neutral All actors 5-8
NeutralBehavNo5_5_8 <- subset(DF_wide_participants, select = c(qInt.n5a5_8_1, qInt.n5a5_8_2, qInt.n5a5_8_3, qInt.n5a5_8_4, qInt.n5a5_8_6, qInt.n5a5_8_7, qInt.n5a5_8_8, qInt.n5a5_8_9))
	alpha(NeutralBehavNo5_5_8)
		 NBno5_5_8 <- fa(NeutralBehavNo5_5_8, nfactors=3, rotate = "varimax")
	 fa.diagram(NBno5_5_8 )
	
	
#Factor analysis
install.packages("GPArotation")
library(GPArotation)

NBAll <- fa(NeutralBehavAll, nfactors=2, rotate = "varimax") 
plot( NBAll)
factor.plot(NBAll, cut = .5)
fa.diagram(NBAll)
fa.parallel(NeutralBehavAll)
vss(NeutralBehavAll, n.obs = 96, rotate = "varimax")
plot( NBAll)

NBAllv3 <- fa(NeutralBehavAll, nfactors=3, rotate = "varimax") 
fa.diagram(NBAllv3)

NBAllO <- fa(NeutralBehavAll, nfactors=3, rotate = "oblimin") #uses oblique transformation by default
 fa.diagram(NBAllO)
plot( NBAllO)

NBAllO <- fa(NeutralBehavAll, nfactors=3, rotate = "oblimin") #uses oblique transformation by default
a.diagram(NBAllO)

NeutralBehavI134_9 <- subset(DF_wide_participants, select = c(qInt.n1a1_4_1, qInt.n1a1_4_3, qInt.n5a5_8_1, qInt.n5a5_8_3, qInt.n1a1_4_4, qInt.n1a1_4_5, qInt.n1a1_4_6, qInt.n1a1_4_7, qInt.n1a1_4_8, qInt.n1a1_4_9, qInt.n5a5_8_4, qInt.n5a5_8_5, qInt.n5a5_8_6, qInt.n5a5_8_7, qInt.n5a5_8_8, qInt.n5a5_8_9))
 NBAllO <- fa(NeutralBehavI134_9, nfactors=3, rotate = "oblimin") #uses oblique transformation by default
 fa.diagram(NBAllO)


fa(NeutralBehavAll, nfactors = 2) 


#Neutral, Emotion items
#Don't use reverse scored dataset for items 6, 8, 9.
#DF_wide_Rev <- read.csv("(0.4-wide)GS-SumScoresRev.csv", header=TRUE, stringsAsFactor = FALSE)
colnames(DF_wide_participants)


NeutralEmoAll <- subset(DF_wide_participants, select = c(qEmo.n1a1_4_1, qEmo.n1a1_4_2, qEmo.n1a1_4_3, qEmo.n1a1_4_4, qEmo.n1a1_4_5, qEmo.n1a1_4_6, qEmo.n1a1_4_7, qEmo.n1a1_4_8, qEmo.n1a1_4_9, qEmo.n1a1_4_10, qEmo.n5a5_8_1, qEmo.n5a5_8_2, qEmo.n5a5_8_3, qEmo.n5a5_8_4, qEmo.n5a5_8_5, qEmo.n5a5_8_6, qEmo.n5a5_8_7, qEmo.n5a5_8_8, qEmo.n5a5_8_9, qEmo.n5a5_8_10))
	
	alpha(NeutralEmoAll, check.keys=TRUE)
	cor(NeutralEmoAll, method = "pearson")
		#All responses correlated - .04-.88
		cor.test(DF_wide_participants$qEmo.n1a1_4_3, DF_wide_participants$qEmo.n5a5_8_3)
		cor.test(DF_wide_participants$qEmo.n1a1_4_4, DF_wide_participants$qEmo.n5a5_8_7)
		
	NeutralEmoFit20 <- principal(NeutralEmoAll, nfactors=20, rotate="varimax")
	plot(NeutralEmoFit20$values, type = "b") #4
	
	NeutralEmoFit4 <- principal(NeutralEmoAll, nfactors=4, rotate="varimax")
	print.psych(NeutralEmoFit4, cut = 0.3, sort = TRUE)
	NeutralEmoFit3 <- principal(NeutralEmoAll, nfactors=3, rotate="varimax")
	print.psych(NeutralEmoFit3, cut = 0.3, sort = TRUE)
	
	NeutralEmoFit20O <- principal(NeutralEmoAll, nfactors=20, rotate="oblimin")
	plot(NeutralEmoFit20O$values, type = "b") #4
	NeutralEmoFit4O <- principal(NeutralEmoAll, nfactors=4, rotate="oblimin")
	print.psych(NeutralEmoFit4O, cut = 0.3, sort = TRUE)
	NeutralEmoFit3O <- principal(NeutralEmoAll, nfactors=3, rotate="oblimin")
	print.psych(NeutralEmoFit3O, cut = 0.5, sort = TRUE)
	NeutralEmoFit2O <- principal(NeutralEmoAll, nfactors=2, rotate="oblimin")
	print.psych(NeutralEmoFit2O, cut = 0.3, sort = TRUE)
	
	#Remove 10
	NeutralEmo1_9 <- subset(DF_wide_participants, select = c(qEmo.n1a1_4_1, qEmo.n1a1_4_2, qEmo.n1a1_4_3, qEmo.n1a1_4_4, qEmo.n1a1_4_5, qEmo.n1a1_4_6, qEmo.n1a1_4_7, qEmo.n1a1_4_8, qEmo.n1a1_4_9, qEmo.n5a5_8_1, qEmo.n5a5_8_2, qEmo.n5a5_8_3, qEmo.n5a5_8_4, qEmo.n5a5_8_5, qEmo.n5a5_8_6, qEmo.n5a5_8_7, qEmo.n5a5_8_8, qEmo.n5a5_8_9))
	
	NeutralEmoFit18 <- principal(NeutralEmo1_9, nfactors=18, rotate="varimax")
	plot(NeutralEmoFit18$values, type = "b") #4
		NeutralEmoFit18O <- principal(NeutralEmo1_9, nfactors=18, rotate="oblimin")
	plot(NeutralEmoFit18O$values, type = "b") #4
	
	NeutralEmoFit18_4O <- principal(NeutralEmo1_9, nfactors=4, rotate="oblimin")
	print.psych(NeutralEmoFit18_4O, cut = 0.3, sort = TRUE)

	NeutralEmoFit18_3O <- principal(NeutralEmo1_9, nfactors=3, rotate="oblimin")
	print.psych(NeutralEmoFit18_3O, cut = 0.5, sort = TRUE)

