curr <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/pcl5_current_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
life <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/pcl5_entire_life_scored_data_export.csv',header=T, na.strings=c("#N/A",NA))

phq9 <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/phq9_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))

gad7 <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/gad7_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
isi <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/ISI_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
isi_med <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/ISI_MedQuestion_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
lec5 <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/LEC-5_lifetime_UPDATED_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
audit <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/AUDIT_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
DRRI_CES <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/DRRICES_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
DRRI_pbe <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/DRRI_PBE_reduced_data_export.csv',header=T,na.strings=c("#N/A",NA))


par(mfrow=c(1,1))
#________________________________________________________________________________________  
# MERGE DATASETS TOGETHER
#------------------------------------------------------------------------
#merge CPRS corefile and full dataset by assesstment id % LAST NAME
dat <- merge(curr, life, by="assessment_id", all = TRUE)
newdata <- na.omit(dat)
dim(dat) 


plot(newdata$pcl_m_total, newdata$pcl_total, xlab= "Current PCL", ylab= "Lifetime PCL", main = "PCL Total Scores",  pch=19 , col = "blue")

#regression of lifetime and current
reg <- lm(newdata$pcl_m_total~ newdata$pcl_total)
summary(reg)
abline(reg)



#________________________________________________________________________________________  
# PHQ-9
#----------------------------------------------------------------------------------------

datphq9 <- merge(dat, phq9, by="assessment_id", all = FALSE)
dim(datphq9) 

#PCL current and PHQ-9
plot(datphq9$pcl_total.x, datphq9$phq9_total, xlab= "Current PCL", ylab= "PHQ-9", main = "Current PCL and PHQ-9 Total Scores", col = "black")
abline(h = 14, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('bottomright',legend=c("PHQ-9 Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)

#PCL lifetime and PHQ-9
plot(datphq9$pcl_total.y, datphq9$phq9_total, xlab= "Lifetime PCL", ylab= "PHQ-9", main = "Lifetime PCL and PHQ-9 Total Scores", col = "black")
abline(h = 14, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('bottomright',legend=c("PHQ-9 Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)

# #current ptsd and phq9
# cor(datphq9[,c(30,83)], use="complete.obs", method="pearson") 
# #lifetime ptsd and phq9
# cor(datphq9[,c(64,83)], use="complete.obs", method="pearson") 
# 
# 

#Current PCL total Score and PHQ9
cor.test(datphq9$pcl_total.x, datphq9$phq9_total, method="pearson") 

#Lifetime PCL total Score and PHQ9
cor.test(datphq9$pcl_total.y, datphq9$phq9_total, method="pearson") 


#________________________________________________________________________________________  
# GAD-7
#----------------------------------------------------------------------------------------

dat00 <- merge(dat, gad7, by="assessment_id", all = FALSE)
dim(dat00) 

#PCL current and GAD-7
plot(dat00$pcl_total.x, dat00$gad7_score, xlab= "Current PCL", ylab= "GAD-7", main = "Current PCL and GAD-7 Total Scores", col = "black")
abline(h = 10, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('bottomright',legend=c("GAD-7 Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)

#PCL lifetime and GAD-7
plot(dat00$pcl_total.y, dat00$gad7_score, xlab= "Lifetime PCL", ylab= "GAD-7", main = "Lifetime PCL and GAD-7 Total Scores", col = "black")
abline(h = 10, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('bottomright',legend=c("GAD-7 Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)



#Current PCL total Score and gad8
cor.test(dat00$pcl_total.x, dat00$gad7_score, method="pearson") 

#Lifetime PCL total Score and gad7
cor.test(dat00$pcl_total.y, dat00$gad7_score, method="pearson") 




#________________________________________________________________________________________  
# ISI 
#----------------------------------------------------------------------------------------
dat000 <- merge(dat, isi, by="assessment_id", all = FALSE)
dim(dat000) 

#PCL Lifetime and isi
plot(dat000$pcl_total.y, dat000$insomnia_total, xlab= "Lifetime PCL", ylab= "ISI", main = "Lifetime PCL and ISI Total Scores", col = "black")
abline(h = 15, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('bottomright',legend=c("ISI Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)

#PCL Lifetime and isi
plot(dat000$pcl_total.x, dat000$insomnia_total, xlab= "Current PCL", ylab= "ISI", main = "Current PCL and ISI Total Scores", col = "black")
abline(h = 15, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('bottomright',legend=c("ISI Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)


#Current PCL total Score and gad8
cor.test(dat000$pcl_total.x, dat000$insomnia_total, method="pearson") 

#Lifetime PCL total Score and gad7
cor.test(dat000$pcl_total.y, dat000$insomnia_total, method="pearson") 




#________________________________________________________________________________________  
# ISI_Med_Question
#----------------------------------------------------------------------------------------

dat0000 <- merge(dat, isi_med, by="assessment_id", all = TRUE)
dim(dat0000) 

dat0000$ISI_Meds <- as.factor(dat0000$ISI_Medications)
levels(dat0000$ISI_Meds) <- c("No meds","Yes meds")


#PCL Lifetime and ISI
# plot(dat0000$ISI_Meds, dat0000$pcl_total.y, main = "Lifetime PCL Score and response to 'taking anything to help sleep'", col=c("red", "blue"))
# plot(dat0000$ISI_Medications, dat0000$pcl_total.y, pch=20, xlab="sleep medication yes/no", ylab="Lifetime PCL", bty="n" )


#PCL Current and ISI yes/no MEDICATION
plot(dat0000$ISI_Meds, dat0000$pcl_total.x, main = "Current PCL Score and response to 'taking anything to help sleep'", col=c("dodgerblue2", "springgreen2"))


#if scatterplot is preferred
plot(jitter(dat0000$ISI_Medications ), dat0000$pcl_total.x, pch=20, xlab="sleep medication", ylab="PCL", bty="n" )
abline(h = 33, lty = 2, lwd=2, col= "blue")
legend('bottomright',legend=c("PCL Cuf-off"), col = c("blue"),lty = 2, lwd=2)



#sUBSET ONLY THE ONES THAT SAID YES TO TAKING MEDS
isi_yes_meds <- dat0000[ which(dat0000$ISI_Meds=='Yes meds' ),]
dim(isi_yes_meds) 

#for these, see how many days they took sleep aids for, compare to PCL
#PCL Current and ISI yes/no MEDICATION
plot(isi_yes_meds$ISI_numberofdays, isi_yes_meds$pcl_total.x, pch=8,
     xlab="Number of days", ylab="Current PCL", 
     main = "Current PCL Score and response to \n'number of days in the past week that sleep aid was taken'")
          


dat_isi_and_meds<- merge(dat000, dat0000, by="assessment_id", all = TRUE)
dim(dat_isi_and_meds)      

#PLOT ISI module against ISI_MED MODULE
plot(dat_isi_and_meds$ISI_Meds, dat_isi_and_meds$insomnia_total, 
     main = "Insomnia total Score and taking anything to aid sleep", ylab = "PCL Score", col=c("dodgerblue2", "springgreen2"))

isi_yes_meds_only <- dat_isi_and_meds[ which(dat_isi_and_meds$ISI_Meds=='Yes meds' ),]
dim(isi_yes_meds_only)


plot(isi_yes_meds_only$ISI_numberofdays, isi_yes_meds_only$insomnia_total, pch=8,
     xlab="Number of days", ylab="ISI Score", 
     main = "Insomnia Score and response to \n'number of days in the past week that sleep aid was taken'")


#________________________________________________________________________________________  
# LEC Question
#----------------------------------------------------------------------------------------
datlec<- merge(dat, lec5, by="assessment_id", all = TRUE)
dim(datlec) 

#PCL Current and ISI yes/no MEDICATION
plot(datlec$pcl_total.x, datlec$CritA2, main = "Current PCL Score and Criterion A", 
      xlab="PCL_Total_Score", ylab="Criterion A", pch=8)


#________________________________________________________________________________________  
# AUDIT
#----------------------------------------------------------------------------------------
dat000au <- merge(dat, audit, by="assessment_id", all = FALSE)
dim(dat000au) 



#PCL Lifetime and AUDIT
plot(dat000au$pcl_total.y, dat000au$audit_total_score_use, xlab= "Lifetime PCL", ylab= "AUDIT", main = "Lifetime PCL and AUDIT Total Scores", col = "black")
abline(h = 8, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('topleft',legend=c("AUDIT Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)

#PCL Lifetime and AUDIT
plot(dat000au$pcl_total.x, dat000au$audit_total_score_use, xlab= "Current PCL", ylab= "AUDIT", main = "Current PCL and AUDIT Total Scores", col = "black")
abline(h = 8, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('topleft',legend=c("AUDIT Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)


#Current PCL total Score and gad8
#computes correlation between a dicotomous variable and a continous variable
library("ltm")
biserial.cor(dat000au$audit_total_score_use, as.factor(dat000au$pcl_5_dsm.x), use = c("complete.obs"), level = 1)


#plot PTSD case/controls VS AUDIT TOTAL SCORE
p0 <- hist(subset(dat000au,pcl_5_dsm.x == 1)$audit_total_score_use,plot=FALSE)
p1 <- hist(subset(dat000au,pcl_5_dsm.x == 0)$audit_total_score_use,plot=FALSE)
yli <- max(p0$density, p1$density  ) #p3$density,p4$density 
transparency_level=0.5
par(mar=c(5, 4, 4, 2) + 0.5)
plot(p0, col=rgb(1,0,0,transparency_level),freq=FALSE,xlim=range(dat000au$audit_total_score_use,na.rm=T),ylim=c(0,yli),ylab="Frequency", xlab="Audit Total Score",cex.axis=1.45,cex.lab=1.6,  main= "Audit Total Score Frequency") 
plot(p1, col=rgb(0,0,1,transparency_level),freq=FALSE,add=T)  # second
legend('topright',legend=c("Current PTSD Case", "Current PTSD Control"),col=c(rgb(1,0,0,transparency_level),rgb(0,0,1,transparency_level)),pch=c(19,19))

View(dat000au[,c("assessment_id" , "pcl_total.x", "audit_total_score_use", "pcl_total.x")])


#violin plot
p <- ggplot(dat000au, aes(x=as.factor(pcl_5_dsm.x), y=audit_total_score_use, fill=as.factor(pcl_5_dsm.x))) + 
  geom_violin(trim=FALSE) +
  stat_summary(fun.y=median, geom="point", shape=23, size=2)

p + labs(x = "PTSD_DX", y= "AUDIT_Total_Score", fill = "PTSD DX", title="Current PTSD DX and Audit Total Score")



#Lifetime PCL total Score and gad7
cor.test(dat000au$pcl_total.y, dat000au$audit_total_score_use, method="pearson") 


#________________________________________________________________________________________  
# DRRI_CES
#----------------------------------------------------------------------------------------
datDRRI_CES <- merge(dat, DRRI_CES, by="assessment_id", all = FALSE)
dim(datDRRI_CES ) 


#lifetime PCL and DRRI_CES
plot(datDRRI_CES$pcl_total, datDRRI_CES$DRRI_CES_scores_total, xlab= "Lifetime PCL", ylab= "DRRI_CES Scores", main = "PCL Lifetime Score and DRRI_CES Score",  pch=19 , col = "blue")

#regression
reg <- lm(datDRRI_CES$DRRI_CES_scores_total~ datDRRI_CES$pcl_total)
summary(reg)
abline(reg)

#correlation
cor.test(datDRRI_CES$pcl_total, datDRRI_CES$DRRI_CES_scores_total, method="pearson") 



#Current PCL and DRRI_CES
plot(datDRRI_CES$pcl_m_total, datDRRI_CES$DRRI_CES_scores_total, xlab= "Current PCL", ylab= "DRRI_CES Scores", main = "PCL Current Score and DRRI_CES Score",  pch=19 , col = "blue")

#regression 
reg <- lm(datDRRI_CES$DRRI_CES_scores_total~ datDRRI_CES$pcl_m_total)
summary(reg)
abline(reg)

#correlation
cor.test(datDRRI_CES$pcl_m_total, datDRRI_CES$DRRI_CES_scores_total, method="pearson")



#________________________________________________________________________________________  
# DRRI_pbe
#----------------------------------------------------------------------------------------
datDRRI_pbe <- merge(dat, DRRI_pbe, by="assessment_id", all = FALSE)
dim(datDRRI_pbe ) 


#lifetime PCL and DRRI_pbe
plot(datDRRI_pbe$pcl_total, datDRRI_pbe$DRRI_pbe_scores_total, xlab= "Lifetime PCL", ylab= "DRRI_PBE Scores", main = "PCL Lifetime Score and DRRI_PBE Score",  pch=19 , col = "blue")

#regression
reg <- lm(datDRRI_pbe$DRRI_pbe_scores_total~ datDRRI_pbe$pcl_total)
summary(reg)
abline(reg)

#correlation
cor.test(datDRRI_pbe$pcl_total, datDRRI_pbe$DRRI_pbe_scores_total, method="pearson") 



#Current PCL and DRRI_pbe
plot(datDRRI_pbe$pcl_m_total, datDRRI_pbe$DRRI_pbe_scores_total, xlab= "Current PCL", ylab= "DRRI_PBE Scores", main = "PCL Current Score and DRRI_PBE Score",  pch=19 , col = "blue")

#regression 
reg <- lm(datDRRI_pbe$DRRI_pbe_scores_total~ datDRRI_pbe$pcl_m_total)
summary(reg)
abline(reg)

#correlation
cor.test(datDRRI_pbe$pcl_m_total, datDRRI_pbe$DRRI_pbe_scores_total, method="pearson")

 


#CHECKING CORRELATION BETWEEN MASQ and PHQ9
PHQ9 <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/phq9_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
MASQ <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/MASQ_reduced_data_export_COMBINED_USE.csv',header=T,na.strings=c("#N/A",NA))


par(mfrow=c(1,1))
#________________________________________________________________________________________  
# MERGE DATASETS TOGETHER
#------------------------------------------------------------------------
#merge CPRS corefile and full dataset by assesstment id % LAST NAME
dat <- merge(PHQ9, MASQ , by="assessment_id", all = TRUE)
newdata <- na.omit(dat)
dim(dat) 


plot(newdata$phq9_total, newdata$AD, xlab= "PHQ9", ylab= "MASQ", main = "PCL Total Scores",  pch=19 , col = "blue")

#regression of lifetime and current
reg <- lm(newdata$AD~ newdata$phq9_total)
summary(reg)
abline(reg)


#cORRELATION TEST BETWEEN EMASURES
cor.test(newdata$AD, newdata$phq9_total, method="pearson") 


