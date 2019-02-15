life <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/20_PCL_5_lifetime/pcl5_entire_life_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
curr <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/21_PCL_5_monthly/pcl5_current_scored_data_export.csv',header=T, na.strings=c("#N/A",NA))
gad7 <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/14_GAD7/gad7_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
isi <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/15_ISI/ISI_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
isi_med <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/16_ISI_MedQuestion/ISI_MedQuestion_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))



#________________________________________________________________________________________  
# MERGE DATASETS TOGETHER
#------------------------------------------------------------------------
#merge CPRS corefile and full dataset by assesstment id % LAST NAME
dat <- merge(curr, life, by="assessment_id", all = TRUE)
newdata <- na.omit(dat)
dim(dat) 


plot(newdata$pcl_total.x, newdata$pcl_total.y, xlab= "Current PCL", ylab= "Lifetime PCL", main = "PCL Total Scores",  pch=19 , col = "blue")

#regression of lifetime and current
reg <- lm(newdata$pcl_total.x~ newdata$pcl_total.y)
summary(reg)
abline(reg)


dat00 <- merge(dat, gad7, by="assessment_id", all = FALSE)
dim(dat00) 


#________________________________________________________________________________________  
# GAD-7
#----------------------------------------------------------------------------------------
#PCL current and GAD-7
plot(dat00$pcl_total.x, dat00$gad7_score, xlab= "Current PCL", ylab= "GAD-7", main = "Current PCL and GAD-7 Total Scores", col = "black")
abline(h = 14, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('bottomright',legend=c("GAD-7 Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)

#PCL lifetime and GAD-7
plot(dat00$pcl_total.y, dat00$gad7_score, xlab= "Lifetime PCL", ylab= "GAD-7", main = "Lifetime PCL and GAD-7 Total Scores", col = "black")
abline(h = 14, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('bottomright',legend=c("GAD-7 Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)


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


