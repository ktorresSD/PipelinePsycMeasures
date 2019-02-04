life <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/20_PCL_5_lifetime/pcl5_entire_life_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
curr <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/21_PCL_5_monthly/pcl5_current_scored_data_export.csv',header=T, na.strings=c("#N/A",NA))
gad7 <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/14_GAD7/gad7_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))

#________________________________________________________________________________________  
# MERGE DATASETS TOGETHER
#------------------------------------------------------------------------
#merge CPRS corefile and full dataset by assesstment id % LAST NAME
dat0 <- merge(curr, life, by="assessment_id", all = FALSE)
newdata <- na.omit(dat0)
dim(dat0) 


plot(newdata$pcl_total.x, newdata$pcl_total.y, xlab= "Current PCL", ylab= "Lifetime PCL", main = "PCL Total Scores",  pch=19 , col = "blue")

#regression of lifetime and current
reg <- lm(newdata$pcl_total.x~ newdata$pcl_total.y)
summary(reg)
abline(reg)


dat00 <- merge(dat0, gad7, by="assessment_id", all = FALSE)
dim(dat00) 


#PCL lifetime and GAD-7
plot(dat00$pcl_total.x, dat00$gad7_score, xlab= "Current PCL", ylab= "GAD-7", main = "Current PCL and GAD-7 Total Scores", col = "blue")

#PCL CURRENT and GAD-7
plot(dat00$pcl_total.y, dat00$gad7_score, xlab= "Lifetime PCL", ylab= "GAD-7", main = "Lifetime PCL and GAD-7 Total Scores", col = "blue")
