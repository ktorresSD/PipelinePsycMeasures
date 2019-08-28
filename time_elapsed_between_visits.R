#########################################################################################
# Last Date modified: 07/31/2019
# Author: Katy Torres
# Description: Look at time elapsed between subject visits
##########################################################################################

#read in corefile
core <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/data/biobank_data_corefile_June_19.csv',header=T, na.strings=c("",NA))

#REMOVE EXCLUDED SUBJECTS
core1 <- core [ ! core$assessment_id %in% c(17071, 25783, 28003), ]

#reshape file to have one row per subject
l.sort <- core1[order(core1$vista_lastname),]

demo_wide <- reshape(l.sort,
                     timevar = "visit_number",
                     idvar = c( "vista_lastname"),
                     direction = "wide")


#Export data
write.csv(demo_wide, 'C:/Users/Nievergelt Lab/Documents/Biobank/data/wide_corefile_07312019.csv')




#read in corefile
time <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/data/time_elapsed_between_visits.csv',header=T, na.strings=c("",NA))
hist(time$t_passed_v1_v2, main = "Days elapsed between visit 1 and 2", col = 'dodgerblue')
hist(time$t_passed_v2_v3, main = "Days elapsed between visit 2 and 3", col = 'lightpink')
