#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 13, exposures and scoring functions
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
dat0 <- read.csv('joined_data_export_20171221.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
datexpo <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        serv_exposed,
                        serv_exp_none,
                        serv_exp_chemical,
                        serv_exp_bio,
                        serv_exp_jp8,
                        serv_exp_asbestos,
                        serv_exp_nerve,
                        serv_exp_radio,
                        serv_exp_sand,
                        serv_exp_uranium,
                        serv_exp_industrial,
                        serv_exp_fumes,
                        serv_exp_paint,
                        serv_exp_bite,
                        serv_exp_burn,
                        serv_exp_pest,
                        serv_exp_other,
                        serv_exp_oth1spec,
                        serv_exp_other,
                        serv_exp_oth2spec,
                        
                        serv_animal_bite,
                        serv_animal_blood,
                        serv_animal_bat,
                        
                        serv_combat,
                        serv_comb_none,
                        serv_comb_attack,
                        serv_comb_fire,
                        serv_comb_hand,
                        serv_comb_wounded,
                        serv_comb_interro,
                        serv_comb_rocket,
                        serv_comb_seebody,
                        serv_comb_clear,
                        serv_comb_ship,
                        serv_comb_detain,
                        serv_comb_recdfire,
                        serv_comb_handbody,
                        serv_comb_killed,
                        serv_comb_enemy,
                        Exposures.formula
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------


#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datexpo, "~/Biobank/13_exposures/exposures_reduced_data_export_20171221.csv",quote=T,row.names=F,na="#N/A")


