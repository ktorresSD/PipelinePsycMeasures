#########################################################################################
# Last Date modified: 06/21/2018
# Author: Katy Torres
# Description: Subset of question 28, Demographic: Social Environment
##########################################################################################
social<- function(dat0, exportdate)
{
  

#Only retain relevant variables
datdemosocial <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        demo_livewith_alone,
                        demo_livewith_parent,
                        demo_livewith_friend,
                        demo_livewith_spouse,
                        demo_livewith_child,
                        demo_livewith_other,
                        demo_livewith_otherspec,
                        
                        demo_emo_none,
                        demo_emo_parents,
                        demo_emo_friends,
                        demo_emo_spouse,
                        demo_emo_therapist,
                        demo_emo_spiritual,
                        demo_emo_children,
                        demo_emo_other,
                        demo_emo_other_spec,
                        demo_rel_hurt,
                        
                        demo_children,
                        child_count,
                        child_agegroup,
                        child_agegroup_0,
                        child_agegroup_1,
                        child_agegroup_2,
                        child_agegroup_3,
                        child_agegroup_4,
                        child_agegroup_5
                    
              ))
#________________________________________________________________________________________              
# Completeness check
#----------------------------------------------------------------------------------------

social_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
#checking for completeness
data_complete_social<- as.numeric(
  sum(
    is.na(
      c(demo_livewith_alone,
        demo_livewith_parent,
        demo_livewith_friend,
        demo_livewith_spouse,
        demo_livewith_child,
        demo_livewith_other,
        demo_emo_none,
        demo_emo_parents,
        demo_emo_friends,
        demo_emo_spouse,
        demo_emo_therapist,
        demo_emo_spiritual,
        demo_emo_children,
        demo_emo_other,
        demo_rel_hurt
        #child_count
      )
    )
  ) == 0
)

data_not_attempted_social<- as.numeric(
  sum(
    is.na(
      c(demo_livewith_alone,
        demo_livewith_parent,
        demo_livewith_friend,
        demo_livewith_spouse,
        demo_livewith_child,
        demo_livewith_other,
        demo_emo_none,
        demo_emo_parents,
        demo_emo_friends,
        demo_emo_spouse,
        demo_emo_therapist,
        demo_emo_spiritual,
        demo_emo_children,
        demo_emo_other,
        demo_rel_hurt
        #child_count
      )
    )
  ) == 15
)



completeness_social<- "1"
if(!(is.na(data_not_attempted_social))){
  if(data_not_attempted_social==1)
  {
    completeness_social <- "not attempted"}else{}
}else{completeness_social<-NA}

if(!(is.na(data_complete_social))){
  if(data_complete_social==1){
    completeness_social <- "complete"} else{}
}else{completeness_social<-NA}


if(data_not_attempted_social==0 & data_complete_social==0){
  completeness_social <- "partially completed"}else{}

scores <- data.frame(data_complete_social, data_not_attempted_social, completeness_social)

return(scores)
}


#Calculate summary scores in data 
datsocial_scored <- adply(datdemosocial, 1, social_score)

#to anonymize data
datsocial_scored1<- within(datsocial_scored,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/28_Demo_Social/Demographic_social_scored_data_export.csv", sep="")
write.csv(datsocial_scored , filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/28_Demo_Social/Demographic_social_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(datsocial_scored1 , filename,quote=T,row.names=F,na="#N/A")

print("28_Demo_social_done")
       
       #return completness column
       myvars <- c("assessment_id", "completeness_social")
       newdata <- datsocial_scored[myvars]
       return(newdata)
}


