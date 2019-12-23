# PipelinePsycMeasures
Pipeline that runs the over 36 scripts I created to score and QC a variety of psychiatric measures and demographic information

Run "0_Pipeline".
This will need the most up to date full dataset from subject database.
Also, the corefile that lists every subjects, their recruitment site information, visit number, subject ID and unique visit ID.

This will:
1. Produce a file that joins all new files with existing database
2. Score and QC of every module/questionnaire 
3. Save each scored output to their individual file in the specified folders.
4. Generate a dataframe listing if the subject completed/partially completed/did no attempt each specific module/questionnaire

