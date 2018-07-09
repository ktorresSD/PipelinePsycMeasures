# PipelinePsycMeasures
Pipeline that runs the 31 scripts I created to score and QC a variety of psychiatric measures and demographic information

Run "0_Pipeline".
This will need the most up to date full dataset from subject database.
Also, the corefile that lists every subjects, their recruitment site information, visit number, subject ID and unique visit ID.

This will produce:
1. a file that joins both files for a complete data file
2. scrore and QC every module/questionnaire 
3. save each scored output to their individual file
4. generate a dataframe listing if the subject completed/partially completed/did no attempt each specific module/questionnaire

