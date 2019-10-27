##################################################################
Replication Materials for:
A Vote for Me is a Vote for America
By Pavielle E. Haines
##################################################################

This file describes the materials that can be used to replicate the analyses presented in the paper and appendices.


########################################
Data Files
########################################

Reproduction of the analyses requires two data files:

1) mturkvotingreplication.csv
2) ssivotingreplication.csv



The data file “mturkvotingreplication.csv” is an Excel file that can be read into R. It contains survey data for respondents assigned to one of three experimental conditions in a 2018 study. Each row entry corresponds to a respondent. It contains relevant demographic data, experimental condition, voting behavior, and political attitudes.

The data file “ssivotingreplication.csv” is an Excel file that can be read into R. It contains survey data for respondents assigned to one of three experimental conditions in a 2016 study. Each row entry corresponds to a respondent. It contains relevant demographic data, experimental condition, voting behavior, and political attitudes.

The data and analysis for the MTurk and SSI studies are effectively identical.

########################################
R Script
########################################

The file “patrioticvotingreplication.R” contains all the necessary code for reproducing the tables and figures in the main paper and online appendix. Before attempting to run the code, ensure that the correct version of R is installed, that you have downloaded all required files, and that the R code and data files are saved to the same location.

The analyses were conducted using R version 3.3.3. You may be required to install additional packages, as indicated in the code, to run the script.

The script is divided into multiple sections and subsections corresponding to each task and analysis. Detailed markup describes the purpose of each chunk of code:

-The first section includes the code required to install all necessary packages. This section of code only needs to be run the first time the script is opened and may subsequently be skipped.

-The second section includes the code for calculating and generating derived variables and datasets. It is *not* necessary to run this section of script prior to running subsequent code.

-The remaining sections includes the code necessary for replicating every analysis presented in the paper and online appendix. Each subsection is clearly labeled to correspond to the results presented in a table, graph, or as part of the text. The code in each subsection functions independently, so (unless otherwise noted) subsections can be in any desired order. Note that some subsections of code will take upwards of five minutes to run.



########################################
User Guides for Data and Sources
########################################

The file “Patriotic Voting Codebook for Data Files.pdf” contains detailed information about all the variables included in the datasets and used the analyses.

