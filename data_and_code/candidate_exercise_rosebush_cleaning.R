## Author: Drew Rosebush
## Title: RISI Data Analyst Candidate Exercise - CLEANING
## Date: July 2021
## Reason: An exercise used to assess one's ability to analyze data using R.
  # The intention of this exercise is to assess your ability to think critically
  # and apply your technical skill set to an analytics problem designed to reflect
  # some of the tasks you are likely to encounter as a Data Analyst at RISI, albeit
  # in a simplified manner. This exercise is deliberately vague and no specific
  # approach or answer is "right".

## Pull in Libraries
library(rstudioapi) ## Used to identify file path which is used to set working directory.
library(stringr)  ## Used to handle strings efficiently


## Setting working directory first by identifying file location then by identifying the directory name where the file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## 1. Pull in data:
  ## 1.1 This script will identify the the csv files located in the directory then make a list (vector) of the file names.
  csv_list=list.files(path=".",pattern=".csv")
  ## Pull in data: Use the csv_list to iterate across. 
  for (i in 1:length(csv_list)){
    dat = read.csv(csv_list[i]) ## dat is a temporary data.frame used throughout the iteration.
    ## Creating a field that identifies source file. This may be helpful in the future. 
    dat$sourceFileName = as.character(csv_list[i]) 
    if(i==1){
      ## If this is the first file, initiate the main file. All other files will be appended to the end. 
      suicideDF = dat
    } else{
      ## ****** Create an error checking. 
      suicideDF = rbind(suicideDF, dat)
    }
  }
  ## Drop temporary files to reduce environment clutter
  rm(i)
  rm(dat)
  rm(csv_list)

  ## 2. Clean the data.frame: #*: exploration code that does not need to be included in the final running script.
  # fields = colnames(suicideDF) #* create an attributes vector
  # sapply(colnames(suicideDF), class) #* Identify what attribute classes
  # head(suicideDF, 10) #* Taking a look at the beginning of the data.
  # tail(suicideDF, 10) #* Taking a look at the end of the data.
  # table(suicideDF$notes) #* Determine what the notes field contains
  ## Pull out years from the year strings.
  suicideDF$year_rangeEnd = as.integer(str_sub(suicideDF$time_period,-4,-1))
  ## Parse Totals by acp_name
  totals = suicideDF[suicideDF$notes=="Total",-(1:3)] ## Drop extra columns
  ## Drop Total (rows) and notes (column) from main data.frame
  suicideDF= suicideDF[suicideDF$notes!="Total",-grep("notes", colnames(suicideDF))]
  ## Age
  # table(suicideDF$five_year_age_groups_code) #* 
  ## Isolating NS
  age_NS = suicideDF[suicideDF$five_year_age_groups_code=="NS",]
  # head(age_NS,10)
  # table(age_NS$deaths) #* Determining whether or not there's valuable data in this group.
  age_NS=age_NS[age_NS$deaths!="Suppressed",]
  suicideDF = suicideDF[suicideDF$five_year_age_groups_code!="NS",]
  
  ## Parse suppressed rows. #*  
  suppressedDF = suicideDF[suicideDF$deaths=="Suppressed",]
  # table(suppressedDF$five_year_age_groups_code) #* Checking the suppressed age boundaries
  ## The suppressed age boundaries. To be consistent across all ages, years, and acp_names 
  # table(suppressedDF$five_year_age_groups,suppressedDF$time_period) #* Determine years impacted by the imbalance
  # table(suppressedDF$five_year_age_groups,suppressedDF$acp_name) #* Determine groups impacted by the imbalance
  ## Aging Farmlands is the group associated with the 20 years of suppressed death data for ages 10-14 (highest lower bound)
  ## American Lands is the group associated with the 2005-2009 suppress death data for ages 80-84 (lowest upper bound)
  ## I conclude that I should subset the main data.frame to only include observations of death between the ages of 15-79.
  
  ## Parse Not Applicable rows. #*  
  notApplicableDF = suicideDF[suicideDF$crude_rate=="Not Applicable",]
  # table(notApplicableDF$five_year_age_groups_code) #* Checking the Not Applicable age boundaries
  ## The Not Applicable age boundaries. To be consistent across all ages, years, and acp_names 
  # table(notApplicableDF$five_year_age_groups,notApplicableDF$time_period) #* Determine years impacted by the imbalance
  # table(notApplicableDF$five_year_age_groups,notApplicableDF$acp_name) #* Determine groups impacted by the imbalance
  ## I conclude that using the suppressed subsetting criteria is a sufficient condition.
  
  ## Parse age ranges impacted by the data imbalance: age groups below 15-19 & above 75-79.
  ages_list = unique(suicideDF$five_year_age_groups_code)
  coreAges = ages_list[5:17] ## Age ranges not in the main list of age ranges
  ages_Below15_and_Above79= suicideDF[!suicideDF$five_year_age_groups_code %in% coreAges,] 
  suicideDF= suicideDF[suicideDF$five_year_age_groups_code %in% coreAges,]
  
  ## Pull out the upper bound age range
  suicideDF$upperAge= as.numeric(str_split_fixed(suicideDF$five_year_age_groups_code,"-",2)[,2])
  
  ## Change attributes to appropriate classes
  sapply(suicideDF, class)
  suicideDF$crude_rate[suicideDF$crude_rate=="Unreliable"]=9999 ## addressing crude_rate Unreliable values before specifying as numeric data class.
  suicideDF[,3:8]= lapply(suicideDF[,3:8], as.numeric)
  totals[,1:6]= lapply(totals[,1:6], as.numeric)
  # sapply(suicideDF[3:8], summary) #* Examine summary stats of each numeric attribute
  
  ## Impute unreliable crude_rate data. I am assuming that the death and population estimates are reliable, as they are not specified as unreliable. 
  suicideDF$crude_rate[suicideDF$crude_rate==9999] = 100000*suicideDF$deaths[suicideDF$crude_rate==9999]/suicideDF$population[suicideDF$crude_rate==9999]
  
  ## Create an Age Groups - by Decade
  suicideDF$ageGroup ="" #* Initializing the new column
  suicideDF$ageGroup[suicideDF$upperAge<20] = "Teens"  
  suicideDF$ageGroup[suicideDF$ageGroup=="" &suicideDF$upperAge<30 ] = "20's"
  suicideDF$ageGroup[suicideDF$ageGroup=="" &suicideDF$upperAge<40 ] = "30's"
  suicideDF$ageGroup[suicideDF$ageGroup=="" &suicideDF$upperAge<50 ] = "40's"
  suicideDF$ageGroup[suicideDF$ageGroup=="" &suicideDF$upperAge<60 ] = "50's"
  suicideDF$ageGroup[suicideDF$ageGroup=="" &suicideDF$upperAge<70 ] = "60's"
  suicideDF$ageGroup[suicideDF$ageGroup=="" &suicideDF$upperAge<80 ] = "70's"
output = list(totals, suicideDF)
rm(list=setdiff(ls(), "output")) 


