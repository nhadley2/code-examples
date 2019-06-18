## Program: R Data Manipulation and Export ##
## Author: Noelle Hadley ##

rm(list = ls())
library(dplyr, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(xlsx, quietly = TRUE)

## Directory ##
Excel.dir <- "C:\\Users\\nhadley\\Documents\\Synthea"

### Bringing in data ###
patients <- read.csv(paste(Excel.dir,"/patients.csv",sep=""),
                           sep=",",
                           header=TRUE,
                           stringsAsFactors=FALSE)
colnames(patients)<-tolower(colnames(patients))

conditions <- read.csv(paste(Excel.dir,"/conditions.csv",sep=""),
                          sep=",",
                          header=TRUE,
                          stringsAsFactors=FALSE)
colnames(conditions)<-tolower(colnames(conditions))

medications <- read.csv(paste(Excel.dir,"/medications.csv",sep=""),
                       sep=",",
                       header=TRUE,
                       stringsAsFactors=FALSE)
colnames(medications)<-tolower(colnames(medications))

## Merge patient, medication, and condition data sets by patient and encounter after renaming duplicate variables
## Only interested in cases where the encounter has both medication and condition information
meds_conds <- conditions %>% rename(cond_start = start,
                                    cond_stop = stop,
                                    cond_code = code,
                                    cond_desc = description) %>%
                merge(patients,
                      by.x = "patient",
                      by.y = "id", 
                      all.x = TRUE) %>%
                merge(medications, 
                      by = c("patient", "encounter")) 

## Converting date variables from character to date format using lubridate package
meds_conds_dates <- meds_conds %>% mutate(start = ymd(start),
                                          stop = ymd(stop),
                                          cond_start = ymd(cond_start),
                                          cond_stop = ymd(cond_stop))

## Create character vector of conditions, randomly sample 3 to create output for
conds.list <- sort(unique(meds_conds_dates$cond_desc))
conds.list.subset <- sample(conds.list, 3)

## Run the loop to get data from subset into excel - want to create separate sheet for each condition
for(i in conds.list.subset){
  
  wb <-  loadWorkbook(
    paste(
      Excel.dir,
      "template_conds.xlsx",
      sep = "/"))
  
  # Subset relevant info to add to sheets in workbook
  meds_conds_dates.i <- meds_conds_dates[which(meds_conds_dates$cond_desc == i),]
  patient.i <- subset(meds_conds_dates.i, select=c(first, last, address, city, state, zip))
  meds.i <- subset(meds_conds_dates.i, select = c(first, last, description, cost, dispenses, totalcost, start, stop))
  conds.i <- subset(meds_conds_dates.i, select = c(first, last, cond_start, cond_stop))
  
  # Assign vectors for sheets in workbook
  sheets <- getSheets(wb)
  px <- sheets[[1]]
  conds <- sheets[[2]]
  meds <- sheets[[3]]
  
  # Add in data to sheets
  addDataFrame(patient.i, 
               sheet = px,
               startRow = 2,
               startColumn = 1,
               col.names = FALSE,
               row.names = FALSE)
  addDataFrame(meds.i, 
               sheet = meds,
               startRow = 2,
               startColumn = 1,
               col.names = FALSE,
               row.names = FALSE)
  addDataFrame(conds.i, 
               sheet = conds,
               startRow = 2,
               startColumn = 1,
               col.names = FALSE,
               row.names = FALSE)

  saveWorkbook(wb, file = paste(
    Excel.dir,paste("Patients_With_",i,".xlsx",sep = ""),sep = "/"))
}