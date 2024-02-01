# The following code is intended as a holdover measure to map existing and available open data (primarily 
# from version 1 of the ODM) to version 2 in order to have more example data available
# a more permanent mapping solution is still under development and will likely use the linkML data transformer

# set up environment

options(scipen = 999)
library(tidyr)
library(dplyr)

#### Original Data Treatment ####

# import the original data

wwMeasure <- read.csv("wwMeasure.csv")
wwVirus <- read.csv("wastewater_virus.csv")

# tranform data tpes to relevant formats

# make date and factor data types
wwMeasure$sampleID <- factor(wwMeasure$sampleID)
wwMeasure$labID <- factor(wwMeasure$labID)
wwMeasure$analysisDate <- as.Date(wwMeasure$analysisDate)
wwMeasure$fractionAnalyzed <- factor(wwMeasure$fractionAnalyzed)
wwMeasure$type <- factor(wwMeasure$type)
wwMeasure$unit <- factor(wwMeasure$unit)
wwMeasure$aggregation <- factor(wwMeasure$aggregation)
wwMeasure$qualityFlag <- factor(wwMeasure$qualityFlag)

wwVirus$sampleDate <- as.Date(wwVirus$sampleDate)
wwVirus$sampleID <- factor(wwVirus$sampleID)
wwVirus$siteID <- factor(wwVirus$siteID)
wwVirus$siteName <- factor(wwVirus$siteName)
wwVirus$reportDate <- as.Date(wwVirus$reportDate)

# fix numeric data for normalized secodary pathogens
wwVirus$InfA_copies_per_pep_copies_avg[wwVirus$InfA_copies_per_pep_copies_avg == "Not tested"] <- 999
wwVirus$InfA_copies_per_pep_copies_avg <- as.numeric(wwVirus$InfA_copies_per_pep_copies_avg)
wwVirus$InfB_copies_per_pep_copies_avg[wwVirus$InfB_copies_per_pep_copies_avg == "Not tested"] <- 999
wwVirus$InfB_copies_per_pep_copies_avg <- as.numeric(wwVirus$InfB_copies_per_pep_copies_avg)
wwVirus$RSV_copies_per_pep_copies_avg[wwVirus$RSV_copies_per_pep_copies_avg == "Not tested"] <- 999
wwVirus$RSV_copies_per_pep_copies_avg <- as.numeric(wwVirus$RSV_copies_per_pep_copies_avg)
wwVirus$MPOX_copies_per_pep_copies_avg[wwVirus$MPOX_copies_per_pep_copies_avg == "Not tested"] <- 999
wwVirus$MPOX_copies_per_pep_copies_avg <- as.numeric(wwVirus$MPOX_copies_per_pep_copies_avg)
# check structure

summary(wwMeasure)
summary(wwVirus)

wwMeasure$sampleID[1]
measMiss <- wwMeasure[wwMeasure$sampleID == wwMeasure$sampleID[1],]

# Combine data tables 

wwData <- full_join(wwMeasure, wwVirus, "sampleID")

#### ODM v2 Tables - Measures ####

measures <- data.frame()

measures$measureRepID
measures$sampleID <- wwMeasure$sampleID
measures$siteID
measures$aDateEnd
measures$specimen
measures$fraction
measures$measure
measures$value
measures$unit
measures$aggregation
