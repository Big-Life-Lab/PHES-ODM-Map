# The following code is intended as a holdover measure to map existing and available open data (primarily 
# from version 1 of the ODM) to version 2 in order to have more example data available
# a more permanent mapping solution is still under development and will likely use the linkML data transformer

# set up environment ####

options(scipen = 999)
library(tidyr)
library(dplyr)
library(writexl)
library(Hmisc)
library(stringr)
library(lubridate)

#### Original Data Treatment ####

#### CPHD Data - will become measures ####
# import the original data
CPHD_QC <- read.csv(file = "centreau_qc_data-V1/CovidPublicHealthData.csv")
describe(CPHD_QC)

CPHD_QC$cphdID <- as.factor(CPHD_QC$cphdID)
CPHD_QC$reporterID <- as.factor(CPHD_QC$reporterID)
CPHD_QC$polygonID <- as.factor(CPHD_QC$polygonID)
CPHD_QC$date <- as.Date(CPHD_QC$date)
CPHD_QC$measure <- as.factor("cov")
CPHD_QC$type <- as.factor(CPHD_QC$type)
CPHD_QC$dateType <- as.factor(CPHD_QC$dateType)
colnames(CPHD_QC) <- c("measureRepID", "organizationID", "polygonID", "reportDate", "unit", "dateType", "value", "notes", "measure")


#### Lab Data - will become organizations ####
# import the original data
LabQC <- read.csv("centreau_qc_data-V1/Lab.csv")
describe(LabQC)

LabQC$labID <- as.factor(LabQC$labID)
LabQC$contactName <- ifelse(LabQC$contactName == "dominic frigon","frigonDominic","vanrolleghemPeter")
LabQC$contactName <- as.factor(LabQC$contactName)
colnames(LabQC) <- c("organizationID", "name", "contactID")


#### Polygon Data -will become polygons ####
# import the original data
PolygonQC <- read.csv("centreau_qc_data-V1/Polygon.csv")
describe(PolygonQC)

PolygonQC$polygonID <- as.factor(PolygonQC$polygonID)
PolygonQC$type <- as.factor(PolygonQC$type)
colnames(PolygonQC) <- c("polygonID", "name", "polyPop", "geoType", "geoWKT", "notes")


#### Reporter Data - will become contacts ####
# import the original data
ReporterQC <- read.csv("centreau_qc_data-V1/Reporter.csv")
describe(ReporterQC)

ReporterQC$reporterID <- as.factor(ReporterQC$reporterID)
ReporterQC <- ReporterQC %>% 
  separate(contactName, c("firstName", "lastName"), fill = "right")
ReporterQC$LabID <- as.factor(ReporterQC$LabID)
colnames(ReporterQC) <- c("contactID", "firstName", "lastName", "organizationID", "notes")


#### Sample Data - will become samples ####
# import the original data
SampleQC <- read.csv("centreau_qc_data-V1/Sample.csv")
describe(SampleQC)

drops <- c("instrumentID","dateTime","children","parent","fieldSampleTempC")
SampleQC <- SampleQC[ , !(names(SampleQC) %in% drops)]
SampleQC$sampleID <- as.factor(SampleQC$sampleID)
SampleQC$siteID <- as.factor(SampleQC$siteID)
SampleQC$reporterID[SampleQC$reporterID == "maryam tohidi"] <- "modeleau_lab_maryam"
SampleQC$reporterID[SampleQC$reporterID == "romain philippe"] <- "modeleau_lab_romain"
SampleQC$reporterID[SampleQC$reporterID == "daniel bolduc"] <- "??_daniel_bolduc"
SampleQC$reporterID <- as.factor(SampleQC$reporterID)
SampleQC$dateTimeStart <- ymd_hms(SampleQC$dateTimeStart)
SampleQC$dateTimeEnd <- ymd_hms(SampleQC$dateTimeEnd)
SampleQC$type[SampleQC$type == "rawwwww"] <- "rawww"
SampleQC$type <- as.factor(SampleQC$type)
SampleQC$collType <- NA
SampleQC$collType[SampleQC$collection == "cptp24h"] <- "timePr"
SampleQC$collType[SampleQC$collection == "cpfp24h"] <- "flowPr"
SampleQC$collType <- as.factor(SampleQC$collType)
SampleQC$collPer <- NA
SampleQC$collPer[SampleQC$collection == "cptp24h"] <- 24
SampleQC$collPer[SampleQC$collection == "cpfp24h"] <- 24
SampleQC$collNum <- NA
SampleQC$collNum[SampleQC$collection == "cptp24h"] <- 24
SampleQC$preTreatment <- as.logical(SampleQC$preTreatment)
SampleQC$pooled <- as.logical(SampleQC$pooled)
SampleQC$shippedOnIce <- as.logical(SampleQC$shippedOnIce)
SampleQC$qualityFlag <- as.logical(SampleQC$qualityFlag)

colnames(SampleQC) <- c("sampleID", "siteID", "contactID", "collDTStart", "collDTEnd", "saMaterial", "collectDEP", 
                        "method_pretreat", "pooled", "measure_sizeL", "index", "measure_shippedOnIce", "measure_stoTemp",
                        "qualityFlag", "notes", "collType", "collPer", "collNum")


#### Site Data - will become sites ####
# import the original data
SiteQC <- read.csv("centreau_qc_data-V1/Site.csv")
describe(SiteQC)

drops <- c("publicHealthDepartment", "healthRegion", "link", "notes")
SiteQC <- SiteQC[ , !(names(SiteQC) %in% drops)]
SiteQC$siteID <- as.factor(SiteQC$siteID)
SiteQC$type <- as.factor(SiteQC$type)
SiteQC$polygonID <- as.factor(SiteQC$polygonID)
SiteQC$sampleShed <- as.factor("municp")
colnames(SiteQC) <- c("siteID","name","descr","siteType","geoLat","geoLong","polygonID","sampleShed")


#### Site Measure Data - will become measures ####
# import the original data
SiteMeasureQC <- read.csv("centreau_qc_data-V1/SiteMeasure.csv")
describe(SiteMeasureQC)

drops <- c("sampleID")
SiteMeasureQC <- SiteMeasureQC[ , !(names(SiteMeasureQC) %in% drops)]
SiteMeasureQC$siteMeasureID <- as.factor(SiteMeasureQC$siteMeasureID)
SiteMeasureQC$siteID <- as.factor(SiteMeasureQC$siteID)
SiteMeasureQC$instrumentID <- as.factor(SiteMeasureQC$instrumentID)
SiteMeasureQC$reporterID <- as.factor(SiteMeasureQC$reporterID)
SiteMeasureQC$dateTime <- ymd_hms(SiteMeasureQC$dateTime)
SiteMeasureQC$type <- as.factor(SiteMeasureQC$type)
SiteMeasureQC$aggregation <- as.factor(SiteMeasureQC$aggregation)
SiteMeasureQC$unit <- as.factor(SiteMeasureQC$unit)
colnames(SiteMeasureQC) <- c("measureRepID","siteID","instrumentID","contactID","aDateEnd","measure","aggregation",
                             "aggregation_descr","value","unit","notes")


#### WW Measure Data - will become measures ####
# import the original data
WWMeasureQC <- read.csv("centreau_qc_data-V1/WWMeasure.csv")
describe(WWMeasureQC)

drops <- c("assayMethodID","reportDate")
WWMeasureQC <- WWMeasureQC[ , !(names(WWMeasureQC) %in% drops)]

WWMeasureQC$wwMeasureID <- as.factor(WWMeasureQC$wwMeasureID)
WWMeasureQC$reporterID <- as.factor(WWMeasureQC$reporterID)
WWMeasureQC$sampleID <- as.factor(WWMeasureQC$sampleID)
WWMeasureQC$labID <- as.factor(WWMeasureQC$labID)
WWMeasureQC$analysisDate <- as.Date(WWMeasureQC$analysisDate)
WWMeasureQC$fractionAnalyzed <- as.factor(WWMeasureQC$fractionAnalyzed)
WWMeasureQC$type <- as.factor(WWMeasureQC$type)
WWMeasureQC$unit <- as.factor(WWMeasureQC$unit)
WWMeasureQC$aggregation <- as.factor(WWMeasureQC$aggregation)
WWMeasureQC$qualityFlag <- as.factor(WWMeasureQC$qualityFlag)
colnames(WWMeasureQC) <- c("measureRepID","contactID","sampleID","organizationID","aDateEnd","fraction","measure","value","unit",
                           "aggregation","index","qualityFlag","notes")

#### ODM v2 Tables - Reference Tables from csv ####

parts <- read.csv("ref-tables-v2/ODM_parts_2.1.0 copy.csv")
sets <- read.csv("ref-tables-v2/ODM_sets.csv")
languages <- read.csv("ref-tables-v2/ODM_languages.csv")
translations <- read.csv("ref-tables-v2/ODM_translations_2.1.0 copy.csv")
countries <- read.csv("ref-tables-v2/ODM_countries.csv")
zones <- read.csv("ref-tables-v2/ODM_zones.csv")

#### ODM v2 Tables - Measures ####

# build measures table, fill in some missing points where possible
# will need to combine CPHD data, site measures data, and ww measures data together.
measuresCPHD <- data.frame(
  measureRepID = CPHD_QC$measureRepID,
  sampleID = NA,
  purpose = NA, # come back to this
  polygonID = CPHD_QC$polygonID,
  siteID = NA, 
  datasetID = CPHD_QC$organizationID,
  measureSetRepID = NA, # come back to this
  aDateStart = NA,
  aDateEnd = NA,
  reportDate = CPHD_QC$reportDate,
  compartment = as.factor("hum"),
  specimen = as.factor("po"), 
  fraction = NA,
  group = as.factor("sarsCov2"),
  class = as.factor("disease"),
  measure = CPHD_QC$measure,
  value = CPHD_QC$value,
  unit = CPHD_QC$unit,
  aggregation = as.factor("sin"),
  nomenclature = NA,
  index = NA,
  measureLic = as.factor("open"), #doublecheck this pls
  reportable = TRUE,
  organizationID = CPHD_QC$organizationID,
  contactID = CPHD_QC$organizationID,
  refLink = "https://zenodo.org/records/5597158", #doublecheck this later
  lastEdited = as.Date("2021-10-21"),
  notes = CPHD_QC$notes
)

measuresSites <- data.frame(
  measureRepID = SiteMeasureQC$measureRepID,
  sampleID = NA,
  purpose = NA, # come back to this
  polygonID = SiteMeasureQC$polygonID,
  siteID = NA, 
  datasetID = SiteMeasureQC$organizationID,
  measureSetRepID = NA, # come back to this
  aDateStart = NA,
  aDateEnd = NA,
  reportDate = SiteMeasureQC$reportDate,
  compartment = as.factor("hum"),
  specimen = as.factor("po"), 
  fraction = NA,
  group = as.factor("sarsCov2"),
  class = as.factor("disease"),
  measure = SiteMeasureQC$measure,
  value = SiteMeasureQC$value,
  unit = SiteMeasureQC$unit,
  aggregation = as.factor("sin"),
  nomenclature = NA,
  index = NA,
  measureLic = as.factor("open"), #doublecheck this pls
  reportable = TRUE,
  organizationID = SiteMeasureQC$organizationID,
  contactID = SiteMeasureQC$organizationID,
  refLink = "https://zenodo.org/records/5597158", #doublecheck this later
  lastEdited = as.Date("2021-10-21"),
  notes = SiteMeasureQC$notes
)

measuresWW <- data.frame(
  measureRepID = WWMeasureQC$measureRepID,
  sampleID = NA,
  purpose = NA, # come back to this
  polygonID = WWMeasureQC$polygonID,
  siteID = NA, 
  datasetID = WWMeasureQC$organizationID,
  measureSetRepID = NA, # come back to this
  aDateStart = NA,
  aDateEnd = NA,
  reportDate = WWMeasureQC$reportDate,
  compartment = as.factor("hum"),
  specimen = as.factor("po"), 
  fraction = NA,
  group = as.factor("sarsCov2"),
  class = as.factor("disease"),
  measure = WWMeasureQC$measure,
  value = WWMeasureQC$value,
  unit = WWMeasureQC$unit,
  aggregation = as.factor("sin"),
  nomenclature = NA,
  index = NA,
  measureLic = as.factor("open"), #doublecheck this pls
  reportable = TRUE,
  organizationID = WWMeasureQC$organizationID,
  contactID = WWMeasureQC$organizationID,
  refLink = "https://zenodo.org/records/5597158", #doublecheck this later
  lastEdited = as.Date("2021-10-21"),
  notes = WWMeasureQC$notes
)


#### ODM v2 Tables - samples ####

# build samples table, fill in some missing points where possible
samples <- data.frame(
  sampleID = wwData$sampleID,
  protocolID = NA,
  organizationID = wwData$labID,
  contactID = as.factor("delatollaLab"),
  siteID = wwData$siteID,
  purpose = as.factor("testing"),
  saMaterial = as.factor("pSludge"),
  datasetID = wwData$labID,
  origin = as.factor("field"), # maybe double check this?
  repType = as.factor("unique"), # maybe double check this?
  collType = as.factor("timePr"),
  collPer = 24,
  collNum = 24,
  pooled = FALSE, # double check this, please
  collDT = wwData$sampleDate,
  collDTStart = NA,
  collDTEnd = NA,
  sentDate = as.factor("NR"),
  recDate  = as.factor("NR"),
  reportable = TRUE,
  lastEdited = as.Date("2024-02-05"),
  notes = NA
)

