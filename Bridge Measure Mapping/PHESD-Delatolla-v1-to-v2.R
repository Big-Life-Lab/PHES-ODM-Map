# The following code is intended as a holdover measure to map existing and available open data (primarily 
# from version 1 of the ODM) to version 2 in order to have more example data available
# a more permanent mapping solution is still under development and will likely use the linkML data transformer

# set up environment

options(scipen = 999)
library(tidyr)
library(dplyr)
library(writexl)

#### Original Data Treatment ####

# import the original data

wwMeasure <- read.csv(file = "PHESD-Delatolla-data-V1/wwMeasure.csv")
wwVirus <- read.csv("PHESD-Delatolla-data-V1/wastewater_virus.csv")

# transform data types to relevant formats

# make factor data types (transform dates to date-type data at the end to avoid merge conflicts)
wwMeasure$sampleID <- factor(wwMeasure$sampleID)
wwMeasure$labID <- factor(wwMeasure$labID)
wwMeasure$analysisDate <- factor(wwMeasure$analysisDate)
wwMeasure$fractionAnalyzed <- factor(wwMeasure$fractionAnalyzed)
wwMeasure$type <- factor(wwMeasure$type)
wwMeasure$unit <- factor(wwMeasure$unit)
wwMeasure$aggregation <- factor(wwMeasure$aggregation)
wwMeasure$qualityFlag <- factor(wwMeasure$qualityFlag)

wwVirus$sampleDate <- factor(wwVirus$sampleDate)
wwVirus$sampleID <- factor(wwVirus$sampleID)
wwVirus$siteID <- factor(wwVirus$siteID)
wwVirus$siteName <- factor(wwVirus$siteName)
wwVirus$reportDate <- factor(wwVirus$reportDate)

# fix numeric data for normalized secondary pathogens
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

# fix absent sampleIDs

#for wwMeasure 
wwMeasure$date2 <- wwMeasure$analysisDate #sampleIDs are made with the date, so we duplicate date to create a broken apart version
wwMeasure <- wwMeasure %>% separate_wider_delim(date2, delim = "-", names = c("year", "month", "day")) #break it apart
wwMeasure <- wwMeasure %>% separate_wider_position(year, c(milenium = 2, year = 2)) # break year apart since sampleID only uses the last 2 digits
wwMeasure$sampleID2 <- paste("o",wwMeasure$month,wwMeasure$day,wwMeasure$year, sep = ".") #paste the new sampleIDs together
wwMeasure$test <- wwMeasure$sampleID2 == wwMeasure$sampleID # see how well they match the originals
summary(wwMeasure$test) #check - p good
wwMeasure <- wwMeasure %>% 
  mutate(sampleID = coalesce(sampleID,sampleID2)) # replace only the NA sampleIDs with the new sampleIDs
#drop extraneous columns
wwMeasure <- wwMeasure[,1:9]

#for wwVirus
wwVirus$date2 <- wwVirus$sampleDate #sampleIDs are made with the date, so we duplicate date to create a broken apart version
wwVirus <- wwVirus %>% separate_wider_delim(date2, delim = "-", names = c("year", "month", "day")) #break it apart
wwVirus <- wwVirus %>% separate_wider_position(year, c(milenium = 2, year = 2)) # break year apart since sampleID only uses the last 2 digits
wwVirus$sampleID2 <- paste("o",wwVirus$month,wwVirus$day,wwVirus$year, sep = ".") #paste the new sampleIDs together
wwVirus$test <- wwVirus$sampleID2 == wwVirus$sampleID # see how well they match the originals
summary(wwVirus$test) #check - p good
wwVirus <- wwVirus %>% 
  mutate(sampleID = coalesce(sampleID,sampleID2)) # replace only the NA sampleIDs with the new sampleIDs
#drop extraneous columns
wwVirus <- wwVirus[,c(1:5,12:27)]

# Split wwVirus because it has some data on samples that isn't in wwWastewater, but also has many rows that report no 
# additional measures. This data can then be merged back into the larger data set later, after dropping blank 
# measurement columns
wwVirusBlanks <- wwVirus[rowSums(is.na(wwVirus[,6:21])) >= 16,]
wwVirusBlanks <- wwVirusBlanks[,1:5]

# this is the half of wwVirus now that does have some measurement data.
wwVirusWide <- wwVirus[rowSums(is.na(wwVirus[,6:21])) <= 15,]

# wwVirus wide to long - renaming the columns to be able to pivot the tables to long format using dplyr with greater ease.
wwVirusWide <- rename(wwVirusWide, 
                  mr_b117test_unitless_single = "testB117",
                  mr_b117detect_unitless_meanNr = "detectB117",
                  mr_b117fraction_proportion_meanNr = "fractionB117",
                  mr_b117fraction_proportion_stdev = "fractionB117_stdev",
                  mr_deltatest_unitless_single = "test_delta",
                  mr_deltadetect_unitless_meanNr = "detect_delta",
                  mr_deltafraction_proportion_meanNr = "fraction_delta",
                  mr_deltafraction_proportion_stdev = "fraction_delta_stdev",
                  mr_c2811ttest_unitless_single = "testC2811T",
                  mr_c2811tdetect_unitless_meanNr = "detectC2811T",
                  mr_c2811tfraction_proportion_meanNr = "fractionC2811T",
                  mr_c2811tfraction_proportion_stdev = "fractionC2811T_stdev",
                  mr_infA_gcPMMoV_meanNr = "InfA_copies_per_pep_copies_avg",
                  mr_infB_gcPMMoV_meanNr = "InfB_copies_per_pep_copies_avg",
                  mr_rsv_gcPMMoV_meanNr = "RSV_copies_per_pep_copies_avg",
                  mr_mpox_gcPMMoV_meanNr = "MPOX_copies_per_pep_copies_avg")

# wwVirus wide to long - dropping NAs, pivoting longer.
wwVirusLong <- wwVirusWide %>% 
  pivot_longer(
    cols = mr_b117test_unitless_single:mr_mpox_gcPMMoV_meanNr,
    names_to = c("type", "unit", "aggregation"), 
    names_pattern = "mr_?(.*)_(.*)_(.*)",
    values_to = "value",
    values_drop_na = TRUE
  )

# Combine data tables 

intermediary <- full_join(wwMeasure, wwVirusBlanks, "sampleID", relationship = "many-to-many")

raw_wwData <- full_join(intermediary, wwVirusLong, #"sampleID", 
                    relationship = "many-to-many")

# impute mising values within a sample to other srows for the same given sample
wwData <- raw_wwData %>%
  group_by(sampleID) %>%
  fill(everything(), .direction = "downup") %>%
  ungroup()

# Done!!!!!!!

#### ODM v2 Tables - Reference Tables from csv ####

parts <- read.csv("ref-tables-v2/ODM_parts_2.1.0 copy.csv")
sets <- read.csv("ref-tables-v2/ODM_sets.csv")
languages <- read.csv("ref-tables-v2/ODM_languages.csv")
translations <- read.csv("ref-tables-v2/ODM_translations_2.1.0 copy.csv")
countries <- read.csv("ref-tables-v2/ODM_countries.csv")
zones <- read.csv("ref-tables-v2/ODM_zones.csv")

#### ODM v2 Tables - Measures ####

# build measures table, fill in some missing points where possible
measures <- data.frame(
  sampleID = wwData$sampleID,
  purpose = as.factor("testing"),
  polygonID = NA,
  siteID = wwData$siteID,
  datasetID = wwData$labID,
  measureSetRepID = NA, #maybe build around samples....?
  aDateStart = NA,
  aDateEnd = wwData$analysisDate,
  reportDate = wwData$reportDate,
  compartment = as.factor("wat"),
  specimen = as.factor("sa"),
  fraction = wwData$fractionAnalyzed,
  group = NA, # code to populate this under development below
  class = NA, # code to populate this under development below
  measure = wwData$type,
  value = wwData$value,
  unit = wwData$unit,
  aggregation = wwData$aggregation,
  nomenclature = NA,
  index = NA,
  measureLic = as.factor("open"),
  reportable = TRUE,
  organizationID = wwData$labID,
  contactID = as.factor("delatollaLab"),
  refLink = "https://github.com/Delatolla-lab/PHESD",
  lastEdited = as.Date("2024-02-05"),
  notes = NA
)

# build unique measure report IDs
measures <- measures %>%
  mutate(measureRepID = paste(sampleID, measure, row_number(), sep = ""))
# populate the protocol IDs
measures <- measures %>%
  mutate(protocolID = ifelse(measure == "covN1", "pgsSolidsN1", 
  ifelse(measure == "covN2", "pgsSolidsN2", 
  ifelse(measure == "nPPMoV", "pgsSolidsPmmv", NA))))
# populate group and class
#measures <- measures %>%
#  mutate(group = ifelse(measure == "covN1", "sarsCov2", 
#                             ifelse(measure == "covN2", "sarsCov2", 
#                                    ifelse(measure == "nPPMoV", "pgsSolidsPmmv", NA))))
#measures <- measures %>%
#  mutate(class = ifelse(measure == "covN1", "allele", 
#                        ifelse(measure == "covN2", "allele", 
#                               ifelse(measure == "nPPMoV", "pgsSolidsPmmv", NA))))
# reorder columns to match ERD
measures <- measures %>%
  select(measureRepID, protocolID, everything())

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

#### ODM v2 Tables - qualityReports ####

# build qualityReports table, fill in some missing points where possible
qualityReports <- data.frame(
  measureRepID = measures$measureRepID,
  sampleID = NA,
  measureSetRepID = NA,
  qualityFlag = ifelse(wwData$qualityFlag ==FALSE, "noConcern", "qf1"),
  severity = NA,
  lastEdited = as.Date("2024-02-05"),
  notes = NA
  )

qualityReports <- qualityReports %>%
  mutate(qualityReportID = paste(qualityFlag, measureRepID))
# reorder columns to match ERD
qualityReports <- qualityReports %>%
  select(qualityReportID, everything())

#### ODM v2 Tables - datasets ####

# build datasets table, fill in some missing points where possible
datasets <- data.frame(
  parDatasetID = NA,
  datasetID = wwData$labID[1],
  datasetDate = as.Date("2020-04-08"),
  name = "Delatolla Group University of Ottawa - Open Data",
  license = "Open",
  descr = "The open data for Ottawa wastewater surveillance from the Delatolla Research group at the University of Ottawa. Testing is done for SARS-CoV-2 and other major pathogens.",
  refLink = "https://github.com/Delatolla-lab/PHESD",
  lang = "eng",
  funderCont = NA,
  custodyCont = "delatollaLab",
  funderID = NA,
  custodyID = "Ottawa-1",
  lastEdited = as.Date("2024-02-05"),
  notes = NA
)

#### ODM v2 Tables - organizations ####

# build organizations table, fill in some missing points where possible
organizations <- data.frame(
  organizationID = wwData$labID[1],
  name = "Delatolla Group - University of Ottawa",
  descr = "The Delatolla Research group is based at the University of Ottawa. Our research focuses on the application of wastewater-based surveillance as an early warning system for future pandemic preparedness and the implementation of wastewater-based surveillance to improve health equity in Canada and globally. Our research group is also currently interested in advancing the detection and quantification of biological targets of population and public health importance in waters and wastewaters. We have particular interest in developing protocols and best practices to translate water and wastewater derived biological target data into population and public health action. In addition, our group performs research to leverage modern analytical methods to advance our understanding of wastewater technologies and to optimize wastewater treatment technologies to protect natural waters.",
  addressID = "delatollaOttawa",
  datasetID = wwData$labID[1],
  orgType = "academ",
  orgLevel = NA,
  orgSector = "research",
  lastEdited = as.Date("2024-02-05"),
  notes = NA
)

#### ODM v2 Tables - sites ####

# build sites table, fill in some missing points where possible
sites <- data.frame(
  parSiteID = NA,
  siteID = wwData$siteID[1],
  datasetID = wwData$labID[1],
  polygonID = NA,
  siteType = "wwtp",
  sampleShed = "municp",
  addressID = "ropec",
  organizationID = wwData$labID[1],
  contactID = NA,
  name = wwData$siteName[1],
  descr = "The Robert O. Pickard Environmental Centre is a waste water treatment facility in Ottawa, Ontario, Canada. It provides secondary treatment to about 720,000 people.",
  repOrg1 = "Ottawa-1",
  repOrg2 = NA,
  healthRegion = "Ontario Health Region - East",
  popServ = 1100000,
  geoLat = 45.454147,
  geoLong = -75.59248,
  geoEPSG = NA,
  lastEdited = as.Date("2024-02-05"),
  notes = NA
)

#### ODM v2 Tables - addresses ####

# build addresses table, fill in some missing points where possible
addresses <- data.frame(
  addressID = c("delatollaOttawa", "ropec"),
  datasetID = rep(wwData$labID[1],2),
  addL1 = c("161 Louis-Pasteur","800 Green Creek Dr"),
  addL2 = c("Room A108",NA),
  city = c("Ottawa","Gloucester"),
  stateProvReg = rep("Ontario",2),
  pCode = c("K1N 6N5","K1J 1K6"),
  country = rep("Canada",2),
  lastEdited = as.Date("2024-02-05"),
  notes = NA,
  isoCode = rep("CA",2),
  isoZone = rep("CA-ON",2)
  )

#### ODM v2 Tables - contacts ####

# build contacts table, fill in some missing points where possible
contacts <- data.frame(
  contactID = "delatollaLab",
  datasetID = wwData$labID[1],
  organizationID = wwData$labID[1],
  firstName = "Robert",
  lastName = "Delatolla",
  email = "robert.delatolla@uottawa.ca",
  phone = NA,
  role = "Principle Investigator",
  lastEdited = as.Date("2024-02-05"),
  notes = "Dr. Robert Delatolla"
)

#### ODM v2 Tables - Protocols tables from csv ####

protocols <- read.csv("protocols-tables-Delatolla-V2/protocols.csv")
protocolRelationships <- read.csv("protocols-tables-Delatolla-V2/protocolRelationships.csv")
protocolSteps <- read.csv("protocols-tables-Delatolla-V2/protocolSteps.csv")

#### ODM v2 Tables - instruments ####

# build instruments table, fill in some missing points where possible
summary(as.factor(protocolSteps$Instrument.ID))

instruments <- data.frame(
  instrumentID = as.factor(protocolSteps$Instrument.ID[complete.cases(protocolSteps$Instrument.ID) == T]),
  datasetID = as.factor(datasets$datasetID),
  name = c(NA,NA,"Delatolla - qPCR thermocycler"),
  model = c(NA,NA,"CFX Connect qPCR thermocycler"),
  manufacturer = c(NA,NA,"Bio-Rad"),
  contactID = as.factor(contacts$contactID),
  organizationID = as.factor(organizations$organizationID),
  descr = c("Centrifuge machine","shaker tray machine","The CFX Connect real-time PCR detection system offers two-target analysis, excellent thermal cycler specifications, and the same reliable performance as the CFX96 Touch™ real-time PCR detection system. The system incorporates innovative optical technologies with powerful software to provide maximal reliability and efficiency for all the real-time PCR needs."),
  refLink = NA,
  insType = as.factor("ola"),
  insTypeOth = NA,
  index = NA,
  lastEdited = as.Date("2024-02-05"),
  notes = NA
  )

#### ODM v2 Tables - measureSets ####

# build measureSets table, fill in some missing points where possible
measureSets <- data.frame(
  measureSetRepID = NA,
  protocolID = NA,
  name = NA,
  organizationID = NA,
  contactID = NA,
  lastEdited = as.Date("2024-02-05"),
  notes = NA
)

#### ODM v2 Tables - polygons ####

# build polygons table, fill in some missing points where possible
polygons <- data.frame(
  polygonID = NA, #Add details once info is available
  datasetID = datasets$datasetID[1],
  name = "ROPEC-Ottawa Metropolitan Area Sewershed",
  descr = "Polygon for the Ottawa Metropolitan area sewershed serviced by the ROPEC wastewater treatment plant",
  polyPop = 1100000,
  geoType = NA,
  geoEPSG = NA,
  geoWKT = NA,
  fileLocation = NA,
  refLink = NA,
  organizationID = organizations$organizationID[1],
  contactID = contacts$contactID[1],
  lastEdited = as.Date("2024-02-05"),
  notes = NA
)

#### ODM v2 Tables - sampleRelationships ####

# build sampleRelationships table, fill in some missing points where possible
sampleRelationships <- data.frame(
  sampleRelationshipsID = NA,
  sampleIDSubject = NA,
  relationshipID = NA,
  sampleIDObject = NA,
  lastEdited = NA,
  notes = NA
)

#### Export Final Data ####

asset.list <- list(parts=parts,sets=sets,languages=languages,translations=translations,countries=countries,zones=zones,
measures=measures,measureSets=measureSets,samples=samples,sampleRelationships=sampleRelationships,qualityReports=qualityReports,
protocols=protocols,protocolRelationships=protocolRelationships,protocolSteps=protocolSteps,
contacts=contacts,sites=sites,addresses=addresses,polygons=polygons,datasets=datasets,organizations=organizations,instruments=instruments)

write_xlsx(
  asset.list, 
  "PHES-ODM Example data V-2-2-0 (delatolla).xlsx")

sub_folder <- "example_data_Delatolla_csv_assets-V2"

dir.create(sub_folder, showWarnings = FALSE)

# Write tibbles to CSV files within the sub-folder
for (name in names(asset.list)) {67
  file_path <- file.path(sub_folder, paste0(name, ".csv"))
  write.csv(asset.list[[name]], file = file_path, row.names = FALSE)
}
