## Process long format data table


library(lutz)
library(countrycode)
library(readxl)
library(reshape2)
library(lubridate)
library(dplyr)
library(ggplot2)
options(dplyr.summarise.inform = FALSE)

## change the default directories as needed
baseDir = "."
baseIPT = "."
baseAnalysis = "."

fileName = "DataSheet_longformat_TEST_v2.xlsx"


## read sheets
## Site INFO
DF.sites = read_xlsx(file.path(baseDir, fileName),sheet = "SiteInfo")
DF.sites = DF.sites[!is.na(DF.sites$COUNTRY),]

## convert time to UTC
## get number of seconds from midnight
secsSTART = (1 -abs(as.numeric(julian(DF.sites$TIME_START)) - (as.integer(julian(DF.sites$TIME_START))))) * (60*60*24)
secsEND = (1 - abs(as.numeric(julian(DF.sites$TIME_END)) - (as.integer(julian(DF.sites$TIME_END))))) * (60*60*24)
dateChar = paste(DF.sites$YEAR, DF.sites$MONTH, DF.sites$DAY, sep="-")
## get timezone and timezone offset
timeZone = tz_lookup_coords(mean(DF.sites$LATITUDE, na.rm=T), mean(DF.sites$LONGITUDE, na.rm=T), method="accurate")
dateOffset = tz_offset(dateChar, timeZone)$utc_offset_h
## create data and time UTC
DF.sites$eventDate = as.POSIXct(dateChar, tz="UTC")
DF.sites$TIME_START = DF.sites$eventDate + seconds(secsSTART) + hours(dateOffset)
DF.sites$TIME_END = DF.sites$eventDate + seconds(secsEND) + hours(dateOffset)
DF.sites$eventTime = paste(format(DF.sites$TIME_START, "%H:%M:%SZ"), format(DF.sites$TIME_END, "%H:%M:%SZ"), sep="/")

## other fields
DF.sites$datasetName = paste0("MBON-P2P-biodiversity-",unique(DF.sites$countryCode))
DF.sites$samplingProtocol = "MBON-P2P_bestpractices-rockyshores"
DF.sites$samplingSizeValue = 0.25
DF.sites$samplingSizeUnit = "square meter"
DF.sites$countryCodeISO = countrycode(DF.sites$COUNTRY, "country.name","iso3c")

## Data
DF.data = read_xlsx(file.path(baseDir, fileName),sheet = "DATA")
DF.data = DF.data[!is.na(DF.data$LOCALITY),]


## spp list
DF.spp = read_xlsx(file.path(baseDir, fileName),sheet = "sppList")

## codes
DF.countryCodes = read_xlsx(file.path(baseDir, fileName),sheet = "Countries")
DF.localityCodes = read_xlsx(file.path(baseDir, fileName),sheet = "Locality")
DF.siteCodes = read_xlsx(file.path(baseDir, fileName),sheet = "Sites")
DF.habitatCodes = read_xlsx(file.path(baseDir, fileName),sheet = "Habitat")


## make IDs
## add codes: SITES
DF.sites = left_join(DF.sites, DF.countryCodes, by = "COUNTRY")
DF.sites = left_join(DF.sites, DF.localityCodes, by = "LOCALITY")
DF.sites = left_join(DF.sites, DF.siteCodes, by = "SITE")
DF.sites = left_join(DF.sites, DF.habitatCodes, by = "HABITAT")

DF.sites$PARENT_UNIT_ID = paste(DF.sites$countryCode, DF.sites$localityCode, DF.sites$siteCode, DF.sites$habitatCode, 
                                paste0(DF.sites$YEAR, DF.sites$MONTH, DF.sites$DAY), sep="_")
DF.sites$UNIT_ID = paste(DF.sites$PARENT_UNIT_ID, DF.sites$STRATA, sep="_")


## add codes: DATA 
## add aphia and rank
DF.data = left_join(DF.data, DF.spp[,c("scientificName", "AphiaID", "Rank")])
DF.data = left_join(DF.data, DF.sites[,c("UNIT_ID", "LOCALITY", "SITE", "STRATA")])
DF.data = DF.data %>% group_by(LOCALITY, SITE, STRATA, SAMPLE) %>% 
  mutate(sampleOrganismID = 1:n(), scientificName, AphiaID, Rank, Variable, Value)
DF.data$occurrenceID = paste(DF.data$UNIT_ID, DF.data$SAMPLE, sprintf("%03d", DF.data$sampleOrganismID), sep="_")

## convert abundance to count per square meter
densityMultiplier = list("FULL QUADRAT" = 4 , "EIGHT RANDOM" = 100/8*4)
DF.data$Value[DF.data$Variable=="ABUNDANCE"] = DF.data$Value[DF.data$Variable=="ABUNDANCE"] * densityMultiplier[[DF.data$AREA_quadrat]]


## other fields for IPT
DF.data$basisOfRecord = "HumanObservation"
DF.data$occurrenceStatus = "present"
DF.data$scientificNameID = paste0("lsid:marinespecies.org:taxname:", DF.data$AphiaID)

## fields for the eMoF
DF.data$measurementTypeID = ifelse(DF.data$Variable=="COVER",
                                   "http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL10/",  ##Coverage (in assayed sample) of biological entity 
                                   "http://vocab.nerc.ac.uk/collection/P06/current/UPMS/") ## number per square meter

DF.data$measurementUnit = ifelse(DF.data$Variable=="COVER", "percent", "count")
DF.data$measurementUnitID = ifelse(DF.data$Variable=="COVER",
                                   "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/",    ## percentage
                                   "http://vocab.nerc.ac.uk/collection/P06/current/UPMS/")   ## number per square meter



DF.data = DF.data %>% arrange(occurrenceID, scientificName)



## Cleanup

IPT.event = DF.sites %>% 
  select(datasetName,
         parentEventID=PARENT_UNIT_ID,
         eventID = UNIT_ID,
         samplingProtocol,
         samplingSizeValue,
         samplingSizeUnit,
         eventDate,
         eventTime,
         year = YEAR,
         month = MONTH,
         day = DAY,
         habitat = HABITAT,
         eventRemarks = REMARKS,
         country = COUNTRY,
         countryCode = countryCodeISO,
         locality = LOCALITY,
         decimalLatitude = LATITUDE,
         decimalLongitude = LONGITUDE,
         coordinateUncertaintyInMeters = GPS_ERROR,
         geodeticDatum = DATUM,
         strata=STRATA)

DF.data.noSubstrate = DF.data %>% 
  filter(! grepl("substrate", scientificName, fixed = T))

IPT.occurrence = DF.data.noSubstrate %>% ungroup() %>% 
  select(eventID = UNIT_ID,
         basisOfRecord,
         occurrenceID,
         scientificNameID,
         scientificName, 
         taxonRank = Rank) 


IPT.mof = data.frame(eventID = DF.data.noSubstrate$UNIT_ID, 
                     occurrenceID = DF.data.noSubstrate$occurrenceID,
                     measurementType = tolower(DF.data.noSubstrate$Variable), 
                     measurmenetTypeID = DF.data.noSubstrate$measurementTypeID,
                     measurementValue = DF.data.noSubstrate$Value,
                     measurementUnit = DF.data.noSubstrate$measurementUnit,
                     measurementUnitID = DF.data.noSubstrate$measurementUnitID
                     )


## generate data anaylisis files
## reformat to wide
DF.dataWide = dcast(occurrenceID+LOCALITY+SITE+STRATA+SAMPLE+scientificName+AphiaID+Rank~Variable, value.var = "Value", data=DF.data, sum)


## save files
rootFileName = paste(unique(DF.sites$countryCodeISO), paste0(unique(DF.sites$localityCode, collapse="-")), 
                     unique(DF.sites$HABITAT), gsub("-","", min(DF.sites$eventDate)), sep="_")

## IPT
readr::write_csv(IPT.event, path = file.path(baseDir,baseIPT,paste0(rootFileName, "_IPT-event.csv")))
readr::write_csv(IPT.occurrence, path = file.path(baseDir,baseIPT,paste0(rootFileName, "_IPT-occurrence.csv")))
readr::write_csv(IPT.mof, path = file.path(baseDir,baseIPT,paste0(rootFileName, "_IPT-mof.csv")))

## Analysis
readr::write_csv(DF.dataWide, path = file.path(baseDir,baseAnalysis,paste0(rootFileName, "_analysis.csv")))
readr::write_csv(DF.sites, path = file.path(baseDir,baseAnalysis,paste0(rootFileName, "_site.csv")))



