## @knitr loadData

################
# Download CSV files from world bank
# Salomon Gilles
# June 11 2016
################

# Load packages
library(repmis)
library(downloader)
library(dplyr)
library(xtable)
library(RCurl)
library(reshape2) 
library(ggplot2)
library(bitops)

setwd("C:/SMU-HomeWork/DataScience/CSUnit06/CaseStudyUnit06")
# Download data for GDP data
FGDPRawData <- source_data("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv")
# Download data for Country Stat data
FSTATRawData <- source_data("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv")

######################
# Clean, Merge, sort final and save
# Salomon Gilles
# Juin 11 2016
######################

## This file extracts columns 1,2, 4 and 5 where they are not null or blank
##  and creates a GDP table before saving : GDPRawData  
## Also extracts columns countrycode and incomegroup from the Stat table:
##  and creates FSTATRawData table before saving 
##
## cleans, GDP field and converts into numeric
##  then merges both tables nto a single FGDPSTATRawData table 

# only get the rows were columns 1,2, 4 and 5 are not null or blank
FGDPDataCleaned <- subset(FGDPRawData, FGDPRawData[1] != "" & FGDPRawData[2] != "" & FGDPRawData[4] != "" & FGDPRawData[5] != "")
# remove all other columns BUT 1, 2, 4, and 5
FGDPDataCleaned <- FGDPDataCleaned[c(-3, -(6:10))]
# set column variable names as below
names(FGDPDataCleaned) <- c("CountryCode", "Ranking", "Country", "Gdp")
# Tidy all commas in gdp field
FGDPDataCleaned$Gdp <- gsub(",", "", FGDPDataCleaned$Gdp)
# convert to numeric
FGDPDataCleaned$Gdp <- as.numeric(FGDPDataCleaned$Gdp)
# only extract countrycode and incomegroup(columns 1 and 3) from each row where not null or blank
FSTATDataCleaned <- subset(FSTATRawData, FSTATRawData[1] != "" & FSTATRawData[3] != "")
# remove all other columns but 1 and 3
FSTATDataCleaned <- FSTATDataCleaned[c(-2, -(4:31))]
# change column variable name for income.group to incomegroup
names(FSTATDataCleaned)[2] <- "IncomeGroup"


# merge FGDPDataCleaned and FSTATDatacleaned using countrycode into FGDPFSTATData
FGdpFStatMerged <- merge(FGDPDataCleaned, FSTATDataCleaned, by = "CountryCode", all = TRUE)
# Subset of incomplete data / values missing one or more variables
FGDPSTATMissing <- subset(FGdpFStatMerged, is.na(FGdpFStatMerged[1]) | is.na(FGdpFStatMerged[2]) | is.na(FGdpFStatMerged[3]) | is.na(FGdpFStatMerged[4]) | is.na(FGdpFStatMerged[5]))
# after stats reporting, final clean dataset - remove any introduced nulls or NA
FinalFGdpFStatMerged <- subset(FGdpFStatMerged, !is.na(FGdpFStatMerged[1]) & !is.na(FGdpFStatMerged[2]) & !is.na(FGdpFStatMerged[3]) & !is.na(FGdpFStatMerged[4]) & !is.na(FGdpFStatMerged[5]))

##------------------------------------------------------------------------------------------
## @knitr highOecdNhighNonOecdAvgData
# select subset of high income : oecd countries and nonoecd countries
HighOECD <- subset(FinalFGdpFStatMerged, FinalFGdpFStatMerged[5] == "High income: OECD")
HighOECD <- mean(HighOECD[,"Gdp"])
print(paste("The average GDP for High income OECD countries is ", HighOECD))
HighnonOECD <- subset(FinalFGdpFStatMerged, FinalFGdpFStatMerged[5] == "High income: nonOECD")
HighnonOECD <- mean(HighnonOECD[,"Gdp"])
print(paste("The average GDP for High income nonOECD countries is ", HighnonOECD))
##------------------------------------------------------------------------------------------------

##------------------------------------------------------------------------------------------
# sort data in ascending order by smallest gdp to largest
## @knitr thirteenCountryData
FinalMergedStorted <- FinalFGdpFStatMerged[order(FinalFGdpFStatMerged["Gdp"]),]
# get the 13th country from the sorted list
thirthCountry <- FinalMergedStorted[13,3]
print(paste("The thirteenth country after performing ascending sort is ", thirthCountry))
##------------------------------------------------------------------------------------------


##------------------------------------------------------------------------------------------
# ggplot gdp for each country colorgroup per income group
## @knitr ggplotData
ggplot(FinalFGdpFStatMerged, aes(FinalFGdpFStatMerged$Country, FinalFGdpFStatMerged$Gdp, color=factor(FinalFGdpFStatMerged$IncomeGroup))) + geom_point()
##------------------------------------------------------------------------------------------

##------------------------------------------------------------------------------------------
## @knitr quantilInfo
FinalQuants <- quantile(FinalFGdpFStatMerged[,"Gdp"])
summary(FinalQuants)
# rearrange from max to min
quantilData <- matrix(c(FinalQuants[1],FinalQuants[2],FinalQuants[3],FinalQuants[4],FinalQuants[5]), ncol = 1, byrow = TRUE)
mytable <- data.frame( quartilData = quantilData)
summary(mytable)


##------------------------------------------------------------------------------------------
## @knitr lmiCountryWithHighGdpData
SortedHigh2Low <- FinalFGdpFStatMerged[order(-FinalFGdpFStatMerged["Gdp"]),]
# select top 38
top38High2Low <- SortedHigh2Low[1:38,]
# count of lower middle income countries within highest gdp
LMIwithHighGdpCount <- subset(top38High2Low, top38High2Low["IncomeGroup"] == "Lower middle income")
print(LMIwithHighGdpCount)
print(paste("The number of countries among top 38 ranking GDP with lower middle income are ", as.numeric(dim(LMIwithHighGdpCount)[1])))
##------------------------------------------------------------------------------------------


##------------------------------------------------------------------------------------------
## @knitr fgdpCountData
##------------------------------------------------------------------------------------------
# country count before match
FGDPCount <- dim(FGDPDataCleaned)[1] 

##------------------------------------------------------------------------------------------
## @knitr gdpstatCountData
##------------------------------------------------------------------------------------------
# count of STats
FSTATCount <- dim(FSTATDataCleaned)[1]

##------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------
# get matching records count
## @knitr mergedMatchingCountData
print(paste("The number of IDs matched by country code is ", as.numeric(dim(FGdpFStatMerged)[1])))

# count of missing/incomplete
##------------------------------------------------------------------------------------------
## @knitr mergeMissingCountData
print(paste("The number of missing NAs found are ", as.numeric(dim(FGDPSTATMissing)[1])))
##------------------------------------------------------------------------------------------


##------------------------------------------------------------------------------------------
## @knitr  finalGdpStatCountData
##------------------------------------------------------------------------------------------
# final count of gdpstat
FinalFGDPFSTATCount <- dim(FinalFGdpFStatMerged)[1]
##------------------------------------------------------------------------------------------


