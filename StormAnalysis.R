## -------------------------------------------------
## Reproducible Research Coursera Course Project 2
## -------------------------------------------------

## STEP 0 LOAD LIBRARIES
## ---------------------
if (!require("stringdist")) {
      install.packages("stringdist", dependencies = TRUE)
      library(stringdist)
}
if (!require("dplyr")) {
      install.packages("dplyr", dependencies = TRUE)
      library(dplyr)
} 
if (!require("reshape2")) {
   install.packages("reshape2", dependencies = TRUE)
   library(reshape2)
} 
if (!require("ggplot2")) {
   install.packages("ggplot2", dependencies = TRUE)
   library(ggplot2)
}

## Damage calculator
damagecalc <- function(dmg=0, expo="0"){
   value <-0
   expo <- toupper(expo)
   if (expo=="H"){
      value <- dmg * 100
   } else if (expo=="K"){
      value <- dmg * 1000
   } else if (expo=="M"){
      value <- dmg * 1000000
   } else if (expo=="B"){
      value <- dmg * 1000000000
   } else if (!grepl("\\D", expo)){
      value <- dmg * (10^as.numeric(expo))
   } else {
      value <- 0
   }
   return(value)
}

## STEP 1 DOWNLOAD AND READ RAW DATA
## ---------------------------------
dfile <- "repdata_data_StormData.csv"
if (!file.exists(dfile)) {
      download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                    destfile = dfile)
}
if (!exists('stormdata'))
      stormdata <- read.csv(dfile, 
                            header = TRUE)

## STEP 2 CLEANSE DATA
## -------------------

## Select only relevant columns and rows
wrkstormdata <- stormdata %>%
      mutate(DATE =as.Date(BGN_DATE, '%m/%d/%Y'), 
             EVTYPE=toupper(trimws(EVTYPE))) %>%
      filter(as.numeric(format(DATE,'%Y'))>=1996) %>%
      select(DATE, STATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

## Master of 48 Official Event Types
EventTypesMaster <- toupper(c(
      "Astronomical Low Tide",
      "Avalanche",
      "Blizzard",
      "Coastal Flood",
      "Cold/Wind Chill",
      "Debris Flow",
      "Dense Fog",
      "Dense Smoke",
      "Drought",
      "Dust Devil",
      "Dust Storm",
      "Excessive Heat",
      "Extreme Cold/Wind Chill",
      "Flash Flood",
      "Flood",
      "Freezing Fog",
      "Frost/Freeze",
      "Funnel Cloud",
      "Hail",
      "Heat",
      "Heavy Rain",
      "Heavy Snow",
      "High Surf",
      "High Wind",
      "Hurricane/Typhoon",
      "Ice Storm",
      "Lakeshore Flood",
      "Lake-Effect Snow",
      "Lightning",
      "Marine Hail",
      "Marine High Wind",
      "Marine Strong Wind",
      "Marine Thunderstorm Wind",
      "Rip Current",
      "Seiche",
      "Sleet",
      "Storm Tide",
      "Strong Wind",
      "Thunderstorm Wind",
      "Tornado",
      "Tropical Depression",
      "Tropical Storm",
      "Tsunami",
      "Volcanic Ash",
      "Waterspout",
      "Wildfire",
      "Winter Storm",
      "Winter Weather"
))

## Cleanse Event Types Data
EventTypesRAWData <- wrkstormdata %>%
   group_by(EVTYPE) %>%
   summarise(CNT = n())
EventTypesRAWData <- cbind(EventTypesRAWData, NEWEVTYPE=EventTypesMaster[amatch(EventTypesRAWData$EVTYPE, EventTypesMaster, maxDist=7)])
EventTypesRAWData$NEWEVTYPE[grep(pattern = "FLD|MINOR FLOODING|RIVER FLOODING|SNOWMELT FLOODING|STREET FLOODING|TIDAL FLOODING|URBAN FLOODING|URBAN/STREET FLOODING", EventTypesRAWData$EVTYPE)] <- "FLOOD"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "COASTAL FLOODING|COASTAL  FLOODING|CSTL FLOODING|CSTL FLOOD", EventTypesRAWData$EVTYPE)] <- "COASTAL FLOOD"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "FIRE", EventTypesRAWData$EVTYPE)] <- "WILDFIRE"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "HURRICANE|TYPHOON", EventTypesRAWData$EVTYPE)] <- "HURRICANE/TYPHOON"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "FREEZING RAIN", EventTypesRAWData$EVTYPE)] <- "SLEET"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "HOT|WARM|EXCESSIVE HEAT|HIGH TEMP|MONTHLY TEMPERATURE|RECORD TEMPERATURE|RECORD TEMPERATURES|TEMPERATURE RECORD", EventTypesRAWData$EVTYPE)] <- "EXCESSIVE HEAT"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "COLD|UNSEASONABLY COOL|CHILL TEMPERATURES", EventTypesRAWData$EVTYPE)] <- "COLD/WIND CHILL"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "EXTREME COLD|EXTENDED COLD|EXCESSIVE COLD|EXTREME WINDCHILL", EventTypesRAWData$EVTYPE)] <- "EXTREME COLD/WIND CHILL"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "SNOWFALL|LIGHT SNOW", EventTypesRAWData$EVTYPE)] <- "HEAVY SNOW"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "MUDSLIDE|LANDSLIDE|LANDSLIDES", EventTypesRAWData$EVTYPE)] <- "DEBRIS FLOW"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "HIGH SURF|HIGH WATER|HEAVY SURF", EventTypesRAWData$EVTYPE)] <- "HIGH SURF"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "MICROBURST|MICOBURST", EventTypesRAWData$EVTYPE)] <- "STRONG WIND"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "SNOW A|SNOW S|EXCESSIVE SNOW", EventTypesRAWData$EVTYPE)] <- "HEAVY SNOW"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "FREEZE|FROST|BLACK ICE", EventTypesRAWData$EVTYPE)] <- "FROST/FREEZE"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "PRECIP|HVY RAIN|HEAVY RAIN|MONTHLY RAINFALL|PROLONGED RAIN|RAIN (HEAVY)|RAIN DAMAGE|RECORD RAINFALL|TORRENTIAL RAINFALL", EventTypesRAWData$EVTYPE)] <- "HEAVY RAIN"
EventTypesRAWData$NEWEVTYPE[grep(pattern = "SEVERE THUNDERSTORM", EventTypesRAWData$EVTYPE)] <- "WINTER STORM"
EventTypesRAWData$NEWEVTYPE[startsWith(EventTypesRAWData$EVTYPE, "TSTM WIND")] <- "THUNDERSTORM WIND"
EventTypesRAWData$NEWEVTYPE[startsWith(EventTypesRAWData$EVTYPE, "MARINE TSTM WIND")] <- "MARINE THUNDERSTORM WIND"
EventTypesRAWData$NEWEVTYPE[startsWith(EventTypesRAWData$EVTYPE, "EXTREME COLD")] <- "EXTREME COLD/WIND CHILL"
EventTypesRAWData$NEWEVTYPE[startsWith(EventTypesRAWData$EVTYPE, "WIND")] <- "STRONG WIND"
EventTypesRAWData$NEWEVTYPE[startsWith(EventTypesRAWData$EVTYPE, "WIND CHILL")] <- "COLD/WIND CHILL"
EventTypesRAWData$NEWEVTYPE[EventTypesRAWData$EVTYPE=="SNOW"] <- "HEAVY SNOW"
EventTypesRAWData$NEWEVTYPE[EventTypesRAWData$EVTYPE=="FOG"] <- "DENSE FOG"
EventTypesRAWData$NEWEVTYPE[EventTypesRAWData$EVTYPE=="THUNDERSTORM"] <- "WINTER STORM"
levels(EventTypesRAWData$NEWEVTYPE) <- c(levels(EventTypesRAWData$NEWEVTYPE), "OTHER")
EventTypesRAWData$NEWEVTYPE[is.na(EventTypesRAWData$NEWEVTYPE)] <- "OTHER"
# Standardize event types
wrkstormdata <- left_join(wrkstormdata, EventTypesRAWData, by="EVTYPE") %>%
   select(DATE, STATE, EVTYPE=NEWEVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
   mutate(YEAR=as.numeric(format(DATE,'%Y')), 
          MONTH=as.numeric(format(DATE,'%m')))
# Calculate total damage in USD
wrkstormdata$TOTPROPDMG <- mapply(damagecalc, wrkstormdata$PROPDMG, wrkstormdata$PROPDMGEXP)
wrkstormdata$TOTCROPDMG <- mapply(damagecalc, wrkstormdata$CROPDMG, wrkstormdata$CROPDMGEXP)

##healthdamagebyyear <- wrkstormdata %>%
##   filter((INJURIES>0 | FATALITIES>0) & EVTYPE %in% healthdamagetop) %>%
##   group_by(EVTYPE, YEAR) %>%
##   summarise(FATALITIES=sum(FATALITIES), INJURIES=sum(INJURIES)) %>%
##   arrange(YEAR, desc(INJURIES))

##ggplot(healthdamagebyyear, aes(x=YEAR, y=INJURIES)) +
##   ggtitle("EVOLUTION OF INJURIES BY EVENT TYPE") +
##   geom_line(aes(color=EVTYPE))

## STEP 3 Calculate which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health
## --------------------------------------------------------------------------------------------------------------------------------
healthdamage <- wrkstormdata %>%
   group_by(EVTYPE) %>%
   summarise(TOTFATALITIES=sum(FATALITIES), TOTINJURIES=sum(INJURIES), TOTHEALTHDMG=TOTFATALITIES+TOTINJURIES) %>%
   arrange(desc(TOTHEALTHDMG))

tophealthdamage <- healthdamage[1:10,1:3]
colnames(tophealthdamage) <- c("EVENT_TYPE", "FATALITIES", "INJURIES")
tophealthdamage <- melt(tophealthdamage, id.vars="EVENT_TYPE")

perctophealthdmg <- sum(tophealthdamage$value) / sum(healthdamage$TOTHEALTHDMG)

ggplot(tophealthdamage, aes(fill=variable, x=reorder(EVENT_TYPE, value), y=value))+
   geom_bar(stat="identity") +
   coord_flip() +
   ggtitle("MOST HARMFUL EVENTS TO POPULATION HEALTH\nBY EVENT TYPE") +
   xlab("EVENT TYPE")+
   ylab("TOTAL NUMBER OF FATALITIES AND INJURIES")

## STEP 4 Calculate which types of events have the greatest economic consequences
## ------------------------------------------------------------------------------
economicdamage <- wrkstormdata %>%
   group_by(EVTYPE) %>%
   summarise(TOTPROPDMG=sum(TOTPROPDMG, na.rm=TRUE), TOTCROPDMG=sum(TOTCROPDMG, na.rm=TRUE), TOTDMG=TOTPROPDMG+TOTCROPDMG) %>%
   arrange(desc(TOTDMG))

topeconomicdamage <- economicdamage[1:10, 1:3]
colnames(topeconomicdamage) <- c("EVENT_TYPE", "PROPERTY_DAMAGE", "CROP_DAMAGE")
topeconomicdamage <- melt(topeconomicdamage, id.vars="EVENT_TYPE")

perctopeconomicdmg <- sum(topeconomicdamage$value) / sum(economicdamage$TOTDMG)

ggplot(topeconomicdamage, aes(fill=variable, x=reorder(EVENT_TYPE, value), y=value/1000000000))+
   geom_bar(stat="identity") +
   coord_flip() +
   ggtitle("GREATEST ECONOMIC CONSEQUENCES\nBY EVENT TYPE") +
   xlab("EVENT TYPE")+
   ylab("TOTAL DAMAGE (USD BILLIONS)")
