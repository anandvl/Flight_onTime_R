# **********************************************
# Import libraries for reshaping and plotting and setting work folder
# **********************************************
library(ggplot2)
library(plyr)
library(stringr)
rm(list=ls()) # Clear all existing variables 
# **********************************************
# Define functions to use
# **********************************************
#  Global Inputs
# **********************************************
dataDir <- 'data'
yyyymmOfInt <- '201801'
flightFile <- 'Flight_onTime'
airpFile <- 'Airport_locations'
airlFile <- 'Carriers'
flightCols <- c('Day', 'Date', 'CAR', 'FLNum', 'ORIGIN_ID', 'DEST_ID',  
                'DEP_SCH', 'DEP_DEL', 'DEP_TAXI', 
                'ARR_TAXI', 'ARR_SCH', 'ARR_DEL', 'CANCEL')
#
flightFile <- paste(flightFile, yyyymmOfInt, sep='_')
yearOfInt <- substring(yyyymmOfInt, 1, 4)
yyyy_mm_OfInt <- paste(substring(yyyymmOfInt,1,4), substring(yyyymmOfInt,5,6), sep='-')
# **********************************************
#  Read in the 'onTime' data file, remove columns with NA values, and rearrange
# **********************************************
allData <- read.csv(paste(dataDir,paste(flightFile, 'csv', sep='.'),sep="/"), stringsAsFactors=FALSE, fill=FALSE)
colnames(allData) <- flightCols
cleanData <- droplevels(subset(allData, !is.na(DEP_DEL) & !is.na(ARR_DEL) &
                                 !is.na(DEP_TAXI) & !is.na(ARR_TAXI)))
#
dep_del <- droplevels(subset(cleanData, 
                             select=c('Day', 'Date', 'CAR', 'ORIGIN_ID', 'DEP_DEL')))
colnames(dep_del) <- c('Day', 'Date', 'CAR', 'AIRPORT_ID', 'DURATION')
dep_del$TYPE = 'DEP_DEL'
#
arr_del <- droplevels(subset(cleanData, 
                             select=c('Day', 'Date', 'CAR', 'DEST_ID', 'ARR_DEL')))
colnames(arr_del) <- c('Day', 'Date', 'CAR', 'AIRPORT_ID', 'DURATION')
arr_del$TYPE = 'ARR_DEL'
#
dep_taxi <- droplevels(subset(cleanData, 
                              select=c('Day', 'Date', 'CAR', 'ORIGIN_ID', 'DEP_TAXI')))
colnames(dep_taxi) <- c('Day', 'Date', 'CAR', 'AIRPORT_ID', 'DURATION')
dep_taxi$TYPE = 'DEP_TAXI'
#
arr_taxi <- droplevels(subset(cleanData, 
                              select=c('Day', 'Date', 'CAR', 'DEST_ID', 'ARR_TAXI')))
colnames(arr_taxi) <- c('Day', 'Date', 'CAR', 'AIRPORT_ID', 'DURATION')
arr_taxi$TYPE = 'ARR_TAXI'
#
rearrData <- rbind(dep_del, arr_del, dep_taxi, arr_taxi)
rm(list=c('allData', 'cleanData', 'dep_del', 'arr_del', 'dep_taxi', 'arr_taxi'))
# **********************************************
#  Read in the airport and airline info file
# **********************************************
airports <- read.csv(paste(dataDir,paste(airpFile, 'csv', sep='.'),sep="/"), stringsAsFactors=FALSE, fill=FALSE)
airports <- droplevels(subset(airports, select=c('AIRPORT_SEQ_ID', 
                                                 'DISPLAY_AIRPORT_NAME',
                                                 'DISPLAY_AIRPORT_CITY_NAME_FULL',
                                                 'LONGITUDE', 'LATITUDE')))
colnames(airports) <- c('AIRPORT_ID', 'NAME', 'AIRPORT', 'LON', 'LAT')
airports$AIRPORT <- paste(airports$AIRPORT, 
                          #abbreviate(airports$NAME, length(unlist(strsplit(airports$NAME, ' ')))), 
                          airports$NAME, 
                          sep = '; ')
airports$NAME <- NULL
airports[grepl('Denver', airports$AIRPORT, ignore.case=TRUE), ]
#
airlines <- read.csv(paste(dataDir,paste(airlFile, 'csv', sep='.'),sep="/"), stringsAsFactors=FALSE, fill=FALSE)
colnames(airlines) <- c('CAR', 'Airline')
# **********************************************
#  Compute stats on rearrData
# **********************************************
#statsD <- ddply(rearrData, c('Date', 'CAR', 'AIRPORT_ID', 'TYPE'), summarize,
#                  MEAN = mean(DURATION), STD = sd(DURATION),
#                  MIN = min(DURATION), MAX = max(DURATION),
#                  count = length(DURATION), MED = median(DURATION)
#q25 = quantile(DURATION, 0.25), q75 = quantile(DURATION, 0.75), 
#q10 = quantile(DURATION, 0.10), q90 = quantile(DURATION, 0.90)
#)
statsM <- ddply(rearrData, c('CAR', 'AIRPORT_ID', 'TYPE'), summarize,
                MEAN = mean(DURATION), STD = sd(DURATION),
                MIN = min(DURATION), MAX = max(DURATION),
                count = length(DURATION), MED = median(DURATION)
                #q25 = quantile(DURATION, 0.25), q75 = quantile(DURATION, 0.75), 
                #q10 = quantile(DURATION, 0.10), q90 = quantile(DURATION, 0.90)
)
#statsM$Date <- yyyy_mm_OfInt
#statsAirpD <- ddply(rearrData, c('Date', 'AIRPORT_ID', 'TYPE'), summarize,
#                  MEAN = mean(DURATION), STD = sd(DURATION),
#                  MIN = min(DURATION), MAX = max(DURATION),
#                  count = length(DURATION), MED = median(DURATION)
#q25 = quantile(DURATION, 0.25), q75 = quantile(DURATION, 0.75), 
#q10 = quantile(DURATION, 0.10), q90 = quantile(DURATION, 0.90)
#)
#statsAirpD$CAR <- 'All'
statsAirpM <- ddply(rearrData, c('AIRPORT_ID', 'TYPE'), summarize,
                    MEAN = mean(DURATION), STD = sd(DURATION),
                    MIN = min(DURATION), MAX = max(DURATION),
                    count = length(DURATION), MED = median(DURATION),
                    q25 = quantile(DURATION, 0.25), q75 = quantile(DURATION, 0.75), 
                    q10 = quantile(DURATION, 0.10), q95 = quantile(DURATION, 0.95)
)
statsAirpM$Date <- yyyy_mm_OfInt
statsAirpM$CAR <- 'All'
#
#statsAll <- rbind(statsD, statsM[colnames(statsD)], 
#                  statsAirpD[colnames(statsD)], statsAirpM[colnames(statsD)])
#
statsAirpM <- merge(statsAirpM, airports, by=c('AIRPORT_ID'))
#write(flightFile, file='tmp.txt')
# #######################################################################
# Define server logic 
# #######################################################################
shinyServer(
  function(input, output) {
    AirpList <- levels(as.factor(statsAirpM$AIRPORT))    
    
    Airport_value <- reactiveValues(Airport = 'Denver, CO; Denver International')	
    
    observeEvent(input$INP_AIRPORT, {
      Airport_value$Airport <- input$INP_AIRPORT
    })    
    
    output$Chose_Airport <- renderUI({
      selectInput("INP_AIRPORT", label = "Select Airport", width=600,
                  choices = setNames(AirpList, AirpList), selectize = TRUE,
                  selected = AirpList[20])      
    }
    )
    
    # Later will investigate 'selectize' to use JS to search throug the
    # as the user types.
    
    output$distOut <- renderPlot({
      if (is.null(Airport_value$Airport)) return()
      
      Airport <- Airport_value$Airport
      airpID <- unique(statsAirpM[statsAirpM$AIRPORT==Airport,]$AIRPORT_ID)
      #write(Airport, file='tmp.txt')
      #
      statsM1 <- droplevels(subset(statsM, AIRPORT_ID==airpID))
      statsM1summ <- ddply(statsM1, c('TYPE'), summarize, 
                           lowerlimit = round(0.005*sum(count), 0))
      statsM1 <- merge(statsM1, statsM1summ, by=c('TYPE'))
      statsM1 <- droplevels(subset(statsM1, count > lowerlimit))
      statsM1 <- ddply(statsM1, c('CAR'), summarize, noFlights = round(mean(count),0))
      statsM1 <- merge(statsM1, airlines, by=c('CAR'))
      statsM1$Airl <- paste(as.data.frame(str_split_fixed(statsM1$Airline, ' ', Inf))$V1, 
                            '(', statsM1$noFlights, ')', sep='')
      statsM1 <- statsM1[with(statsM1, order(-noFlights)),]
      Airl <- unique(statsM1$CAR)
      #
      statsAirpM1 <- droplevels(subset(statsAirpM, AIRPORT_ID==airpID))
      ymin <- 30*floor(min(statsAirpM1$q10)/30)
      ymax <- 30*ceiling(max(statsAirpM1$q95/30))
      #
      rearrData1 <- droplevels(subset(rearrData, AIRPORT_ID==airpID & CAR %in% Airl))
      rearrData1 <- merge(rearrData1, statsM1, by=c('CAR'))
      #statsAll1 <- droplevels(subset(statsAll, AIRPORT_ID==airpID & CAR %in% c(Airl, 'All')))
      # **********************************************
      #  Plots:
      #  Box plots vs airlines of all 4 'TYPE's vs airlines
      # **********************************************
      plt <- ggplot(rearrData1, aes(x=Airl, y=DURATION, fill=TYPE)) +
        geom_boxplot() +
        theme_bw(base_size=24) +
        theme(axis.text.x = element_text(size=14),
              axis.text.y = element_text(size=14),
              legend.position="top") +    
        scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, 30)) +
        scale_x_discrete(limits=statsM1$Airl) +
        xlab('Airlines') +
        ylab('Duration (mins)') +
        ggtitle(paste('Stats for', Airport, 'for 2018-01', sep=' '))
      
      print(plt)
            
    }
    )
        
  }
)

