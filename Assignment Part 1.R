setwd("C:/Users/User/Desktop/Foundation IT/Data Mining")
shark_atk <- read.csv("attacks.csv",stringsAsFactors = FALSE)
dim(shark_atk)
test<-shark_atk

#Remove Age column
test$Age <- NULL

#Remove Name column
test$Name<-NULL

#Remove Case.Number column
test$Case.Number <-NULL

#Remove Area column
test$Area<-NULL

#Remove Location column
test$Location<-NULL

#Remove Species column
test$Species<-NULL

#Remove Investigator.or.Source column
test$Investigator.or.Source<-NULL

#Rhange column name Sex to Gender
colnames(test)[6]<-"Gender"

#Remove rows before 1951
test<-test[test$Year > 1950,]

#Cast Year as Integer class
test$Year<-as.integer(test$Year) 

#Change 'Invalid' Type into NA
test$Type[test$Type == 'Invalid']<-NA
library(lubridate)
test$Date<-format(dmy(test$Date), format = "%B")

#then change the column name to month
colnames(test)[1]<-"Month"

#Remove unconfirmed and unwanted cases records
test<-test[!grepl("Shark involvement",test$Injury,ignore.case=TRUE),]
test<-test[!grepl("unknown",test$Fatal..Y.N.,ignore.case=TRUE),]

#Fix inconsistent data
test$Injury[grepl("no injury",test$Injury,ignore.case = TRUE)]<-"No Injury"
test$Injury[grepl("not injured",test$Injury,ignore.case = TRUE)]<-"No Injury"
test$Injury[grepl("no inury",test$Injury,ignore.case = TRUE)]<-"No Injury"
test$Injury[grepl("no inujury",test$Injury,ignore.case = TRUE)]<-"No Injury"
test$Injury[grepl("no ijnury",test$Injury,ignore.case = TRUE)]<-"No Injury"
test$Injury[grepl("unhurt",test$Injury,ignore.case = TRUE)]<-"No Injury"
test$Injury[grepl("FATAL",test$Injury,ignore.case = TRUE)]<-"Fatal"
test$Injury[grepl("Y",test$Fatal..Y.N.,ignore.case = TRUE)]<-"Fatal"
test$Injury[!(grepl("Fatal",test$Injury,ignore.case = TRUE) | grepl("No Injury",test$Injury,ignore.case = TRUE))]<-"Injured"

test$Type[grepl("Boat",test$Type,ignore.case = TRUE)]<-"Boating"
test$Gender[!(grepl("M",test$Gender,ignore.case = TRUE) | grepl("F",test$Gender,ignore.case = TRUE))]<-NA

#Remove Fatal..Y.N. column 
test$Fatal..Y.N.<-NULL

#Categorize messy data in Activity column
activityCategories <- c("Diving","Bathing","Adrifting","Air/Sea Disaster",
                        "Jumping","Fishing","Boating","Canoeing","Racing",
                        "Filming","Swimming","Surfing","Kayaking","Murdered",
                        "Wading","Shipwreck","Playing","Standing","Washing",
                        "Sitting","Attempting to approach sharks","Others")

test$Activity[grepl("dive",test$Activity,ignore.case = TRUE) | 
              grepl("diving",test$Activity,ignore.case = TRUE) |
              grepl("dive",test$Activity,ignore.case = TRUE) |
              grepl("dive",test$Activity,ignore.case = TRUE) |
              grepl("snorkel",test$Activity,ignore.case = TRUE)
              ] <-activityCategories[1]

test$Activity[grepl("bath",test$Activity,ignore.case = TRUE) |
              grepl("batin",test$Activity,ignore.case = TRUE)
              ] <-activityCategories[2]

test$Activity[grepl("adrift",test$Activity,ignore.case = TRUE)] <-activityCategories[3]
test$Activity[grepl("disaster",test$Activity,ignore.case = TRUE)] <-activityCategories[4]
test$Activity[grepl("jump",test$Activity,ignore.case = TRUE)]<-activityCategories[5]

test$Activity[grepl("fish",test$Activity,ignore.case = TRUE) | 
                grepl("net",test$Activity,ignore.case = TRUE) |
                grepl("chum",test$Activity,ignore.case = TRUE) |
                grepl("clam",test$Activity,ignore.case = TRUE) |
                grepl("catch",test$Activity,ignore.case = TRUE) |
                grepl("crab",test$Activity,ignore.case = TRUE) |
                grepl("shrimp",test$Activity,ignore.case = TRUE) |
                grepl("hunt",test$Activity,ignore.case = TRUE)
              ]<-activityCategories[6]

test$Activity[grepl("boat",test$Activity,ignore.case = TRUE) | 
                grepl("overboard",test$Activity,ignore.case = TRUE) |
                grepl("jet",test$Activity,ignore.case = TRUE) |
                grepl("sail",test$Activity,ignore.case = TRUE) |
                grepl("cruising",test$Activity,ignore.case = TRUE)
              ]<-activityCategories[7]

test$Activity[grepl("canoe",test$Activity,ignore.case = TRUE)] <-activityCategories[8]

test$Activity[grepl("race",test$Activity,ignore.case = TRUE) | 
                grepl("compete",test$Activity,ignore.case = TRUE) |
                grepl("competing",test$Activity,ignore.case = TRUE)
              ]<-activityCategories[9]

test$Activity[grepl("film",test$Activity,ignore.case = TRUE)] <-activityCategories[10]

test$Activity[grepl("swim",test$Activity,ignore.case = TRUE) | 
                grepl("float",test$Activity,ignore.case = TRUE) |
                grepl("tread",test$Activity,ignore.case = TRUE)
              ]<-activityCategories[11]


test$Activity[grepl("surf",test$Activity,ignore.case = TRUE) | 
                grepl("boarding",test$Activity,ignore.case = TRUE) |
                grepl("board",test$Activity,ignore.case = TRUE) |
                grepl("ski",test$Activity,ignore.case = TRUE)
              ]<-activityCategories[12]

test$Activity[grepl("kayak",test$Activity,ignore.case = TRUE) |
              grepl("paddle",test$Activity,ignore.case = TRUE) |
              grepl("paddling",test$Activity,ignore.case = TRUE)|
              grepl("rowing",test$Activity,ignore.case = TRUE) 
              ]<-activityCategories[13]

test$Activity[grepl("murder",test$Activity,ignore.case = TRUE)]<-activityCategories[14]

test$Activity[grepl("wading",test$Activity,ignore.case = TRUE) | 
                grepl("walk",test$Activity,ignore.case = TRUE) 
              ]<-activityCategories[15]

test$Activity[grepl("shipwreck",test$Activity,ignore.case = TRUE) | 
                grepl("sinking",test$Activity,ignore.case = TRUE) |
                grepl("wreck",test$Activity,ignore.case = TRUE) |
                grepl("aircraft",test$Activity,ignore.case = TRUE)
              ]<-activityCategories[16]

test$Activity[grepl("play",test$Activity,ignore.case = TRUE) |
              grepl("splash",test$Activity,ignore.case = TRUE)
              ]<-activityCategories[17]

test$Activity[grepl("stand",test$Activity,ignore.case = TRUE) |
              grepl("stamd",test$Activity,ignore.case = TRUE)
              ]<-activityCategories[18]

test$Activity[grepl("wash",test$Activity,ignore.case = TRUE)]<-activityCategories[19]
test$Activity[grepl("sit",test$Activity,ignore.case = TRUE)]<-activityCategories[20]
test$Activity[grepl("shark",test$Activity,ignore.case = TRUE)]<-activityCategories[21]
test$Activity[!(test$Activity %in% activityCategories) & test$Activity!=""]<-activityCategories[22]

#Fix messy data in Time column
getmode <- function(v) 
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
testmorning <- testmorning[(testmorning >= 2000 & testtime <= 2359) | testtime <= 0559]
testTime <- test$Time

testmorning <- gsub("h","",testTime)
testmorning <- as.numeric(testmorning)
testmorning <- na.omit(testmorning)
testmorning <- testmorning[(testmorning >= 0600 & testmorning <= 1159)]
test$Time[grepl("morning",test$Time,ignore.case = T)] <- getmode(testmorning)

testafternoon <- gsub("h","",testTime)
testafternoon <- as.numeric(testafternoon)
testafternoon <- na.omit(testafternoon)
testafternoon <- testafternoon[(testafternoon >= 1200 & testafternoon <= 1659)]
test$Time[grepl("afternoon",test$Time,ignore.case = T)] <- getmode(testafternoon)

testevening <- gsub("h","",testTime)
testevening <- as.numeric(testevening)
testevening <- na.omit(testevening)
testevening <- testevening[(testevening >= 1700 & testevening <= 1959)]
test$Time[grepl("evening",test$Time,ignore.case = T)] <- getmode(testevening)

testnight <- gsub("h","",testTime)
testnight <- as.numeric(testnight)
testnight <- na.omit(testnight)
testnight <- testnight[((testnight >= 2000 & testnight <= 2359) | testnight <= 0559)]
test$Time[grepl("night",test$Time,ignore.case = T)] <- getmode(testnight)
test$Time[grepl("midday",test$Time,ignore.case = T)] <- 1200
test$Time[grepl("pm",test$Time,ignore.case = T)] <- 2000
test$Time[grepl("p.m",test$Time,ignore.case = T)] <- 2000
test$Time[grepl("after dark",test$Time,ignore.case = T)] <- 2000
test$Time[grepl("Just before",test$Time,ignore.case = T)] <- 1000
test$Time[grepl("a.m",test$Time,ignore.case = T)] <- 1100
test$Time[grepl("am",test$Time,ignore.case = T)] <- 1100
test$Time[grepl("after dusk",test$Time,ignore.case = T)] <- 2000
test$Time[grepl("after noon",test$Time,ignore.case = T)] <- 1400
test$Time[grepl("after lunch",test$Time,ignore.case = T)] <- 1400
test$Time[grepl("before",test$Time,ignore.case = T)] <- 0630
test$Time[grepl("Between 05h00 and 08h00",test$Time,ignore.case = T)] <- 0700
test$Time[grepl("Between 06h00 & 07h20",test$Time,ignore.case = T)] <- 0645
test$Time[grepl("dark",test$Time,ignore.case = T)] <- 2000
test$Time[grepl("dawn",test$Time,ignore.case = T)] <- 0700
test$Time[grepl("daybreak",test$Time,ignore.case = T)] <- 1100
test$Time[grepl("daytime",test$Time,ignore.case = T)] <- 1400
test$Time[grepl("dusk",test$Time,ignore.case = T)] <- 1900
test$Time[grepl("Just after 12h00",test$Time,ignore.case = T)] <- 1230
test$Time[grepl("Sometime between 06h00 & 08hoo",test$Time,ignore.case = T)] <- 0700
test$Time[grepl("Just before noon",test$Time,ignore.case = T)] <- 1100
test$Time[grepl("Lunchtime",test$Time,ignore.case = T)] <- 1230
test$Time[grepl("Just before sundown",test$Time,ignore.case = T)] <- 1800
test$Time[grepl("noon",test$Time,ignore.case = T)] <- 1200
test$Time[grepl("Prior to 10h37",test$Time,ignore.case = T)] <- 1037
test$Time[grepl("Shortly before 13h00",test$Time,ignore.case = T)] <- 1245
test$Time[grepl("Sunset",test$Time,ignore.case = T)] <- 1900
test$Time[grepl("30 minutes after 1992.07.08.a",test$Time,ignore.case = T)] <- getmode(testmorning)
test$Time[grepl("2 hours after Opperman",test$Time,ignore.case = T)] <- getmode(testmorning)

test$Time[test$Time=="  "] <- NA
test$Time[test$Time=="--"] <- NA
test$Time[test$Time=="X"] <- NA

test$Time <- gsub("\\s\\-.*","",test$Time)
test$Time <- gsub("\\-.*","",test$Time)
test$Time <- gsub("\\s\\/.*","",test$Time)
test$Time <- gsub("\\sor.*","",test$Time)
test$Time <- gsub("\\sto.*","",test$Time)
test$Time <- gsub("\\?.*","",test$Time)
test$Time <- gsub("\\s+","",test$Time)
test$Time[test$Time==""] <-NA

test$Time <- gsub("h","",test$Time)
test$Time <- gsub("j","",test$Time)
test$Time <- gsub("<","",test$Time)
test$Time <- gsub(">","",test$Time)
test$Time[grepl("11115",test$Time,ignore.case = T)] <- 1115 
test$Time[grepl("13345",test$Time,ignore.case = T)] <- 1335


###Exploratory Analysis###

#1-Shark attacks over years
plot(test$Year, type="o", col="blue")

#2-By injury type
injury <- test[order(test$Injury,decreasing = TRUE),]
injury.count = count(test, 'Activity')
barplot(injury.count, las=2, horiz=TRUE, main="Global Shark Attacks by Types of Injury", 
        xlab="Injury Type",names.arg=c("Fatal", "Injured", "No Injury"))

#3-Top 10 location where shark attacks occurred 
library(RColorBrewer)
library(maptools)
library(plyr)
library(sqldf)
country = count(test, 'Country')
country <- country[with(country, order(-freq)),]
country <- head(country,10)

pal <- colorRampPalette(brewer.pal(9, 'Reds'))(length(country$freq))
pal <- pal[with(country, findInterval(freq, sort(unique(freq))))]

col <- rep(grey(0.8), length(wrld_simpl@data$NAME))
col[match(ddf$Country, wrld_simpl@data$NAME)] <- pal

plot(wrld_simpl, col = col)

#4-Top 10 triggering activities
library(ggplot2)
activity = count(test, 'Activity')
activity <- activity[with(activity, order(-freq)),]
activity <- head(activity,10)
activity.count <- ggplot(data.frame(activity),aes(x=activity))
barplot(activity, main="Global Shark Attacks by Types of Injury", 
        xlab="Injury Type")
barplot(summary(activity$Activity))

#to be tested
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)
points(airports$lon, airports$lat, col = "red", cex = .6)

library(rworldmap)
library(mapdata)
library(maps)



write.csv(test,"test.csv")
















