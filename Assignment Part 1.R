#Set working directory
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
test$Activity[test$Activity==""]<-NA

#Remove missing activity records
test<-test[!is.na(test$Activity),]

#Fix messy data in Time column
getmode <- function(v) 
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
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

#Impute mode to NA values in Month
library(Hmisc)
test$Month <- with(test, impute(Month, getmode(test$Month)))

#Impute mode to NA values in Type
test$Type[which(is.na(test$Type))] <- getmode(test$Type)

#Impute mode to NA values in Gender
test$Gender[which(is.na(test$Gender))] <- getmode(test$Gender)

#country 
test$Country[which(nchar(test$Country)<=1)] <-NA # we got 12 empty Country
test$Country[grepl("\\?", test$Country)] <- NA # and 1 country with "?"

#All these are to be removed
test<-test[!is.na(test$Country),]
o <- order(test$Country)

#function to impute time, improve accuracy by ordering it with Country
seqImpute <- function(x)
{
  last=getmode(testafternoon)
  n <- length(x)
  x <- c(x, last)
  i <- is.na(x)
  while (any(i))
  {
    x[i] <- x[which(i)+1]
    i <- is.na(x)
  }
  x[1:n]
}

test$Time <-seqImpute(test$Time[o])

###Exploratory Analysis###
library(plyr)
#1-Shark attacks over years
year = count(test, 'Year')
yeargraph <- ggplot(data=year,aes(x=Year, y= freq)) +
  geom_point() +
  ggtitle("Shark Attack over the Years") +
  scale_x_continuous(breaks=seq(1950, 2016, 5)) +
  labs(x="Frequency",y="Years") + theme_dark()
yeargraph

#2-By injury type
injury = count(test, 'Injury')
injury <- injury[with(injury, order(-freq)),]

injury.graph<-ggplot(data=injury, aes(x=reorder(Injury,freq), y=freq, fill=Injury)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), hjust=1.2, size=4) +
  ggtitle("Type of Injury")
injury.graph + coord_flip()

#3-Top 10 location where shark attacks occurred 
library(RColorBrewer)
install.packages("maptools")
library(maptools)

data("wrld_simpl")
country = count(test, 'Country')
country <- country[with(country, order(-freq)),]
country <- head(country,10)
country

#Convert country name to lowercase 
lowerCase <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

country$Country <- sapply(country$Country, lowerCase)
country$Country[grepl("USA",country$Country,ignore.case = TRUE)]<-"United States"

#Plot the map
pal <- colorRampPalette(brewer.pal(9, 'Reds'))(length(country$freq))
pal <- pal[with(country, findInterval(freq, sort(unique(freq))))]

col <- rep(grey(0.8), sum(length(wrld_simpl@data$NAME)))
col[match(country$Country, wrld_simpl@data$NAME)] <- pal

plot(wrld_simpl, col = col, main = "Global Shark Attacks Location")

#4 top 10 triggering activities
library(ggplot2)
activity = count(test, 'Activity')
activity <- activity[with(activity, order(-freq)),]
activity <- head(activity,5)

act.graph <-ggplot(data=activity, aes(x=reorder(Activity,freq), y=freq, fill=Activity)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), hjust=1.2, size=4) +
  ggtitle("Top 10 Triggering Activities") +
  labs(x="Activities",y="Frequency")
act.graph + coord_flip()

#5 Time of shark attacks occurred in a day
time = count(test, 'Time')
time$Time <- as.integer(time$Time)
time.graph <-ggplot(data=time, aes(x=Time, y=freq)) +
  geom_line() +
  scale_x_continuous(breaks=seq(0, 2400, 200)) +
  ggtitle("Time of Shark Attacks in a Day") +
  labs(x="Time",y="Frequency")
time.graph

str(test) 

write.csv(test,"attack_final.csv")










