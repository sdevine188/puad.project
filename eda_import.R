## Set working directory to PUAD Project
 
## manually enter which year dataset to import
year <- 2012

## turn off scientific notation and call stringr library
options(scipen=999)
library(stringr)

## import data, mark blanks to NA, and set column names
setwd("eda")
files <- list.files()
file.name <- grep(year, files, value = TRUE)
file.name
eda <- read.csv(filename, na.strings = c("", NA))
names(eda) <- c("state", "project", "grantee", "location", "program", "funds")
eda <- eda[ , 1:6]

## remove blank rows
eda[rowSums(is.na(eda)) == ncol(eda), ]
eda <- eda[rowSums(is.na(eda)) != ncol(eda), ]

## create function to copy states in state column for eda dataset
copy.state <- function(x) {
        state.name <- x$state[1] 
        for(i in 1:length(x$state)) {
                if(is.na(x$state[i])) {
                        x$state[i] <- state.name
                } else {                      
                        if(str_trim(x$state[i], side = "both") != "Total" & str_trim(x$state[i], side = "both") != "State") {
                                state.name <- x$state[i]
                        }                                           
                } 
        }
        return(x)
}

## run copy.state function
eda <- copy.state(eda)

## remove rows with Totals
eda[eda$state == "Total" | eda$state == "U.S. Total" | eda$state == "Grand Total", ]
eda <- eda[eda$state != "Total" & eda$state != "U.S. Total" & eda$state != "Grand Total", ]

## remove rows with page headers
eda[eda$grantee == "Grantee", ]
eda <- eda[eda$grantee != "Grantee", ]

## change text fields to lower case
eda$state <- tolower(eda$state)
eda$grantee <- tolower(eda$grantee)
eda$location <- tolower(eda$location)
eda$program <- tolower(eda$program)

## trim strings on states
eda$state <- str_trim(eda$state, side = "both")

## remove rows where Location = "multi city", since there is no granular detail about which county received grant
## may lose eda observations
eda[eda$location == "multi city", ]
eda <- eda[eda$location != "multi city", ]

## limit dataset to public works and econ adjust assistance programs
eda[eda$program == "public works" | eda$program == "econ edjust implementation" | 
            eda$program == "econ adjust strategy", ]
eda <- eda[eda$program == "public works" | eda$program == "econ edjust implementation" | 
                           eda$program == "econ adjust strategy", ]

## convert funds column into numeric
eda$funds <- sub("$", "", eda$funds, fixed = TRUE)
eda$funds <- gsub(",", "", eda$funds)
eda$funds <- as.numeric(eda$funds)

## import counties dataset
## import 1841 US counties from ppc campaign generator website (once cleaned in R)
## website is  http://ppccampaigngenerator.com/wp-content/uploads/2012/09/List-of-Cities-States-and-Counties.xlsx
setwd("../")
setwd("counties")
counties <- read.csv("counties.csv")
counties <- counties[counties$State != "PR" & counties$State != "GU" & counties$State != "AS" & counties$State 
                     != "MH" & counties$State != "VI" & counties$State != "AE" & counties$State != "AA" & 
                             counties$State != "AP" & counties$State != "PW" & counties$State != "FM" & 
                             counties$State != "MP", ]
counties$merged.city.state <- paste(str_trim(tolower(counties$City), side = "both"), str_trim(counties
                              $State, side = "both"), sep = " ")
dup <- duplicated(counties$merged.city.state)
dup.index <- which(dup == TRUE)
counties <- counties[-dup.index, ]


## count counties
##counties$merged.city.state <- paste(counties$State, counties$County)
##length(unique(counties$merged.city.state))


## add counties to eda dataframe
## may lose eda observations for PR, VI, or other non-US states
setwd("../")
setwd("state_abbreviations")
abb <- read.csv("state_abbreviations.csv")
names(abb) <- c("state", "abb")
abb$state <- tolower(abb$state)
eda.x <- merge(eda, abb, by = "state")

eda.x$merged.city.state <- paste(str_trim(eda.x$location, side = "both"), str_trim(eda.x$abb, side = 
                        "both"), sep = " ")

eda.x$county <- 0
find.county <- function(x) {
        for(i in 1:length(x$merged.city.state)) {
                match.index <- which(counties$merged.city.state == x$merged.city.state[i])
                if(length(match.index == 1)) {
                        x$county[i] <- tolower(as.character(counties$County[match.index]))
                } else {
                        if(length(match.index > 1)) {
                                print("error: more than one match found for eda$merged.city.state below")
                                print(x$merged.city.state[i])
                        } else {
                                print("error: zero matches found for eda$merged.city.state below")
                                print(x$merged.city.state[i])
                        }
                }
        }
        return(x)
}

eda.y <- find.county(eda.x)

## manually add any counties for city/county combos not found in counties

## 2013
##index <- which(eda.y$merged.city.state == "winston-salem NC")
##eda.y$county[index] <- "forsyth" 

##index <- which(eda.y$merged.city.state == "bayview TX")
##eda.y$county[index] <- "cameron" 

## 2012
## college park, GA is multi-county (fulton and clayton)

index <- which(eda.y$merged.city.state == "dover?foxcroft ME")
eda.y$county[index] <- "piscataquis" 

index <- which(eda.y$merged.city.state == "st. joseph MO")
eda.y$county[index] <- "buchanan" 

index <- which(eda.y$merged.city.state == "midwest city OK")
eda.y$county[index] <- "oklahoma" 

## create merged.county.state variable
eda.y$merged.county.state <- paste(eda.y$county, eda.y$abb, sep = " ")

## add fips codes for state and county
## fips includes AS, GU, MP, PR, UM, and VI
setwd("../")
setwd("fips.codes")
fips <- read.csv("fips.codes.txt")
fips <- fips[ , -5]

## function to create fips$county and then ultimately fips3$merged.county.state
split.counties <- function(x) {
        x$county <- 0
        for(i in 1:length(x$County.Name)) {
                county.name <- sub(" County", "", x$County.Name[i])
                x$county[i] <- tolower(county.name)
        }
        return(x)
}

fips2 <- split.counties(fips)

fips3 <- fips2
fips3$merged.county.state <- paste(fips3$county, fips3$State, sep = " ")
fips3$merged.county.state <- gsub("parish ", "", fips3$merged.county.state)
fips3$fips.state.county <- paste(fips3$State.ANSI, fips3$County.ANSI, sep = ".")

## remove fips3 rows for non-US states
non.states <- which(fips3$State == "AS" | fips3$State == "GU" | fips3$State == "MP" | fips3$State == "PR" |
                            fips3$State == "UM" | fips3$State == "VI")
fips3 <- fips3[-non.states, ]

## merge fips with eda using merged.county.state as the id
eda.z <- merge(eda.y, fips3, by = "merged.county.state")
names(eda.z)[names(eda.z) == "State.ANSI"] <- "state.fips"
names(eda.z)[names(eda.z) == "County.ANSI"] <- "county.fips"
names.eda <- names(eda.z)
for(i in 1:length(names.eda)){
        new.name <- paste("eda.", names.eda[i], sep = "")
        names(eda.z)[i] <- new.name
}

## import bls data for given year
## raw bls files are pre-cleaned by filtering for "All Covered" in Ownership column K, 
## un-hiding columns A-E, copy/pasting columns A-R to a .csv, 
## and multiplying state and county columns (B & C) by 1 to remove leading zeroes
## raw data is in bls folder for reference
setwd("../")
setwd("bls")

files <- list.files()
file.name <- grep(year, files, value = TRUE)
file.name
bls <- read.csv(file.name)

## clean data by removing excess columns and naming columns
bls.1 <- bls[ , -1]
rownames(bls.1) <- NULL
bls.1 <- bls.1[ , -c(3:6)]
bls.1 <- bls.1[ , -c(6:8)]
bls.1 <- bls.1[ , -c(9:12)]
names(bls.1) <- c("bls.state.fips", "bls.county.fips", "bls.type", "bls.state", "bls.area", "bls.estab", "bls.emp", 
                  "bls.wages")

## remove national row, State total rows, and county = 999 rows, leaving only county rows
bls.1 <- bls.1[-1, ]
bls.1 <- subset(bls.1, bls.1$bls.type == "County")
bls.1 <- subset(bls.1, bls.1$bls.county.fips != 999)

## create fips.state.county variable for merging with eda dataset
bls.1$fips.state.county <- paste(bls.1$bls.state.fips, bls.1$bls.county.fips, sep = ".")

## create bls.county variable for reference check when merging with eda dataset
bls.2 <- bls.1
bls.2$bls.county <- 0
for(i in 1:length(bls.2$bls.area)){
        splits <- strsplit(as.character(bls.2$bls.area[i]), ",")
        bls.2$bls.county[i] <- splits[[1]][1]
}

## convert bls factors into numerics, where appropriate
bls.2$bls.estab <- gsub(",", "", bls.2$bls.estab)
bls.2$bls.estab <- as.numeric(bls.2$bls.estab)
 
bls.2$bls.emp <- gsub(",", "", bls.2$bls.emp)
bls.2$bls.emp <- as.numeric(bls.2$bls.emp)

bls.2$bls.wages <- gsub(",", "", bls.2$bls.wages)
bls.2$bls.wages <- as.numeric(bls.2$bls.wages)

bls.national$bls.wages <- gsub(",", "", bls.national$bls.wages)
bls.national$bls.wages <- as.numeric(bls.national$bls.wages)

## merge bls with eda
eda.1 <- merge(eda.z, bls.2, by = "fips.state.county")

## convert factor columns to character to avoid coerced NA's during rbind
eda.1$eda.County.Name <- as.character(eda.1$eda.County.Name)
eda.1$eda.abb <- as.character(eda.1$eda.abb)
eda.1$eda.project <- as.character(eda.1$eda.project)
eda.1$eda.State <- as.character(eda.1$eda.State)

## rbind unmatched counties beneath eda.1
## to do this, need to sync up column names between eda.1 and bls.2
bls.3 <- bls.2

names.eda <- names(eda.1)
names.bls <- names(bls.3)
eda.only.names <- names.eda[which(!(names.eda %in% names.bls))]

for(i in 1:length(eda.only.names)) {
        col.number <- ncol(bls.3)
        new.col.number <- col.number + 1
        bls.3[ , new.col.number] <- 0
        names(bls.3)[new.col.number] <- eda.only.names[i]
}

## test to ensure no dropped counties
num.eda.only.counties <- length(unique(eda.1$fips.state.county))
bls.unused <- which(!(bls.3$fips.state.county %in% eda.1$fips.state.county))
bls.non.eda <- bls.3[bls.unused, ]
num.bls.only.counties <- length(unique(bls.non.eda$fips.state.county))
## these two sums should be equal if no counties were dropped
num.bls.only.counties + num.eda.only.counties
length(unique(bls.3$fips.state.county))

eda.1 <- eda.1[ , order(names(eda.1))]
bls.non.eda <- bls.non.eda[ , order(names(bls.non.eda))]

eda.2 <- rbind(eda.1, bls.non.eda)

## check to see no lost rows
nrow(eda.2)
nrow(eda.1) + nrow(bls.non.eda)

## add empty columns for multipe grants to same county

eda.3 <- eda.2

eda.3$eda.grantee2 <- 0
eda.3$eda.location2 <- 0
eda.3$eda.funds2 <- 0
eda.3$eda.program2 <- 0
eda.3$eda.project2 <- 0

eda.3$eda.grantee3 <- 0
eda.3$eda.location3 <- 0
eda.3$eda.funds3 <- 0
eda.3$eda.program3 <- 0
eda.3$eda.project3 <- 0

multiple.grants <- function(x){
        dups.index <- duplicated(eda.3$fips.state.county) 
        dups <- which(dups.index == TRUE)
        if(length(dups) > 1){
                for(i in 1:length(dups)){
                        dups.row <- dups[i]
                        dups.fips <- eda.3$fips.state.county[dups.row]
                        dups.fips.df <- eda.3[eda.3$fips.state.county == dups.fips, ]
                        all.dups.row <- which(eda.3$fips.state.county == dups.fips)
                        initial.dups.row <- all.dups.row[1]
                        eda.3$eda.grantee2[initial.dups.row] <- dups.fips.df$eda.grantee[2]
                        eda.3$eda.location2[initial.dups.row] <- dups.fips.df$eda.location[2]
                        eda.3$eda.funds2[initial.dups.row] <- dups.fips.df$eda.funds[2]
                        eda.3$eda.program2[initial.dups.row] <- dups.fips.df$eda.program[2]
                        eda.3$eda.project2[initial.dups.row] <- dups.fips.df$eda.project[2]
                        
                        if(length(dups.fips.df$fips.state.county) > 2){
                                eda.3$eda.grantee3[initial.dups.row] <- dups.fips.df$eda.grantee[3]
                                eda.3$eda.location3[initial.dups.row] <- dups.fips.df$eda.location[3]
                                eda.3$eda.funds3[initial.dups.row] <- dups.fips.df$eda.funds[3]
                                eda.3$eda.program3[initial.dups.row] <- dups.fips.df$eda.program[3]
                                eda.3$eda.project3[initial.dups.row] <- dups.fips.df$eda.project[3]
                                
                                if(length(dups.fips.df$fips.state.county) > 3){
                                        print(dups.fips.df$fips.state.county[1])
                                        print("has more than three grants")
                                        ## then manually add extra grant columns, as needed
                                }
                        }
                }        
        }
        return(eda.3)
}

eda.3 <- multiple.grants(eda.3)

## check on mutliple grants
dups.index <- duplicated(eda.3$fips.state.county) 
dups <- which(dups.index == TRUE)
dups
##eda.3[ , ]

## remove duplicate rows
eda.3 <- eda.3[-dups, ]

## create total funds variable
eda.3$eda.total.funds <- eda.3$eda.funds + eda.3$eda.funds2 + eda.3$eda.funds3

eda.4 <- eda.3
## import and clean bea data
## read in bea data

##getwd()

## if needed, set working directory to bea
setwd("../") 
setwd("bea")

## create list all bea files
files <- list.files()

## create first bea file df to serve as template for rbind
file.name <- files[1]
bea <- read.csv(file.name)
bea.1 <- bea
bea.1 <- bea.1[ , -c(3:6)]
bea.1 <- bea.1[ , -c(4:41)]
num.rows <- nrow(bea.1)
del.rows <- c(num.rows, num.rows - 1, num.rows - 2, num.rows - 3)
bea.1 <- bea.1[-del.rows, ]
bea.1$X2007 <- as.character(bea.1$X2007)
bea.1$X2008 <- as.character(bea.1$X2008)
bea.1$X2009 <- as.character(bea.1$X2009)
bea.1$X2010 <- as.character(bea.1$X2010)
bea.1$X2011 <- as.character(bea.1$X2011)
bea.1$X2012 <- as.character(bea.1$X2012)
bea.1$X2013 <- as.character(bea.1$X2013)
bea.df <- bea.1

## rbind all other bea files to bea.df 
for(i in 2:length(files)){
        file.name <- files[i]
        bea <- read.csv(file.name)
        bea.1 <- bea
        bea.1 <- bea.1[ , -c(3:6)]
        bea.1 <- bea.1[ , -c(4:41)]
        num.rows <- nrow(bea.1)
        del.rows <- c(num.rows, num.rows - 1, num.rows - 2, num.rows - 3)
        bea.1 <- bea.1[-del.rows, ]
        bea.df <- rbind(bea.df, bea.1)
}

## remove rownames
rownames(bea.df) <- NULL

## split geofips to create fips.state.county

## initial split of GeoFIPS
for(i in 1:length(bea.df$GeoFIPS)){
        splits <- strsplit(as.character(bea.df$GeoFIPS[i]), "")
        if(nchar(as.character(bea.df$GeoFIPS[i])) < 5){
                state.fips <- splits[[1]][1]
                bea.df$state.fips[i] <- state.fips
                county.fips <- paste(splits[[1]][2], splits[[1]][3], splits[[1]][4], sep = "")
                bea.df$county.fips[i] <- county.fips
        }
        if(nchar(as.character(bea.df$GeoFIPS[i])) > 4){
                state.fips <- paste(splits[[1]][1], splits[[1]][2], sep = "")
                bea.df$state.fips[i] <- state.fips
                county.fips <- paste(splits[[1]][3], splits[[1]][4], splits[[1]][5], sep = "")
                bea.df$county.fips[i] <- county.fips
        }
}

## remove county = 000 rows, and convert character strings to numeric
bea.2 <- bea.df
rownames(bea.2) <- NULL

bea.2 <- subset(bea.df, bea.df$county.fips != "000")

bea.2$state.fips <- as.numeric(bea.2$state.fips)
bea.2$county.fips <- as.numeric(bea.2$county.fips)

## create fips.state.county variable
bea.2$fips.state.county <- paste(as.character(bea.2$state.fips), as.character(bea.2$county.fips),
                                 sep = ".")

## remove rows with NA values year variables
nas <- which(bea.2$X2007 == "(NA)" | bea.2$X2008 == "(NA)" | bea.2$X2009 == "(NA)" | 
                     bea.2$X2010 == "(NA)" | bea.2$X2011 == "(NA)" | bea.2$X2012 == "(NA)" | 
                     bea.2$X2013 == "(NA)")
nas
## to check the rows with (NA)
## bea.2[nas, ]
bea.2 <- bea.2[-nas, ]

## dcast bea.2 by year to rearrange income, population, and per cap income rows as columns
bea.3 <- bea.2
names(bea.3) <- c("bea.geo.fips", "bea.location", "bea.description", "2007", "2008", "2009", "2010",
                  "2011", "2012", "2013", "bea.state.fips", "bea.county.fips", "fips.state.county")

bea.2007 <- bea.3[ , c(1, 2, 3, 4, 11, 12, 13)]
cast.bea.2007 <- dcast(bea.2007, bea.geo.fips + bea.location + bea.state.fips + bea.county.fips + 
                               fips.state.county ~ bea.description, value.var = "2007")
names(cast.bea.2007) <- c("bea.geo.fips", "bea.location", "bea.state.fips", "bea.county.fips",
                          "fips.state.county", "bea.per.cap.inc", "bea.income", "bea.population")
cast.bea.2007$bea.income <- as.numeric(cast.bea.2007$bea.income) * 1000

bea.2008 <- bea.3[ , c(1, 2, 3, 5, 11, 12, 13)]
cast.bea.2008 <- dcast(bea.2008, bea.geo.fips + bea.location + bea.state.fips + bea.county.fips + 
                               fips.state.county ~ bea.description, value.var = "2008")
names(cast.bea.2008) <- c("bea.geo.fips", "bea.location", "bea.state.fips", "bea.county.fips",
                          "fips.state.county", "bea.per.cap.inc", "bea.income", "bea.population")
cast.bea.2008$bea.income <- as.numeric(cast.bea.2008$bea.income) * 1000

bea.2009 <- bea.3[ , c(1, 2, 3, 6, 11, 12, 13)]
cast.bea.2009 <- dcast(bea.2009, bea.geo.fips + bea.location + bea.state.fips + bea.county.fips + 
                               fips.state.county ~ bea.description, value.var = "2009")
names(cast.bea.2009) <- c("bea.geo.fips", "bea.location", "bea.state.fips", "bea.county.fips",
                          "fips.state.county", "bea.per.cap.inc", "bea.income", "bea.population")
cast.bea.2009$bea.income <- as.numeric(cast.bea.2009$bea.income) * 1000

bea.2010 <- bea.3[ , c(1, 2, 3, 7, 11, 12, 13)]
cast.bea.2010 <- dcast(bea.2010, bea.geo.fips + bea.location + bea.state.fips + bea.county.fips + 
                               fips.state.county ~ bea.description, value.var = "2010")
names(cast.bea.2010) <- c("bea.geo.fips", "bea.location", "bea.state.fips", "bea.county.fips",
                          "fips.state.county", "bea.per.cap.inc", "bea.income", "bea.population")
cast.bea.2010$bea.income <- as.numeric(cast.bea.2010$bea.income) * 1000

bea.2011 <- bea.3[ , c(1, 2, 3, 8, 11, 12, 13)]
cast.bea.2011 <- dcast(bea.2011, bea.geo.fips + bea.location + bea.state.fips + bea.county.fips + 
                               fips.state.county ~ bea.description, value.var = "2011")
names(cast.bea.2011) <- c("bea.geo.fips", "bea.location", "bea.state.fips", "bea.county.fips",
                          "fips.state.county", "bea.per.cap.inc", "bea.income", "bea.population")
cast.bea.2011$bea.income <- as.numeric(cast.bea.2011$bea.income) * 1000

bea.2012 <- bea.3[ , c(1, 2, 3, 9, 11, 12, 13)]
cast.bea.2012 <- dcast(bea.2012, bea.geo.fips + bea.location + bea.state.fips + bea.county.fips + 
                               fips.state.county ~ bea.description, value.var = "2012")
names(cast.bea.2012) <- c("bea.geo.fips", "bea.location", "bea.state.fips", "bea.county.fips",
                          "fips.state.county", "bea.per.cap.inc", "bea.income", "bea.population")
cast.bea.2012$bea.income <- as.numeric(cast.bea.2012$bea.income) * 1000

bea.2013 <- bea.3[ , c(1, 2, 3, 10, 11, 12, 13)]
cast.bea.2013 <- dcast(bea.2013, bea.geo.fips + bea.location + bea.state.fips + bea.county.fips + 
                               fips.state.county ~ bea.description, value.var = "2013")
names(cast.bea.2013) <- c("bea.geo.fips", "bea.location", "bea.state.fips", "bea.county.fips",
                          "fips.state.county", "bea.per.cap.inc", "bea.income", "bea.population")
cast.bea.2013$bea.income <- as.numeric(cast.bea.2013$bea.income) * 1000

## merge eda.4 and cast.bea.year

cast.year <- paste("cast.bea.", year, sep = "")
eda.4 <- merge(eda.4, get(cast.year), id.var = "fips.state.county")
eda.5 <- eda.4

## convert bea.population and bea.per.cap.inc from character to numeric
eda.5$bea.population <- as.numeric(eda.5$bea.population)
eda.5$bea.per.cap.inc <- as.numeric(eda.5$bea.per.cap.inc)

## create year variable
eda.5$year <- year
      
## add CPI 
setwd("../")
setwd("cpi")
cpi <- read.csv("chained.cpi.clean.csv")
cpi$year <- 0
for(i in 1:length(cpi$DATE)) {
        cpi$year[i] <-  str_split(cpi$DATE, "-")[[i]][1]
}

cpi.row <- which(cpi$year == year)
eda.5$cpi <- as.numeric(as.character(cpi$VALUE[cpi.row]))

## create real 2013 dollars funds
eda.5$eda.total.real.funds <- (eda.5$eda.total.funds / eda.5$cpi) * 
        as.numeric(as.character(cpi$VALUE[which(cpi$year == 2013)]))

## create real 2013 dollars income
eda.5$bea.real.income <- (eda.5$bea.income / eda.5$cpi) * 
        as.numeric(as.character(cpi$VALUE[which(cpi$year == 2013)]))

## create real 2013 dollars per capita income
eda.5$bea.real.per.cap.inc <- (as.numeric(eda.5$bea.per.cap.inc) / eda.5$cpi) * 
        as.numeric(as.character(cpi$VALUE[which(cpi$year == 2013)]))

## create national popolation variable
## for years 2010-2013, script will get error when running pop.old for loop
## for years 2007-2009, script will get error when running pop for loop
## bea.population for counties is the same exact count as census.population for counties, so census is omitted

eda.6 <- eda.5

setwd("../")
setwd("population")

nat.pop <- read.csv("nat.pop.2010-2013.csv")
names(nat.pop)[8:11] <- c("2010", "2011", "2012", "2013")
nat.pop <- nat.pop[ , c(5, 8, 9, 10, 11)]
nat.pop <- nat.pop[1, ]
melt.nat.pop <- melt(nat.pop, id.vars = "NAME")

nat.pop.old <- read.csv("nat.pop.2000-2009.csv")
names(nat.pop.old)[15:17] <- c("2007", "2008", "2009")
nat.pop.old <- nat.pop.old[ , c(5, 15, 16, 17)]
nat.pop.old <- nat.pop.old[1, ]
melt.nat.pop.old <- melt(nat.pop.old, id.vars = "NAME")

if(year > 2009){
        melt.row <- subset(melt.nat.pop, melt.nat.pop$variable == year) 
        eda.6$census.nat.pop <- melt.row$value
}

if(year < 2010){
        melt.row <- subset(melt.nat.pop.old, melt.nat.pop.old$variable == year) 
        eda.6$census.nat.pop <- melt.row$value
}

## import national income data
setwd("../")
setwd("gdp")
gdp <- read.csv("gdp.csv")

## create real 2013 dollars real national income variable
names(gdp) <- c("year", "nom.gdp")
gdp$nom.gdp <- gdp$nom.gdp * 1000000000
for(i in 1:length(gdp$year)){
        gdp.year <- gdp$year[i]
        gdp$real.gdp[i] <- (gdp$nom.gdp[i] / as.numeric(as.character(cpi$VALUE[which(cpi$year == gdp.year)])) * 
                                 as.numeric(as.character(cpi$VALUE[which(cpi$year == 2013)])))
}
eda.6$bea.real.national.income <- gdp$real.gdp[which(gdp$year == year)]

## create real 2013 dollars real national per capita income variable 
eda.6$bea.real.nat.per.cap.inc <- eda.6$bea.real.national.income / eda.6$census.nat.pop

## create percent of real.nat.per.cap.wages variable
eda.6$bea.pct.real.nat.per.cap.inc <- eda.6$bea.real.per.cap.inc / eda.6$bea.real.nat.per.cap.inc

## create dummy variable for "80% or less of real.nat.per.cap.inc"
eda.6$bea.80pct.real.nat.per.cap.inc <- 0
for(i in 1:length(eda.6$bea.pct.real.nat.per.cap.inc)){
        if(eda.6$bea.pct.real.nat.per.cap.inc[i] <= .8){
                eda.6$bea.80pct.real.nat.per.cap.inc[i] <- 1
        }
        if(eda.6$bea.pct.real.nat.per.cap.inc[i] > .8){
                eda.6$bea.80pct.real.nat.per.cap.inc[i] <- 0
        }
}

## check to see how many counties above and below 80pct cutoff
above <- subset(eda.6, eda.6$bea.80pct.real.nat.per.cap.inc == 0)
print("above")
nrow(above)
below <- subset(eda.6, eda.6$bea.80pct.real.nat.per.cap.inc == 1)
print("below")
nrow(below)

## create county unemployment variable
 
setwd("../")
setwd("bls.ue")

files <- list.files()
file.name <- grep(year, files, value = TRUE)
file.name
ue <- read.csv(file.name)
ue.1 <- ue
ue.1 <- ue.1[-c(1:5), -c(1,6)]
rownames(ue.1) <- NULL
names(ue.1) <- c("blsu.state.fips", "blsu.county.fips", "blsu.county.state", "blsu.year", "blsu.labor.force",
                 "blsu.employed", "blsu.unemployed", "blsu.ue.rate")
ue.1$blsu.state.fips <- as.numeric(as.character(str_trim(ue.1$blsu.state.fips, side = "both")))
ue.1$blsu.county.fips <- as.numeric(as.character(str_trim(ue.1$blsu.county.fips, side = "both")))
ue.1$fips.state.county <- paste(ue.1$blsu.state.fips, ue.1$blsu.county.fips, sep = ".")

ue.1$blsu.ue.rate <- gsub(",", "", ue.1$blsu.ue.rate) 
ue.1$blsu.ue.rate <- as.numeric(as.character(str_trim(gsub(",", "", ue.1$blsu.ue.rate), side = "both")))
ue.1$blsu.unemployed <- as.numeric(as.character(str_trim(gsub(",", "", ue.1$blsu.unemployed), side = "both")))
ue.1$blsu.employed <- as.numeric(as.character(str_trim(gsub(",", "", ue.1$blsu.employed), side = "both")))
ue.1$blsu.labor.force <- as.numeric(as.character(str_trim(gsub(",", "", ue.1$blsu.labor.force), side = "both")))
ue.1$blsu.year <- as.numeric(as.character(str_trim(gsub(",", "", ue.1$blsu.year), side = "both")))

## check how many na rows, then delete them
## note, ue.1 also contains PR rows, but when we merge with eda.6 by fips.state.county they will not be included
## if PR rows are removed though, ue.1 has 3142 unique fips.state.county
na.row <- which(is.na(ue.1$blsu.state.fips))
na.row
ue.1 <- ue.1[-na.row, ]

## merge ue.1 with eda.7
eda.7 <- eda.6

eda.7 <- merge(eda.7, ue.1, id.vars = "fips.state.county")

## create national unemployment variable
nat.ue <- read.csv("nat.ue.csv")
eda.7$blsu.nat.ue.rate <- nat.ue$blsu.nat.ue.rate[which(nat.ue$blsu.year == year)]

## create dummy variable for "1pctpt.above.nat.ue"
## need to first add in all year data, since need 2yr avg of county ue.rate



## assign df a "df.year" name and set aside for later rbind
## e.g. "df.2012"
assign(paste("df.", year, sep = ""), eda.7)


## need to get county populations and calculate per cap income
## then need to load in demographic and UE data from ACS 3 years

