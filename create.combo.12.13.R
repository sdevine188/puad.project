cast.09.10 <- rbind(cast.bea.2010, cast.bea.2009)
cast.10 <- cast.bea.2010[ , c(5, 6)]


cast.11 <- cast.bea.2011[ , c(5, 6)]
year <- 2010
cpi.row <- which(cpi$year == year)
cast.11$cpi <- as.numeric(as.character(cpi$VALUE[cpi.row]))
## create real 2013 dollars per capita income
cast.11$bea.real.per.cap.inc <- (as.numeric(cast.11$bea.per.cap.inc) / cast.11$cpi) * 
        as.numeric(as.character(cpi$VALUE[which(cpi$year == 2013)]))

df.2011 <- merge(cast.11, ue.1, id.vars = "fips.state.county")
df.2010 <- merge(cast.10, ue.1, id.vars = "fips.state.county")
head(df.2010)
names(df.2010)
names(df.2011)[4] <- "bea.real.per.cap.inc.2011"
names(df.2011)[10] <- "blsu.employed.2011"
names(df.2011)[12] <- "blsu.ue.rate.2011"

names(df.2010)[4] <- "bea.real.per.cap.inc.2010"
names(df.2010)[10] <- "blsu.employed.2010"
names(df.2010)[12] <- "blsu.ue.rate.2010"

df2.2011 <- df.2011[ , c(1, 4, 10, 12)]
df2.2010 <- df.2010[ , c(1, 4, 10, 12)]

df2.2012 <- df.2012

df2.2012 <- merge(df.2012, df2.2010, id.vars = "fips.state.county")
df2.2012 <- merge(df2.2012, df2.2011, id.vars = "fips.state.county")

new.names <- names(df2.2012)[55:60]
df2.2013 <- df.2013

for(i in 1:length(new.names)) {
        col.number <- ncol(df2.2013)
        new.col.number <- col.number + 1
        df2.2013[ , new.col.number] <- 0
        names(df2.2013)[new.col.number] <- new.names[i]
}

for(i in 1:length(df2.2012$eda.total.real.funds)){
        if(df2.2012$eda.total.real.funds[i] == 0){
                df2.2012$eda.total.real.funds[i] <- 1
        }
}

df2.2012$eda.ln.total.real.funds <- as.numeric(log(df2.2012$eda.total.real.funds))
df2.2012$ue.rate.prior.year <- df2.2012$blsu.ue.rate.2011
df2.2012$ue.change.tm2.to.tm1 <- df2.2012$blsu.ue.rate.2011 - df2.2012$blsu.ue.rate.2010
df2.2012$blsu.employ.growth.tm2.to.tm1 <- (df2.2012$blsu.employed.2011 - df2.2012$blsu.employed.2010) / 
        df2.2012$blsu.employed.2010
df2.2012$blsu.employ.prior.year <- df2.2012$blsu.employed.2011
df2.2012$blsu.ln.employ.prior.year <- log(df2.2012$blsu.employ.prior.year)
df2.2012$blsu.ln.employed <- log(df2.2012$blsu.employed)
df2.2012$bea.ln.real.pc.inc.prior.year <- log(df2.2012$bea.real.per.cap.inc.2011)
df2.2012$bea.ln.real.pc.inc <- log(df2.2012$bea.real.per.cap.inc)
df2.2012$bea.real.pc.inc.growth.tm2.to.tm1 <- (df2.2012$bea.real.per.cap.inc.2011 - 
                                                       df2.2012$bea.real.per.cap.inc.2010) / df2.2012$bea.real.per.cap.inc.2010
df2.2012$eda.ln.total.real.funds.prior.year <- 1
df2.2012$bea.ten.pct.band.80.pct.nat.real.pc.inc <- 0
for(i in 1:nrow(df2.2012)){
        if(df2.2012$bea.pct.real.nat.per.cap.inc[i] > .7){ 
                if(df2.2012$bea.pct.real.nat.per.cap.inc[i] < .9){
                        df2.2012$bea.ten.pct.band.80.pct.nat.real.pc.inc[i] <- 1
                }
        }
}

for(i in 1:length(df2.2013$eda.total.real.funds)){
        if(df2.2013$eda.total.real.funds[i] == 0){
                df2.2013$eda.total.real.funds[i] <- 1
        }
}

df2.2013$eda.ln.total.real.funds <- as.numeric(log(df2.2013$eda.total.real.funds))
df2.2013$ue.rate.prior.year <- df2.2012$blsu.ue.rate
df2.2013$ue.change.tm2.to.tm1 <- df2.2012$blsu.ue.rate - df2.2012$blsu.ue.rate.2011
df2.2013$blsu.employ.growth.tm2.to.tm1 <- (df2.2012$blsu.employed - df2.2012$blsu.employed.2011) / 
        df2.2012$blsu.employed.2011
df2.2013$blsu.employ.prior.year <- df2.2012$blsu.employed
df2.2013$blsu.ln.employ.prior.year <- log(df2.2012$blsu.employed)
df2.2013$blsu.ln.employed <- log(df2.2013$blsu.employed)
df2.2013$bea.ln.real.pc.inc.prior.year <- log(df2.2012$bea.real.per.cap.inc)
df2.2013$bea.ln.real.pc.inc <- log(df2.2013$bea.real.per.cap.inc)
df2.2013$bea.real.pc.inc.growth.tm2.to.tm1 <- (df2.2012$bea.real.per.cap.inc - 
                                                       df2.2012$bea.real.per.cap.inc.2011) / df2.2012$bea.real.per.cap.inc.2011
df2.2013$eda.ln.total.real.funds.prior.year <- df2.2012$eda.ln.total.real.funds
df2.2013$bea.ten.pct.band.80.pct.nat.real.pc.inc <- 0
for(i in 1:nrow(df2.2013)){
        if(df2.2013$bea.pct.real.nat.per.cap.inc[i] > .7){ 
                if(df2.2013$bea.pct.real.nat.per.cap.inc[i] < .9){
                        df2.2013$bea.ten.pct.band.80.pct.nat.real.pc.inc[i] <- 1
                }
        }
}         

df3.2012 <- merge(df2.2012, fips3, id.vars = "fips.state.county")
df3.2013 <- merge(df2.2013, fips3, id.vars = "fips.state.county")

df.12.13 <- rbind(df3.2012, df3.2013)

write.csv(df.12.13, file = "df.12.13.csv")

df.band <- subset(df.12.13, df.12.13$bea.ten.pct.band.80.pct.nat.real.pc.inc == 1)
write.csv(df.band, file = "df.band.csv")
