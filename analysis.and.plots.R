## merge df.2012 and df.2013 datasets 
combo.12.13 <- rbind(df.2012, df.2013)
all.grant.2012 <- df.2012[df.2012$eda.grant == 1, ]
all.grant.2013 <- df.2013[df.2013$eda.grant == 1, ]
all.grant.12.13 <- rbind(all.grant.2012, all.grant.2013)

## create histogram showing EDA income criteria cutoff for grant recipients and non-recipients
combo.12.13$eda.grant.cat <- "no grant"
for(i in 1:nrow(combo.12.13)){
        if(combo.12.13$eda.grant[i] == 1){
                combo.12.13$eda.grant.cat[i] <- "grant"
        }
}
ggplot(combo.12.13, aes(x = bea.pct.real.nat.per.cap.inc, fill = eda.grant.cat)) + geom_histogram(binwidth = .025, 
                position = "identity", alpha = .5) + geom_vline(aes(xintercept = .8), 
                color = "red", linetype = "dashed", size = 1) + xlab("Real county per capita income as percentage
                of real national per capita income") + ggtitle("County grant recipients and county non-recipients 
                at EDA's income criteria cutoff")