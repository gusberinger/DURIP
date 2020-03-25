setwd("~/Documents/manifestos/")
rm(list=ls())
gc()

library(tidyr)
library(dplyr)
library(ggplot2)
library(haven)
library(MASS)
library(texreg)


manifesto.df <- read.csv("MPDataset_MPDS2019b.csv")
manifesto.df <- manifesto.df %>% drop_na(c("rile", "pervote"))
manifesto.df$rile <- scale(manifesto.df$rile)
manifesto.df$pervote <- manifesto.df$pervote / 100
manifesto.df$date <- as.Date(manifesto.df$edate, "%d/%m/%Y")
manifesto.df$year <- as.numeric(format(manifesto.df$date, "%Y"))
manifesto.df <- manifesto.df[, c(1, 2, 3, 4, 5, 6, 175, 7:174)]


poldf <- data.frame(year=as.numeric(),
                    country=as.integer(), 
                    countryname=character(), 
                    manifesto.polarization=double(),
                    stringsAsFactors=FALSE)


for (x in unique(manifesto.df$date)) {
  election.df <- subset(manifesto.df, date == x, select = c("country", "countryname", "partyabbrev", "pervote", "rile", "year"))
  for (countryid in unique(election.df$country)) {
    country.df <- subset(election.df, country == countryid)
    winner.index <- which.max(country.df$pervote)
    winner <- country.df[winner.index,]
    losers <- country.df[-c(winner.index), ]
    losers$t <- losers$pervote * losers$rile
    polarization <- (winner$rile - sum((losers$pervote * losers$rile) / (1 - winner$pervote))) ** 2
    poldf[nrow(poldf) + 1,] <- list(winner$year, countryid, as.character(winner$countryname), polarization)
  }
}
poldf <- poldf[order(poldf$country, poldf$year),]
qplot(year, polarization, data = subset(poldf, countryname == "France"), geom="line")
head(poldf)

dpi.df <- read_dta("DPI2017.dta")
dpi.df <- dpi.df %>% rename(dpi.polarization = "polariz")
dpi.df <- dpi.df[, c("dpi.polarization", "countryname", "year")]
dpi.df <- dpi.df %>% drop_na("dpi.polarization")
dpi.df$dpi.polarization <- ordered(as.factor(dpi.df$dpi.polarization))


# no integer country code in DPI
dpi.df$countryname[dpi.df$countryname == "Czech Rep."] <- "Czech Republic"
dpi.df$countryname[dpi.df$countryname == "GDR"] <- "German Democratic Republic"
dpi.df$countryname[dpi.df$countryname == "FRG/Germany"] <- "Germany"
dpi.df$countryname[dpi.df$countryname == "USA"] <- "United States"
dpi.df$countryname[dpi.df$countryname == "UK"] <- "United Kingdom"
dpi.df$countryname[dpi.df$countryname == "Macedonia"] <- "North Macedonia"
dpi.df$countryname[dpi.df$countryname == "Bosnia-Herz"] <- "Bosnia-Herzegovina"
dpi.df$countryname[dpi.df$countryname == "S. Africa"] <- "South Africa"
dpi.df$countryname[dpi.df$countryname == "S. Korea"] <- "South Korea"


merged <- merge(poldf, dpi.df)
probit.model <- polr(dpi.polarization~manifesto.polarization, data=merged, method="probit")
p <- 2*pt(-abs(1.761),df=nrow(merged) - 1)
summary(probit.model)


texreg(probit.model,
       stars = c(0.01,  0.05, 0.1),
       booktabs=TRUE,
       float.pos = "h!",
       dcolumn = TRUE,
       digits=4,
       custom.model.names = "Ordered Probit",
       custom.coef.names = "Manifesto Polarization")