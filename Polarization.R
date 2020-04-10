rm(list=ls())
gc()
setwd("~/Documents/DURIP/")


library(tidyr)
library(MASS)
library(dplyr)
library(ggplot2)
library(haven)
library(zoo)


# import manifesto data
manifesto <- read.csv("MPDataset_MPDS2019b.csv")
manifesto <- manifesto %>% mutate(rile = c(scale(rile)))
manifesto <- manifesto %>% mutate(pervote = pervote / 100)
manifesto <- manifesto %>% mutate(date = as.Date(edate, "%d/%m/%Y"))
manifesto <- manifesto %>% mutate(year = as.numeric(format(date, "%Y")))
manifesto <- select(manifesto, country:partyabbrev, date, year, pervote, rile)
manifesto <- manifesto %>% filter(!is.na(rile) | pervote > .10)

poldf <- data.frame(year=as.numeric(),
                    country=as.integer(), 
                    countryname=character(), 
                    manifesto.polarization=double(),
                    stringsAsFactors=FALSE)
for (date_ in unique(manifesto$date)) {
  election.df <- subset(manifesto, date == date_, select = c("country", "countryname", "partyabbrev", "pervote", "rile", "year"))
  for (countryname_ in unique(election.df$countryname)) {
    country.df <- subset(election.df, countryname == countryname_)
    winner.index <- which.max(country.df$pervote)
    winner <- country.df[winner.index,]
    losers <- country.df[-c(winner.index), ]
    if (sum(country.df$pervote, na.rm = TRUE) > .90) {
      polarization <- (winner$rile - sum((losers$pervote * losers$rile) / (1 - winner$pervote))) ** 2
      poldf[nrow(poldf) + 1,] <- list(winner$year, countryid, as.character(winner$countryname), polarization)
    }
  }
}
poldf <- poldf %>% arrange(countryname, year)
head(poldf)
table(is.na(poldf$manifesto.polarization))


# check with parlgov
parl <- read.csv("parlgov_cabinet.csv")
parl <- parl %>% mutate(date = as.Date(election_date, "%Y-%m-%d"))
parl <- rename(parl, "countryname" = "country_name")
setdiff(unique(poldf$countryname), unique(parl$country_name))


compare.leader <- merge(parl, poldf, by="")


dpi <- read_dta("DPI2017.dta")
dpi <- rename(dpi, "dpi.polarization" = "polariz")
dpi <- dpi %>% mutate(dpi.polarization = replace(dpi.polarization, dpi.polarization == -999, NA))
dpi <- dpi %>% select(dpi.polarization, countryname, year)
dpi <- dpi %>% drop_na(dpi.polarization)
dpi <- dpi %>% mutate(ordered(as.factor(dpi.polarization)))

# match up DPI
dpi$countryname[dpi$countryname == "Czech Rep."] <- "Czech Republic"
dpi$countryname[dpi$countryname == "GDR"] <- "German Democratic Republic"
dpi$countryname[dpi$countryname == "FRG/Germany"] <- "Germany"
dpi$countryname[dpi$countryname == "USA"] <- "United States"
dpi$countryname[dpi$countryname == "UK"] <- "United Kingdom"
dpi$countryname[dpi$countryname == "Macedonia"] <- "North Macedonia"
dpi$countryname[dpi$countryname == "Bosnia-Herz"] <- "Bosnia-Herzegovina"
dpi$countryname[dpi$countryname == "S. Africa"] <- "South Africa"
dpi$countryname[dpi$countryname == "S. Korea"] <- "South Korea"


# match up vdem
vdem <- readRDS("Country_Year_V-Dem_Full+others_R_v9/V-Dem-CY-Full+Others-v9.rds")
vdem$country <- vdem$country_name
vdem <- vdem[, c("country", "COWcode", "year", "v2smpolsoc")]
vdem <- vdem %>% drop_na(c("v2smpolsoc"))
vdem$country[vdem$country == "United States of America"] <- "United States"
vdem$country[vdem$country == "Macedonia"] <- "North Macedonia"
vdem$country[vdem$country == "Bosnia and Herzegovina"] <- "Bosnia-Herzegovina"


# compare manifesto with dpi
merged <- merge(poldf, dpi)
manifesto.dpi.model <- polr(dpi.polarization~manifesto.polarization, data = subset(merged, year > 1990))
summary(manifesto.dpi.model)
extract(manifesto.dpi.model)
texreg(manifesto.dpi.model, 
       booktabs = TRUE,
       custom.coef.names = c("Manifesto Polarization"))


# print images
for (c in unique(merged$countryname)) {
  df <- subset(merged, countryname == c)
  
  if (nrow(df) > 3) {
    ggplot(df, aes(x=year)) +
      geom_point(aes(y=manifesto.polarization), color="blue") +
      geom_point(aes(y=as.numeric(as.character(dpi.polarization))), color="red") +
      ylab("Polarization") + xlab("Year") + ggtitle(c)
    
    ggsave(file = paste("images-dpi-manifestos-point/", c, ".png", sep=""))
    
    ggplot(df, aes(x=year)) +
      geom_line(aes(y=manifesto.polarization), color="blue") +
      geom_line(aes(y=as.numeric(as.character(dpi.polarization))), color="red") +
      ylab("Polarization") + xlab("Year") + ggtitle(c)
    
    ggsave(file = paste("images-dpi-manifestos-line/", c, ".png", sep=""))
  }
}


# compare vdem with dpi
merged <- merge(dpi, vdem)
# dpi.vdem.model <- polr(dpi.polarization~v2smpolsoc, data = merged)
dpi.vdem.model <- lm(v2smpolsoc~as.numeric(as.character(dpi.polarization)), data=merged)
summary(dpi.vdem.model)
extract(dpi.vdem.model)
head(merged)



# create some graphs
qplot(manifesto$year) + xlab("Year") + ylab("Count")
ggsave("images/year.png")
qplot(manifesto$rile) + xlab("Standarized rile") + ylab("Count")
ggsave("images/standardrile.png")


