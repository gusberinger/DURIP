rm(list=ls())
gc()
setwd("~/Documents/DURIP/")


library(MASS)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(haven)

# Manifesto Data ---------------------------------


# import manifesto data
manifesto <- read.csv("data/MPDataset_MPDS2019b.csv")
manifesto <- manifesto %>% mutate(rile = c(scale(rile)))
manifesto <- manifesto %>% mutate(pervote = pervote / 100)
manifesto <- manifesto %>% mutate(date = as.Date(edate, "%d/%m/%Y"))
manifesto <- manifesto %>% mutate(year = as.numeric(format(date, "%Y")))
manifesto <- select(manifesto, country:partyabbrev, date, year, pervote, rile)
manifesto <- manifesto %>% drop_na(pervote)
manifesto <- manifesto %>% filter(!is.na(rile) | pervote > .10)


# helper function to find the winning party of a distinct election
get_winner <- function(df) {
  countryname = as.character(df$countryname[[1]])
  date = df$date[[1]]
  year = df$year[[1]]
  switch(as.character(df$countryname[[1]]),
          "Australia" = {
            if (year %in% c(1949, 1951, 1954, 1955, 1958, 1961, 1963, 1969, 1974, 1975, 
                          1977, 1980, 1996, 1998, 2001, 2013, 2016)) {return("Liberal Party of Australia")}
           },
          "Austria" = {
            if (year %in% c(1999, 1953, 1959)) {return("Austrian People’s Party")}
          },
          ""
          )
  
  winner.row <- df %>% filter(pervote == max(pervote))
  winner.name <- as.character(winner.row$partyname)
  return(winner.name)
}



# define the polarization for a distinct election
get_polarization <- function(df) {
  winner_name = get_winner(df)
  winner <- df %>% filter(partyname == winner_name)
  losers <- df %>% filter(partyname != winner_name)
  polarization <- (winner$rile - sum((losers$pervote * losers$rile) / (1 - winner$pervote), na.rm=TRUE)) ** 2
  if (sum(df$pervote, na.rm=TRUE) < .60) {
    return(NA)
  }
  return(polarization)
}


test <- manifesto %>% filter(countryname == "United States", year == 2008)
get_winner(test)
get_polarization(test)

# map polarization to each election group
manifesto.scores <- manifesto %>% 
  group_by(countryname, date) %>% 
  nest() %>%
  mutate(polarization = map(data, get_polarization)) %>%
  unnest(polarization) %>%
  drop_na(polarization)
manifesto.scores <- manifesto.scores %>% mutate(year = as.numeric(format(date, "%Y")))


## Verify Data with Parl ---------------------------------
parl <- read.csv("data/parlgov_cabinet.csv")
parl <- rename(parl, countryname = "country_name", parl_party = "party_name_english")
parl <- parl %>% mutate(date = as.Date(election_date, "%Y-%m-%d"))
parl <- parl %>% mutate(year = as.numeric(format(date, "%Y")))
parl <- parl %>% filter(prime_minister == 1)
parl <- parl %>% distinct()
parl.winner <- parl %>% group_by(countryname, election_date) %>% top_n(1, start_date)
manifesto.winner <- manifesto %>% group_by(countryname, date) %>% top_n(1, pervote)
compare.leader <- merge(parl.winner, manifesto.winner)

format_partyname <- function(text) {
  text = as.character(text)
  text = str_to_lower(text)
  text = str_replace(text, "party", "")
  text = str_replace(text, "['’’']", "")
  text = trimws(text)
  return(text)
}

compare.leader <- compare.leader %>% 
  select(countryname, date, parl_party, partyname)
diff <- compare.leader %>% 
  filter(format_partyname(parl_party) != format_partyname(partyname)) %>%
  filter(countryname != "Ireland") %>%
  filter(countryname != "Italy")




# Database of Political Institutions Data ----------------------------------------
dpi <- read_dta("DPI2017.dta")
dpi <- rename(dpi, "dpi.polarization" = "polariz")
dpi <- dpi %>% mutate(dpi.polarization = replace(dpi.polarization, dpi.polarization == -999, NA))
dpi <- dpi %>% select(dpi.polarization, countryname, year)
dpi <- dpi %>% drop_na(dpi.polarization)
dpi <- dpi %>% mutate(ordered(as.factor(dpi.polarization)))
dpi$countryname[dpi$countryname == "Czech Rep."] <- "Czech Republic"
dpi$countryname[dpi$countryname == "GDR"] <- "German Democratic Republic"
dpi$countryname[dpi$countryname == "FRG/Germany"] <- "Germany"
dpi$countryname[dpi$countryname == "USA"] <- "United States"
dpi$countryname[dpi$countryname == "UK"] <- "United Kingdom"
dpi$countryname[dpi$countryname == "Macedonia"] <- "North Macedonia"
dpi$countryname[dpi$countryname == "Bosnia-Herz"] <- "Bosnia-Herzegovina"
dpi$countryname[dpi$countryname == "S. Africa"] <- "South Africa"
dpi$countryname[dpi$countryname == "S. Korea"] <- "South Korea"


# visualizations
manifesto.scores %>%
  ggplot(aes(year)) + geom_histogram() + facet_wrap(~countryname)


manifesto.scores %>% filter(year > 1980) %>% group_by(countryname) %>% filter(!any(polarization > 8)) %>% filter(n() > 6) %>%
  ggplot(aes(x = date, y = polarization)) + geom_line() + facet_wrap(~countryname)

