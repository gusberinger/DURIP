# Standard Setup Stuff ---------------------------
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
manifesto <- manifesto %>% mutate(orile = rile,
                                  rile = c(scale(rile)),
                                  pervote = pervote / 100,
                                  date = as.Date(edate, "%d/%m/%Y"),
                                  year = as.numeric(format(date, "%Y")))
manifesto <- manifesto %>% select(countryname, party:partyabbrev, date, year, pervote, rile, orile)
manifesto <- manifesto %>% drop_na(pervote)
manifesto <- manifesto %>% filter(!is.na(rile) | pervote > .10)


##  Find the Polarization for each election ------------------------------

get_parl_winner <- function(countryname, date) {
  year = as.numeric(format(date, "%Y"))
  switch(as.character(countryname),
         "Australia" = {
           if (year %in% c(1949, 1951, 1954, 1955, 1958, 1961, 1963, 1969, 1974, 1975,
                           1977, 1980, 1996, 1998, 2001, 2013, 2016)) {return("Liberal Party of Australia")}
         },
         "Austria" = {
           if (year %in% c(1999, 1953, 1959)) {return("Austrian People’s Party")}
         },
         "Belgium" = {
           if (year %in% c(1946, 1954)) {return ("Belgian Socialist Party")}
         }
  )
  return(NA)
}

get_naive_winner <- function(df) {
  #' Find the party with the most amout of votes in the election dataframe.
  winner.row <- df %>% filter(pervote == max(pervote))
  winner.name <- as.character(winner.row$partyname)
  return(winner.name)
}

get_polarization <- function(df, winner_name) {
  #' define the polarization for a distinct election
  winner <- df %>% filter(partyname == winner_name)
  losers <- df %>% filter(partyname != winner_name)
  polarization <- (winner$rile - sum((losers$pervote * losers$rile) / (1 - winner$pervote), na.rm=TRUE)) ** 2
  if (sum(df$pervote, na.rm=TRUE) < .60) {
    return(NA)
  }
  return(polarization)
}


# map polarization to each election group
manifesto.scores <- manifesto %>% 
  group_by(countryname, date) %>% 
  nest() %>%
  mutate(naive_winner = map(.x = data, .f = get_naive_winner),
         parl_winner = map2(countryname, date, get_parl_winner),
         winner = coalesce(parl_winner, naive_winner),
         polarization = map2_dbl(data, winner, get_polarization),
         year = as.numeric(format(date, "%Y"))) %>%
  arrange(countryname, date)
manifesto.scores <- manifesto.scores %>% mutate(year = as.numeric(format(date, "%Y")))


## Verify Data with Parl ---------------------------------
parl <- read.csv("data/parlgov_cabinet.csv")
parl <- rename(parl, countryname = "country_name", parl_party = "party_name_english")
parl <- parl %>% mutate(date = as.Date(election_date, "%Y-%m-%d"))
parl <- parl %>% mutate(year = as.numeric(format(date, "%Y")))
parl <- parl %>% filter(prime_minister == 1)
parl <- parl %>% distinct()
parl.winner <- parl %>% group_by(countryname, election_date) %>% top_n(1, start_date)

manifesto.winner <- manifesto.scores %>% select()
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
  select(countryname, date, parl_party, party_name)
diff <- compare.leader %>% 
  filter(format_partyname(parl_party) != format_partyname(party_name)) %>%
  filter(countryname != "Ireland") %>%
  filter(countryname != "Italy")



# Database of Political Institutions Data ----------------------------------------
## Load and reshape data ----------------------
dpi <- read_dta("data/DPI2017.dta")
dpi <- rename(dpi, "dpi.polarization" = "polariz")
dpi <- dpi %>% mutate(dpi.polarization = replace(dpi.polarization, dpi.polarization == -999, NA))
dpi <- dpi %>% mutate(dpi.polarization = ordered(dpi.polarization, levels=(0:2)))
dpi <- dpi %>% select(dpi.polarization, countryname, year)
dpi <- dpi %>% drop_na(dpi.polarization)
dpi$countryname[dpi$countryname == "Czech Rep."] <- "Czech Republic"
dpi$countryname[dpi$countryname == "GDR"] <- "German Democratic Republic"
dpi$countryname[dpi$countryname == "FRG/Germany"] <- "Germany"
dpi$countryname[dpi$countryname == "USA"] <- "United States"
dpi$countryname[dpi$countryname == "UK"] <- "United Kingdom"
dpi$countryname[dpi$countryname == "Macedonia"] <- "North Macedonia"
dpi$countryname[dpi$countryname == "Bosnia-Herz"] <- "Bosnia-Herzegovina"
dpi$countryname[dpi$countryname == "S. Africa"] <- "South Africa"
dpi$countryname[dpi$countryname == "S. Korea"] <- "South Korea"

## Compare Data With Manifesto Project -------------------------------------------

dpi_manifesto <- merge(dpi, manifesto.scores)
model <- polr(dpi.polarization~polarization, data = dpi_manifesto)


# Visualizations -----------------------------------------------------------------
manifesto.scores %>% group_by(countryname) %>% filter(n() > 10) %>%
  ggplot(aes(date, polarization)) + geom_line() + facet_wrap(~countryname)


# View change of party overtime
manifesto %>% group_by(party, partyname) %>% summarise(diff = range(rile)) %>% arrange(desc(diff))
manifesto %>% filter(party == 34210) %>%
  ggplot(aes(date, rile)) + geom_line()

manifesto.scores %>% arrange(polarization)

dpi_manifesto %>% group_by(countryname) %>%
  ggplot(aes(x = year)) + 
  geom_point(aes(y = dpi.polarization), color="blue", alpha=.5) +
  geom_line(aes(y = polarization), color="red", alpha = .5) +
  facet_wrap(~countryname)

