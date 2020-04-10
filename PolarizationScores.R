rm(list=ls())
gc()
setwd("~/Documents/DURIP/")


library(MASS)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(haven)



# import manifesto data
manifesto <- read.csv("data/MPDataset_MPDS2019b.csv")
manifesto <- manifesto %>% mutate(rile = c(scale(rile)))
manifesto <- manifesto %>% mutate(pervote = pervote / 100)
manifesto <- manifesto %>% mutate(date = as.Date(edate, "%d/%m/%Y"))
manifesto <- select(manifesto, country:partyabbrev, date, pervote, rile)
manifesto <- manifesto %>% drop_na(pervote)
manifesto <- manifesto %>% filter(!is.na(rile) | pervote > .10)

# lots of elections have incomplete pervote
manifesto %>% 
  group_by(countryname, date) %>% 
  summarize(totvotes = sum(pervote, na.rm=TRUE))


# define the polarization for a distinct election
get_polarization <- function(df) {
  winner <- df %>% filter(pervote == max(pervote))
  losers <- df %>% filter(pervote < max(pervote))
  polarization <- (winner$rile - sum((losers$pervote * losers$rile) / (1 - winner$pervote))) ** 2
  if (sum(df$pervote, na.rm=TRUE))
  return(polarization)
}


# map polarization to each election group
manifesto.scores <- manifesto %>% 
  group_by(countryname, date) %>% 
  nest() %>%
  mutate(polarization = map(data, get_polarization)) %>%
  unnest(polarization) %>%
  select(countryname, date, polarization)
manifesto.scores <- manifesto.scores %>% mutate(year = as.numeric(format(date, "%Y")))


# verify the data with parl
parl <- read.csv("data/parlgov_cabinet.csv")
parl <- rename(parl, countryname = "country_name", parl_party = "party_name_english")
parl <- parl %>% mutate(date = as.Date(election_date, "%Y-%m-%d"))
parl <- parl %>% mutate(year = as.numeric(format(date, "%Y")))
parl <- parl %>% filter(prime_minister == 1)
parl <- parl %>% distinct()
parl.winner <- parl %>% group_by(countryname, election_date) %>% top_n(1, start_date)
manifesto.winner <- manifesto %>% group_by(countryname, date) %>% top_n(1, pervote)
compare.leader <- merge(parl.winner, manifesto.winner)
compare.leader <- compare.leader %>% 
  select(countryname, date, parl_party, partyname)
# View(compare.leader)

manifesto.scores %>%
  ggplot(aes(year)) + geom_histogram() + facet_wrap(~countryname)

manifesto %>%
  ggplot(aes(rile)) + geom_histogram() + facet_wrap(~countryname)

manifesto.scores %>% filter(year > 1980) %>% filter(any(polarization > 2)) %>% group_by(countryname) %>% filter(n() > 6) %>%
  ggplot(aes(x = date, y = polarization)) + geom_line() + facet_wrap(~countryname)

