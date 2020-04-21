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


## Weird things in manifesto data ------

range <- function(x) {return (max(x) - min(x))}

manifesto %>% group_by(countryname, partyname) %>% 
  summarize(diff = range(rile)) %>% arrange(desc(diff))


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
           if (year %in% c(1968, 1974, 1977)) {return ("Christian People’s Party")}
           if (year %in% c(2010)) {return ("Francophone Socialist Party")}
         },
         "Bulgaria" = {
           if (year %in% c(1994)) {return ("Union of Democratic Forces")}
         },
         "Canada" = {
           if (year %in% c(1957, 1979)) {return ("Progressive Conservative Party")}
         },
         "Cyprus" = {
           if (year %in% c(1999)) {return ("Democratic Coalition")}
           if (year %in% c(2001)) {return ("Democratic Party")}
         },
         "Denmark" = {
           if (date == "1953-04-21" | year %in% c(1945, 1950, 1973, 2015)) {return ("Liberals")}
           if (year %in% c(1968)) {return ("Danish Social-Liberal Party")}
           if (year %in% c(1981, 1984, 1987, 1988)) {return ("Conservative People’s Party")}
           if (year %in% c(2011)) {return ("Social Democratic Party")}
         },
         "Estonia" = {
           if (year %in% c(1999, 2003)) {return ("Estonian Reform Party")}
           if (year %in% c(2015)) {return ("Estonian Center Party")}
         },
         "Finland" = {
           if (year %in% c(1945)) {return ("Finnish People’s Democratic Union")}
           # if (year %in% c(1948)) {return ("Finnish Centre")}
           if (year %in% c(1951)) {return ("Agrarian Union")}
         },
         "France" = {
           if (year %in% c(1946, 1951)) {return ("French Section of the Workers' International")}
         },
         "Germany" = {
           if (year %in% c(1969, 1976)) {return ("Social Democratic Party of Germany")}
         },
         "Iceland" = {
           if (year %in% c(1946, 1956, 1959, 1978)) {return ("Social Democratic Party")}
           if (year %in% c(1971, 1983, 1987, 2013)) {return ("Progressive Party")}
           if (year %in% c(2007)) {return ("The Alliance")}
           if (year %in% c(2017)) {return ("Left Green Movement")}
         },
         "Israel" = {
           if (year %in% c(1984, 1996, 1999, 2009)) {return ("The Consolidation")}
         },
         "Latvia" = {
           if (year %in% c(1998)) {return ("Latvian Way Union")}
           if (year %in% c(2002)) {return ("People’s Party")}
           if (year %in% c(2006)) {return ("New Era")}
           if (year %in% c(2011, 2018)) {return ("Unity")}
           if (year %in% c(2014)) {return ("Greens' and Farmers’ Union")}
         },
         "Lithuania" = {
           # if (year %in% c(2000, 2012)) {return ("A. Brazauskas Social Democratic Coalition")}
           if (year %in% c(2016)) {return ("Lithuanian Peasant and Green Union")}
         },
         "Luxembourg" = {
           if (year %in% c(1948, 1964)) {return ("Christian Social People’s Party")}
           if (year %in% c(1974, 2013)) {return ("Democratic Party")}
         },
         "Netherlands" = {
           if (year %in% c(1948)) {return ("Labour Party")}
           if (year %in% c(1956)) {return ("Catholic People’s Party")}
           if (year %in% c(1963, 1971)) {return ("Anti-Revolutionary Party")}
           if (year %in% c(1977, 1982)) {return ("Christian Democratic Appeal")}
         },
         "New Zealand" = {
           if (year %in% c(1971, 1978)) {return ("New Zealand National Party")}
           if (year %in% c(2017)) {return ("New Zealand Labour Party")}
         },
         "Norway" = {
           if (year %in% c(1965)) {return ("Centre Party")}
           if (year %in% c(1969, 2001)) {return ("Christian People’s Party")}
           if (year %in% c(1981, 2013, 2017)) {return ("Conservative Party")}
         },
         "Portugal" = {
           if (year %in% c(1979)) {return ("Social Democratic Party")}
           if (year %in% c(2015)) {return ("Socialist Party")}
         },
         "Romania" = {
           if (year %in% c(2004)) {return ("Justice and Truth Alliance")}
           if (year %in% c(2016)) {return ("Alliance of Liberals and Democrats")}
         },
         "Slovakia" = {
           if (year %in% c(1990)) {return ("Christian Democratic Movement")}
           # TODO
         },
        "Slovenia" = {
          if (year %in% c(1992)) {return ("Slovenian People's Party")}
        },
        "Spain" = {
          if (year %in% c(2016)) {return ("Spanish Socialist Workers’ Party")}
        },
        "Sweden" = {
          if (year %in% c(1976)) {return ("People’s Party")}
          if (year %in% c(1979)) {return ("Centre Party")}
          if (year %in% c(1991, 2006, 2010)) {return ("Moderate Coalition Party")}
        },
        "Turkey" = {
          if (year %in% c(1995)) {return ("Democratic Left Party")}
        },
        "United Kingdom" = {
          if (year %in% c(1951)) {return ("Conservative Party")}
          if (year %in% c(1974)) {return ("Labour Party")}
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
  if (nrow(winner) != 1) {
    print("ERROR")
    print(winner_name)
  }
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
  mutate(naive_winner = map(data, get_naive_winner),
         parl_winner = map2(countryname, date, get_parl_winner),
         winner = coalesce(parl_winner, naive_winner),
         polarization = map2_dbl(data, winner, get_polarization),
         naive_polarization = map2_dbl(data, naive_winner, get_polarization),
         year = as.numeric(format(date, "%Y"))) %>%
  arrange(countryname, date)
manifesto.scores <- manifesto.scores %>% mutate(year = as.numeric(format(date, "%Y")))


## Verify Data with Parl ---------------------------------
parl <- read.csv("data/parlgov_cabinet.csv")
parl <- rename(parl, countryname = "country_name")
parl <- parl %>%
  filter(prime_minister == 1) %>%
  mutate(date = as.Date(election_date, "%Y-%m-%d"),
         year = as.numeric(format(date, "%Y")))
  distinct()

parl.winner <- parl %>% 
  group_by(countryname, date) %>%
  top_n(1, start_date) %>% select(countryname, date, party_name_english)
# manifesto.scores$naive_winner <- unlist(manifesto.scores$naive_winner)
# manifesto.winner <- manifesto.scores %>% select(countryname, date, naive_winner)
compare.leader <- merge(parl.winner, manifesto.scores) %>%
  select(countryname, data, date, naive_winner, party_name_english)

format_partyname <- function(text) {
  text = as.character(text)
  text = str_replace(text, "New Zealand Labour Party", "Labour Party")
  text = str_replace(text, "New Zealand National Party", "National Party")
  text = str_to_lower(text)
  text = str_replace(text, "party", "")
  text = str_replace(text, "['’’']", "")
  
  text = trimws(text)
  return(text)
}

diff <- compare.leader %>% 
  filter(format_partyname(party_name_english) != format_partyname(naive_winner)) %>%
  filter(countryname != "Ireland") %>%
  filter(countryname != "Italy")
  # filter(countryname == "Bulgaria")
View(diff)



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
model <- polr(dpi.polarization~naive_polarization, data = dpi_manifesto)
summary(model)

# Visualizations -----------------------------------------------------------------
manifesto.scores %>% group_by(countryname) %>% filter(n() > 10) %>%
  ggplot(aes(date, polarization)) + geom_line() + facet_wrap(~countryname)


manifesto.scores %>% filter(countryname == "Netherlands") %>%
  ggplot(aes(date)) + 
  geom_line(aes(y = polarization), color = "blue") + 
  geom_line(aes(y = naive_polarization), color = "red", alpha = .9)

# View change of party overtime
manifesto %>% group_by(party, partyname) %>% summarise(diff = range(rile)) %>% arrange(desc(diff))
manifesto %>% filter(party == 61320) %>%
  ggplot(aes(date, rile)) + geom_line() +
  geom_hline(yintercept = 0, color="red")

manifesto.scores %>% arrange(polarization)

dpi_manifesto %>% group_by(countryname) %>%
  ggplot(aes(x = year)) + 
  geom_point(aes(y = dpi.polarization), color="blue", alpha=.5) +
  geom_line(aes(y = polarization), color="red", alpha = .5) +
  facet_wrap(~countryname)

