## import manifesto project data, ignore missing values, standardize rile pervote and convert election date
# manifesto_df = pd.read_csv("MPDataset_MPDS2019b.csv")
# needed_columns = ['edate', 'countryname', 'country', 'pervote', 'rile', 'party', 'partyabbrev']
# manifesto_df = manifesto_df.dropna(subset=needed_columns)
# manifesto_df["zrile"] = stats.zscore(df["rile"])
# manifesto_df["pervote"] = manifesto_df["pervote"] / 100
# manifesto_df["year"] = pd.to_datetime(manifesto_df["edate"]).dt.year
setwd("~/Documents/manifestos/")

manifesto.df <- read.csv("MPDataset_MPDS2019b.csv")
needed.columns <- c('edate', 'countryname', 'pervote', 'rile', 'party', 'partyabbrev')
s <- na.omit(manifesto.df, cols="countryname")
