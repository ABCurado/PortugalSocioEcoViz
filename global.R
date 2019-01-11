library(dplyr)


#df <- read.csv('data/dataframe_2015.csv', header = TRUE, sep = ',')
#read.csv('data/dataframe_2015.csv', header = TRUE, sep = ',')
#saveRDS(df, file="data/dataframe_2015.rds")

#df <- read.csv('data/dataframe_2011.csv', header = TRUE, sep = ',')
#read.csv('data/dataframe_2011.csv', header = TRUE, sep = ',')
#saveRDS(df, file="data/dataframe_2011.rds")

df_2011 <- readRDS("data/dataframe_2011.rds")
df_2015 <- readRDS("data/dataframe_2015.rds")


allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )
