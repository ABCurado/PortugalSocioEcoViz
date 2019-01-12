library(dplyr)
library(rjson)
#df <- read.csv('data/dataframe_2015.csv', header = TRUE, sep = ',')
#read.csv('data/dataframe_2015.csv', header = TRUE, sep = ',')
#saveRDS(df, file="data/dataframe_2015.rds")

#df <- read.csv('data/dataframe_2011.csv', header = TRUE, sep = ',')
#read.csv('data/dataframe_2011.csv', header = TRUE, sep = ',')
#saveRDS(df, file="data/dataframe_2011.rds")

df_2011 <- readRDS("data/dataframe_2011.rds")
df_2015 <- readRDS("data/dataframe_2015.rds")
sapply(df_2015, class)

cleantable <- df_2015

geojson <- readLines("data/portugal_municipios.geojson") %>%
  paste(collapse = "\n")  %>%
  fromJSON()

#geojson2 <- geojsonio::geojson_read("data/portugal_municipios.geojson",
#                              what = "sp")

