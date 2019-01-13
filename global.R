library(dplyr)
library(rjson)
library(data.table)
#df <- read.csv('data/dataframe_2015.csv', header = TRUE, sep = ',')
#read.csv('data/dataframe_2015.csv', header = TRUE, sep = ',')
#saveRDS(df, file="data/dataframe_2015.rds")

#df <- read.csv('data/dataframe_2011.csv', header = TRUE, sep = ',')
#read.csv('data/dataframe_2011.csv', header = TRUE, sep = ',')
#saveRDS(df, file="data/dataframe_2011.rds")

df_2011 <- readRDS("data/dataframe_2011.rds")
df_2015 <- readRDS("data/dataframe_2015.rds")
sapply(df_2015, class)
df_2015$Municipality <- as.character(df_2015$Municipality)
Encoding(df_2015$Municipality) <- "UTF-8"
cleantable <- df_2015


## Preparing the data for the Socio-Economic Table
## 1. Creating percentages of votes
## 2. Calculating the Turnout
## 3. Renaming
## 4. Dropping unnecessary columns
sociotable <- copy(cleantable)
sociotable <- sociotable %>% mutate(
  BE = round((BE/Total)*100, digits=1),
  PCP.PEV = round((PCP.PEV/Total)*100, digits =1),
  PPD.PSD.CDS.PP = round((PPD.PSD.CDS.PP/Total)*100, digits =1),
  PS = round((PS/Total)*100, digits =1),
  Others = round(((Others+NC+PAN)/Total)*100, digits =1),
  Turnout = round((Total/Total_Number_People_x)*100, digits =1),
  Uneducated = round(Fraction_Without_Education*100, digits = 1)
)

names(sociotable)[names(sociotable) == 'Total_Number_People_x'] <- 'Population'
names(sociotable)[names(sociotable) == 'Total_Average_income'] <- 'Average Income'
names(sociotable)[names(sociotable) == 'Unemployment_.Rate'] <- 'Unemployment Rate'
names(sociotable)[names(sociotable) == 'Uneducated'] <- 'Uneducated Population'


sociotable <- select (sociotable,-c(X,NC,PAN, Total, Winning_Party, Winning_Party_Fraction_Votes,Total_65.,
                                    Fraction_0.14, Fraction_15.65, Fraction_65.,Total_Number_People_y,
                                    Without_Education, Basic_First_Cycle, Basic_Second_Cycle, Basic_Third_Cycle,
                                    Secondary, Superior, Fraction_Without_Education, Fraction_Basic_First_Cycle,
                                    Fraction_Basic_Second_Cycle, Fraction_Basic_Third_Cycle, Fraction_Secondary,
                                    Fraction_Superior,Unemployment_Rate, Coordinates, Total_0.14, Total_15.65 ))



geojson <- readLines("data/portugal_municipios.geojson") %>%
  paste(collapse = "\n")  %>%
  fromJSON()

#geojson2 <- geojsonio::geojson_read("data/portugal_municipios.geojson",
#                              what = "sp")

