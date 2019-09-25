
library(dplyr)
library(rjson)
library(data.table)
#df <- read.csv('data/dataframe_2015_small.csv', header = TRUE, sep = ',')
#saveRDS(df, file="data/dataframe_2015.rds")

#df <- read.csv('data/dataframe_2011.csv', header = TRUE, sep = ',')
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
  Turnout = round((Total/(Total_Number_People_x*(1-Fraction_0.14)))*100, digits =1),
  Uneducated = round(Fraction_Without_Education*100, digits = 1),
  Young = round(Fraction_0.14*100, digits = 1)
)

names(sociotable)[names(sociotable) == 'Total_Number_People_x'] <- 'Population'
names(sociotable)[names(sociotable) == 'Total_Average_income'] <- 'Average Income'
names(sociotable)[names(sociotable) == 'Unemployment_.Rate'] <- 'Unemployment Rate'
names(sociotable)[names(sociotable) == 'Uneducated'] <- 'Uneducated Population'
names(sociotable)[names(sociotable) == 'Young'] <- 'Young Population'


sociotable <- select (sociotable,-c(X,NC,PAN, Total, Winning_Party, Winning_Party_Fraction_Votes,Total_65.,
                                     Fraction_15.65, Fraction_65.,Total_Number_People_y,
                                    Without_Education, Basic_First_Cycle, Basic_Second_Cycle, Basic_Third_Cycle,
                                    Secondary, Superior, Fraction_Without_Education, Fraction_Basic_First_Cycle,
                                    Fraction_Basic_Second_Cycle, Fraction_Basic_Third_Cycle, Fraction_Secondary,
                                    Fraction_Superior,Unemployment_Rate, Coordinates, Total_0.14, Total_15.65, x, y ))

sociotable <- sociotable[c("Municipality", "BE", "PCP.PEV", "PPD.PSD.CDS.PP", "PS", "Others", "Turnout",
                           "Population", "Average Income", "Unemployment Rate", "Uneducated Population",'Young Population' )]


## Preparing a table for the percentage points difference btw. 2011 and 2015
df_diff <- df_2015 [c("Municipality", "BE", "PCP.PEV", "PPD.PSD.CDS.PP", "PS", "Others")]
names(df_diff)[names(df_diff) == 'PPD.PSD.CDS.PP'] <- 'PSD'
names(df_diff)[names(df_diff) == 'PCP.PEV'] <- 'PCP'
df_diff$BE <- (df_2015$BE/df_2015$Total - df_2011$BE/df_2011$Total)*100
df_diff$PCP <- (df_2015$PCP.PEV/df_2015$Total - df_2011$PCP.PEV/df_2011$Total)*100
df_diff$PSD <- (df_2015$PPD.PSD.CDS.PP/df_2015$Total - (df_2011$CDS.PP + df_2011$PPD.PSD)/df_2011$Total)*100
df_diff$PS <- (df_2015$PS/df_2015$Total - df_2011$PS/df_2011$Total)*100
df_diff$Others <- ((df_2015$Others+df_2015$NC+df_2015$PAN)/df_2015$Total - (df_2011$Others+df_2011$PAN)/df_2011$Total)*100

geojson <- readLines("data/portugal_municipios_small.geojson") %>%
  paste(collapse = "\n")  %>%
  fromJSON()

######### party explorer tab ############

parties <- list("BE", "PCP.PEV", "PPD/PSD.CDS.PP", "PS", "Others")

copy_cleantable <- copy(cleantable)
party_columns <- c("Municipality", "BE", "PCP.PEV", "PPD.PSD.CDS.PP", "PS", "Others", "Winning_Party")
pieplot_values <- colSums(cleantable[,c("BE", "PCP.PEV", "PPD.PSD.CDS.PP", "PS", "Others")])

results_table <- table(copy_cleantable[c("Winning_Party")])
results_table <- data.frame(data=results_table)
names(results_table)<-c("Party","Municipalty Victories")

df_temp<-data.frame("BE","0")
names(df_temp)<-c("Party","Municipalty Victories")
results_table <- rbind(results_table, df_temp)

df_temp<-data.frame("Others","0")
names(df_temp)<-c("Party","Municipalty Victories")
results_table <- rbind(results_table, df_temp)

results_table$`Municipalty Victories` <- sapply(results_table$`Municipalty Victories`, as.numeric)
votes <- list(pieplot_values[[2]], pieplot_values[[3]], pieplot_values[[4]], pieplot_values[[1]], pieplot_values[[5]])

results_table$`Votes` <- votes
results_table$`Votes` <- sapply(results_table$`Votes`, as.numeric)


total_values <- colSums(results_table[,c("Votes","Municipalty Victories")])

results_table$Party <- sapply(results_table$Party, as.character)
total_values_list <- list("Total", total_values[2], total_values[1])

results_table[nrow(results_table) + 1,] = total_values_list

results_table$`%Voting` <- results_table$`Votes` * 100 / results_table[6,3] 
results_table$`%Voting` <- round(results_table$`%Voting`, digits = 1)

###### Results table 2011 ######

results_2011 <- copy(df_2011)

results_2011$Others <- df_2011$Others + df_2011$PAN
results_2011$`PPD/PSD.CDS.PP` <- df_2011$CDS.PP + df_2011$PPD.PSD
results_2011$PS <- df_2011$PS 
results_2011$PCP.PEV <- df_2011$PCP.PEV 
results_2011$BE <- df_2011$BE

columns_2011 <- c('Others', 'PPD/PSD.CDS.PP', 'PS', 'PCP.PEV', 'BE', 'Total')
results_2011 <- results_2011[, columns_2011]
results_2011 <- sapply(results_2011, as.numeric)

column_sums_2011 <- colSums(results_2011)
total_votes_2011 <- column_sums_2011[[6]]

Others <- round(column_sums_2011[[1]] / total_votes_2011 * 100 , digits = 1)
`PPD/PSD.CDS.PP` <- round(column_sums_2011[[2]] / total_votes_2011 * 100 , digits = 1)
PS <- round(column_sums_2011[[3]] / total_votes_2011 * 100 , digits = 1)
`PCP.PEV` <- round(column_sums_2011[[4]] / total_votes_2011 * 100 , digits = 1)
BE <- round(column_sums_2011[[5]] / total_votes_2011 * 100 , digits = 1)
total_votes_2011 <- round(total_votes_2011 , digits = 0)

results_2011 <- data.frame(Party = c('PCP-PEV', 'PPD/PSD.CDS-PP', 'PS', 'BE', 'Others', 'Total'))
results_2011$`Voting%` <- c(`PCP.PEV`, `PPD/PSD.CDS.PP`, PS, BE, Others, total_votes_2011)

results_2011$diff2015 <- results_table$`%Voting` - results_2011$`Voting%` 
results_2011[6,3] <- (results_table[6,3] - results_2011[6,3]) / results_table[6,3]
results_2011$diff2015 <- round(results_2011$diff2015, digit=1)

results_2011$Party <- sapply(results_2011$Party, as.character)
results_2011[6,1] <- "Total Votes"
