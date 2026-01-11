#set pathway
setwd("~/Downloads/TJFV5-SOCS0100-Assignment-2")

#load packages required
if (!require("pacman")) {
  install.packages("pacman")
}
library(pacman)
pacman::p_load(
  kableExtra,
  flextable,
  skimr,
  ggplot2,
  dplyr,
  purrr,
  tidyverse,
  gridExtra,
  httr,
  jsonlite,
  janitor
)

#World Bank Data360 API
url <- "https://data360api.worldbank.org/data360/data"

#function to retrieve and filter data from API
retrieve_data <- function(query_list){
  resp <- GET(url, query = query_list, accept("application/json"))
  stop_for_status(resp)
  raw_json <- content(resp, as = "text",encoding = "UTF-8")
  parsed <- fromJSON(raw_json, flatten = FALSE)
  df<-tibble(parsed$value)
  
  df <- subset(df, select = c("TIME_PERIOD","OBS_VALUE"))%>%
    rename("Year" = "TIME_PERIOD")%>%
    arrange(desc(Year))
  return(df)
}

#list of indicators required
q1 <- list(
  database_ID = "WB_WDI",
  indicator = "WB_WDI_SE_XPD_TOTL_GD_ZS", #government expenditure on education (% of GDP)
  REF_AREA = "SGP"
)

q2 <- list(
  database_ID = "WB_WDI",
  indicator = "WB_WDI_NY_GDP_PCAP_KN", #GDP per capita (annual % growth)
  REF_AREA = "SGP"
)
q3 <- list(
  database_ID = "WB_WBL",
  indicator = "WB_WBL_SG_LAW_INDX", #WBL Index
  REF_AREA = "SGP"
)
q4 <- list(
  database_ID = "WB_WDI",
  indicator = "WB_WDI_SP_POP_TOTL", #Total population
  REF_AREA = "SGP"
)

#retrieving data from API
ee_df <- retrieve_data(q1)%>%
  rename("Education Expenditure (% of GDP)" = "OBS_VALUE")
gdp_df <- retrieve_data(q2)%>%
  rename("GDP per capita (constant LCU)" = "OBS_VALUE")
women_df <- retrieve_data(q3)%>%
  rename("WBL Index Score" = "OBS_VALUE")
population_df <- retrieve_data(q4)%>%
  rename("Total Population" = "OBS_VALUE")

data360 <- merge(ee_df,gdp_df,by="Year")
data360 <- merge(data360,women_df,by="Year")
data360 <- merge(data360,population_df,by="Year")

#data wrangling
str(data360) #understanding the structure of the dataset currently

data360 <- data360 %>%
  mutate_all(as.numeric) %>% #this changes the column vectors from character to double
  mutate("Education Spending per Capita"= (`Education Expenditure (% of GDP)`/100)*`GDP per capita (constant LCU)`)
view(data360)

#store singstat indicator into data frame
singstat_indicators <- read.csv("singstat-raw.csv")
str(singstat_indicators)
view(singstat_indicators)

#data wrangling process for singstat indicators
singstat_indicators <- singstat_indicators%>%
  slice(10:16)%>%
  mutate(across(where(is.character),~na_if(.,"na")))%>%
  mutate(across(2:63,as.double))

#Tidying names
singstat_indicators <- t(singstat_indicators)%>%
  as.data.frame(singstat_indicators)%>%
  janitor::row_to_names(row_number = 1)%>%
  rename(Year=`Data Series`)

#Tidying row names
rownames(singstat_indicators) <- NULL

#Retaining relevant rows
singstat_indicators <- subset(singstat_indicators,select=c(1:4))

singstat_indicators <- singstat_indicators%>%
  mutate(`Year` = as.integer(`Year`))

view(singstat_indicators)

#Merging dataframes
merged_df <- merge(data360,singstat_indicators,by="Year")
merged_df <- merged_df %>%
  slice(15:35)

view(merged_df)
str(merged_df)

#Creating wrangling dataframe .csv file
write.csv(merged_df,file = "education-expenditure-and-gender-gap.csv",row.names = FALSE)
