 # 1. Clear the environment
rm(list = ls(all = TRUE))

#2 use tidycensus to import data
library(tidycensus)
census_api_key("afc631566ccac6e269614a9c9906d9e42d3ccf83",
               install = TRUE,
               overwrite = TRUE
)
#finding variables
v15 <- load_variables(year = 2015,
                      "acs5")
#importing/ adding a year column
inequality_panel <- get_acs(geography = "state",
                            variables = c(gini = "B19083_001"),
                            year = 2010
)
inequality_panel$year = 2010
inequality_panel2 <- get_acs(geography = "state",
                             variables = c(gini = "B19083_001"),
                             year = 2015)
inequality_panel2$year = 2015
#binding
library(tidyverse)
suppressMessages(library(bit64))
inequality_panel <- bind_rows(inequality_panel,inequality_panel2)

#rename estimate as gini
library(data.table)
setnames(inequality_panel,"estimate", "gini" )

#rename name to state
library(data.table)
setnames(inequality_panel,"NAME", "state" )

#Create a year variable
#done already through introducing a year column and by binding the rows 

#take a peek
head(inequality_panel)

# 3 Reshape wide 
inequality_wide <-
  inequality_panel %>% 
  pivot_wider(id_cols = c("state","GEOID"),
              names_from = "year",
              values_from = "gini",
              names_prefix = "year_")
#quick peek at the data
head(inequality_wide)

# 4 reshape to long format
inequality_long <-
  inequality_wide %>%
  pivot_longer(cols = starts_with("year"), #use columns that start with year
               names_to="year", #name of the new column
               names_prefix = "year_", #part of string to drop
               values_to = "gini", # where to put numeric values
               values_drop_na = FALSE) %>% #don't drop NA's
  filter(!(gini == 0)) 

#take a quick peek
head(inequality_long)

#5 show same number of obs
count(inequality_long) == count(inequality_panel)

#6. collapse the inequality long dataframe 
library(tidyverse)
inequality_collapsed <- inequality_long %>%
  group_by(state, GEOID) %>% 
  summarize(gini = mean(gini))

#7 produce a map of the US that will 
# Prepping the US graph
  #load packages
library(devtools)
library(remotes)
library(rio)
library(tidyverse)
library(googlesheets4)
library(labelled)
library(data.table)
library(varhandle)
library(ggrepel)
library(geosphere)
library(rgeos)
library(viridis)
library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(raster)
library(sp)
library(sf)
library(Imap)

#load the US borders file
us_borders <- st_read("C:/Users/rohin/Documents/Summer2020/DataScience/Exam3/EXAM2/cb_2018_us_state_500k.shp")
head(us_borders)
# transform to WGS 84 format
borders <- st_transform(us_borders,"+proj=latlong +ellps=WGS84 +datum=WGS84")
rm(us_borders)
#converting df to sf
inequality_collapsed_sf = st_sf(inequality_collapsed)
# rename resource to capital resource
#plot
US_map = ggplot() +
  geom_sf(data = borders) +
  geom_sf(data = inequality_collapsed, aes(inequality_c)) +
  scale_fill_viridis(option = "viridis") +
  ggtitle("Mean gini coeffcient per state") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_void()


### 8. import GDP in current USD
library(WDI)
current_GDP =WDI(country = "all", indicator =c("NY.GDP.MKTP.CD"),
                   start = 2006,# start of foreign aid data
                   end = 2007,# end of of foreign aid data
                   extra = FALSE, cache = NULL)

#rename the variable
library(data.table)
setnames(current_GDP,"NY.GDP.MKTP.CD","gdp_current")

### 9.  deflate gdp current
#import deflator data
deflator_data =WDI(country = "all", indicator =c("NY.GDP.DEFL.ZS"),
                   start = 2006,
                   end = 2007,
                   extra = FALSE, cache = NULL)
#change name

setnames(deflator_data,"NY.GDP.DEFL.ZS", "deflator")
#chose 2015 because the deflator is in base year (100)
#only need the US deflator since it is in USD
usd_deflator = subset(deflator_data, country=="United States")
setnames(usd_deflator,"NY.GDP.DEFL.ZS", "deflator")
#remove uncessary columns 
rm(deflator_data)
usd_deflator$iso2c <- NULL
usd_deflator$country <- NULL
# merge
deflated_data= left_join(current_GDP,
                         usd_deflator,
                         by=c("year"))
#do the deflation
deflated_data$gdp_deflated = deflated_data$gdp_current/
  (deflated_data$deflator/100)

head(deflated_data)

### 10. parts to a server
### 11. pull a pdf
library(pdftools)
mytext =pdf_text(pdf = "https://pdf.usaid.gov/pdf_docs/PA00TNMG.pdf")
mytext

### 12. convert to a df
armeniatext=as.data.frame(mytext, stringsAsFactors = FALSE)
armeniatext$page = c(1:65)

### 13. tokenize and remove stop words
library(tidytext)
data(stop_words)
armeniatext <-  armeniatext %>% 
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

## 14. figure out top 5 words
hpfreq <- armeniatext %>%
  count(word, sort = TRUE)
head(hqfreq)

##15 load up hot 100
#loading packages
library(rvest)
library(dplyr)

## load thelist hot100 exam
hot100page <- "https://www.billboard.com/charts/hot-100"
hot100exam <- read_html(hot100page)

##16 identify all of the nodes in the webpage
#see the nodes
body_nodes <-  hot100exam %>% 
  html_node("body") %>% 
  html_children()
# go further
body_nodes %>% 
  html_children()

#17 pull the data
rank <- hot100exam %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//span[contains(@class,
'chart-element__rank__number')]") %>%
  rvest::html_text()
artist <- hot100exam %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//span[contains(@class,
'chart-element__information__artist')]") %>%
  rvest::html_text()
title <- hot100exam %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//span[contains(@class,
'chart-element__information__song')]") %>%
  rvest::html_text()
Last_Week <- hot100exam %>% 
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//span[contains(@class,
'chart-element__meta__text')]") %>%
  rvest::html_text()

