######################################
# Add all the required Libraries
######################################

library(shiny) # Shiny Library
library(plotly) # Plotly graphing librarry
library(highcharter) # Highcharter Libarry
library(shinydashboard) # Shiny Dashboard LIbrary
library(data.tree) # For data tables
library(treemap)
library(leaflet) # For maps and Choropleth
library(stringr)
library(shinyWidgets) # Shiny Widgets
library(dplyr) # Data Manipulation Library
library(shinythemes) # Shiny App themes


######################################
# Read the dataset
######################################
cities_dataset <-
  read.csv("data/cities_r2.csv")

######################################
# Arrange and group by state
######################################
state_group <-
  cities_dataset %>% group_by(state_name) %>% summarise(
    Total = n(),
    Population = sum(population_total),
    Male_Population = sum(population_male),
    Female_Population = sum(population_female),
    Male_Percent = Male_Population / Population * 100,
    Female_Percent = Female_Population / Population * 100,
    Male_Population_Mean = Male_Population / Total,
    Female_Population_Mean = Female_Population / Total,
    Literates = sum(literates_total),
    Male_Literates = sum(literates_male),
    Female_Literates = sum(literates_female),
    Male_Literates_Percent = Male_Literates / Male_Population * 100,
    Female_Literates_Percent = Female_Literates / Female_Population * 100,
    Literates_Percent = Literates / Population * 100,
    Graduates = sum(total_graduates),
    Male_Grads = sum(male_graduates),
    Female_Grads = sum(female_graduates),
    Grads_Percent = Graduates / Population * 100,
    Male_Grads_Percent = Male_Grads / Male_Population * 100,
    Female_Grads_Percent = Female_Grads / Female_Population * 100,
    Male_Grads_Mean = Male_Grads / Total,
    Female_Grads_Mean = Female_Grads / Total,
    State_Code = sum(state_code) / Total,
    Sex_Ratio = sum(sex_ratio) / Total,
    Child_Sex_Ratio = sum(child_sex_ratio) / Total
  ) %>% arrange(desc(Total))

state_group$state_name <-
  as.character(state_group$state_name)

######################################
# Download India Map Geoson file
######################################
mapdata <-
  get_data_from_map(download_map_data("countries/in/custom/in-all-andaman-and-nicobar"))

######################################
# Correcting the data to match the data frames
######################################
mapdata$name <-
  ifelse(mapdata$name == "Andaman and Nicobar",
         "Andaman & Nicobar Islands",
         mapdata$name)
mapdata$name <-
  ifelse(mapdata$name == "Jammu and Kashmir",
         "Jammu & Kashmir",
         mapdata$name)
mapdata$name <-
  ifelse(mapdata$name == "Uttaranchal", "UTTARAKHAND", mapdata$name)
mapdata$name <-
  ifelse(mapdata$name == "Delhi", "NCT OF DELHI", mapdata$name)
mapdata$name <-
  ifelse(mapdata$name == "Manipur", "MPUR", mapdata$name)
state_group$state_name <-
  ifelse(
    state_group$state_name == "MANIPUR ",
    "MPUR",
    as.character(state_group$state_name)
  )
state_group$state_name <- as.factor(state_group$state_name)

# Get the codes for all the states
hcmap.state_codes <-
  dplyr::select(filter(
    mapdata,
    tolower(mapdata$name) %in% tolower(state_group$state_name)
  ), c("hc-a2", "name"))

hcmap.state_codes$name <- toupper(hcmap.state_codes$name)

# Merge the codes with the cities dataset
cities_dataset.merge <-
  merge(state_group,
        hcmap.state_codes,
        by.x = "state_name",
        by.y = "name")
cities_dataset.merge$state_name <-
  ifelse(
    cities_dataset.merge$state_name == "MPUR ",
    "MANIPUR",
    as.character(cities_dataset.merge$state_name)
  )

cities_dataset.merge$state_name <-
  as.factor(cities_dataset.merge$state_name)

state_group$state_name <-
  ifelse(
    state_group$state_name == "MPUR ",
    "MANIPUR",
    as.character(state_group$state_name)
  )

state_group$state_name <- as.factor(state_group$state_name)


lon.lng.split <- str_split_fixed(cities_dataset$location, ",", 2)
cities_dataset$Longitude <- as.numeric(lon.lng.split[, 1])
cities_dataset$Latitude <- as.numeric(lon.lng.split[, 2])

spllitted_cities <- split(cities_dataset, cities_dataset$state_code)

by_state_order <-
  state_group[order(state_group$state_name), ]




