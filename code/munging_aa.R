library(tidyverse)
library(janitor)
library(sf)
library(spData)
library(skimr)
library(knitr)
library(naniar)
library(GGally)
library(styler)
library(ggtext)
library(data.table)
library(grid)
library(plotly)
library(ggplot2)
library("scales")
library(timeDate)
library(ggiraph)
library(ggridges)
library(ggimage)


# Read in olympic dataset
olympic_raw <- read_csv("data/athlete_events.csv") |> clean_names() |> 
  filter(season == "Summer")

olympic_raw <- olympic_raw |> mutate(medal = case_when(
  is.na(medal) ~ "No Medal",
  TRUE ~ medal
))

# Read in NOC regions
noc_regions <- read_csv("data/noc_regions.csv") |> clean_names()

# Get countries by joining both
olympic_regions <- olympic_raw |> left_join(noc_regions , by = "noc")
olympic_regions <- olympic_regions |> rename(country = region)



# Match country names manually as per the ones in world data
world_data = world


world_data <- world_data |> mutate(country = case_when(
  name_long == "United States" ~"USA" ,
  name_long == "Russian Federation" ~ "Russia",
  name_long =="Bolivia" ~ "Boliva",
  name_long =="The Gambia" ~ "Gambia",
  name_long =="Republic of Korea" ~ "South Korea",
  name_long =="Northern Cyprus" ~ "Cyprus",
  name_long =="Republic of the Congo" ~ "Democratic Republic of the Congo",
  name_long =="Dem. Rep. Korea" ~ "North Korea",
  name_long =="United Kingdom" ~ "UK",
  name_long =="Somaliland" ~ "Somalia",
  name_long =="Trinidad and Tobago" ~ "Trinidad",
  TRUE ~ name_long
))


olympic <- olympic_regions |> mutate(host_country = case_when(
  city == "Rio de Janeiro" ~ "Brazil",
  city == "London" ~ "UK",
  city == "Beijing" ~  "China",
  city == "Athina" ~  "Greece",
  city == "Sydney" | city == "Melbourne" ~  "Australia",
  city == "Atlanta" | city == "Los Angeles" | city == "St. Louis" ~  "USA",
  city == "Barcelona" ~  "Spain",
  city == "Seoul" ~  "South Korea",
  city == "Moskva" ~  "Russia",
  city == "Montreal" ~  "Canada",
  city == "Munich" | city == "Berlin" ~  "Germany",
  city == "Mexico City" ~  "Mexico",
  city == "Tokyo" ~  "Japan",
  city == "Roma" ~  "Italy",
  city == "Paris" ~  "France",
  city == "Helsinki" ~  "Finland",
  city == "Amsterdam" ~  "Netherlands",
  city == "Antwerpen" ~  "Belgium",
  city == "Stockholm" ~  "Sweden",
  TRUE ~ "Other")
  )

olympic2 <- olympic |> mutate(bmi = (weight/(height*height))*10000)




write.csv(olympic2, file = "data_munged/munged_olympic_avi.csv",
          row.names = FALSE, na="")
