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
olympic2 <- read_csv("data_munged/munged_olympic_avi.csv") |> 
  clean_names() |> 
  filter(season == "Summer")


# Summer participants by year
total_participants <- olympic2 |> 
  group_by(country) |> tally() |> 
  rename(participants_count = n)


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

# Join with world data to get latitude and longitude for countries
total_participants <- total_participants |> 
  left_join(world_data , by = "country")


total_participants <- total_participants |>
  mutate(tooltip_text = paste0(toupper(name_long), "\n", participants_count ))


total_participants <-  total_participants |> 
  rename(Participants = participants_count)

# Making world data map

# To Do - Make this for every year and animate, 
#change color as per the subtitile

x = total_participants  |> 
  ggplot() +
  geom_sf_interactive(aes(
    geometry = geom,
    fill = Participants,
    data_id = name_long,
    tooltip =tooltip_text
  )) +
  viridis :: scale_fill_viridis('Participants', 
                                option='magma', 
                                trans = "sqrt",
                                labels = scales::label_number_si())+
  theme_classic()+
  labs(
    title = paste0("**Which *country* has the most participation 
    in *Olympics* history**<br>
    <span style='font-size:11pt'>Count is a sequential scale where
    <span style='color:#57106e;'>lower</span> to
    <span style='color:#f98e09;'>higher</span> values are represented
    </span>"),
    # caption = "<span style='font-size:8pt'>Data:
    # <span style='color:#756bb1;'>World Olympics Data</span> <br> Graph by:
    # <span style='color:#756bb1;'>Avi Arora</span>
    # </span>",
    shape="Admission in hospital",
    color='NEW LEGEND TITLE')+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        plot.caption = element_markdown(lineheight = 1.1),
        legend.position="bottom",
        legend.key.width = unit(1.2, "cm"),
        plot.background = element_rect(fill = "#f0f0f0"),
        panel.background = element_rect(fill = "#f0f0f0",
                                        colour = "#f0f0f0",
                                        size = 0.5, linetype = "solid"),
        legend.key = element_rect(fill = "transparent"))

x
x = girafe(ggobj = x , width_svg = 6, height_svg = 4) |>
  girafe_options(opts_hover(css = "fill:cyan;"))
x


htmlwidgets::saveWidget(x, "plots/country_participation_aa.html", 
                        selfcontained = TRUE)
