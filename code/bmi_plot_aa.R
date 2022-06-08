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



age_bmi <- olympic2 |> group_by(sport,sex) |> 
  summarise(mean_age = mean(age,na.rm = TRUE),
            mean_bmi = mean(bmi,na.rm = TRUE)) |> 
  ungroup() |> mutate(sex = case_when(
    sex == "F" ~ "Female",
    sex == "M" ~ "Male"
  ))
age_bmi <- na.omit(age_bmi)

# age_bmi <- age_bmi |> filter(sport %in% c("Wrestling",
#                                           "Weightlifting",
#                                           "Volleyball",
#                                           "Water Polo",
#                                           "Triathlon"))


country_bmi <- read_csv("data/avg_bmi.csv")


country_bmi <- country_bmi |> pivot_longer(cols =-Country,names_to = "sex")


bmi_plot <- ggplot() + geom_point(
  data = age_bmi,
  mapping = aes(
    x = mean_age,
    y = mean_bmi,
    color = sport
  )
)+
  facet_wrap(~sex)+
  theme_bw()+
  theme(
    plot.background = element_rect(fill = "#f0f0f0"),
    panel.background = element_rect(fill = "#f0f0f0",
                                    colour = "#f0f0f0",
                                    size = 0.5, linetype = "solid"),
    legend.key = element_rect(fill = "transparent"),
    strip.background =element_rect(fill="#efedf5"))+
  labs(
    x = "Mean Age of Participants",
    y = "Mean BMI of Participants"
  )

bmi_plot <- ggplotly(bmi_plot) |> 
  layout(title = list(text = paste0('<b>Comparing avergae Age and 
                                    BMI of every sport</b><br>')), 
         title_x=0.5,
         margin = list(l = 0, r = 0,
                       b = 30, t = 80,
                       pad = 0)) |>
  layout(legend = list(bgcolor = '#f0f0f0')) |> 
  layout(annotations = 
           list(x = 1.4, y = -0.13, text = "*Scroll legend to see all", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=12, color="#756bb1"))
  ) |> 
  config(displayModeBar = FALSE)

bmi_plot

htmlwidgets::saveWidget(as_widget(bmi_plot), "plots/bmi_aa.html")
