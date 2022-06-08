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





# -------------------------


athlete_performance <- olympic2 |> 
  group_by(name,medal) |> tally()

athlete_participation <- athlete_performance |> 
  group_by(name) |> summarise(events_participation = sum(n))



athlete_medals <- athlete_performance |> filter(medal != "No Medal") |> 
  group_by(name) |> summarise(medal_count = sum(n))

athlete_performance2 <- athlete_medals |> 
  left_join(athlete_participation , by = "name")


athlete_performance2 <- athlete_performance2 |> 
  mutate(win_percent = medal_count/events_participation)




athelete_country <- olympic2 |> select(name,country,sport,sex)

athlete_performance3 <- athlete_performance2 |> 
  left_join(athelete_country , by = "name") |> 
  filter(events_participation >7)

athlete_performance3 <- athlete_performance3 |> distinct()





athlete_plot <- ggplot()+
  geom_point(data = athlete_performance3,
             mapping = aes(
               x = events_participation,
               y = medal_count,
               color = sex,
               text = paste("Name:", name,"\nCountry:",country,"\nSport:",sport)
             ),show.legend = FALSE)+
  geom_rect(data=athlete_performance3,
            aes(xmin=7,
                xmax=60,
                ymin=0,
                ymax=10),
            fill="grey",alpha = 0.5) +
  theme_classic()+
  scale_color_manual(values = c("F" = "#fc8d62", "M" = "#7570b3"))+
  theme(
    plot.background = element_rect(fill = "#f0f0f0"),
    panel.background = element_rect(fill = "#f0f0f0",
                                    colour = "#f0f0f0",
                                    size = 0.5, linetype = "solid"),
    legend.key = element_rect(fill = "transparent"),
    strip.background =element_rect(fill="#efedf5"))+
  labs(
    y = "Count of Medals",
    x = "No. of events participated"
  )
# geom_image(size = 0.13,data =athlete_performance2 |>
#              filter(name == "Michael Fred Phelps, II"),
#            image = "images/Picture1.png",aes(
#              x = events_participation,
#              y = medal_count
#            ))+


athlete_plot


athlete_plot <- ggplotly(athlete_plot) |> 
  layout(annotations = list(x = 38, y = 25, 
                            text = "Domination of Micheal Phelps", 
                            showarrow = F)) |> 
  add_annotations(
    x = 30.5,
    y = 27.7,
    xref = "x",
    yref = "y",
    axref = "x",
    ayref = "y",
    text = "",
    showarrow = TRUE,
    arrowhead = 1,
    arrowsize = 1,
    ax = 33,
    ay = 26) |> 
  layout(annotations = list(x = 36.5, y = 18, 
                            text = "Highest Performing Female Athlete : Larysa", 
                            showarrow = F)) |> 
  add_annotations(
    x = 19.8,
    y = 18,
    xref = "x",
    yref = "y",
    axref = "x",
    ayref = "y",
    text = "",
    showarrow = TRUE,
    arrowhead = 1,
    arrowsize = 1,
    ax = 23,
    ay = 18) |> 
  layout(title = list(text = 
                        paste0('<b>Which are the highest performing athletes of 
                               all times?</b><br>','<sup>',
                               '<span style= "font-size:12pt">Split by',' 
                               <span style= "color:#7570b3;">Male</span> and ','
                               <span style="color:#fc8d62;">','Female</span> 
                               athletes</sup></span>')), 
         title_x=0.5,
         margin = list(l = 0, r = 0,
                       b = 0, t = 50,
                       pad = 0)) |>
  config(displayModeBar = FALSE) |> 
  layout(showlegend = FALSE)
athlete_plot

htmlwidgets::saveWidget(as_widget(athlete_plot), "plots/athlete_plot_aa.html")
