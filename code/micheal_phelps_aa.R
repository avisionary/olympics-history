
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

# -----------------------------

# Domination of Micheal Phelps

# Present in only 2008, 2012, 2016

dom_graph <- olympic2 |> filter(year >= 2004)|>
  filter(medal != "No Medal") |> 
  select(country,medal,event,year) |> 
  distinct() |> 
  group_by(country,medal) |> tally()

Micheal_phelps <- olympic2 |> filter(name == "Michael Fred Phelps, II")|>
  filter(medal != "No Medal") |> 
  group_by(name,medal) |> tally() |> 
  mutate(name = "Micheal Phelps") |> 
  rename(country = name)



dom_graph <- rbind(dom_graph,Micheal_phelps)
dom_graph_plot <- dom_graph |> group_by(country) |> 
  mutate(total_medal = sum(n))  |> 
  filter(medal == "Gold") |> 
  arrange(desc(n))

dom_graph_plot <- head(dom_graph_plot,20)
dom_graph_plot <- dom_graph_plot |> mutate(color = case_when(
  country == "USA" ~ "gold",
  country == "China" ~ "silver",
  country == "Russia" ~ "bronze",
  country == "Micheal Phelps" ~ "mi",
  TRUE ~ "other"
))




micheal_phelps <- ggplot()+
  geom_bar(
    data =dom_graph_plot,
    mapping = aes(
      y = reorder(country,n),
      x = n,
      fill = color
    ),
    position = "stack",
    stat = "identity",
    show.legend = FALSE
  )+
  # geom_rect(data=athlete_performance3,
  #           aes(xmin=0,
  #               xmax=Inf,
  #               ymin=6.5,
  #               ymax=7.5),
  #           fill="grey",alpha = 0.03)+
  annotate("rect", 
           xmin = 0, xmax = Inf, 
           ymin = 6.5, ymax = 7.5, 
           fill = "steelblue", alpha = 0.2) +
  geom_image(data =dom_graph_plot |>
               filter(country == "Micheal Phelps"),
             image = "images/swimming.png",
             aes(
               y = country,
             ),
             x = 50,
             size = 0.08
             #hjust = -5
  )+
  geom_image(data =dom_graph_plot |>
               filter(country == "Micheal Phelps"),
             image = "images/Picture1.png",
             aes(
               y = country,
             ),
             x = 150,
             size = 0.13
             #hjust = -5
  )+
  theme_classic()+
  scale_fill_manual(values = c("gold" = "#D6AF36",
                               "silver" = "#d6d6d6",
                               "bronze" = "#977547",
                               "mi" = "#b2df8a",
                               "other" = "#fdc086"))+
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length.x  = unit(0,"cm"),
    axis.line.x = element_blank(), 
    plot.title = element_markdown(lineheight = 1.1),
    plot.caption = element_markdown(lineheight = 1.1),
    legend.position="bottom",
    legend.key.width = unit(1.2, "cm"),
    plot.background = element_rect(fill = "#f0f0f0"),
    panel.background = element_rect(fill = "#f0f0f0",
                                    colour = "#f0f0f0",
                                    size = 0.5, linetype = "solid"),
    legend.key = element_rect(fill = "transparent"))+
  labs(
    x = "Count of Medals",
    y = "",
    title = paste0("**If *Micheal Phelps* was a country?**<br>
    <span style='font-size:11pt'>Number of medals won by
    <span style='color:#b2df8a;'>Micheal Phelps</span> and
    <span style='color:#fdc086;'>Countries</span> during 2002-2016 Olympics
    </span>"),
    # caption = "<span style='font-size:8pt'>Data:
    # <span style='color:#756bb1;'>World Olympics Data</span> <br> Graph by:
    # <span style='color:#756bb1;'>Avi Arora</span>
    # </span>",
    shape="Admission in hospital",
    color='NEW LEGEND TITLE')

micheal_phelps


ggsave("plots/micheal_phelps_aa.png",micheal_phelps)




