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




#---------------------

# Medal Counts

medal_winners <- olympic2  |> 
  select(country,year,medal,host_country,event) |> 
  distinct() |> 
  group_by(country,year,medal,host_country) |> tally() |> 
  rename(medal_count = n)


medal_winners <- medal_winners |> mutate(is_host = case_when(
  host_country == country ~ "Host",
  TRUE ~ "Not Host"
))


medal_forplot = medal_winners |> filter(medal != "No Medal")  |> 
  filter(country %in% c("USA", "UK", "Russia", "China", "Japan")) |> 
  group_by(country,year,is_host) |> 
  summarise(medal_count = sum(medal_count)) |> 
  ungroup()


host_edge_plot = ggplot() + 
  geom_point(data= medal_forplot |> filter(is_host == "Not Host"),
             mapping = aes(
               x = medal_count,
               y = country,
               color = is_host
             ),fill= "#fdc086",size = 3,alpha = 0.4,show.legend = FALSE)+
  geom_point(data= medal_forplot |> filter(is_host == "Host"),
             mapping = aes(
               x = medal_count,
               y = country,
               color = is_host
             ),fill = "#beaed4",size = 3,alpha =0.8 ,show.legend = FALSE)+
  theme_classic()+
  scale_color_manual(values = c("Host" = "#7570b3", "Not Host" = "#fdc086" ))+
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
    title = paste0("**Does *hosting* give a competitive edge?**<br>
    <span style='font-size:11pt'>Count of number of medals won by
    <span style='color:#7570b3;'>Host</span> and
    <span style='color:#fdc086;'>Non Host</span> countries.
    </span>"),
    caption = "<span style='font-size:8pt'>Data:
    <span style='color:#756bb1;'>World Olympics Data</span> <br> Graph by:
    <span style='color:#756bb1;'>Avi Arora</span>
    </span>",
    shape="Admission in hospital",
    color='NEW LEGEND TITLE')



host_edge_plot



ggsave("plots/medals_host_aa.png",host_edge_plot)
