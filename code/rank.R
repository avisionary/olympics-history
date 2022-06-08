library(dplyr)
library(janitor)
library(tidyverse)
library(plotly)
library(gganimate)
library(ggpubr)
library(scales)
library(ggalt)
library(ggplot2)
library(grid)
library(ggimage)
library(ggtext)
library(magick)

rank_raw <- read_csv("data/SummerOlympics_medals.csv")
rank_raw <- clean_names(rank_raw)

rank_top <- rank_raw %>% filter(year %in% c(2012,2008,2004,2000) & rank <= 10)

rank_top_longer <- rank_top %>% 
  pivot_longer(cols = -c(rank,year,medals,country),
               names_to = 'medal_color',
               values_to = 'medal_count') 

rank_top_longer <- rank_top_longer %>% 
  mutate(weight = case_when(medal_color == "golds" ~ 3,
                            medal_color == "silvers" ~ 2,
                            medal_color == "bronzes" ~ 1))

rank_top_longer <- rank_top_longer %>%
  mutate(medal_weight = medal_count * weight)

rank_weight <- rank_top_longer %>%
  group_by(rank,country,year,medals) %>%
  summarise(calculated_total = sum(medal_weight))

rank_weight <- rank_weight %>%
  group_by(year) %>%
  arrange(desc(calculated_total)) %>%
  mutate(new_rank = 1:n())

# modified_rank <- rank_weight %>% 
#   left_join(rank_top_longer, by = c("rank" = "rank")) %>% 
#   select(c(rank,calculated_total,new_rank,medals)) %>% distinct() 




flawed_rank <- ggplot()+
  geom_point(data = rank_weight,
             mapping = aes(
               x = new_rank,
               y = medals,
               text = paste("Country:",country)
             ),
             color = "#fc8d62",
             size = 3
  )+
  geom_point(data = rank_weight,
            mapping = aes(
              x = rank,
              y = medals,
              text = paste("Country:",country)
            ),
            color = "#7570b3",
            size = 2,
            show.legend = FALSE)+
  facet_wrap(~year)+
  theme_bw()+
  scale_color_manual(values = c("rank" = "#7570b3","new_rank" = "#fc8d62" ))+
  theme(
    strip.background =element_rect(fill="#f1eef6"),
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
    x = "Rank",
    y = "Medals",
    #title = paste0("Gender parity in Olympic Sports"),
    #caption = "<span style='font-size:5pt'>Data:
    #<span style='color:#756bb1;'>World Olympics Data</span> <br> Graph by:
    #<span style='color:#756bb1;'>Avi Arora</span>
    #</span>",
    shape="Admission in hospital",
    color='NEW LEGEND TITLE')



plotly_flawed_rank <- ggplotly(flawed_rank) %>%  
  layout(title = list(text = paste0('<b>Is the ranking system of Olympics 
                                    flawed?</b><br>',
                                    '<sup>','<span style= "font-size:12pt">',
                                    ' <span style= "color:#7570b3;">
                                    Original</span> and ',
                                    '<span style="color:#fc8d62;">',
                                    'Weighted</span> rank across the years
                                    </sup></span>')), 
         title_x=0.5,
         margin = list(l = 0, r = 0,
                       b = 0, t = 100,
                       pad = 0)) %>% 
  config(displayModeBar = FALSE)
plotly_flawed_rank

htmlwidgets::saveWidget(as_widget(plotly_flawed_rank), "plots/flawed_rank.html")

