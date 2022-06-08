#Loading necessary libraries
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
dom_graph_top <- dom_graph |> group_by(country) |> 
  filter(medal == "Gold") |> 
  arrange(desc(n))

dom_graph_top <- head(dom_graph_top,15)

#dom_graph_top <- dom_graph_top %>% select(country)



dom_graph2 <- olympic2 |> filter(year >= 2004)|>
  filter(medal != "No Medal") |> 
  select(country,medal,event,year) |> 
  distinct() |> 
  group_by(country,medal,year) |> tally()

Micheal_phelps2 <- olympic2 |> filter(name == "Michael Fred Phelps, II")|>
  filter(medal != "No Medal") |> 
  group_by(name,medal,year) |> tally() |> 
  mutate(name = "Micheal Phelps") |> 
  rename(country = name)



dom_graph2 <- rbind(dom_graph2,Micheal_phelps2)

dom_graph_top2 <- dom_graph2 |> group_by(country) |> 
  filter(medal == "Gold") |> 
  arrange(desc(year))

dom_graph_plot <- dom_graph_top2 %>% 
  right_join(dom_graph_top, by = c("country" = "country")) %>%
  rename(n = n.x) %>% select(country,year,n)


dom_graph_plot_2000 <- dom_graph_top %>%
  mutate(n=replace(n,medal == 'Gold',0))

dom_graph_plot_2000 <- dom_graph_plot_2000 %>%
  mutate(year=2000) %>% select(country,year,n)



dom_graph_plot2 <- rbind(dom_graph_plot_2000,dom_graph_plot)


#Add values according to year
dom_graph_plot_wider <- dom_graph_plot2 %>%
  pivot_wider(names_from = year,
              values_from = n)

dom_graph_plot_wider <- dom_graph_plot_wider %>% 
  mutate(`2004` = `2000` + `2004`) %>% 
  mutate(`2008` = `2004` + `2008`) %>% 
  mutate(`2012` = `2008` + `2012`) %>% 
  mutate(`2016` = `2012` + `2016`) 
  

dom_graph_plot_longer <- dom_graph_plot_wider %>% 
  pivot_longer(cols = -c(country), names_to = 'year', values_to = 'n') 



qw <- dom_graph_plot_longer%>% filter(year %in% c(2000,2016))
qw$year <- as.numeric(qw$year)




#Graph code 
micheal_phelps <- ggplot()+
  geom_emoji(data = qw,
             mapping = aes(
    y = reorder(country,n),
    x = n,
    #frame = year,
    image = '1f3ca'),
    image_fun =image_flop,
    size = 0.06)+
  geom_text(data = qw,
            x = 110,
            y = "Micheal Phelps",
            label = "Micheal Phelps ranks 14th amongst all countries",
            vjust = 0.3,size = 2)+
  geom_hline(yintercept = 0.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 1.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 2.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 3.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 4.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 5.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 6.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 7.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 8.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 9.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 10.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 11.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 12.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 13.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 14.5,color = "darkblue",size = 1.1)+
  geom_hline(yintercept = 15.5,color = "darkblue",size = 1.1)+
transition_reveal(year)+
theme_bw()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length.x  = unit(0,"cm"),
    axis.line.x = element_blank(), 
    plot.title = element_markdown(lineheight = 1.1),
    plot.caption = element_markdown(lineheight = 1.1),
    legend.position="bottom",
    legend.key.width = unit(1.2, "cm"),
    plot.background = element_rect(fill = "#E7E6E1",color = "#E7E6E1"),
    panel.background = element_rect(fill = "#55acee",
                                    colour = "#55acee",
                                    size = 0.5, linetype = "solid"),
    legend.key = element_rect(fill = "transparent"))+
  labs(
    x = "Count of Medals",
    y = "",
    title = paste0("**If *Micheal Phelps* was a country?**<br>
    <span style='font-size:11pt'>Number of gold medals won
     during 2004-2016 Olympics
    </span>"),
    # caption = "<span style='font-size:8pt'>Data:
    # <span style='color:#756bb1;'>World Olympics Data</span> <br> Graph by:
    # <span style='color:#756bb1;'>Avi Arora</span>
    # </span>",
    shape="Admission in hospital",
    color='NEW LEGEND TITLE')


#micheal_phelps






animate_micheal_phelps <-animate(micheal_phelps, nframes = 120,end_pause = 40,
                     width = 5,
                     height = 4,
                     units = "in",
                     res = 200,
                     renderer = magick_renderer())

animate_micheal_phelps

image_write(animate_micheal_phelps, "plots/animate_micheal_phelps_plot.gif")
