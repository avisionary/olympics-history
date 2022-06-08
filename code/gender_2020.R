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

gender_2020 <- read_csv("data/EntriesGender.csv")
gender_2020 <- clean_names(gender_2020)

gender_2020_category <- gender_2020 %>% 
  mutate(discipline = case_when(
    discipline == "3x3 Basketball" ~ "Basketball",
    discipline == "Artistic Gymnastics" | 
      discipline == "Trampoline Gymnastics" ~ "Gymnastics",
    discipline == "Marathon Swimming" ~ "Swimming",
    discipline == "Beach Volleyball" ~ "Volleyball",
    discipline == "Cycling BMX Freestyle" |
      discipline =="Cycling BMX Racing" |
      discipline == "Cycling Mountain Bike" |
      discipline == "Cycling Road" |
      discipline ==  "Cycling Track" ~ "Cycling",
    TRUE ~ discipline))

gender_2020_category <- gender_2020_category %>% 
  group_by(discipline) %>% 
  summarise_all(sum)




gender_2020_category <- gender_2020_category %>% 
  mutate(diff = abs(male-female))

gender_2020_category <- gender_2020_category %>% 
  mutate(diff_gender = case_when(
  male>female~ "male",
  male<female~ "female",
  male==female~ "zero"
))



gender_2020_plot <- gender_2020_category %>% 
  filter(discipline != "Athletics" &
         discipline != "Canoe Slalom" &
         discipline != "Modern Pentathlon" &
         discipline != "Skateboarding" &
         discipline != "Sport Climbing" &
         discipline != "Surfing")

g<- ggplot() +
  geom_segment(data = gender_2020_plot,
               mapping = aes(
                 y = discipline, yend=discipline, x=0, xend=.5),
               color="#b2b2b2", size=0.15)+
  geom_dumbbell(data=gender_2020_plot,
                mapping = aes(
                  y=discipline,
                  x=female,
                  xend=male),
                size=1.5,
                color="#b2b2b2",
                size_x=3,
                size_xend = 3,
                colour_x = "#fc8d62",
                colour_xend = "#7570b3",
                alpha_xend = 0.6,show.legend = FALSE)+
  geom_text(data=gender_2020_plot %>% 
              filter(discipline != "Rhythmic Gymnastics" &
                    discipline != "Artistic Swimming"), 
            mapping = aes(
              x=female,
              y=discipline,
              label = female),
            color="#fc8d62",
            size=2.75,
            hjust=2) +
  geom_text(data=gender_2020_plot %>% 
              filter(discipline != "Rhythmic Gymnastics" &
                       discipline != "Artistic Swimming"),
            color="#7570b3",
            size=2.75,
            hjust=-1,
            aes(
              x=male,
              y=discipline,
              label=male))+
  geom_text(data=gender_2020_plot %>% 
              filter(discipline == "Rhythmic Gymnastics" |
                       discipline == "Artistic Swimming"), 
            mapping = aes(
              x=female,
              y=discipline,
              label = female),
            color="#fc8d62",
            size=2.75,
            hjust=-1) +
  geom_text(data=gender_2020_plot %>% 
              filter(discipline == "Rhythmic Gymnastics" |
                       discipline == "Artistic Swimming"),
            color="#7570b3",
            size=2.75,
            hjust=2,
            aes(
              x=male,
              y=discipline,
              label=male))


g <- g + geom_rect(data=gender_2020_plot,
                   aes(xmin=480,
                       xmax=550,
                       ymin=-Inf,
                       ymax=Inf),
                   fill="grey") +
  geom_text(data=gender_2020_plot,
            aes(
              label=diff,
              y=discipline, x=515,
              color = diff_gender),
            fontface="bold",
            size=3,show.legend = FALSE) +
  geom_text(data=filter(gender_2020_plot,
                        discipline=="Wrestling"),
            aes(
              x=515,
              y=discipline,
              label="Difference"),
            color="black",
            size=3.3,
            vjust=-1.3,
            fontface="bold")+
  scale_x_continuous(limits=c(-2, 550)) +
  scale_y_discrete(expand=c(0.07,0))+
  theme_classic()+
  scale_color_manual(values = c("male" = "#7570b3",
                                "female" = "#fc8d62",
                                "zero" = "#e41a1c" ))+
  theme( 
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
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
    x = "No. of Participants",
    y = "Sports",
    title = paste0("**Gender *parity* in Olympic Sports**<br>
    <span style='font-size:11pt'>
    <span style='color:#7570b3;'>Male</span> and
    <span style='color:#fc8d62;'>Female</span>
    participation in various sports in **Tokyo Olympics** 2020
    </span>"),
    #caption = "<span style='font-size:5pt'>Data:
    #<span style='color:#756bb1;'>World Olympics Data</span> <br> Graph by:
    #<span style='color:#756bb1;'>Avi Arora</span>
    #</span>",
    shape="Admission in hospital",
    color='NEW LEGEND TITLE')
  
g

ggsave("plots/gender_2020.png", plot = g)

