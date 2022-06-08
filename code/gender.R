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

#Reading the data 
olympics_raw <- read_csv("data/athlete_events.csv")
olympics_raw <- clean_names(olympics_raw)

#Our analysis is on Summer olympics only 
olympic_gender <- olympics_raw %>% 
  filter(season == "Summer") %>% 
  group_by(year,sex) %>% 
  summarise(gender_total = n())

olympic_gender <- olympic_gender %>% ungroup() %>% 
  add_row(tibble_row(year = 2020, sex = "F", gender_total = 5432))
olympic_gender <- olympic_gender %>% ungroup() %>% 
  add_row(tibble_row(year = 2020, sex = "M", gender_total = 5884))



olympic_gender_wider <- olympic_gender %>%
  pivot_wider(names_from = sex,
              values_from = gender_total)

olympic_gender_wider <- olympic_gender_wider %>% 
  mutate(F = replace_na(F, 0)) %>% 
  mutate(diff = M - F)

olympic_gender_longer <- olympic_gender_wider %>% 
  pivot_longer(cols = -c(year), names_to = 'sex', values_to = 'gender_total') 
 
olympic_gender <- olympic_gender %>% 
  mutate(icon = ifelse(sex == 'M','male-sharp','female-sharp')) 


p <- ggplot()+
  geom_line(data = olympic_gender %>% filter(sex == "M" | sex == "F"),
            mapping = aes(
              x = year,
              y = gender_total,
              group = sex,
              color = sex
            ),show.legend = FALSE, size = 1.5
            )+
  geom_icon(data=olympic_gender %>% filter(sex == "M" | sex == "F"),
             mapping = aes(
               x = year,
               y = gender_total,
               group = sex,
               image = icon
             ),
             size = 0.08,
             show.legend = FALSE)+
  scale_y_continuous(labels = comma)+
  transition_reveal(year) +
  theme_classic()+
  scale_color_manual(values = c("M" = "#3e76ec","F" = "#179a13" ))+
  theme( 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
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
    x = "",
    y = "Total Participants",
    title = paste0("**Gender *parity* in Olympics**<br>
    <span style='font-size:8pt'>
    <span style='color:#3e76ec;'>Male</span> and
    <span style='color:#179a13;'>Female</span> participation over the years
    </span>"),
    #caption = "<span style='font-size:5pt'>Data:
    #<span style='color:#756bb1;'>World Olympics Data</span> <br> Graph by:
    #<span style='color:#756bb1;'>Avi Arora</span>
    #</span>",
    shape="Admission in hospital",
    color='NEW LEGEND TITLE')


animate_p <-animate(p, nframes = 120,end_pause = 40,
                    width = 3.8,
                    height = 1.8,
                    units = "in",
                    res = 200,
                    renderer = magick_renderer())
#animate_p



####################################################################



p1 <- ggplot()+
  geom_bar(data = olympic_gender_longer %>% filter(sex == "F" | sex == "M"),
           mapping = aes(
             fill=sex,
             y=gender_total,
             x=factor(year)),
             #frame = year),
           position="stack",
           stat="identity",
           show.legend = FALSE) +
  scale_y_continuous(labels = comma)+
  transition_reveal(year) +
  theme_classic()+
  scale_fill_manual(values = c("M" = "#3e76ec","F" = "#179a13" ))+
  theme( 
    axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
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
    x = "Year",
    y = "Total Participants",
    #title = paste0("**Gender *parity* in Olympics**<br>
    #<span style='font-size:11pt'>
    #<span style='color:#7570b3;'>Male</span> and
    #<span style='color:#fc8d62;'>Female</span> participation over the years
    #</span>"),
    #caption = "<span style='font-size:8pt'>Data:
    #<span style='color:#756bb1;'>World Olympics Data</span> <br> Graph by:
    #<span style='color:#756bb1;'>Avi Arora</span>
    #</span>",
    shape="Admission in hospital",
    color='NEW LEGEND TITLE')
  #scale_x_continuous("year", labels = as.character(year), breaks = year)
  

animate_p1 <-animate(p1, nframes = 120,end_pause = 40,
                     width = 3.8,
                     height = 1.8,
                     units = "in",
                     res = 200,
                     renderer = magick_renderer())
#animate_p1

new_gif <- image_append(c(animate_p[1], animate_p1[1]), stack = TRUE)
for(i in 2:120){
  combined <- image_append(c(animate_p[i], animate_p1[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}
new_gif

image_write(new_gif, "plots/combined_gender_plot.gif")
