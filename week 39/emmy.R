library(tidyverse)
library(ggtext)
library(extrafont)
library(Cairo)
library(gridExtra)
library(grid)
library(gridtext)
loadfonts(device = "win")
theme_set(theme_minimal())

font_add_google('Fira Sans')
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!


# Getting the data --------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-09-21')
tuesdata <- tidytuesdayR::tt_load(2021, week = 39)

nominees <- tuesdata$nominees

df <- nominees %>% 
  filter(distributor %in% c('Netflix','HBO') & year >= 2013) %>% 
  select(category, distributor, title, year, type) %>% 
  mutate(category = case_when(
    str_detect(category, pattern = 'Drama') ~ 'Drama',
    str_detect(category, pattern = 'Comedy') ~ 'Comedy',
    str_detect(category, pattern = 'Documentary|Nonfiction') ~ 'Documentary/Nonfiction'
  )) %>% 
  na.omit() %>% 
  group_by(distributor, category, type, year) %>% 
  count() %>% 
  pivot_wider(names_from = type, values_from = n) %>% 
  mutate(Winner = ifelse(is.na(Winner), 0, Winner),
         total = Nominee + Winner)


text_size <- 3
y_size <- 16
title_size <- 18
label_size <- 3.5

# Drama -------------------------------------------------------------------

drama <- df %>%
  group_by(distributor, category) %>% 
  select(-year) %>% 
  summarise_if(is.numeric, sum) %>% 
  filter(category == 'Drama') %>% 
  ggplot() +
  geom_linerange(aes(x = distributor, ymin = Winner, ymax = total), 
                 size = 2, alpha = 0.7, color = 'gray') +
  geom_point(aes(distributor, total, color = distributor), size = 4, pch = 21, stroke = 2) +
  geom_point(aes(distributor, Winner, color = distributor),  size = 5) +
  geom_text(aes(x = distributor, y = total, label = total, color = distributor),
            nudge_x = 0.15, fontface = 'bold', size = text_size, family = 'Fira Sans') +
  geom_text(aes(x = distributor, y = Winner, label = Winner, color = distributor),
            nudge_x = 0.15, fontface = 'bold', size = text_size, family = 'Fira Sans') +
  scale_color_manual(values = c('#004369','#DB1F48')) +
  ylim(0,550) +
  coord_flip() +
  geom_text(x = 1.5, y = 100, 
            label = 'HBO had more than twice\nas many awards as Netflix', 
            family = 'Fira Sans', color = "#004369", size = label_size) +
  labs(x = NULL, y = NULL, 
       title = 'Drama') +
  geom_curve(x = 1, y = 170, xend = 1.2, yend = 100, color = '#004369',
             curvature = -.4, arrow = arrow(length = unit(0.3, 'cm'))) +
  theme(line = element_blank(),
        plot.title = element_text(size = title_size, face = 'bold', color = 'gray20', 
                                  hjust = 0.4, family = 'Montserrat', vjust = -1),
        legend.position = 'none', 
        panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = y_size, face = 'bold', hjust = .4,
                                   color = c('#004369','#DB1F48'), family = 'Arvo'),
        plot.background = element_rect(fill = 'white', color = 'white'))



# Comedy ------------------------------------------------------------------

comedy <- df %>%
  group_by(distributor, category) %>% 
  select(-year) %>% 
  summarise_if(is.numeric, sum) %>% 
  filter(category == 'Comedy') %>% 
  ggplot() +
  geom_linerange(aes(x = distributor, ymin = Winner, ymax = total), 
                 size = 2, alpha = 0.7, color = 'gray') +
  geom_point(aes(distributor, total, color = distributor), size = 4, pch = 21, stroke = 2) +
  geom_point(aes(distributor, Winner, color = distributor),  size = 5) +
  geom_text(aes(x = distributor, y = total, label = total, color = distributor),
            nudge_x = 0.15, fontface = 'bold', size = text_size, family = 'Fira Sans') +
  geom_text(aes(x = distributor, y = Winner, label = Winner, color = distributor),
            nudge_x = 0.15, fontface = 'bold', size = text_size, family = 'Fira Sans') +
  scale_color_manual(values = c('#004369','#DB1F48')) +
  ylim(0,550) +
  coord_flip() +
  geom_text(x = 1.5, y = 220, 
            label = 'Biggest difference of all, Netflix hasn\'t won\neven one-sixth of HBO awards',
            family = 'Fira Sans', color = "#DB1F48", size = label_size) +
  geom_curve(x = 1.9, y = 9, xend = 1.5, yend = 48, color = '#DB1F48',
             curvature = .4, arrow = arrow(length = unit(0.3, 'cm'))) +
  labs(x = NULL, y = NULL, 
       title = 'Comedy') +
  theme(line = element_blank(),
        plot.title = element_text(size = title_size, face = 'bold', color = 'gray20', 
                                  hjust = 0.4, family = 'Montserrat', vjust = -1),
        legend.position = 'none', 
        panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = y_size, face = 'bold', hjust = .4,
                                   color = c('#004369','#DB1F48'), family = 'Arvo'),
        plot.background = element_rect(fill = 'white', color = 'white'))


# Documentary -------------------------------------------------------------

doc <- df %>%
  group_by(distributor, category) %>% 
  select(-year) %>% 
  summarise_if(is.numeric, sum) %>% 
  filter(category == 'Documentary/Nonfiction') %>% 
  ggplot() +
  geom_linerange(aes(x = distributor, ymin = Winner, ymax = total), 
                 size = 2, alpha = 0.7, color = 'gray') +
  geom_point(aes(distributor, total, color = distributor), size = 4, pch = 21, stroke = 2) +
  geom_point(aes(distributor, Winner, color = distributor), size = 5) +
  geom_text(aes(x = distributor, y = total, label = total, color = distributor),
            nudge_x = 0.15, fontface = 'bold', size = text_size, family = 'Fira Sans') +
  geom_text(aes(x = distributor, y = Winner, label = Winner, color = distributor),
            nudge_x = 0.15, fontface = 'bold', size = text_size, family = 'Fira Sans') +
  scale_color_manual(values = c('#004369','#DB1F48')) +
  ylim(0,550) +
  coord_flip() +
  geom_text(x = 1.5, y = 160, 
            label = 'The category where they are closest,\nboth in nominations and awards.',
            family = 'Fira Sans', color = "gray20", size = label_size) +
  labs(x = NULL, y = NULL, 
       title = 'Documentary/Nonfiction') +
  theme(line = element_blank(),
        plot.title = element_text(size = title_size, face = 'bold', color = 'gray20', 
                                  hjust = 0.4, family = 'Montserrat', vjust = -1),
        legend.position = 'none', 
        panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = y_size, face = 'bold', hjust = .4,
                                   color = c('#004369','#DB1F48'), family = 'Arvo'),
        plot.background = element_rect(fill = 'white', color = 'white'))


# Merging -----------------------------------------------------------------

CairoPNG(filename = 'emmy.png', width = 650, height = 800)

drama + comedy + doc + 
  plot_layout(ncol = 1) +
  plot_annotation(
  title = "<b style='color:#DB1F48'>Netflix</b> and <b style='color:#004369'>HBO's</b> Nominations
  and <b style='color:#f1ad14'>Emmy</b> Awards",
  subtitle = 'HBO had more nominations and awards since 2015 in categories<br>related to drama, comedy, and documentary/nonfiction',
  caption = 
    '<b>Data</b> : emmys.com | <b>Graph</b>: @joaomyname | <b>Github</b> : JoaoMore/tidytuesday', 
  theme = theme(plot.title = element_markdown(size = title_size, face = 'bold',
                                              hjust = 0.4, color = 'gray10',
                                              family = 'Montserrat', vjust = -1),
                plot.subtitle = element_markdown(size = title_size-5, face = 'bold',
                                                 hjust = 0.4, color = 'gray60',
                                                 family = 'Fira Sans', vjust = -1), 
                plot.caption = element_markdown(color = 'gray60', hjust = 0.5,
                                                family = 'Fira Sans', size = 12))
) 

dev.off()
