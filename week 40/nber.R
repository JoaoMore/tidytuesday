library(tidyverse)
library(waffle)
library(gender)
library(genderdata)
library(mefa)
library(ggtext)
library(extrafont)
library(patchwork)
library(sysfonts)
library(showtext)
theme_set(theme_minimal())
font_import()
font_add_google('Roboto Slab')
font_add_google('Enriqueta')
font_add_google('Fira Sans')


# Get the Data ----

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest
# Or read in the data manually

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')


# Join --------------------------------------------------------------------


joined_df <- left_join(papers, paper_authors) %>% 
  left_join(authors) %>% 
  left_join(paper_programs) %>% 
  left_join(programs)%>% 
  mutate(
    catalogue_group = str_sub(paper, 1, 1),
    catalogue_group = case_when(
      catalogue_group == "h" ~ "Historical",
      catalogue_group == "t" ~ "Technical",
      catalogue_group == "w" ~ "General"
    ),
    .after = paper
  ) 

df <- joined_df %>% 
  select(name, program_desc) %>% 
  filter(!is.na(program_desc)) %>% 
  separate(name, into = c('name',NA))

gender_names <- gender(df$name) %>% 
  select(name, gender) %>% 
  distinct()

final <- df %>% 
  count(name, program_desc) %>% 
  left_join(gender_names) %>% 
  na.omit() %>% 
  group_by(program_desc, gender) %>% 
  summarise(n = sum(n)) %>% 
  mutate(prop = round(100*n/sum(n))) %>% 
  select(-n) %>% 
  ungroup()

teste <- df %>% 
  left_join(gender_names) %>% 
  na.omit()

final %>% 
  group_by(program_desc, gender) %>% 
  group_modify(~rep(.x, times = .x$prop)) %>% 
  select(-prop) %>% 
  ungroup(gender) %>% 
  group_modify(~waffle_iron(.x, aes_d(group = gender))) %>% 
  ggplot(aes(x, y, fill = group)) +
  geom_waffle(nrow = 20) +
  coord_equal() +
  facet_wrap(~program_desc, nrow = 7) +
  theme(legend.position = 'none')

# plot --------------------------------------------------------------------

plot.waffle <- function(data = final, group, 
                        bgc = '#3F6FC8', title.colour = '#F9FEFF',
                        wc = '#F13343', mc = '#1B2B2B', ...) {
  res <- data %>% 
    filter(program_desc == group) %>% 
    pull(prop) %>% 
    waffle(rows = 5, colors = c(wc,mc)) +
    labs(title = group) +
    theme(legend.position = 'none', 
          plot.title = element_text(hjust = 0.5, colour = title.colour, 
                                    face = 'bold', size = 8),
          rect=element_rect(fill=bgc,
                            color=bgc),
          plot.background=element_rect(fill=bgc),
          strip.background = element_rect(colour=NA, fill=NA),
          panel.background=element_rect(fill=bgc, color=bgc))
  
  res$layers[[1]]$aes_params$colour <- bgc
  
  res
}

plots <- list()

groups <- final %>% 
  pivot_wider(names_from = gender, values_from = prop) %>% 
  mutate(ratio = female/(male+female)) %>% 
  arrange(ratio) %>% 
  pull(program_desc)

for (i in 1:length(groups)) {
  plots[[i]] <- plot.waffle(group = groups[i], 
                            mc = 'seashell', bgc = "#1E435D",
                            title.colour = 'white')
}

showtext_auto()

png(filename = 'nber.png', width = 800, height = 900)

wrap_plots(plots, ncol = 3, ) +
  plot_annotation(
    title = "Publications in National Bureau of Economic Research<br>are dominated by men",
  subtitle = "For every 100 publications in Monetary Economics program, only <b style='color:#F13343'>12</b><br> of them had <b style='color:#F13343'>female</b> authors<br>",
  caption = 
    '<b>Data</b> : National Bureau of Economic Research | <b>Graphic</b>: @joaomyname | <b>Github</b> : JoaoMore/tidytuesday', 
  theme = theme(plot.title = element_markdown(face = 'bold', family = 'Roboto Slab', 
                                              size = 20, color = 'seashell', vjust = -1),
                plot.subtitle = element_markdown(face = 'bold', family = 'Enriqueta', 
                                                 color = 'gray90', vjust = -1, size = 12), 
                plot.caption = element_markdown(color = 'seashell', hjust = 0.5,
                                                family = 'Fira Sans', size = 10, valign = 1),
                plot.background = element_rect(fill = '#1E435D', color = '#1E435D'),
                plot.margin = unit(c(1,1,1,1), "cm"),
                panel.background = element_rect(fill = '#1E435D'))
  )


