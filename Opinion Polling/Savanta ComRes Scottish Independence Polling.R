## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)

# I set the plotting theme
theme_clean <- theme_bw(base_family="Calibri") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b=12)),
        plot.caption = element_text(size = 12),
        plot.margin = unit(c(.5,.5,.5,.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
theme_set(theme_clean)

## Drawing and tidying the data
# I draw the data frame from a prepared file
# The Savanta ComRes polling archive is here:
# https://comresglobal.com/our-work/poll-archive/

savanta_comres_scotind_df <- read_excel("Opinion Polling/Savanta ComRes Scottish Independence Polling - 2021-03-15.xlsx",
                                        col_types = c("text", "text", "date",
                                                      "date", "numeric", "text",
                                                      "numeric", "numeric", "numeric",
                                                      "numeric", "text", "numeric"))

# I then tidy the data frame, creating columns for the Yes and No shares
# In presenting results for this question, Yes is normally before No
savanta_comres_scotind_tidy_df <- savanta_comres_scotind_df %>%
  mutate(yes_share = round(100*yes_resp/weighted_base_calc, digits = 0),
         no_share = round(100*no_resp/weighted_base_calc, digits = 0),
         fieldwork_end = as_date(fieldwork_end)) %>%
  select(fieldwork_end, weighting, yes_share, no_share) %>%
  pivot_longer(cols = 3:4,
               names_to = "vi_choice",
               values_to = "share") %>%
  mutate(vi_choice = factor(vi_choice,
                            levels = c("yes_share", "no_share")))

## Making a graph
savanta_comres_scotind_gg <- savanta_comres_scotind_tidy_df %>%
  ggplot(aes(x = fieldwork_end,
             y = share,
             group = vi_choice)) +
  geom_line(aes(color = vi_choice),
            size = 1.5) +
  geom_point(aes(color = vi_choice),
             size = 3) +
  facet_wrap(~weighting) +
  labs(title = "Savanta ComRes polling estimates a small No lead in recent Scottish independence surveys.",
       subtitle = "Rounded vote intention shares [%], responding to the question:
       'If there were a referendum on Scottish independence tomorrow with the question, “Should Scotland be an independent country?”, how would you vote?'.",
       x = "Fieldwork end date",
       y = "",
       caption = "Source: Savanta ComRes online polls of Scotland, with about 1,000 Scottish adults (16+) per sample.") +
  scale_colour_manual(name = "Vote intention choice",
                      values = c("#008080", "#800000"),
                      labels = c("Yes", "No")) +
  scale_x_date(date_labels = "%d-%b") +
  scale_y_continuous(limits = c(36, 54),
                     expand = c(0,0)) +
  theme(strip.background = element_rect(fill = "white"),
          strip.text = element_text(face = "bold"))