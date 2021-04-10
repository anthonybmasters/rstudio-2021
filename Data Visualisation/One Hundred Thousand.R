## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(patchwork)

# Next, I set the plotting theme
theme_clean3 <- theme_clean2 <- theme_bw(base_family="Gill Sans Nova") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 20, face = "italic", margin = margin(b=12)),
        plot.caption = element_text(size = 14),
        plot.margin = unit(c(.5,.5,.5,.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
theme_set(theme_clean2)

## Create the graphs
# I produce a grid of 10,000 dots
ten_thousand_df <- expand.grid(x = 1:100, y = 1:100) %>%
  as_tibble()

# We produce two sets of graphs, one with a highlighted dot
ten_thousand_gg <- ten_thousand_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(colour = "#008080",
             alpha = 0.5,
             size = 0.9) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0,0,0,0, unit = "pt")) +
  coord_fixed()

ten_thousand_one_gg <- ten_thousand_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(colour = stat(x == 47 & y == 79)),
             alpha = 0.5,
             size = 0.9) +
  scale_colour_manual(values = c("#008080", "#800000")) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0,0,0,0, unit = "pt"),
        legend.position = "none") +
  coord_fixed()
  
# We can then produce one graph of 10,000 dots with labels
ten_thousand_dots_gg <- ten_thousand_one_gg +
  labs(title = "This is 10,000 dots.",
       subtitle = "One dot in the square of 10,000 is highlighted.")

# This is an empty plot of the same size
empty_gg <- ten_thousand_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0,
             size = 0.9) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0,0,0,0, unit = "pt")) +
  coord_fixed()

# Using patchwork, we stitch together these plots
one_hundred_thousand_dots_gg <-
  (ten_thousand_gg|ten_thousand_gg|ten_thousand_gg|ten_thousand_gg)/
  (ten_thousand_gg|ten_thousand_one_gg|ten_thousand_gg|ten_thousand_gg)/
  (empty_gg|ten_thousand_gg|ten_thousand_gg|empty_gg)