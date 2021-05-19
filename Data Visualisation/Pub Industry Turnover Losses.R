## Packages and themes
# First, download the packages we need
library(tidyverse)
library(ggdist)
library(scales)
library(patchwork)

# Set the plotting theme
theme_clean4 <- theme_bw(base_family = "Gill Sans Nova") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 20, face = "italic",
                                     margin = margin(b=12)),
        plot.caption = element_text(size = 20,
                                    vjust = -1),
        plot.margin = unit(c(.5,.5,.5,.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 20, face = "bold"))
theme_set(theme_clean4)

## Set parameters and create the table
number_of_simulations <- 10000
ind_2019_mean <- 23000
ind_2019_sd <- 300
red_2020_shape1 <- 62
red_2020_shape2 <- 40
adult_pop_mill <- 52.673
price_per_pint <- 3.90
set.seed(4744)

pub_industry_df <- tibble(
  ind_2019_est = rnorm(n = number_of_simulations,
                       mean = ind_2019_mean,
                       sd = ind_2019_sd),
  red_2020_est = rbeta(n = number_of_simulations,
                       shape1 = red_2020_shape1,
                       shape2 = red_2020_shape2)) %>%
  mutate(loss_2020_est = ind_2019_est*red_2020_est,
         pints_per_adult = loss_2020_est / (adult_pop_mill*price_per_pint)
  )

## Create summaries
pub_industry_df %>% mean_qi(x = pints_per_adult,
                            .width = 0.9)

pub_industry_gg1 <- pub_industry_df %>%
  ggplot(aes(x = loss_2020_est)) + 
  stat_histinterval(fill = "#008000") +
  scale_x_continuous(labels = scales::label_dollar(
    prefix = "£", scale = 1/1000, suffix = "bn")) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  theme(plot.subtitle = element_text(size = 20,
                                     face = "bold",
                                     color = "#008000")) +
  labs(subtitle = "Estimated 2020 pub industry contraction",
       x = "Loss of turnover from 2019 to 2020",
       y = "")

pub_industry_gg2 <- pub_industry_df %>%
  ggplot(aes(x = pints_per_adult)) +
  stat_dotsinterval(point_interval = mean_qi,
                    quantiles = 1000,
                    fill = "#008080",
                    slab_colour = "#008080") +
  stat_slab(alpha = 0.2,
            fill = "#008080") +
  scale_x_continuous(limits = c(45, 90)) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  theme(plot.subtitle = element_text(size = 20,
                                     face = "bold",
                                     color = "#008080")) +
  labs(subtitle = "Expressed in pints per UK adult",
       x = "Pints per adult (assuming an average of £3.90)",
       y = "")

pub_industry_gg <- pub_industry_gg1 + pub_industry_gg2 +
  plot_annotation(title = "The pub industry's 2020 contraction is worth around 55-75 pints per adult.",
       subtitle = "Modelling the UK pub industry 2020 reduction in turnover, with 10,000 simulations.",
       caption = "Assumptions: industry size ~ N(23,000, 300); contraction ~ Beta(62,40); 52.67m UK adults; price is £3.90.")