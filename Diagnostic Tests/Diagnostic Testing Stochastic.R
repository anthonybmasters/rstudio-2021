## Packages and Themes
# Install the packages we need
library(tidyverse)
library(ggdist)
library(patchwork)

# I set the plotting theme
# I am trying out larger text font, to improve readability
theme_clean2 <- theme_bw(base_family="Calibri") + 
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

## Run simulations
# For debugging purposes, you may wish to set a seed
set.seed(47448)

# Next, we set the prevalence, population, false positive and false negative rates
prevalence = 0.0004
population = 1000000
false_pos_rate = 0.0003
false_neg_rate = 0.5
number_sims = 10000

# We run our simulations, organised into the numbers of each result 
simulations_df <- tibble(
  false_neg = rbinom(n = number_sims,
                     size = round(prevalence*population),
                     prob = false_neg_rate),
  false_pos = rbinom(n = number_sims,
                     size = round((1-prevalence)*population),
                     prob = false_pos_rate),
  true_neg = round((1-prevalence)*population) - false_pos,
  true_pos = round(prevalence*population) - false_neg) %>%
  mutate(ppv = true_pos/(true_pos + false_pos),
         npv = true_neg/(true_neg + false_neg))

# We can summarise the two simulated distributions
simulations_df %>% mean_qi(100*ppv)
simulations_df %>% mean_qi(100*npv)

ppv_ce <- round(mean_qi(100*simulations_df$ppv)$y, digits = 0)
ppv_lower <- round(mean_qi(100*simulations_df$ppv)$ymin, digits = 1)
ppv_upper <- round(mean_qi(100*simulations_df$ppv)$ymax, digits = 1)
ppv_interval_text <- paste0(ppv_ce, "% (", ppv_lower, "% to ", ppv_upper, "%)")

## Making the graphs
# Set the dynamic title, subtitle and caption, based on the inputs and summary
simulations_title <- paste0("The precision is ",
                            ppv_interval_text, 
                            ", whilst the true share of negative results is very high.")
simulations_subtitle <- paste0("The approximate distributions for precision and negative predictive value, from ",
                               format(number_sims, big.mark = ","),
                               " simulations.")
simulations_caption <- paste0("Assumes an independent Binomial distribution for each false result type. The prevalence is ",
                              round(100*prevalence, digits = 2),
                              "%, the false-positive rate is ",
                              round(100*false_pos_rate, digits = 2),
                              "%, and the false-negative rate is ",
                              round(100*false_neg_rate, digits = 2),
                              "%.")

# First, this is the graph for the precision (positive predictive value)
simulations_ppv_gg <- simulations_df %>%
  ggplot(aes(x = 100*ppv)) +
  stat_dotsinterval(point_interval = mean_qi,
               fill = "#008080",
               slab_colour = "#008080",
               quantiles = 1000) +
  theme(plot.subtitle = element_text(size = 16,
                                     face = "plain")) +
  labs(subtitle = "Precision (true share of all positive results)",
       x = "Precision [%]",
       y = "") +
  scale_x_continuous(limits = c(30,50)) +
  scale_y_continuous(labels = NULL,
                     breaks = NULL)

# Second, this is the graph for the negative predictive value
# The choice of x-axis limits is difficult here
simulations_npv_gg <- simulations_df %>%
  ggplot(aes(x = 100*npv)) +
  stat_dotsinterval(point_interval = mean_qi,
               fill = "#800000",
               slab_colour = "#800000",
               quantiles = 1000) +
  theme(plot.subtitle = element_text(size = 16,
                                     face = "plain")) +
  labs(subtitle = "Negative predictive value (true share of all negative results)",
       x = "Negative predictive value [%]",
       y = "") +
  scale_x_continuous(limits = c(99.97, 99.99)) +
  scale_y_continuous(labels = NULL,
                     breaks = NULL)

# Using the patchwork package, I put these two graphs together
simulations_summary_gg <- simulations_ppv_gg +
  simulations_npv_gg +
  plot_annotation(title = simulations_title,
                  subtitle = simulations_subtitle,
                  caption = simulations_caption)