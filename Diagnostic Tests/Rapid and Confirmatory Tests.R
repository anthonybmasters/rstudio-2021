## Packages and Themes
# First, load the packages we need
library(tidyverse)
library(scales)

# Next, I set the plotting theme
theme_clean5 <- theme_bw(base_family = "Gill Sans Nova") + 
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
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title.position = "plot",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 20, face = "bold"))
theme_set(theme_clean5)

## Creating the data
# First, we make the function which performs the main calculation
lr_func <- function(pre_prob, likelihood_ratio){
  if(pre_prob == 1) {post_prob = 1}
  else {
   pre_odds = pre_prob/(1-pre_prob)
   post_odds = likelihood_ratio*pre_odds
   post_prob = post_odds/(post_odds+1)
  }
  return(post_prob)
}

# Set the parameters
# fn stands for false negative, and fp means false positive
rapid_fn <- 0.2
rapid_fp <- 0.001
conf_fn <- 0.05
conf_fp <- 0.00005

# Next, we calculate the likelihood ratios and create the functions
lr_conflicted <- ((1-rapid_fn)*conf_fn)/(rapid_fp*(1-conf_fp))
lr_rapid_neg <- rapid_fn/(1-rapid_fp)

lr_conf_func <- function(pre_prob){
  return(lr_func(pre_prob, likelihood_ratio = lr_conflicted))
}

lr_rn_func <- function(pre_prob){
  return(lr_func(pre_prob, likelihood_ratio = lr_rapid_neg))
}

# Make the tidy data frame
test_tidy_df <- tibble(pre_prob = seq(0, 1, 0.001)) %>%
  mutate(conflicted = map_dbl(pre_prob, lr_conf_func),
         rapid_neg = map_dbl(pre_prob, lr_rn_func)) %>%
  pivot_longer(cols = 2:3,
               names_to = "test_results",
               values_to = "post_prob")

## Create the graphs
test_results_gg <- test_tidy_df %>%
  ggplot(aes(x = pre_prob, y = post_prob,
             group = test_results,colour = test_results)) +
  geom_line(size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(0,1), expand = c(0,0),
                     labels = scales::percent_format()) +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent_format()) +
  scale_colour_manual(values = c("#008080", "#800000"),
                      guide = "none") +
  annotate("text", x = 0.12, y = 0.80,
           size = 8, colour = "#008080",
           label = "Rapid positive then\nconfirmatory negative",
           hjust = 0, fontface = "bold") +
  annotate("text", x = 0.65, y = 0.25,
           size = 8, colour = "#800000",
           label = "Rapid negative",
           hjust = 0, fontface = "bold") +
  labs(title = str_wrap("With prevalence at 2.5%, a rapid positive test followed by a confirmatory negative test means you are more likely than not to have the virus.", 80),
       subtitle = "Pre-test (tested population prevalence) and post-test probabilities of infection for different test results.",
       x = "Pre-test probability (tested population prevalance)",
       y = "Post-test probability",
       caption = str_wrap("Assumptions: the rapid test has a false-negative rate of 20% and a false-positive rate of 0.1%. The confirmatory test has a false-negative rate of 5% and a false-positive rate of 0.005%.", width = 100))