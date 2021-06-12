## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(tidybayes)

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

## Set priors and input data
# This is for an A/B test between two versions of the same page
# The prior distributions are uniform
set.seed(4744)
xA <- 300; nA <- 1000; alphaA <- 1; betaA <- 1
xB <- 340; nB <- 1000; alphaB <- 1; betaB <- 1
number_sims <- 50000

# Apply the updating rule
alphaA_post <- alphaA + xA; betaA_post <- betaA + nA - xA
alphaB_post <- alphaB + xB; betaB_post <- betaB + nB - xB

# Run simulations for the two distributions
pA <- rbeta(number_sims, alphaA_post, betaA_post)
pB <- rbeta(number_sims, alphaB_post, betaB_post)

pB_minus_pA <- 100*(pB - pA) %>% as_tibble()

## Producing summaries
# In the tidybayes package, we can produce a credible interval
pB_minus_pA_cred <- pB_minus_pA %>%
  mean_qi(mean = value) %>%
  rename(lower = .lower, upper = .upper) %>%
  mutate(name = "Numerical simulation", type = "Bayesian") %>%
  select(name, type, mean, lower, upper)

# Instead, we could use formal approximation
muA_post <- alphaA_post / (alphaA_post + betaA_post)
muB_post <- alphaB_post / (alphaB_post + betaB_post)
diff_approx_var <- muA_post * (1 - muA_post) / (alphaA_post + betaA_post + 1) +
  muB_post * (1 - muB_post) / (alphaB_post + betaB_post + 1)

pB_minus_pA_approx <- tibble(
  name = "K&M approximation",
  type = "Bayesian",
  mean = 100*(muB_post - muA_post),
  lower = 100*(muB_post - muA_post - qnorm(0.975)*sqrt(diff_approx_var)),
  upper = 100*(muB_post - muA_post + qnorm(0.975)*sqrt(diff_approx_var)))

# We can compare that to a classical calculation
freq_shareA <- xA/nA; freq_shareB <- xB/nB
freq_share_pooled <- (xA + xB)/(nA + nB)
freq_var <- freq_share_pooled*(1-freq_share_pooled)*(1/nA + 1/nB)

pB_minus_pA_freq <- tibble(
  name = "Z-test of two proportions",
  type = "Frequentist",
  mean = 100*(freq_shareB - freq_shareA),
  lower = 100*((freq_shareB - freq_shareA) - qnorm(0.975)*sqrt(freq_var)),
  upper = 100*((freq_shareB - freq_shareA) + qnorm(0.975)*sqrt(freq_var)))

pB_minus_pA_intervals_df <- bind_rows(
  pB_minus_pA_cred, pB_minus_pA_approx, pB_minus_pA_freq)

## Make graphs
# This sets the caption, common across both graphs
pB_minus_pA_caption <- paste0("Data: page A: ",
                              format(xA, big.mark = ","), " conversions, ",
                              format(nA, big.mark = ",") ," users; page B: ",
                              format(xB, big.mark = ","), " conversions, ",
                              format(nB, big.mark = ",") ," users.")

# This graph compares intervals
pB_minus_pA_intervals_gg <- pB_minus_pA_intervals_df %>%
  ggplot(aes(x = mean, xmin = lower, xmax = upper,
             y = name, colour = type)) +
  geom_pointrange(size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_colour_manual(values = c("#008080", "#800000")) +
  guides(colour = "none") +
  labs(title = "For an A/B test, the uncertainty intervals are near-identical.",
       subtitle = "95% confidence and 95% credible (mean with equal tails) intervals for the difference between two proportions.",
       x = "Arithmetic difference between B and A [percentage points]",
       y = "",
       caption = pB_minus_pA_caption) +
  theme(plot.title.position = "plot")

# We set the titles
pB_minus_pA_prob <- sum(pB_minus_pA >= 0) / number_sims
pB_minus_pA_title <- paste0("The probability that B has higher conversion than A is ~",
                            round(pB_minus_pA_prob*100, digits = 0), "%")
pB_minus_pA_subtitle <- paste0("The posterior distribution of the difference between two proportions, based on ",
                               format(number_sims, big.mark = ","), " numerical simulations.\nThe test sample has ",
                               format(nA + nB, big.mark = ","), " total users, with uniform priors.")

# Make the dots graph
pB_minus_pA_gg <- pB_minus_pA %>%
  ggplot(aes(x = value,
             fill = stat(x >= 0),
             slab_colour = stat(x >= 0))) +
  stat_dotsinterval(quantiles = 1000) +
  scale_colour_manual(aesthetics = c("fill", "slab_colour"),
                      values = c("#800000", "#008080"),
                      name = "Difference (B minus A)",
                      labels = c("Less than 0", "More than or equal to 0")) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  labs(title = pB_minus_pA_title,
       subtitle = pB_minus_pA_subtitle,
       x = "Arithmetic difference between B and A [percentage points]",
       y = "",
       caption = pB_minus_pA_caption)