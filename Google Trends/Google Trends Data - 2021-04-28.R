## Packages and themes
# First, download the packages we need
library(tidyverse)
library(readxl)
library(lubridate)

## Next, I set the plotting theme
theme_clean4 <- theme_bw(base_family="Gill Sans Nova") + 
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

## Draw and tidy data
# Draw the statistics from a prepared file
# This is a summary of three queries of Google Trends (US)
# https://trends.google.com/trends/explore?date=all&geo=US&q=lost%20my%20job,%22lost%20my%20job%22
# https://trends.google.com/trends/explore?date=all&geo=US&q=laid%20off,%22laid%20off%22
# https://trends.google.com/trends/explore?date=all&geo=US&q=can%27t%20pay%20bills,%22can%27t%20pay%20bills%22
google_trends_df <- read_excel("Google Trends/Google Trends Data - 2021-04-28.xlsx", 
                                            col_types = c("date", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric")) %>%
  mutate(month = as_date(month))

# Tidy the data, for months between Jan-08 and Mar-21
google_tidy_df <- google_trends_df %>%
  select(month, 2, 4, 6) %>%
  filter(month >= as_date("2008-01-01"),
         month <= as_date("2021-03-01")) %>%
  pivot_longer(cols = 2:4,
               names_to = "term",
               values_to = "index")

# Set the factors for the order
google_tidy_df$term <- factor(google_tidy_df$term,
                              levels = c("laid_off", "cant_pay_bills", "lost_my_job"))

# Create a label vector
google_term_names <- c(laid_off = "Laid off",
                       cant_pay_bills = "Can't pay bills",
                       lost_my_job = "Lost my job")

## Creating the graph
# The recommended size is 1000 x 1500
google_trends_gg <- google_tidy_df %>%
  ggplot(aes(x = month,
             y = index,
             group = term)) +
  geom_line(aes(colour = term),
            size = 1.5) +
  scale_x_date(expand = c(0,0),
               breaks = c("2010-01-01", "2015-01-01", "2020-01-01") %>% as_date(),
               date_labels = "%b\n%Y") +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~term,
             labeller = as_labeller(google_term_names)) +
  scale_colour_manual(values = c("#008080", "#800000", "#400080")) +
  guides(colour = "none") +
  geom_vline(xintercept = c("2010-01-01", "2015-01-01") %>% as_date(),
             linetype = "dotted") +
  labs(title = "Each search term had a spike in the US in March-April 2020.",
       subtitle = str_wrap("Rounded index of US Google search volumes. 100 is the highest monthly volume for each term. Google applied changes to geographic assignment on Jan-10 and Jan-15."),
       x = "Month",
       y = "Index (rounded)",
       caption = "Source: Google Trends (January 2008 to March 2021) in the United States.")