## Packages and Themes
# First, we install the packages we need
library(tidyverse)
library(readxl)
library(patchwork)
library(scales)
library(zoo)
library(ggdist)

# I set the plotting theme
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

## Drawing and tidying the data
# I draw the figures from the prepared file
# You can download the original version from the ONS here:
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12735annualdeathsandmortalityrates1938to2020provisional
ons_deaths_mortality_df <- read_excel("ONS Mortality Rates/ONS England and Wales Death Registrations - 1838 to 2020 - 2020-02-13.xlsx",
                                      sheet = "DATA")

# This table shows the arithmetic and relative change
# versus the past 5-year average
ons_deaths_df <- ons_deaths_mortality_df %>%
  arrange(year) %>%
  rename(crude = crude_mortality_rate_per100000,
         agest = age_standardised_mortality_rate_per100000) %>%
  mutate(crude_5yrmean = rollmean(lag(crude, 1),
                                  k = 5,
                                  fill = NA,
                                  align = "right"),
         agest_5yrmean = rollmean(lag(agest, 1),
                                  k = 5,
                                  fill = NA,
                                  align = "right"),
         crude_change = crude - crude_5yrmean,
         agest_change = agest - agest_5yrmean,
         crude_rel = 100*crude_change/crude_5yrmean,
         agest_rel = 100*agest_change/agest_5yrmean)

# We can find the top 10 years for changes in age-standardised mortality rates
ons_deaths_df %>%
  select(year, agest_rel) %>%
  arrange(desc(agest_rel)) %>%
  slice_head(n = 10)

# The summary function provides an overview of relative changes in mortality rates
ons_deaths_df %>%
  filter(year <= 2019) %>%
  select(crude_rel, agest_rel) %>%
  summary()

## Create the graph
# First, I set the joint title and caption
ons_deaths_title <- "In England and Wales, 2020 was the largest relative increase in age-standardised mortality since 1951."
ons_deaths_caption <- "Author's calculations. Source: Office for National Statistics, 1900 to 2020 (provisional)."

# This is the left-hand graph, on age-standardised mortality rates in E&W
ons_deaths_agest_gg1 <- ons_deaths_df %>%
  select(year, agest) %>%
  drop_na() %>%
  ggplot(aes(x = year,
             y = agest)) +
  geom_line(color = "#008080",
            size = 1.5) +
  scale_x_continuous(limits = c(1940,2020)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,2500),
                     labels = label_comma()) +
  geom_point(aes(color = year == 2020),
             size = 5) +
  scale_colour_manual(values = c("NA", "#008080")) +
  theme(legend.position = "None",
        plot.subtitle = element_text(size = 20,
                                     face = "bold",
                                     color = "#008080")) +
  labs(subtitle = "Age-standardised mortality rates (per 100,000 people)",
       x = "Year",
       y = "") +
  annotate("text", x = 1980, y = 500,
           label = "The age-standardised mortality rate\nwas last higher in 2008.",
           size = 5) +
  annotate("curve",
           x = 2000, xend = 2020,
           y = 500, yend = 900,
           curvature = 0.4,
           arrow = arrow(length = unit(2, "mm")),
           color = "#008080")

# This is the right-hand graph, for changes in age-standardised mortality rates
ons_deaths_agest_gg2 <- ons_deaths_df %>%
  select(year, agest_rel) %>%
  drop_na() %>%
  ggplot(aes(x = year,
             y = agest_rel)) +
  geom_line(color = "#800000",
            size = 1.5) +
  scale_x_continuous(limits = c(1940,2020)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(-15,15)) +
  geom_point(aes(color = year == 2020,
                 size = 5)) +
  scale_colour_manual(values = c("NA", "#800000")) +
  theme(legend.position = "None",
        plot.subtitle = element_text(size = 20,
                                     face = "bold",
                                     color = "#800000")) +
  labs(subtitle = "Relative change [%] in mortality versus past five-year average",
       x = "Year",
       y = "") +
  geom_hline(yintercept = 0,
             linetype = "dashed")

# Using patchwork, we join these two graphs
# In patchwork, the plot annotation function creates overarching notes
# Due to its size, it should be displayed in 700 x 1700
ons_deaths_agest_ggp <- ons_deaths_agest_gg1 +
  ons_deaths_agest_gg2 +
  plot_annotation(title = ons_deaths_title,
                  caption = ons_deaths_caption)

# The second graph is a dot-plot for each year of relative change
# The dots interval will show the interval for 1947 to 2019
ons_agest_dots_gg <- ons_deaths_df %>%
  select(year, agest_rel) %>%
  drop_na() %>%
  ggplot(aes(x = floor(agest_rel),
             fill = year != 2020,
             slab_colour = year != 2020)) +
  stat_dotsinterval(aes(point_alpha = year != 2020),
                    point_interval = mean_qi,
                    normalize = TRUE,
                    binwidth = 1) +
  scale_point_alpha_discrete(range = c(0,1)) +
  scale_colour_manual(aesthetics = c("fill", "slab_colour"),
                      values = c("#FCAB10", "#800000")) +
  scale_x_continuous(limits = c(-15,15)) + 
  scale_y_continuous(labels = NULL,
                     breaks = NULL) +
  theme(legend.position = "none") +
  labs(title = "Age-standardised mortality usually decreases against the past five-year average.",
  subtitle = "Dot-plot of relative change [%] in age-standardised mortality versus past five-year average.
  This is for England and Wales, between 1947 and 2020 (provisional).",
  x = "Relative change [%] in age-standardised mortality",
  y = "",
  caption = ons_deaths_caption) +
  annotate("text",
           x = 7, y = 0.5,
           label = "In 2020, the age-standardised death rate was 8% higher
           in England and Wales than the 2015-2019 average.",
           size = 5) +
  annotate("curve",
           x = 7, xend = 8.2,
           y = 0.4, yend = 0.1,
           curvature = 0.4,
           arrow = arrow(length = unit(2, "mm")),
           color = "#FCAB10")

# An issue with the above graph is the misaligned bins
ons_deaths_df %>%
  select(year, agest_rel) %>%
  drop_na() %>%
  ggplot(aes(x = agest_rel,
             fill = year!= 2020)) +
  geom_histogram(binwidth = 1,
                 boundary = 0)