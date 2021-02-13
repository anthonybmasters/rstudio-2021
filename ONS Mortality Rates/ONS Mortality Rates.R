## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(readxl)
library(patchwork)
library(scales)

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
# I draw the figures from the prepared file
# You can download the original version from the Office for National Statistics here:
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12735annualdeathsandmortalityrates1938to2020provisional
ons_deaths_mortality_df <- read_excel("ONS Mortality Rates/ONS England and Wales Death Registrations - 1838 to 2020 - 2020-02-13.xlsx",
                                                                                sheet = "DATA")
# First, a simple table of the death registrations
ons_deathregs_df <- ons_deaths_mortality_df %>%
  filter(year >= 1900) %>%
  select(year, number_of_deaths)

# Next, we want a tidy data-set of the two mortality rates
ons_mortality_rates_df <- ons_deaths_mortality_df %>%
  filter(year >= 1900) %>%
  select(year, crude_mortality_rate_per100000,
         age_standardised_mortality_rate_per100000) %>%
  pivot_longer(cols = 2:3,
               names_to = "measures",
               values_to = "rate") %>%
  drop_na()

# Third, we want to understand annual changes in mortality rates
ons_mortality_change_df <- ons_deaths_mortality_df %>%
  arrange(year) %>%
  mutate(crude_change = crude_mortality_rate_per100000 -
           lag(crude_mortality_rate_per100000, 1),
         agest_change = age_standardised_mortality_rate_per100000 - 
           lag(age_standardised_mortality_rate_per100000, 1)) %>%
  filter(year >= 1900) %>%
  select(year, crude_change, agest_change) %>%
  pivot_longer(cols = 2:3,
               names_to = "measures",
               values_to = "change") %>%
  drop_na()

## Making the graphs
# This is the first half of the first graph
ons_mortality_gg1 <- ons_deathregs_df %>%
  ggplot(aes(x = year,
             y = number_of_deaths)) +
  geom_bar(stat = "identity",
           width = 1,
           aes(fill = number_of_deaths <= 600000)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,700000),
                     labels = label_comma(),
                     breaks = breaks_pretty(n = 7)) +
  theme(legend.position = "NONE",
        plot.subtitle = element_text(size = 14,
                                     face = "plain")) +
  labs(subtitle = "Number of death registrations in England and Wales",
       x = "Year",
       y = "") +
  annotate("text", x = 1950, y = 650000,
           label = "There are two years where death registrations exceeded 600,000:
           in 1918 (flu pandemic) and 2020 (COVID-19 pandemic).")

# This is the second half of the first graph
ons_mortality_gg2 <- ons_mortality_rates_df %>%
  ggplot(aes(x = year,
             y = rate,
             group = measures)) +
  geom_line(aes(color = measures),
            size = 1.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,2500),
                     labels = label_comma()) +
  theme(legend.position = "NONE",
        plot.subtitle = element_text(size = 14,
                                     face = "plain")) +
  labs(x = "Year",
       y = "",
       subtitle = "Mortality rates in England and Wales (per 100,000 people)") +
  scale_colour_manual(values = c("#008080", "#800000")) +
  annotate("text", x = 1925, y = 1000,
           label = "Crude mortality rate",
           color = "#800000") +
  annotate("text", x = 1975, y = 2200,
           label = "Age-standardised mortality rate",
           color = "#008080")

# Using patchwork, we join these two graphs using the + operator
# In patchwork, the plot_annotation function creates overarching annotations
ons_mortality_ggp <- ons_mortality_gg1 +
  ons_mortality_gg2 +
  plot_annotation(title = "In 2020, there were over 608,000 death registrations in England and Wales.",
                  caption = "Source: Office for National Statistics, 1900 to 2020 (provisional).")

# Lastly, we create the graph showing changes in mortality rates
ons_mortality_change_gg <- ons_mortality_change_df %>%
  ggplot(aes(x = year,
             y = change,
             group = measures)) +
  geom_line(aes(color = measures),
            size = 1.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "2020 saw the biggest increase in age-standardised mortality since 1951.",
       subtitle = "Annual changes in mortality rates (death registrations per 100,000 people) in England and Wales.",
       x = "Year",
       y = "",
       caption = "Author's calculations. Source: Office for National Statistics, 1899 to 2020 (provisional).") +
  scale_colour_manual(values = c("#008080", "#800000"),
                      name = "Mortality rates",
                      labels = c("Age-standardised", "Crude"))