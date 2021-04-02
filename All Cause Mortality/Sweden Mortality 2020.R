## Packages and themes
# First, we install the packages we need
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)

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

## Draw and tidy data
# I draw the excess mortality file from Our World in Data
# https://ourworldindata.org/excess-mortality-covid
owid_all_cause_deaths_df <- read_csv(file = "All Cause Mortality/excess-mortality-raw-death-count.csv")

# I filter to get the rows for Sweden
sweden_deaths_df <- owid_all_cause_deaths_df %>%
  filter(Entity == "Sweden") %>%
  mutate(Day = as_date(Day))

# We calculate the 2015-2019 range of deaths
# The fill function copies down the week 52 value to week 53
sweden_summ_df <- sweden_deaths_df %>%
  select(Day, 6:10) %>%
  pivot_longer(2:6,
               names_to = "owid_measure",
               values_to = "deaths_all_ages") %>%
  group_by(Day) %>%
  summarise(deaths_2015_2019_min = min(deaths_all_ages, na.rm = FALSE),
            deaths_2015_2019_max = max(deaths_all_ages, na.rm = FALSE)) %>%
  fill(deaths_2015_2019_min,
       deaths_2015_2019_max)

# I join the summary to the main table
sweden_deaths_tidy_df <- full_join(sweden_deaths_df,
                                   sweden_summ_df,
                                   by = "Day") %>%
  select(Day, average_deaths_2015_2019_all_ages,
         deaths_2020_all_ages, deaths_2015_2019_min,
         deaths_2015_2019_max) %>%
  pivot_longer(2:3,
               names_to = "owid_measure",
               values_to = "deaths_all_ages")

## Create the graph
# This code creates the ONS style graph for Swedish mortality
sweden_breaks <- c("2020-01-12", "2020-04-05", "2020-06-28", "2020-09-20", "2020-12-13") %>%
  as_date()

# The graph uses labeling with geom_text, turning the legend off
# The recommended size is 600 x 1200
sweden_deaths_gg <- sweden_deaths_tidy_df %>%
  ggplot(aes(x = Day)) +
  geom_line(aes(y = deaths_all_ages,
                colour = owid_measure,
                linetype = owid_measure),
            size = 1.5) +
  geom_ribbon(aes(ymin = deaths_2015_2019_min,
                  ymax = deaths_2015_2019_max,
                  fill = owid_measure),
              alpha = 0.3) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  scale_fill_manual(values = c("#white", "#0AFFFF")) +
  scale_colour_manual(values = c("#008080", "#800000")) +
  scale_x_date(date_labels = "%d-%b",
               breaks = sweden_breaks,
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(),
                     breaks = pretty_breaks(),
                     limits = c(0,2700)) +
  labs(title = "Sweden suffered two waves of high deaths in 2020.",
       subtitle = "Number of deaths reported to Statistics Sweden by week. Excludes deaths with unknown dates.",
       x = "Week of death",
       y = "",
       caption = "Source: Our World in Data: Excess mortality during the Coronavirus pandemic (COVID-19).") +
  theme(legend.position = "none") +
  geom_text(data = filter(sweden_deaths_tidy_df,
                          Day == as_date("2020-06-28") &
                            owid_measure == "deaths_2020_all_ages"),
            aes(x = Day, y = deaths_all_ages, label = "Deaths in 2020"),
            color = "#800000", vjust = -2, size = 5, fontface = "bold") +
  geom_text(data = filter(sweden_deaths_tidy_df,
                          Day == as_date("2020-06-28") &
                            owid_measure == "deaths_2020_all_ages"),
            aes(x = Day, y = deaths_2015_2019_min,
                label = "Average and range of deaths\n2015 to 2019"),
            color = "#008080", vjust = 1.5, size = 5, fontface = "bold")