## Packages and Themes
# First, we download the packages we need
library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(janitor)
library(scales)

# Next, we set the plotting theme
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

## Drawing and tidying the data
# Draw the data straight from the online file
ons_deaths_url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2021/publishedweek492021.xlsx"
ons_deaths_range <- "A5:R97"
ons_caption <- "Source: Office for National Statistics – Deaths registered weekly in England and Wales."
ons_first_week_date <- as_date("2020-03-13")
ons_last_week_date <- as_date("2021-12-10")

temp <- tempfile()
temp <- curl_download(url = ons_deaths_url, destfile = temp,
                      quiet = FALSE, mode = "wb")

ons_deaths_df <- read_excel(temp,
                            sheet = "Weekly Excess Deaths",
                            range = ons_deaths_range) %>%
  dplyr::select(1:2, starts_with("Excess deaths")) %>%
  clean_names() %>%
  dplyr::rename(home = 3, hospital = 4, care_home = 5, other = 6) %>%
  mutate(week_date = seq(ons_first_week_date, ons_last_week_date,
                         by = "7 days")) %>%
  pivot_longer(cols = 3:6,
               names_to = "location",
               values_to = "excess_deaths")

# Set the factors and facet labels
ons_deaths_df$location <- factor(ons_deaths_df$location,
                                 levels = c("home", "hospital",
                                            "care_home", "other"))

ons_deaths_location_labels <- as_labeller(c(home = "Home",
                                            hospital = "Hospital (acute or community)",
                                            care_home = "Care home",
                                            other = "Other"))

## Making the graph
# This graph shows excess deaths by location
ons_deaths_location_gg <- ons_deaths_df %>%
  ggplot(aes(x = week_date, y = excess_deaths)) +
  geom_col(fill = "#008080") +
  facet_wrap(~location,
             labeller = ons_deaths_location_labels) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               breaks = seq(ons_first_week_date,
                            ons_last_week_date,
                            by = "24 weeks"),
               expand = c(0,0)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Deaths at home continue to be above the 2015-2019 average.",
         subtitle = str_wrap("Excess deaths in England and Wales are registered deaths minus the 2015-2019 average for those weeks. Death registrations are between 7th March 2020 and 10th December 2021.",
                             width = 120),
         x = "Week end date",
         y = "Excess deaths",
         caption = ons_caption)