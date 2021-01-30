## Install packages
# Depending on your version of R, you may need to use remotes installation
library(remotes)
remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
library(tidyverse)
library(lubridate)
library(readxl)
library(ukcovid19)

# I set the plotting theme
theme_clean <- theme_bw(base_family="Calibri") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b=12)),
        plot.caption = element_text(size = 10),
        plot.margin = unit(c(.5,.5,.5,.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
theme_set(theme_clean)

## Drawing and tidying the data
# We draw confirmed death figures from the PHE COVID-19 dashboard API
covid19_structure <- list(
  date = "date",
  areaType = "areaType",
  areaName = "areaName",
  areaCode = "areaCode",
  newDeaths28DaysByDeathDate = "newDeaths28DaysByDeathDate")

covid19_newdeaths_UK_df <- ukcovid19::get_data(
  filters = c("areaType=overview"),
  structure = covid19_structure)

covid19_newdeaths_nation_df <- ukcovid19::get_data(
  filters = c("areaType=nation"),
  structure = covid19_structure)

covid19_newdeaths_region_df <- ukcovid19::get_data(
  filters = c("areaType=region"),
  structure = covid19_structure)

# We then bind the rows of these separate draws together, and transform the date column
covid19_newdeaths_df <- bind_rows(
  covid19_newdeaths_UK_df,
  covid19_newdeaths_nation_df,
  covid19_newdeaths_region_df) %>%
  mutate(date = as_date(date))

# ONS figures come from a prepared file
covid19_ons_registrations_df <- read_excel(
  "Coronavirus Deaths by Data Source/ONS COVID-19 Death Registrations - 2021-01-30.xlsx",
  sheet = "DATA", col_types = c("date", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric","numeric", "numeric",
                                "numeric", "numeric", "numeric"))

# We need to make the data frame tidy
covid19_ons_registrations_df <- covid19_ons_registrations_df %>%
  pivot_longer(cols = 2:15,
               names_to = "areaName",
               values_to = "death_registrations") %>%
  mutate(date = as_date(date),
         areaType = case_when(
           areaName == "United Kingdom" ~ "overview",
           areaName %in% c("England", "Scotland",
                           "Wales", "Northern Ireland") ~ "nation",
           TRUE ~ "region"))

covid19_bysource_df <- full_join(
  covid19_newdeaths_df, covid19_ons_registrations_df,
  by = c("date", "areaType", "areaName")) %>%
  filter(date <= "2021-01-10" & date >= "2020-03-01") %>%
  pivot_longer(cols = 5:6,
               names_to = "measure",
               values_to = "death_count") %>%
  mutate(death_count = replace_na(death_count, 0))

## Creating the graphs
# We want to create two graphs (UK, and facets for English regions)

covid19_subtitle <- "COVID-19 death measures by date of death: death registrations (Office for National Statistics) and confirmed deaths (Public Health England). These provisional figures may be revised."
covid19_caption <- "Data sources: Office for National Statistics: Deaths registered weekly in England and Wales, provisional: week ending 15 January 2021; Public Health England COVID-19 Dashboard API."

covid19_uk_gg <- covid19_bysource_df %>%
  filter(areaName == "United Kingdom") %>%
  ggplot(aes(x = date,
         y = death_count,
         group = measure)) +
  geom_line(aes(color = measure),
            size = 1.5) +
  scale_x_date(date_labels = "%d-%b-%Y",
                   expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1500),
                     expand = c(0,0)) +
  scale_color_manual(name = "",
                     breaks = c("death_registrations",
                                "newDeaths28DaysByDeathDate"),
                     labels = c("ONS: registrations involving COVID-19, up to 15th January 2021",
                                "PHE: confirmed deaths with SARS-CoV-2, recorded up to 29th January 2021"),
                     values = c("#e5942e","#04867b")) +
  labs(title = "Due to limited testing, there was a large difference in the two death counts earlier in the pandemic.",
       subtitle = covid19_subtitle,
       x = "Date of Death",
       y = "",
       caption = covid19_caption)

covid19_regions_gg <- covid19_bysource_df %>%
  filter(areaType == "region") %>%
  ggplot(aes(x = date,
             y = death_count,
             group = measure)) +
  geom_line(aes(color = measure),
            size = 1.5) +
  facet_wrap(~areaName) +
  scale_x_date(date_labels = "%d-%b-%y",
               expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 350),
                     expand = c(0,0)) +
  scale_color_manual(name = "",
                     breaks = c("death_registrations",
                                "newDeaths28DaysByDeathDate"),
                     labels = c("ONS: registrations involving COVID-19 up to 15th January 2021",
                                "PHE: deaths within 28 days of a positive SARS-CoV-2 test, recorded up to 29th January 2021"),
                     values = c("#e5942e","#04867b")) +
  labs(title = "We see similar patterns of divergence and convergence across each English region.",
       subtitle = covid19_subtitle,
       x = "Date of Death",
       y = "",
       caption = covid19_caption)