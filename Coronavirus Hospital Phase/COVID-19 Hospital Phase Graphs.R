## Install packages
# Depending on your version of R, you may need to use remotes installation
library(remotes)
remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
library(tidyverse)
library(lubridate)
library(scales)
library(ukcovid19)
library(gganimate)
library(transformr)
library(gifski)

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
# I draw the figures from the PHE COVID-19 Dashboard API
covid19_structure <- list(
  date = "date",
  areaType = "areaType",
  areaName = "areaName",
  areaCode = "areaCode",
  newAdmissions = "newAdmissions",
  hospitalCases = "hospitalCases")

covid19_hospital_uk_df <- ukcovid19::get_data(
  filters = c("areaType=overview"),
  structure = covid19_structure)

covid19_hospital_nations_df <- ukcovid19::get_data(
  filters = c("areaType=nation"),
  structure = covid19_structure)

covid19_hospital_regions_df <- ukcovid19::get_data(
  filters = c("areaType=region"),
  structure = covid19_structure)

# I then bind the rows of these separate draws together,
# and transform the date column
covid19_hospital_df <- bind_rows(
  covid19_hospital_uk_df,
  covid19_hospital_nations_df,
  covid19_hospital_regions_df) %>%
  mutate(date = as_date(date)) %>%
  drop_na()

## Make the graphs
# First, we create the static graph for England
covid19_hospital_count <- covid19_hospital_df %>%
  filter(areaName == "England") %>%
  count %>% as.numeric()

covid19_hospital_caption <- "Source: Public Health England COVID-19 Dashboard API."
covid19_scale_breaks <- c("2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01")
covid19_scale_breaks <- covid19_scale_breaks %>% as_date()

covid19_hospital_eng_gg <- covid19_hospital_df %>%
  filter(areaName == "England") %>%
  ggplot(aes(x = newAdmissions,
             y = hospitalCases)) +
  geom_point(aes(color = date),
             size = 3,
             alpha = 0.2) +
  labs(title = "There were over 4,000 COVID-19 admissions in England on 12 January 2021.",
      subtitle = "Plot of COVID-19 admissions versus COVID-19 patients in English hospitals over time.",
      x = "COVID-19 admissions (new admissions plus in-patient diagnoses)",
      y = "COVID-19 patients in hospital",
      caption = covid19_hospital_caption) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 4500)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 35000)) +
  scale_color_gradientn(name = "Date",
                        colors = rainbow(6),
                        breaks = covid19_scale_breaks,
                        labels = date_format("%b-%y")) +
  guides(color = guide_colorbar(barwidth = unit(10, "cm")))

# We will create the animated graph for England
covid19_hospital_eng_agg <- covid19_hospital_df %>% 
  filter(areaName == "England") %>%
  ggplot(aes(x = newAdmissions,
             y = hospitalCases)) +
  geom_point(size = 3,
             color = "#008080") +
  labs(title = "COVID-19 admissions in England peaked on 12 January 2021.",
       subtitle = "Plot of COVID-19 admissions and COVID-19 patients in hospital in England over time.
       Date: {frame_time}",
       x = "COVID-19 admissions (new admissions plus in-patient diagnoses)",
       y = "COVID-19 patients in hospital",
       caption = covid19_hospital_caption) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 4500)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 35000)) +
  transition_time(time = date) +
  shadow_wake(wake_length = 0.1,
              alpha = FALSE)

# The animate function then creates the GIF, with an end-pause
covid19_hospital_eng_gif <- covid19_hospital_eng_agg %>%
  animate(nframes = covid19_hospital_count + 10,
          height = 700,
          width = 1000,
          end_pause = 10)