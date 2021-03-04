## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)

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
# I draw the data frame from a prepared file
# The original file is here:
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/964734/Weekly_Influenza_and_COVID19_report_data_W8.xlsx
phe_covid_flu_w8_df <- read_excel("PHE Weekly Reports/Weekly_Influenza_and_COVID19_report_data_W8 - 2021-03-03.xlsx",
                                  col_types = c("numeric", "date", "text",
                                                "numeric", "numeric", "numeric",
                                                "numeric", "numeric", "numeric",
                                                "numeric", "numeric", "numeric"))

# I then tidy the data frame
phe_covid_flu_w8_df <- phe_covid_flu_w8_df %>%
  pivot_longer(cols = 4:12,
               names_to = "phe_region",
               values_to = "admission_rate") %>%
  mutate(week_end_date = as_date(week_end_date))

## Making the graph
# I set the date breaks I want to use in the graph
phe_date_breaks = c("2020-08-16", "2020-10-12", "2020-12-06", "2021-01-31") %>%
  as_date()

# This graph makes small multiples of the PHE regions
phe_covid_flu_w8_gg <- phe_covid_flu_w8_df %>%
  ggplot(aes(x = week_end_date,
             y = admission_rate,
             group = disease)) +
  geom_line(aes(color = disease),
            size = 1.5) +
  facet_wrap(~phe_region) +
  labs(title = "The weekly COVID-19 admission rates continue to decrease in all English regions.",
       subtitle = "Weekly hospital admission rate (per 100,000 people) by PHE Centre for new COVID-19 positive cases and influenza, reported through SARI Watch.",
       caption = "Source: Public Health England National flu and COVID-19 surveillance report (week 8, provisional).",
       x = "Week end date",
       y = "") +
  scale_color_manual(name = "Disease",
                     values = c("#008080", "#800000")) +
  scale_x_date(expand = c(0,0),
               breaks = phe_date_breaks,
               date_labels = "%d-%b") +
  scale_y_continuous(limits = c(0,60)) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"))