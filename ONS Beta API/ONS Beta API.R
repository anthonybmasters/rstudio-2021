## Packages and Themes
# First, install the packages we need
library(httr)
library(jsonlite)
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
# Using the GET function, I draw the data from the beta API
ons_labourmarket_js <- GET("https://api.beta.ons.gov.uk/v1/datasets/labour-market/editions/time-series/versions/4/observations?time=*&agegroups=16-64&economicactivity=in-employment&geography=K02000001&seasonaladjustment=seasonal-adjustment&sex=all-adults&unitofmeasure=rates")

# We then transform our file
ons_labourmarket_js2 <- ons_labourmarket_js$content %>%
  rawToChar() %>%
  fromJSON()

# This is turned into a data frame, where we need to coerce data formats
ons_lm_json_df <- ons_labourmarket_js2$observations %>%
  as_tibble() %>%
  mutate(time_label = dimensions$Time$label,
         rolling3m_date = my(substr(time_label, 5, 12)),
         estimate = as.numeric(observation)) %>%
  select(rolling3m_date, time_label, estimate) %>%
  arrange(rolling3m_date)

# When I created the data-frame, there was an error in the original file
# This error is at Nov-Jan 2020 (with a value of 31.6m), corrected from A02:
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/employmentunemploymentandeconomicinactivityforpeopleaged16andoverandagedfrom16to64seasonallyadjusteda02sa
ons_lm_json_df$estimate[102] <- as.numeric("76.46")

# Instead, we could get read data from the CSV
# That requires filtering, then a similar process of column coercion
ons_lm_csv_df <- read_csv("https://download.beta.ons.gov.uk/downloads/datasets/labour-market/editions/time-series/versions/4.csv") %>%
  filter(UnitOfMeasure == "Rates",
         EconomicActivity == "In Employment",
         AgeGroups == "16-64",
         Sex == "All adults",
         SeasonalAdjustment == "Seasonally Adjusted") %>%
  rename(estimate = V4_1,
         rolling3m_char = 3) %>%
  mutate(rolling3m_date = my(substr(rolling3m_char, 5, 12))) %>%
  arrange(rolling3m_date)

# The error in the Nov-Jan 2020 rate is also present in the CSV
ons_lm_csv_df$estimate[102] <- as.numeric("76.46")

## Making a graph
# Here, I will use the data frame derived from the beta API
# Due to the way I named the columns, we can substitute ons_lm_csv_df
# and get the same graph

# We can set up a frame for the date breaks and labels
ons_lm_date_breaks <- c("2012-01-01", "2014-01-01", "2016-01-01", "2018-01-01", "2020-01-01") %>%
  as_tibble() %>%
  mutate(rolling3m_date = as_date(value)) %>%
  left_join(ons_lm_json_df,
            by = "rolling3m_date") %>%
  select(rolling3m_date, time_label)

ons_lm_json_gg <- ons_lm_json_df %>%
  ggplot(aes(x = rolling3m_date,
             y = estimate)) +
  geom_line(size = 1.5) +
  scale_x_date(breaks = ons_lm_date_breaks$rolling3m_date,
               labels = ons_lm_date_breaks$time_label,
               expand = c(0,0)) +
  scale_y_continuous(limits = c(60,80),
                     expand = c(0,0)) +
  labs(title = "The estimated employment rate among those aged 16 to 64 fell since the start of 2020.",
       subtitle = "Central estimates of the UK employment rate [%] among adults aged 16 to 64, seasonally adjusted.",
       caption = "Source: Office for National Statistics (Beta API Labour Market v4 and A02 SA).",
       x = "Rolling Three-Month Period",
       y = "")