## Packages and Themes
# First, we install the packages we need
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(scales)

# Next, I set the plotting theme
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

## Draw data
# We draw the data straight from the StatsWales open data series
sw_vaccine_use_url <- "http://open.statswales.gov.wales/en-gb/dataset/hlth6011"
sw_vaccine_use_js <- httr::GET(sw_vaccine_use_url)
sw_vaccine_use_js2 <- sw_vaccine_use_js$content %>%
  rawToChar() %>%
  fromJSON()

# We clean the data, replacing the assigned NA value with NA
sw_vaccine_use_df <- sw_vaccine_use_js2$value %>%
  as_tibble() %>%
  dplyr::select(HealthBoard_ItemName_ENG, Atpmon_ItemName_ENG,
                Vaccine_ItemName_ENG, Data) %>%
  dplyr::rename(health_board = HealthBoard_ItemName_ENG,
                date = Atpmon_ItemName_ENG,
                vaccine = Vaccine_ItemName_ENG,
                percentage = Data) %>%
  dplyr::mutate(date = dmy(date),
                percentage =
                  case_when(percentage == -999999900.0 ~ NA_real_,
                  TRUE ~ percentage)) %>%
  tidyr::drop_na()

# This table is for label placement
sw_vaccine_use_tl <- tribble(
  ~date, ~vaccine, ~percentage,
  "2021-05-01", "Total", 0.52,
  "2021-07-20", "Moderna", 0.22,
  "2021-05-01", "Oxford/AstraZeneca", 0.32,
  "2021-05-01", "Pfizer BioNTech", 0.92) %>%
  dplyr::mutate(date = as_date(date))

## Create the graph
sw_vaccine_use_gg <- sw_vaccine_use_df %>%
  filter(health_board == "Wales") %>%
  ggplot(aes(x = date, y = percentage, colour = vaccine)) +
  geom_line(size = 1.5) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b\n%Y") +
  scale_y_continuous(labels = percent_format(scale = 1,
                                             accuracy = 0.5),
                     expand = c(0,0)) +
  scale_colour_manual(guide = "none",
                      values = c("#0941B3", "#008080",
                                 "#800000", "black")) +
  geom_text(data = sw_vaccine_use_tl,
            aes(x = date, y = percentage,
                label = vaccine, colour = vaccine),
            size = 7, hjust = 0, fontface = "bold") +
  annotate("text", x = as_date("2021-06-01"), y = 1.8, size = 7,
           label = "Around 40,000 Oxford/AstraZeneca doses expired") +
  annotate("curve", x = as_date("2021-08-01"), xend = as_date("2021-09-02"),
           y = 1.9, yend = 1.9, curvature = -0.3,  size = 1.3,
           arrow = arrow(length = unit(2,"mm")), colour = "#008080") +
  labs(title = "There was a sharp rise in total wastage of Oxford/AstraZeneca doses in Wales.",
       subtitle = str_wrap("The cumulative number of doses not suitable for use, by week, expressed as a percentage of adminstered and unsuitable doses. This is based on records in the Welsh Immunisation System.", width = 100),
       x = "Reporting date",
       y = "Percentage",
       caption = "Source: StatsWales: Cumulative COVID-19 vaccine doses not suitable for use, by vaccine type.") +
  theme(plot.title.position = "plot")