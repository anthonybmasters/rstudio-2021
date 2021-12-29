## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(curl)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(patchwork)

# Next, set the plotting theme
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

## Draw and tidy the data
# Draw the data straight from the online files
nhs_england_url_sep21 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/12/Primary-Diagnosis-Supplement-20211209-20210618-20210930.xlsx"
nhs_england_range_sep21 <- "C13:DD22"
nhs_england_url_dec21 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/12/Primary-Diagnosis-Supplement-20211223.xlsx"
nhs_england_range_dec21 <- "C13:CG22"

# Set up a function to undertake the drawing process
nhs_england_draw <- function(input_url, input_sheet,
                             input_range, input_name){
  temp <- tempfile()
  temp <- curl_download(url = input_url, destfile = temp,
                        quiet = TRUE, mode = "wb")
  nhs_england_df <- read_excel(temp,
                               sheet = input_sheet,
                               range = input_range) %>%
    drop_na() %>% t() %>%
    as_tibble(rownames = NA, .name_repair = "unique") %>%
    rownames_to_column() %>%
    row_to_names(row_number = 1) %>%
    dplyr::rename(date = 1) %>%
    pivot_longer(cols = 2:9,
                 names_to = "region",
                 values_to = "dummy_name") %>%
    mutate(date = excel_numeric_to_date(as.numeric(date)),
           dummy_name = as.numeric(dummy_name)) %>%
    dplyr::rename(!!sym(input_name) := dummy_name)
  return(nhs_england_df)
}

# We then draw from the two files
nhs_tot_tbl_sep21 <- nhs_england_draw(input_url = nhs_england_url_sep21,
                                      input_sheet = "Total Beds Occupied Covid",
                                      input_range = nhs_england_range_sep21,
                                      input_name = "covid_total_beds_occupied")

nhs_pri_tbl_sep21 <- nhs_england_draw(input_url = nhs_england_url_sep21,
                                      input_sheet = "Primarily Covid",
                                      input_range = nhs_england_range_sep21,
                                      input_name = "covid_primary_beds_occupied")

nhs_tot_tbl_dec21 <- nhs_england_draw(input_url = nhs_england_url_dec21,
                                      input_sheet = "Total Beds Occupied Covid",
                                      input_range = nhs_england_range_dec21,
                                      input_name = "covid_total_beds_occupied")

nhs_pri_tbl_dec21 <- nhs_england_draw(input_url = nhs_england_url_dec21,
                                      input_sheet = "Primarily Covid",
                                      input_range = nhs_england_range_dec21,
                                      input_name = "covid_primary_beds_occupied")

# Join the tables together
nhs_sep21_df <- full_join(nhs_tot_tbl_sep21, nhs_pri_tbl_sep21,
                          by = c("date", "region"))

nhs_dec21_df <- full_join(nhs_tot_tbl_dec21, nhs_pri_tbl_dec21,
                          by = c("date", "region"))

nhs_covid_beds_df <- bind_rows(nhs_sep21_df, nhs_dec21_df) %>%
  mutate(non_primary_beds_occupied = covid_total_beds_occupied - covid_primary_beds_occupied,
         covid_primary_share  = covid_primary_beds_occupied / covid_total_beds_occupied)

nhs_beds_tidy_df <- nhs_covid_beds_df %>%
  dplyr::select(date, region, covid_primary_beds_occupied, non_primary_beds_occupied) %>%
  pivot_longer(cols = 3:4,
               names_to = "measures",
               values_to = "count")

## Creating the graphs
# The graphs cover London, showing total Covid-19 beds by primary diagnosis
nhs_london_beds_gg1 <- nhs_beds_tidy_df %>%
  dplyr::filter(region == "London") %>%
  ggplot(aes(x = date, y = count, group = measures)) +
  geom_col(aes(fill = measures),
           position = position_stack(reverse = TRUE)) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(),
                     expand = c(0,0)) +
  annotate("text", x = as_date("2021-06-21"), y = 1300,
           label = "Primarily Covid-19", hjust = 0,
           size = 7, fontface = "bold", colour = "#008080") +
  annotate("text", x = as_date("2021-06-21"), y = 1500,
           label = "Other primary diagnosis", hjust = 0,
           size = 7, fontface = "bold", colour = "#800000") +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 20, face = "bold")) +
  scale_fill_manual(values = c("#008080", "#800000")) +
  labs(subtitle = "Total beds occupied by confirmed Covid-19\npatients, by primary diagnosis",
       x = "Date",
       y = "")

# The second graph shows the beds share for a primary diagnosis
nhs_london_beds_gg2 <- nhs_covid_beds_df %>%
  dplyr::filter(region == "London") %>%
  ggplot(aes(x = date, y = covid_primary_share)) +
  geom_line(size = 1.5, colour = "#008080") +
  scale_x_date(date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_percent(scale = 100),
                     expand = c(0,0),
                     limits = c(0,1)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold",
                                     colour = "#008080")) +
  labs(subtitle = "Primarily Covid-19 share of Covid-19 beds",
       x = "Date",
       y = "")

# Using patchwork, we then put these two graphs together
nhs_london_beds_gg <- nhs_london_beds_gg1 + nhs_london_beds_gg2 +
  plot_annotation(title = "About three in four positive patients in London have Covid-19 as their primary diagnosis.",
                  subtitle = str_wrap("Total beds occupied in London (NHS acute providers only) by patients with a positive SARS-CoV-2 test (less than 14 days before admission or after admission). This is split by primary diagnosis on a best endeavours basis.",
                                      width = 115),
                  caption = "Source: NHS England Covid-19 Hospital Activity: Primary Diagnosis Supplements (up to 21st December 2021.)")