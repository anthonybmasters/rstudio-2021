## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(readxl)
library(curl)
library(scales)
library(janitor)
library(lubridate)
library(patchwork)

# Set the plotting theme
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

## Draw the data
# We draw the data straight from the online file
temp <- tempfile()
nhs_hospital_url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/Primary-Diagnosis-Supplement-20210729.xlsx"
temp <- curl_download(url = nhs_hospital_url, destfile = temp,
                      quiet = FALSE, mode = "wb")

# As the Excel date gets read as a character, we use the janitor function
nhs_tot_tbl <- read_excel(temp,
                          sheet = "Total Beds Occupied Covid",
                          range = "D13:AQ14") %>%
  t() %>%
  as_tibble(rownames = NA,
            .name_repair = "unique") %>%
  rownames_to_column() %>%
  rename(date = 1, covid_total_beds_occupied = 2) %>%
  mutate(date = excel_numeric_to_date(as.numeric(date)))

nhs_pri_tbl <- read_excel(temp,
                          sheet = "Primarily Covid",
                          range = "D13:AQ14") %>%
  t() %>%
  as_tibble(rownames = NA,
            .name_repair = "unique") %>%
  rownames_to_column() %>%
  rename(date = 1, covid_primary_beds_occupied = 2) %>%
  mutate(date = excel_numeric_to_date(as.numeric(date)))

# We join the tables together
nhs_covid_beds_df <- full_join(nhs_tot_tbl, nhs_pri_tbl,
                               by = "date") %>%
  mutate(non_primary_beds_occupied = covid_total_beds_occupied - covid_primary_beds_occupied,
         covid_primary_share = covid_primary_beds_occupied / covid_total_beds_occupied)

nhs_beds_tidy_df <- nhs_covid_beds_df %>%
  dplyr::select(date, covid_primary_beds_occupied, non_primary_beds_occupied) %>%
  pivot_longer(cols = 2:3,
               names_to = "measures",
               values_to = "count")

# Create the graphs
# The left-hand graph shows total Covid-19 beds by primary admission reason
nhs_covid_beds_gg1 <- nhs_beds_tidy_df %>%
  ggplot(aes(x = date, y = count, group = measures)) +
  geom_col(aes(fill = measures),
           position = position_stack(reverse = TRUE)) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(),
                     expand = c(0,0)) +
  annotate("text", x = as_date("2021-06-21"), y = 2500,
           label = "Primarily Covid-19", hjust = 0,
           size = 7, fontface = "bold", colour = "#008080") +
  annotate("text", x = as_date("2021-06-21"), y = 3000,
           label = "Other primary reason", hjust = 0,
           size = 7, fontface = "bold", colour = "#800000") +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 20, face = "bold")) +
  scale_fill_manual(values = c("#008080", "#800000")) +
  labs(subtitle = "Total beds occupied by confirmed Covid-19\npatients, by primary admission reason",
       x = "Date",
       y = "")

# The right-hand graph shows the share of primary reasons
nhs_covid_beds_gg2 <- nhs_covid_beds_df %>%
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
nhs_covid_beds_gg <- nhs_covid_beds_gg1 + nhs_covid_beds_gg2 +
  plot_annotation(title = "For most Covid-19 patients in England, the disease is the primary admission reason.",
                  subtitle = str_wrap("Total beds occupied in NHS England by patients with a positive SARS-CoV-2 test (less than 14 days before admission or after admission). This is split by the primary cause of admission on a best endeavours basis.",
                                      width = 110),
                  caption = "Source: NHS England Covid-19 Hospital Activity: Primary Diagnosis Supplement, 29 July 2021.")