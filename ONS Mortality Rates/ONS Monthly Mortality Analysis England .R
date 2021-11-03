## Packages and Themes
# First, load the packages we need
library(tidyverse)
library(curl)
library(readxl)
library(janitor)
library(lubridate)
library(scales)

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

clean5_colours <- c("#008080", "#800000", "#D58E00", "#D56100", "#0F368F", "black")

## Draw and tidy the data
# Draw the data straight from the online file
ons_mortality_url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlymortalityanalysisenglandandwales%2fseptember2021/monthlymortalityanalysissep2021.xlsx"
ons_mortality_table1_range <-"A5:R254"
ons_mortality_table8c_range <-"A5:X254"
ons_period <- "January 2001 to September 2021."
ons_source <- "Source: ONS: Monthly mortality analysis, September 2021."

ons_table8c_column_names <- c("date",
"75to79_deaths", "75to79_central", "75to79_lower", "75to79_upper",
"80to84_deaths", "80to84_central", "80to84_lower", "80to84_upper",
"85to89_deaths", "85to89_central", "85to89_lower", "85to89_upper",
"90plus_deaths", "90plus_central", "90plus_lower", "90plus_upper")

# This data frame gives the ESP 2013 weights
# https://www.nrscotland.gov.uk/files//statistics/consultation-groups/pams-9-october-2013/paper4-pams-%2813%29-13-esp.pdf
esp_2013_df <- tribble(~age_group, ~esp_weight, ~age_label,
                       "all_ages", 1.000, "All ages",
                       "74under", 0.910, "0 to 74",
                       "75to79", 0.040, "75 to 79",
                       "80to84", 0.025, "80 to 84",
                       "85to89", 0.015, "85 to 89",
                       "90plus", 0.010, "90 and over")

# Download the data into the browser
temp <- tempfile()
temp <- curl_download(url = ons_mortality_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

# This table is of age-standardised mortality in all ages in England
ons_table1_tbl <- read_excel(temp, sheet = "Table 1",
                             range = ons_mortality_table1_range) %>%
  janitor::clean_names() %>%
  dplyr::select(1, 14:15, 17:18) %>%
  rename(date = 1, number_of_deaths = 2,
         rate_central = 3, rate_lower = 4, rate_upper = 5) %>%
  mutate(date = lubridate::my(date),
         age_group = "all_ages")

# This table is for 74 and under
ons_table4_tbl <- read_excel(temp, sheet = "Table 4",
                             range = ons_mortality_table1_range) %>%
  janitor::clean_names() %>%
  dplyr::select(1, 14:15, 17:18) %>%
  rename(date = 1, number_of_deaths = 2,
         rate_central = 3, rate_lower = 4, rate_upper = 5) %>%
  mutate(date = lubridate::my(date),
         age_group = "74under")

# This table covers age-specific rates for groups 75 and over
ons_table8c_tbl <- read_excel(temp, sheet = "Table 8c",
                              range = ons_mortality_table8c_range) %>%
  janitor::clean_names() %>%
  dplyr::select(-starts_with("x")) %>%
  rename_with(~ons_table8c_column_names) %>%
  pivot_longer(cols = 2:17,
               names_to = c("age_group", "measure"),
               names_pattern = "(.*)_(.*)",
               values_to = "value") %>%
  pivot_wider(names_from = "measure",
              values_from = "value") %>%
  rename(number_of_deaths = 3, rate_central = 4,
         rate_lower = 5, rate_upper = 6) %>%
  mutate(date = lubridate::my(date))

# Bind the rows together
ons_mortality_df <- bind_rows(ons_table1_tbl, ons_table4_tbl,
                              ons_table8c_tbl) %>%
  full_join(esp_2013_df, by = "age_group") %>%
  mutate(age_standardised_component = esp_weight*rate_central)

## Making the graphs
# The first graph shows the different rates over time
ons_agestandardised_gg <- ons_mortality_df %>%
  ggplot(aes(x = date, y = rate_central,
             ymin = rate_lower, ymax = rate_upper,
             colour = age_label, fill = age_label)) +
  geom_line(size = 1.1) +
  geom_ribbon(alpha = 0.5) +
  facet_wrap(~age_label, scales = "free") +
  scale_x_date(expand = c(0,0),
               date_labels = "%b\n%Y") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, NA),
                     labels = scales::comma_format()) +
  scale_colour_manual(aesthetics = c("colour", "fill"),
                      name = "Age group",
                      values = clean5_colours,
                      guide = "none") +
  labs(title = "In England, there was a large rise in mortality across age groups in April 2020.",
       subtitle = str_wrap(paste0("Mortality rates per 100,000 people, in England in ",
                                  ons_period,
                                  " The rates for the age group 0-74 and 'All ages' are age-standardised. Other rates are age-specific. Each graph has its own scale."),
                           width = 110),
       x = "Month",
       y = "Mortality rates per 100,000 people",
       caption = ons_source)

# This graph calculates the component of the age-standardised rate
ons_component_gg <- ons_mortality_df %>%
  filter(age_label != "All ages") %>%
  ggplot(aes(x = date, y = age_standardised_component,
             group = age_label, fill = age_label)) +
  geom_col(position = "stack") +
  scale_x_date(expand = c(0,0),
               date_labels = "%b\n%Y") +
  scale_y_continuous(expand = c(0,0),
                     labels = scales::comma_format()) +
  scale_fill_manual(name = "Age group",
                    values = clean5_colours) +
  labs(title = "Age-standardised mortality can be broken into its components.",
         subtitle = str_wrap(paste0("Age-standardised mortality components in England for five age groups, in ",
                                    ons_period,
                                    "Age-standardised mortality rates are per 100,000 people."),
                             width = 100),
         x = "Month",
         y = "Mortality rate per 100,000 people",
         caption = paste0("Author's calculations. ", ons_source))