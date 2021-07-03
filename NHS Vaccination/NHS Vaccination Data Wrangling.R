## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(readxl)
library(curl)
library(scales)

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

## Draw data
# We draw the data straight from the online file
temp <- tempfile()
nhs_vaccine_url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/COVID-19-weekly-announced-vaccinations-01-July-2021.xlsx"
temp <- curl_download(url = nhs_vaccine_url, destfile = temp,
                      quiet = FALSE, mode = "wb")

onedose_tbl <- read_excel(temp,
                          sheet = "NHS Region",
                          range = "D13:Q14") %>%
  t() %>%
  as_tibble(rownames = NA,
            .name_repair = "unique") %>%
  rownames_to_column() %>%
  rename(age_group = rowname, onedose = 2)

twodose_tbl <- read_excel(temp,
                          sheet = "NHS Region",
                          range = "U13:AH14") %>%
  t() %>%
  as_tibble(rownames = NA,
            .name_repair = "unique") %>%
  rownames_to_column() %>%
  rename(age_group = rowname, twodose = 2)

ons_population_tbl <- read_excel(temp,
                                 sheet = "Population estimates (ONS)",
                                 range = "D13:Q14") %>%
  t() %>%
  as_tibble(rownames = NA,
            .name_repair = "unique") %>%
  rownames_to_column() %>%
  rename(age_group = rowname, ons_population_estimate = 2)

nims_population_tbl <- read_excel(temp,
                                  sheet = "Population estimates (NIMS)",
                                  range = "F13:S14") %>%
  t() %>%
  as_tibble(rownames = NA,
            .name_repair = "unique") %>%
  rownames_to_column() %>%
  rename(age_group = rowname, nims_population_estimate = 2)

nhs_content_information <- read_excel(path = temp,
                                      sheet = "Contents",
                                      range = "B4:C7") %>%
  t() %>%
  as_tibble(rownames = NA, .name_repair = "unique") %>%
  rownames_to_column() %>%
  rename(period = rowname, source = 2, basis = 3, publish_date = 4) %>%
  slice_tail()

nhs_vaccine_df <- full_join(onedose_tbl, twodose_tbl,
                            by = "age_group") %>%
  full_join(ons_population_tbl, by = "age_group") %>%
  full_join(nims_population_tbl, by = "age_group") %>%
  rowwise() %>%
  mutate(ons_onedosecoverage = min(onedose/ons_population_estimate,1),
         ons_twodosecoverage = min(twodose/ons_population_estimate,1),
         nims_onedosecoverage = onedose/nims_population_estimate,
         nims_twodosecoverage = twodose/nims_population_estimate)

nhs_vaccine_tidy_df <- nhs_vaccine_df %>%
  select(1, 6:9) %>%
  pivot_longer(cols = 2:5,
               names_to = c("population", "measure"),
               names_pattern = "(.+)_(.+)",
               values_to = "coverage")

nhs_facet_labels <- c(ons = "ONS 2019 population estimates",
                      nims = "NIMS counts")

## Make the graph
# This code makes a graph of vaccine coverage by age group
nhs_vaccine_subtitle <- paste0("Covid-19 vaccination coverage in English adults (18 or over) by age group and by population estimates, in the period ",
                               nhs_content_information$period, ".")

nhs_vaccine_caption <- paste0("Data: NHS England: Covid-19 weekly announced vaccinations, ",
                              nhs_content_information$publish_date, ".")

nhs_vaccine_gg <- nhs_vaccine_tidy_df %>%
  filter(age_group != "Under 18") %>%
  ggplot(aes(y = age_group, x = coverage, fill = measure)) +
  geom_col(position = "identity") +
  facet_wrap(~population,
             labeller = as_labeller(nhs_facet_labels)) +
  scale_x_continuous(labels = percent_format(scale = 100)) +
  scale_fill_manual(name = "",
                    labels = c("One dose", "Two doses"),
                    values = c("#800000", "#008080")) +
  labs(title = "Covid vaccine coverage exceeds 90% in some age groups in England.",
       subtitle = str_wrap(nhs_vaccine_subtitle, width = 100),
       x = "Vaccination coverage (% of the population that has received a vaccine)",
       y = "Age group",
       caption = nhs_vaccine_caption) +
  theme(axis.ticks.y = element_blank())