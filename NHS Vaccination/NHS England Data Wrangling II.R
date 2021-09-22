## Packages and Themes
# First, we install the packages we need
library(tidyverse)
library(readxl)
library(curl)
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
# We draw the data from an online file
temp <- tempfile()
nhs_vaccine_url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/09/COVID-19-weekly-announced-vaccinations-16-September-2021.xlsx"
temp <- curl_download(url = nhs_vaccine_url, destfile = temp,
                      quiet = FALSE, mode = "wb")

onedose_tbl <- read_excel(temp,
                          sheet = "NHS Region",
                          range = "D12:Q13") %>%
  t() %>%
  as_tibble(rownames = NA,
            .name_repair = "unique") %>%
  rownames_to_column() %>%
  rename(age_group = rowname, onedose = 2)

twodose_tbl <- read_excel(temp,
                          sheet = "NHS Region",
                          range = "U12:AH13") %>%
  t() %>%
  as_tibble(rownames = NA,
            .name_repair = "unique") %>%
  rownames_to_column() %>%
  rename(age_group = rowname, twodose = 2)

onspop_tbl <- read_excel(temp,
                         sheet = "Population estimates (ONS 2020)",
                         range = "B15:D29") %>%
  dplyr::select(-2) %>%
  rename(age_group = 1, ons_population_est = 2)

nimspop_tbl <- read_excel(temp,
                         sheet = "Population estimates (NIMS)",
                         range = "F13:S14") %>%
  t() %>%
  as_tibble(rownames = NA,
            .name_repair = "unique") %>%
  rownames_to_column() %>%
  rename(age_group = rowname, nims_population_count = 2)

nhs_content_information <- read_excel(path = temp,
                                      sheet = "Contents",
                                      range = "B4:C7") %>%
  t() %>%
  as_tibble(rownames = NA, .name_repair = "unique") %>%
  rownames_to_column() %>%
  rename(period = rowname, data_source = 2, basis = 3, publish_date = 4) %>%
  slice_tail()

nhs_vaccine_df <- full_join(onedose_tbl, twodose_tbl,
                            by = "age_group") %>%
  full_join(onspop_tbl, by = "age_group") %>%
  full_join(nimspop_tbl, by = "age_group") %>%
  rename("1st" = 2, "2nd" = 3, ons = 4, nims = 5) %>%
  pivot_longer(cols = 2:3,
               names_to = "dose_number",
               values_to = "vaccinated") %>%
  mutate(vaxprop_ons = vaccinated/ons,
         vaxprop_nims = vaccinated/nims)

# Next, we create an 'All adults' age group row
nhs_vaccine_df <- nhs_vaccine_df %>%
  filter(age_group != "Under 18") %>%
  group_by(dose_number) %>%
  summarise(vaccinated=sum(vaccinated),
            ons = sum(ons),
            nims = sum(nims)) %>%
  ungroup() %>%
  mutate(vaxprop_ons = vaccinated/ons,
         vaxprop_nims = vaccinated/nims,
         age_group = "All adults (18+)") %>%
  bind_rows(nhs_vaccine_df)

# These two frames help with our graphs
pop_compare_df <- nhs_vaccine_df %>%
  filter(dose_number == "1st") %>%
  dplyr::select(age_group, ons, nims) %>%
  pivot_longer(cols = 2:3,
               names_to = "pop_source",
               values_to = "pop_estimate")

nhse_1stdose_alladults_df <- nhs_vaccine_df %>%
  filter(dose_number == "1st",
         age_group == "All adults (18+)")

## Making the graphs
nhs_vaccine_subtitle = paste0("First dose vaccine coverage in England by different population denominators for adult age groups (18+), in the period ",
                              nhs_content_information$period, ".")

nhs_vaccine_caption <- paste0("Data: NHS England: Covid-19 weekly announced vaccinations, ",
                              nhs_content_information$publish_date, ".")

# The first graph shows the different population counts
pop_compare_gg <- pop_compare_df %>%
  filter(age_group != "Under 18", age_group != "All adults (18+)") %>%
  ggplot(aes(x = pop_estimate, y = age_group, group = pop_source)) +
  geom_col(aes(fill = pop_source),
           position = "dodge") +
  annotate("text", y = 11.3, x = 2900000,
           size = 5, colour = "#008080",
           label = "ONS mid-year population estimate for 2020",
           hjust = 0, fontface = "bold") +
  annotate("text", y = 10.7, x = 2900000,
           size = 5, colour = "#800000",
           label = "NIMS database count",
           hjust = 0, fontface = "bold") +
  scale_fill_manual(values = c("#800000", "#008080"),
                    guide = "none") +
  scale_x_continuous(labels = scales::label_number_si(),
                     expand = c(0,0)) +
  labs(title = "The NIMS database in England includes some duplicate records.",
       subtitle = str_wrap("English population statistics by adult (18+) age groups using two different sources: the Office for National Statistics (ONS) mid-year estimates and the National Immunisation Management System (NIMS).", width = 100),
       x = "Population estimate", y = "Age group",
       caption = nhs_vaccine_caption) +
  theme(plot.title.position = "plot")

# The second graph takes inspiration from Colin Angus
# https://github.com/VictimOfMaths/Routine-Data/blob/master/EngPopComparisons.R
nhse_vaccine_1st_gg <- nhs_vaccine_df %>%
  filter(age_group != "Under 18", dose_number == "1st") %>%
  ggplot() +
  geom_segment(aes(x=vaxprop_ons, xend = vaxprop_nims,
                   y = age_group, yend = age_group),
               show.legend = FALSE, size = 2) +
  geom_point(aes(x = vaxprop_ons, y = age_group),
             size = 9, colour = "#008080") +
  geom_point(aes(x = vaxprop_nims, y = age_group),
             size = 9, colour = "#800000") +
  geom_text(aes(x = vaxprop_ons, y = age_group,
                label = round(100*vaxprop_ons)),
            colour = "white", size = 5) +
  geom_text(aes(x = vaxprop_nims, y = age_group,
                label = round(100*vaxprop_nims)),
            colour = "white", size = 5) +
  geom_text(data = nhse_1stdose_alladults_df,
            aes(x = vaxprop_ons, y = age_group),
            nudge_x = 0.02, label = "ONS",
            fontface = "bold", size = 5, colour = "#008080") +
  geom_text(data = nhse_1stdose_alladults_df,
            aes(x = vaxprop_nims, y = age_group),
            nudge_x = -0.02, label = "NIMS",
            fontface = "bold", size = 5, colour = "#800000") +
  scale_x_continuous(label = scales::percent_format(accuracy = 1),
                     limits = c(0.6, 1.01), expand = c(0,0)) +
  labs(title = "Using different population estimates leads to different vaccine coverage statistics.",
       subtitle = str_wrap(nhs_vaccine_subtitle, width = 100),
       x = "Proportion receiving their first Covid-19 vaccine dose",
       y = "Age group",
       caption = nhs_vaccine_caption) +
  theme(plot.title.position = "plot")