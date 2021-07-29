## Packages and themes
# First, install the packages we need
library(tidyverse)
library(curl)

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

# Set the colours
clean4_colours <- c("#008080", "#800000", "#0941B3", "#D99C16", "#089C32")

## Draw data
# We draw data straight from the online file
temp <- tempfile()
phe_vaccine_latest_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumVaccinationFirstDoseUptakeByPublishDatePercentage&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage&metric=cumVaccinationSecondDoseUptakeByPublishDatePercentage&metric=cumVaccinationSecondDoseUptakeByVaccinationDatePercentage&format=csv"
temp <- curl_download(url = phe_vaccine_latest_url, destfile = temp,
                      quiet = FALSE, mode = "wb")

phe_vaccine_latest_tbl <- read_csv(temp) %>%
  mutate(onsPopulationEstimate = "Mid-2020")

temp <- tempfile()
phe_vaccine_archive_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumVaccinationFirstDoseUptakeByPublishDatePercentage&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage&metric=cumVaccinationSecondDoseUptakeByPublishDatePercentage&metric=cumVaccinationSecondDoseUptakeByVaccinationDatePercentage&format=csv&release=2021-07-21"
temp <- curl_download(url = phe_vaccine_archive_url, destfile = temp,
                      quiet = FALSE, mode = "wb")

phe_vaccine_archive_tbl <- read_csv(temp) %>%
  mutate(onsPopulationEstimate = "Mid-2019")

# We then bind the two tables together
phe_vaccine_df <- bind_rows(phe_vaccine_latest_tbl, phe_vaccine_archive_tbl) %>%
  filter(date >= "2021-01-10", date <= "2021-07-20") %>%
  rename(firstDosePublishDate = 5, secondDosePublishDate = 7) %>%
  dplyr::select(areaName, date, onsPopulationEstimate,
                firstDosePublishDate, secondDosePublishDate) %>%
    pivot_longer(cols = 4:5,
                 names_to = "measure",
                 values_to = "percentage")

# Set the facet labels
measure_labels <- c("First dose", "Second dose")
names(measure_labels) <- c("firstDosePublishDate", "secondDosePublishDate")

## Create the graph
# This creates the graph showing the revision by doses uptake
phe_vaccine_gg <- phe_vaccine_df %>%
  ggplot(aes(x = date, y = percentage, colour = onsPopulationEstimate)) +
  geom_line(size = 1.2) +
  facet_wrap(~measure,
             labeller = labeller(measure = measure_labels)) +
  scale_colour_manual(name = "ONS (18+) population estimate denominator",
                      values = clean4_colours) +
  scale_x_date(date_labels = "%d-%b\n%Y") +
  labs(title = "With updated estimates, vaccine coverage in England underwent small revisions.",
       subtitle = str_wrap("Estimated cumulative vaccine coverage (total vaccinations divided by adult population estimates) rounded to 0.1 points, by reporting date in England using different population estimates.",
                           width = 100),
       x = "Reporting date",
       y = "Vaccine coverage [%]",
       caption = "Source: Public Health England Coronavirus (Covid-19) Dashboard data download.") +
  theme(plot.title.position = "plot")