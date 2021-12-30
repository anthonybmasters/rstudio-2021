## Packages and Themes
# First, we install the packages we need
library(tidyverse)
library(curl)
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

## Drawing and tidying the data
# We draw the UKHSA figures for England straight from the online file
# https://coronavirus.data.gov.uk/details/download
ukhsa_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesLFDConfirmedPCRBySpecimenDate&metric=newCasesLFDOnlyBySpecimenDate&metric=newCasesPCROnlyBySpecimenDate&format=csv"
ukhsa_subtitle <- "Confirmed cases for test types in England by specimen date, from 1st April 2020 to 21st December 2021. Positive lateral flow device tests can be confirmed by PCR tests within 72 hours. Figures are subject to change, as new cases report."
ukhsa_caption <- "Source: UKHSA Coronavirus (Covid-19) in the UK Dashboard - Data Download (on 30th December 2021)."
ukhsa_start_date <- as_date("2020-04-01")
ukhsa_end_date <- as_date("2021-12-21")

temp <- tempfile()
temp <- curl_download(url = ukhsa_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

# Next, we read and tidy that data file
ukhsa_cases_df <- read_csv(file = temp) %>%
  dplyr::rename(lfd_confirmed_by_pcr = 5,
                lfd_only = 6,
                pcr_only = 7) %>%
  pivot_longer(cols = 5:7,
               names_to = "measure",
               values_to = "cases_by_specimen_date") %>%
  drop_na() %>%
  dplyr::filter(date >= ukhsa_start_date,
                date <= ukhsa_end_date) %>%
  dplyr::select(date, measure, cases_by_specimen_date)

# Calculate the proportions of each test type by date
ukhsa_cases_df <- ukhsa_cases_df %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(cases_prop = cases_by_specimen_date / sum(cases_by_specimen_date))

# Set the factors, colours and breaks for the graph
ukhsa_cases_df$measure <- factor(ukhsa_cases_df$measure,
                                 levels = c("lfd_only", "lfd_confirmed_by_pcr",
                                            "pcr_only"))

ukhsa_cases_colour <- c("#800000", "#0941B3", "#008080")

ukhsa_date_breaks = seq(as_date("2020-06-01"), as_date("2021-12-01"),
                        by = "6 months")

# A table for November 2021
ukhsa_cases_df %>% filter(date >= as_date("2021-11-01"),
                          date <= as_date("2021-11-30")) %>%
  group_by(measure) %>%
  summarise(total = sum(cases_by_specimen_date)) %>%
  mutate(share = total/sum(total))

## Making the graphs
# The first graph shows confirmed cases by test type
ukhsa_cases_gg1 <- ukhsa_cases_df %>%
  ggplot(aes(x = date, y = cases_by_specimen_date, group = measure)) +
  geom_col(aes(fill = measure)) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               breaks = ukhsa_date_breaks,
               expand = c(0,0)) +
  scale_y_continuous(labels = comma_format()) +
  scale_fill_manual(values = ukhsa_cases_colour,
                    guide = "none") +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Confirmed cases by test type",
       x = "Specimen date",
       y = "")
  
# The second graph shows the proportion in each test type by date
ukhsa_cases_gg2 <- ukhsa_cases_df %>%
  ggplot(aes(x = date, y = cases_prop, group = measure)) +
  geom_line(aes(colour = measure), size = 1.5) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               breaks = ukhsa_date_breaks,
               expand = c(0,0)) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_colour_manual(values = ukhsa_cases_colour,
                      guide = "none") +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) + 
  labs(subtitle = "Proportion of cases by test type",
       x = "Specimen date",
       y = "") +
  annotate("text", x = as_date("2020-06-01"), y = 0.95,
           label = "PCR only",
           hjust = 0, size = 7, fontface = "bold", colour = "#008080") +
  annotate("text", x = as_date("2020-06-01"), y = 0.15,
           label = "LFD (positive PCR)",
           hjust = 0, size = 7, fontface = "bold", colour = "#0941B3") +
  annotate("text", x = as_date("2020-06-01"), y = 0.05,
           label = "LFD only",
           hjust = 0, size = 7, fontface = "bold", colour = "#800000")

# We put these two graphs together
ukhsa_cases_gg <- ukhsa_cases_gg1 + ukhsa_cases_gg2 +
  plot_annotation(title = "In November 2021, about one in five new cases in England involved a rapid test.",
                  subtitle = str_wrap(ukhsa_subtitle, width = 120),
                  caption = ukhsa_caption)