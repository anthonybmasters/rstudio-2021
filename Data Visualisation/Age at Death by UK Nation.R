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

# Set the colours
clean4_colours <- c("#008080", "#800000", "#0941B3", "#D99C16", "#089C32")

## Draw data
# We draw the data straight from the online file
temp <- tempfile()
ons_lifetables_url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2flifeexpectancies%2fdatasets%2fnumbersdyingbetweenexactagexandx1dxbysingleyearofage%2fcurrent/dxtimeseries.xlsx"
temp <- curl_download(url = ons_lifetables_url, destfile = temp,
                      quiet = FALSE, mode = "wb")

eng_m_tbl <- read_excel(temp,
                        sheet = "E Males dx",
                        range = "A9:AM111") %>%
  drop_na() %>% rename(age = 1) %>%
  mutate(nation = "England", sex = "Males")

eng_f_tbl <- read_excel(temp,
                        sheet = "E Females dx",
                        range = "A9:AM111") %>%
  drop_na() %>% rename(age = 1) %>%
  mutate(nation = "England", sex = "Females")

wal_m_tbl <- read_excel(temp,
                        sheet = "W Males dx",
                        range = "A9:AM111") %>%
  drop_na() %>% rename(age = 1) %>%
  mutate(nation = "Wales", sex = "Males")

wal_f_tbl <- read_excel(temp,
                        sheet = "W Females dx",
                        range = "A9:AM111") %>%
  drop_na() %>% rename(age = 1) %>%
  mutate(nation = "Wales", sex = "Females")

sco_m_tbl <- read_excel(temp,
                        sheet = "S Males dx",
                        range = "A9:AM111") %>%
  drop_na() %>% rename(age = 1) %>%
  mutate(nation = "Scotland", sex = "Males")

sco_f_tbl <- read_excel(temp,
                        sheet = "S Females dx",
                        range = "A9:AM111") %>%
  drop_na() %>% rename(age = 1) %>%
  mutate(nation = "Scotland", sex = "Females")

ni_m_tbl <- read_excel(temp,
                        sheet = "NI Males dx",
                        range = "A9:AM111") %>%
  drop_na() %>% rename(age = 1) %>%
  mutate(nation = "Northern Ireland", sex = "Males")

ni_f_tbl <- read_excel(temp,
                        sheet = "NI Females dx",
                        range = "A9:AM111") %>%
  drop_na() %>% rename(age = 1) %>%
  mutate(nation = "Northern Ireland", sex = "Females")

# We bind these rows together
uk_nations_dx_df <- bind_rows(eng_m_tbl, eng_f_tbl,
                              wal_m_tbl, wal_f_tbl,
                              sco_m_tbl, sco_f_tbl,
                              ni_m_tbl, ni_f_tbl) %>%
  pivot_longer(cols = 2:39,
               names_to = "period",
               values_to = "dx_period")

## Make the graph
uk_nations_dx_gg <- uk_nations_dx_df %>%
  filter(period == "2017-19") %>%
  ggplot(aes(x = age, y = dx_period, colour = nation)) +
  geom_line(size = 1.3) +
  facet_wrap(~sex) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 5000),
                     labels = scales::label_comma()) +
  scale_colour_manual(name = "Nation",
                      values = clean4_colours) +
  labs(title = "Assuming 2017-19 UK mortality, the modal age at death is 88 for women.",
       subtitle = str_wrap("Expected numbers of people dying by sex and single year of age (dx), applying age-specific mortality in 2017-19 to a synthetic cohort of 100,000 live births in each of the four UK nations.",
                           width = 100),
       x = "Age at Death",
       y = "Distribution of 100,000 deaths",
       caption = "Source: Office for National Statistics: Numbers dying between exact age x and x+1 (dx), by single year of age.")