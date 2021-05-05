## Packages and themes
# First, download the packages we need
library(tidyverse)
library(readxl)
library(lubridate)

## Next, I set the plotting theme
theme_clean4 <- theme_bw(base_family="Gill Sans Nova") + 
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

## Draw and tidy data
# Draw the data from a prepared file
senedd_constituency_df <- read_excel("Opinion Polling/Senedd 2021 Vote Intention Polls - 2021-05-05.xlsx",
                                     sheet = "DATA-Constituency",
                                     col_types = c("text", "text", "date", "date", "text",
                                                   "numeric", "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric", "text")) %>%
  mutate(vote_type = "constituency")

# This is the Senedd list vote intention
senedd_list_df <- read_excel("Opinion Polling/Senedd 2021 Vote Intention Polls - 2021-05-05.xlsx",
                                     sheet = "DATA-List",
                                     col_types = c("text", "text", "date", "date", "text",
                                                   "numeric", "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric", "text")) %>%
  mutate(vote_type = "list")

# We bind these tables
senedd_combined_df <- bind_rows(senedd_constituency_df,
                                senedd_list_df)

# That is put in the tidy format
senedd_tidy_df <- senedd_combined_df %>%
  select(company, fieldwork_end_date, 7:15, vote_type) %>%
  mutate(fieldwork_end_date = as_date(fieldwork_end_date)) %>%
  pivot_longer(cols = 3:11,
               names_to = "party",
               values_to = "vi_share")

# Next, I set factors and labels for the parties
senedd_tidy_df$party <- factor(senedd_tidy_df$party,
                                     levels = c("lab", "con", "pc", "ldem",
                                                "ab", "ref", "grn", "ukip", "oth"))

senedd_party_labels <- c(lab = "Labour",
                         con = "Conservatives",
                         pc = "Plaid Cymru",
                         ldem = "Liberal Democrats",
                         ab = "Abolish",
                         ref = "Reform UK",
                         grn = "Green Party",
                         ukip = "UKIP",
                         oth = "Other")

bpc_colours <- c("#1a383c", "#00BCF2", "#F3ACB3", "#FECE4E", "#7C64C3")

## Create the graphs
# The goal is to create a 'mosaic' graph, coloured by polling company
senedd_constituency_gg <- senedd_tidy_df %>%
  filter(party %in% c("lab", "con", "pc", "ldem"),
         vote_type == "constituency") %>%
  ggplot(aes(x = fieldwork_end_date,
             y = vi_share,
             group = company)) +
  geom_point(aes(colour = company),
             size = 3,
             alpha = 0.5) +
  facet_wrap(~party,
             labeller = as_labeller(senedd_party_labels)) +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(0,40)) +
  scale_colour_manual(values = bpc_colours) +
  labs(title = "Labour lead in Senedd constituency vote intentions.",
       subtitle = "Senedd constituency vote intention estimates [%], by company and fieldwork end date.",
       caption = "Source: British Polling Council member archives.",
       x = "Fieldwork end date",
       y = "Constituency vote intention [%]",
       colour = "Polling Company")

# This is for the list
senedd_list_gg <- senedd_tidy_df %>%
  filter(party %in% c("lab", "con", "pc", "ldem"),
         vote_type == "list") %>%
  ggplot(aes(x = fieldwork_end_date,
             y = vi_share,
             group = company)) +
  geom_point(aes(colour = company),
             size = 3,
             alpha = 0.5) +
  facet_wrap(~party,
             labeller = as_labeller(senedd_party_labels)) +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(0,40)) +
  scale_colour_manual(values = bpc_colours) +
  labs(title = "Labour lead across different companies in Senedd list estimates.",
       subtitle = "Senedd list vote intention estimates [%], by company and fieldwork end date.",
       caption = "Source: British Polling Council member archives.",
       x = "Fieldwork end date",
       y = "List vote intention [%]",
       colour = "Polling Company")