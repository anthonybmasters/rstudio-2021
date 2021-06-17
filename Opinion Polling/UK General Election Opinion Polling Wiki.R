## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)

# We also install Jack Bailey's britpol package
# https://github.com/jackobailey/britpol
devtools::install_github("jackobailey/britpol")
library(britpol)

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

## Draw and tidy data
# Draw in opinion polling statistics from a prepared file
# This is based on the Wikipedia page
# https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_United_Kingdom_general_election
opinion_polls_wiki_df <- read_excel("Opinion Polling/UK General Election Opinion Polling Wiki - 2021-06-16.xlsx",
                                    sheet = "DATA",
                                    col_types = c("text", "text", "text",
                                                  "date", "date", "text",
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "text")) %>%
  mutate(fw_date_start = as_date(fw_date_start),
         fw_date_end = as_date(fw_date_end))

opinion_polls_tidy_df <- opinion_polls_wiki_df %>%
  select(company, fw_date_end, con, lab, ldem) %>%
  pivot_longer(cols = 3:5,
               names_to = "party",
               values_to = "vote_intention_share")

# We can also tidy the PollBasePro posterior estimates
pollbasepro_tidy_df <- pollbasepro %>%
  filter(date >= as_date("2019-12-12")) %>%
  pivot_longer(cols = 2:7,
               names_to = c("party", ".values"),
               names_pattern = "(.+)_(.+)") %>%
  pivot_wider(names_from = ".values",
              values_from = "value") %>%
  mutate(lower = est - qnorm(0.975)*err,
         upper = est + qnorm(0.975)*err)

## Making graphs
# First, we set a plotting function
opinion_polls_wiki_plot <- function(span_input) {
  opinion_polls_wiki_gg <- opinion_polls_tidy_df %>%
    ggplot(aes(x = fw_date_end,
               y = 100*vote_intention_share,
               colour = party)) +
    geom_point(size = 2, alpha = 0.3) +
    geom_smooth(aes(fill = party),
                method = "loess",
                formula = "y ~ x",
                span = span_input,
                alpha = 0.1) +
    scale_x_date(date_breaks = "6 months",
                 date_labels = "%d-%b\n%Y") +
    scale_y_continuous(labels = label_percent(scale = 1),
                       limits = c(0,60), expand = c(0,0)) +
    labs(subtitle = paste0("Span = ", span_input),
         x = "Fieldwork end-date",
         y = "VI share") +
    theme(plot.subtitle = element_text(size = 20,
                                       face = "plain")) +
    scale_colour_manual(aesthetics = c("colour", "fill"),
                        values = c("#0087DC", "#E4003B", "#FAA61A")) +
    guides(colour = "none", fill = "none")
  return(opinion_polls_wiki_gg)
}

opinion_polls_wiki_gg_01 <- opinion_polls_wiki_plot(span_input = 0.1) +
  annotate("text", x = as_date("2020-07-01"), y = 50,
           label = "Conservatives", hjust = 0, size = 7,
           colour = "#0087DC", fontface = "bold") +
  annotate("text", x = as_date("2020-07-01"), y = 32,
           label = "Labour", hjust = 0, size = 7,
           colour = "#E4003B", fontface = "bold") +
  annotate("text", x = as_date("2020-07-01"), y = 12,
           label = "Liberal Democrats", hjust = 0, size = 7,
           colour = "#FAA61A", fontface = "bold")

opinion_polls_wiki_gg_03 <- opinion_polls_wiki_plot(span_input = 0.3)
opinion_polls_wiki_gg_05 <- opinion_polls_wiki_plot(span_input = 0.5)
opinion_polls_wiki_gg_07 <- opinion_polls_wiki_plot(span_input = 0.7)

# We put these mini-plots in a patchwork
opinion_polls_wiki_gg <- 
  (opinion_polls_wiki_gg_01 + opinion_polls_wiki_gg_03) /
  (opinion_polls_wiki_gg_05 + opinion_polls_wiki_gg_07) +
  plot_annotation(title = "Different spans in the Loess curve can change the story.",
                  subtitle = "UK/GB vote intention poll estimates between 1st January 2020 and 16th June 2021.\nThe local regression (Loess) curves are shown by different spans.",
                  caption = "Source: Wikipedia: Opinion polling for the next United Kingdom general election.") +
  theme(plot.title.position = "plot")

# This graph shows the PollBasePro posterior estimates by day
pollbasepro_gg <- pollbasepro_tidy_df %>%
  ggplot(aes(x = date, y = est,
             ymin = lower, ymax = upper,
             colour = party, fill = party)) +
  geom_line(size = 1.5) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(labels = label_percent(scale = 100,
                                            accuracy = 1),
                     limits = c(0, 0.6), expand = c(0,0)) +
  labs(title = "For 13th June 2021, the Conservative vote intention estimate is 42% (39% - 45%).",
       subtitle = "Posterior estimates and credible intervals of UK vote intention since 12th December 2019.",
       caption = "Source: PollBasePro (Bailey, Pack, and Mansillo, 2021).",
       x = "Date",
       y = "Modelled VI estimates") +
  theme(plot.title.position = "plot") +
  scale_colour_manual(aesthetics = c("colour", "fill"),
                      values = c("#0087DC", "#E4003B", "#FAA61A")) +
  guides(colour = "none", fill = "none") +
  annotate("text", x = as_date("2020-07-01"), y = 0.46,
           label = "Conservatives", hjust = 0, size = 9,
           colour = "#0087DC", fontface = "bold") +
  annotate("text", x = as_date("2020-07-01"), y = 0.34,
           label = "Labour", hjust = 0, size = 9,
           colour = "#E4003B", fontface = "bold") +
  annotate("text", x = as_date("2020-07-01"), y = 0.12,
           label = "Liberal Democrats", hjust = 0, size = 9,
           colour = "#FAA61A", fontface = "bold")