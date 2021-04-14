## Packages and themes
# First, download the packages we need
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

# Next, I set the plotting theme
theme_clean3 <- theme_bw(base_family="Gill Sans Nova") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 20, face = "italic", margin = margin(b=12)),
        plot.caption = element_text(size = 14),
        plot.margin = unit(c(.5,.5,.5,.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
theme_set(theme_clean3)

## Draw and tidy the data
# Draw the statistics from a prepared file
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/bulletins/deathsregisteredweeklyinenglandandwalesprovisional/weekending2april2021
ons_deathregistration_figure3_df <- read_excel("ONS Mortality Rates/ONS Weekly Death Registrations Figure 3 - 2021-04-14.xlsx",
                                               sheet = "DATA",
                                               col_types = c("numeric", "text", "date", "numeric", "numeric", "numeric"))

ons_deathreg_tidy_df <- ons_deathregistration_figure3_df %>%
  mutate(week_end_date = as_date(week_end_date)) %>%
  pivot_longer(cols = 4:6,
               names_to = "ons_measure",
               values_to = "count")

## Set the hpline function
# If you are able to do so in your version of R
# library(ungeviz)
# If not, this is copied from Claus Wilke's ungeviz documentation
# https://github.com/wilkelab/ungeviz/blob/master/R/geom_hpline.R
geom_hpline <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHpline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_hpline
#' @format NULL
#' @usage NULL
#' @export
GeomHpline <- ggproto("GeomHpline", GeomSegment,
                      required_aes = c("x", "y"),
                      non_missing_aes = c("size", "colour", "linetype", "width"),
                      default_aes = aes(
                        width = 0.5, colour = "black", size = 2, linetype = 1,
                        alpha = NA
                      ),
                      
                      draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                                            lineend = "butt", linejoin = "round", na.rm = FALSE) {
                        data <- mutate(data, x = x - width/2, xend = x + width, yend = y)
                        ggproto_parent(GeomSegment, self)$draw_panel(
                          data, panel_params, coord, arrow = arrow, arrow.fill = arrow.fill,
                          lineend = lineend, linejoin = linejoin, na.rm = na.rm
                        )
                      }
)

## Create the graph
ons_week_breaks <- c("2020-01-03", "2020-03-13", "2020-05-22", "2020-07-31", "2020-10-09", "2020-12-18", "2021-04-02") %>%
  as_date()

ons_deathreg_figure3_gg <-
  ggplot(data = filter(ons_deathreg_tidy_df,
                       ons_measure != "all_deaths_2015_2019_average"),
         aes(x = week_end_date)) +
  geom_bar(aes(y = count,
               fill = ons_measure),
           position = "stack",
           stat = "identity") +
  geom_hpline(data = filter(ons_deathreg_tidy_df,
                            ons_measure == "all_deaths_2015_2019_average"),
              aes(x = week_end_date,
                  y = count,
                  linetype = ons_measure),
              stat = "identity",
              width = 6, size = 2) +
  scale_x_date(breaks = ons_week_breaks,
               date_labels = "%d-%b\n%Y",
               expand = c(0,5)) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(0,25000)) +
  scale_linetype_manual(name = "",
                        labels = "2015-2019 average",
                        values = "solid") +
  scale_fill_manual(name = "",
                    labels = c("Deaths involving COVID-19", "Deaths not involving COVID-19"),
                    values = c("#800000", "#008080")) +
  labs(title = "England and Wales had two periods of sustained high deaths.",
       subtitle = str_wrap("Number of deaths registered by week in England and Wales, 28th December 2019 to 2nd April 2021.",
                           width = 60),
       x = "Week end date",
       y = "",
       caption = "Source: Office for National Statistics – Deaths registered weekly in England and Wales") +
  geom_text(x = as_date("2021-01-15"), y = 21000,
            label = "Bank holidays\naffected registrations",
            size = 6) +
  geom_curve(x = as_date("2021-01-01"), xend = as_date("2020-12-30"),
             y = 19000, yend = 13000,
             arrow = arrow(), curvature = 0.2, size = 1.2) +
  geom_curve(x = as_date("2021-02-20"), xend = as_date("2021-04-02"),
             y = 19000, yend = 12000,
             arrow = arrow(), curvature = -0.2, size = 1.2)