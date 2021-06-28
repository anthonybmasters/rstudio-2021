## Install packages and set themes
# First, install the packages we need
library(tidyverse)
library(scales)
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

## Create data frames
unvaccinated_list <- c(11, 27, 52, 79, 87)
deaths_list <- c(11, 19, 27, 32, 45, 52, 60, 71, 79, 87, 95)

effectiveness_df <- expand.grid(x = 1:10, y = 1:10) %>%
  mutate(vaccination_status = if_else(row_number() %in% unvaccinated_list,
                                      "unvaccinated", "vaccinated"),
         life_status = if_else(row_number() %in% deaths_list,
                               "died", "alive"))

coverage_df <- tibble(number_vaccinated = 0:100) %>%
  mutate(vaccinated = round(0.06*number_vaccinated),
         unvaccinated = 100 - number_vaccinated,
         vaccinated_share = vaccinated / (vaccinated + unvaccinated))

## Create graphs
# The first graph shows coverage
effectiveness_gg1 <- effectiveness_df %>%
  ggplot(aes(x = x, y = y,
             fill = vaccination_status)) +
  geom_tile(colour = "white", size = 0.5) +
  coord_equal() +
  scale_x_continuous(labels = NULL, breaks = NULL, name = NULL) +
  scale_y_continuous(labels = NULL, breaks = NULL, name = NULL,
                     limits = c(0.5, 11.5)) +
  labs(subtitle = str_wrap("A vaccine has 95% coverage...", width = 20)) +
  annotate("text", x = 0.5, y = 11, size = 6, colour = "#008080",
           label = "Vaccinated", hjust = 0, fontface = "bold") +
  annotate("text", x = 5.5, y = 11, size = 6, colour = "#800000",
           label = "Unvaccinated", hjust = 0, fontface = "bold") +
  scale_fill_manual(values = c("#800000", "#008080")) +
  guides(fill = "none") +
  theme(plot.subtitle = element_text(face = "bold"))

# The second shows deaths by vaccination status
effectiveness_gg2 <- effectiveness_df %>%
  ggplot(aes(x = x, y = y,
             fill = vaccination_status, alpha = life_status)) +
  geom_tile(colour = "white", size = 0.5) +
  coord_equal() +
  scale_x_continuous(labels = NULL, breaks = NULL, name = NULL) +
  scale_y_continuous(labels = NULL, breaks = NULL, name = NULL,
                     limits = c(0.5, 11.5)) +
  labs(subtitle = str_wrap("...and lowers deaths among those vaccinated by 94%.", width = 30)) +
  annotate("text", x = 0.5, y = 11, size = 6, colour = "black",
           label = "Deaths by vaccination status", hjust = 0, fontface = "plain") +
  scale_fill_manual(values = c("#800000", "#008080")) +
  guides(fill = "none", alpha = "none") +
  theme(plot.subtitle = element_text(face = "bold"))

# We join these graphs together
effectiveness_gg <- effectiveness_gg1 + effectiveness_gg2 +
  plot_annotation(title = "Immunisation failures can exceed unvaccinated deaths.",
                  subtitle = "An infected population, with 95% vaccine coverage giving 94% protection against death.",
                  caption = "In this illustrative at-risk population, all would die after infection.")

# The left-hand graph shows deaths by vaccination status
coverage_gg1 <- coverage_df %>%
  select(-vaccinated_share) %>%
  pivot_longer(cols = 2:3,
               names_to = "vaccination_status",
               values_to = "number_of_deaths") %>%
  ggplot(aes(x = number_vaccinated, y = number_of_deaths,
             fill = vaccination_status)) +
  geom_col(position = "stack") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("#800000", "#008080")) +
  annotate("text", x = 75, y = 35, size = 6, colour = "#008080",
           label = "Vaccinated", hjust = 0, fontface = "bold") +
  annotate("text", x = 20, y = 85, size = 6, colour = "#800000",
           label = "Unvaccinated", hjust = 0, fontface = "bold") +
  labs(subtitle = "Deaths in at-risk population by status",
       x = "Vaccine coverage (number of people vaccinated)",
       y = "Number of deaths") +
  guides(fill = "none") +
  theme(plot.subtitle = element_text(face = "bold"))

coverage_gg2 <- coverage_df %>%
  ggplot(aes(x = number_vaccinated, y = vaccinated_share)) +
  geom_line(size = 1.5, colour = "#008080") +
  scale_y_continuous(expand = c(0,0),
                     labels = percent_format(accuracy = 1,
                                             scale = 100)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(subtitle = "Vaccinated share of deaths",
       x = "Vaccine coverage (number of people vaccinated)",
       y = "") +
  theme(plot.subtitle = element_text(face = "bold",
                                     colour = "#008080"))

# We put those two parts together
coverage_gg <- coverage_gg1 + coverage_gg2 +
  plot_annotation(title = "As vaccine coverage widens, total deaths decrease but the vaccinated share increases.",
                  subtitle = "An illustrative at-risk population of 100, by vaccination coverage with 94% protection against death.",
                  caption = "In this illustrative at-risk population, all would die after infection.")