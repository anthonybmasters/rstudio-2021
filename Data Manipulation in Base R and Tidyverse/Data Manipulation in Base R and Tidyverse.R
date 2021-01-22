## Install packages
library(tidyverse)

## Counting a single value
# This turns the vector into 1s and 0s, and then sums.
sum(iris$Species == "setosa")

# Instead, we filter to get the Setosa species, and then count.
iris %>%
  filter(Species == "setosa") %>%
  count()

## Frequency tables
# This applies the length function to each species.
tapply(iris$Species, iris$Species, length)

# This counts by species.
iris %>% count(Species)

## Creating a subset
# The subset function the rows which satisfy the criteria.
iris_setosa_df <- subset(iris, iris$Species == "setosa")

# Filter performs that function.
iris_setosa_df <- iris %>% filter(Species == "setosa")

## Getting the top slice
# The order function puts the vector elements in a certain order,
# which is then used on the data-set. The head function takes the top six.
head(iris_setosa_df[order(iris_setosa_df$Petal.Length, decreasing = TRUE), ], n = 6)

# Here, arrange and desc (as the names imply) arrange the data-set
# in descending order. slice_head then takes the top 6 rows.
iris_setosa_df %>%
  arrange(desc(Petal.Length)) %>%
  slice_head(n = 6)

## Creating new variables
# There are two different ways of doing creating new variables in base R.
iris_setosa_df$sepal_ratio <- iris_setosa_df$Sepal.Length / iris_setosa_df$Sepal.Width
iris_setosa_df[["sepal_ratio"]] <- iris_setosa_df[["Sepal.Length"]] / iris_setosa_df[["Sepal.Width"]]

# The mutate function is to create a new variable (including overwriting).
iris_setosa_df <- iris_setosa_df %>%
  mutate(sepal_ratio = Sepal.Length / Sepal.Width)