# Load required libraries
library(tidyverse)

# Read in the Netflix dataset
netflix <- read_csv("netflix_titles (1).csv")


# Seeing the view the structure of the dataset
glimpse(netflix)

#  Movies vs TV Shows
movievsshows_count <- netflix %>%
  count(type)

print(movievsshows_count)


# Count content by country (top 10)
country_count <- netflix %>%
  filter(!is.na(country)) %>%
  separate_rows(country, sep = ", ") %>%
  count(country, sort = TRUE)

head(country_count, 10)

# Rate Disrbution 
rate_count <- netflix %>%
  filter(
    !is.na(rating),
    !str_detect(rating, "min")
  ) %>%
  count(rating, sort = TRUE)

print(rate_count)

# The total number of original content made by netflix over the years
ggplot(year_count, aes(x = release_year, y = n)) +
  geom_line() +
  labs(
    title = "Netflix Titles by Release Year",
    x = "Release Year",
    y = "Number of Titles"
  )


