library(dplyr)
library(tidyr)
library(stringr)

# Load data
nola_business <- readRDS("data/nola_business.rds")
nola_reviews  <- readRDS("data/nola_reviews.rds")

# 1. One row per business-category combination, cleaned
restaurant_categories <- nola_business |>
  filter(!is.na(categories)) |>
  select(business_id, name, categories) |>
  separate_longer_delim(categories, delim = ",") |>
  rename(category = categories) |>
  mutate(category = str_to_lower(str_squish(category))) |>
  filter(category != "") |>
  distinct(business_id, category, .keep_all = TRUE)

# 2. Count categories (look here to confirm the names you want)
category_counts <- restaurant_categories |>
  count(category, sort = TRUE)

head(category_counts, 25)

# 3. Select categories (lowercase to match the cleaned data)
selected_categories <- c("cajun/creole", "southern", "seafood")

selected_category_businesses <- restaurant_categories |>
  filter(category %in% selected_categories) |>
  distinct(business_id)

# 4. Keep only reviews of the selected restaurants
selected_reviews <- nola_reviews |>
  semi_join(selected_category_businesses, by = "business_id") |>
  inner_join(
    nola_business |> select(business_id, name, stars_business = stars),
    by = "business_id"
  )

# Sanity checks
nrow(selected_reviews)
n_distinct(selected_reviews$business_id)

saveRDS(selected_reviews, "data/selected_reviews.rds")