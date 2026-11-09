# Top 5 highest-rated neighborhoods (NSA) at each minimum-restaurant threshold
#
# Requires `area_summary` from areaMethodolodyComparison.R, so run that script
# first in the same R session (through the analyze_layer / results / area_summary steps).

library(dplyr)
library(purrr)
library(sf)

# ---- 1. Neighborhood-level table ----
nsa_stats <- area_summary |>
  st_drop_geometry() |>
  filter(as.character(layer) == "Neighborhood (NSA)") |>
  select(area_name, n_restaurants, mean_stars)

# ---- 2. Loop over thresholds 1-10 ----
# A neighborhood qualifies when it has AT LEAST `min_n` restaurants.
top_by_threshold <- map_dfr(1:10, function(min_n) {
  nsa_stats |>
    filter(n_restaurants >= min_n, !is.na(mean_stars)) |>
    arrange(desc(mean_stars), desc(n_restaurants)) |>   # ties: more restaurants first
    slice_head(n = 5) |>
    mutate(
      threshold = min_n,
      rank = row_number(),
      mean_stars = round(mean_stars, 2)
    )
})

# ---- 3. Print each threshold's top 5 ----
for (min_n in 1:10) {
  cat("\n=== Minimum restaurants per neighborhood:", min_n, "===\n")
  
  n_qualifying <- sum(nsa_stats$n_restaurants >= min_n)
  cat("Neighborhoods meeting the threshold:", n_qualifying, "\n")
  
  top5 <- top_by_threshold |>
    filter(threshold == min_n) |>
    select(rank, area_name, n_restaurants, mean_stars)
  
  if (nrow(top5) == 0) {
    cat("No neighborhoods meet this threshold.\n")
  } else {
    print(as.data.frame(top5), row.names = FALSE)
  }
}

# ---- 4. How stable are the rankings? ----
# Number of thresholds (out of 10) at which each neighborhood makes the top 5
cat("\n=== Neighborhoods by number of thresholds in the top 5 ===\n")
top_by_threshold |>
  count(area_name, name = "times_in_top5", sort = TRUE) |>
  as.data.frame() |>
  print(row.names = FALSE)