library(dplyr)
library(purrr)
library(readr)
library(sf)
library(ggplot2)
library(tigris)
library(jsonlite)
options(tigris_use_cache = TRUE)

# ---- 1. Restaurants -> sf points ----
restaurant_points <- new_orleans_map_data |>
  st_drop_geometry() |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  select(business_id, name, stars, review_count, latitude, longitude)

restaurant_points <- restaurant_points |>
  mutate(adj_rating = NA_real_)
# ---- 3. Area layers (all standardized to area_id, area_name, geometry) ----

# Census layers: Orleans + Jefferson so the Harahan/Metairie/Kenner points can match
zcta <- tigris::zctas(cb = TRUE, year = 2020) |>
  st_transform(4326) |>
  st_filter(st_as_sfc(st_bbox(restaurant_points)))
zcta_id <- grep("^ZCTA5CE", names(zcta), value = TRUE)[1]
zcta <- zcta |> transmute(area_id = .data[[zcta_id]], area_name = .data[[zcta_id]])

tracts <- tigris::tracts(state = "LA", county = c("Orleans", "Jefferson"),
                         cb = TRUE, year = 2024) |>
  st_transform(4326) |>
  transmute(area_id = GEOID, area_name = GEOID)

block_groups <- tigris::block_groups(state = "LA", county = c("Orleans", "Jefferson"),
                                     cb = TRUE, year = 2024) |>
  st_transform(4326) |>
  transmute(area_id = GEOID, area_name = GEOID)

# CSV layers: these need a geometry column stored as WKT text
# FIRST run names(read_csv("data/wards.csv")) and set the column names below.
read_area_csv <- function(path, wkt_col, id_col, name_col = id_col) {
  x <- read_csv(path, show_col_types = FALSE) |>
    filter(!is.na(.data[[wkt_col]])) |>
    st_as_sf(wkt = wkt_col, crs = 4326) |>
    st_make_valid()
  
  st_sf(
    area_id   = as.character(x[[id_col]]),
    area_name = as.character(x[[name_col]]),
    geometry  = st_geometry(x)
  )
}

wards <- read_area_csv("data/wards.csv",
                       wkt_col = "the_geom", id_col = "WARD")           # <- adjust
nsa   <- read_area_csv("data/neighborhoods.csv",
                       wkt_col = "the_geom", id_col = "GNOCDC_LAB")     # <- adjust

layers <- list(
  "ZCTA" = zcta, "Census tract" = tracts, "Block group" = block_groups,
  "Neighborhood (NSA)" = nsa, "Ward" = wards
)

# ---- 4. Spatial join + summaries for one layer ----
analyze_layer <- function(points, areas, layer_name, min_n = 5) {
  joined <- points |>
    st_join(areas, join = st_within, left = TRUE) |>
    distinct(business_id, .keep_all = TRUE)
  
  stats <- joined |>
    st_drop_geometry() |>
    filter(!is.na(area_id)) |>
    group_by(area_id) |>
    summarise(
      n_restaurants = n(),
      mean_stars    = mean(stars, na.rm = TRUE),
      mean_adj      = mean(adj_rating, na.rm = TRUE),
      n_rated_adj   = sum(!is.na(adj_rating)),
      .groups = "drop"
    )
  
  areas_out <- areas |>
    left_join(stats, by = "area_id") |>
    mutate(
      n_restaurants = coalesce(n_restaurants, 0L),
      low_n = n_restaurants < min_n,
      layer = layer_name
    )
  
  list(
    areas = areas_out,
    unmatched = tibble(
      layer = layer_name,
      restaurants = nrow(joined),
      unmatched_na = sum(is.na(joined$area_id)),
      pct_unmatched = round(100 * mean(is.na(joined$area_id)), 1)
    )
  )
}

results <- imap(layers, \(areas, nm) analyze_layer(restaurant_points, areas, nm))

area_summary <- map_dfr(results, "areas")
unmatched    <- map_dfr(results, "unmatched")

# ---- 5. Checks ----
unmatched   # how many restaurants fell in no area, per layer

# Areas with very few restaurants (<5), per layer
area_summary |>
  st_drop_geometry() |>
  group_by(layer) |>
  summarise(
    n_areas        = n(),
    n_zero         = sum(n_restaurants == 0),
    n_low_1_to_4   = sum(n_restaurants %in% 1:4),
    pct_low_or_zero = round(100 * mean(low_n), 1),
    median_per_area = median(n_restaurants)
  )

# ---- 6. Plots ----
area_summary <- area_summary |>
  mutate(layer = factor(layer, levels = names(layers)))

bb <- st_bbox(restaurant_points)

# (a) Mean stars by area, only shown where there are enough restaurants
ggplot(area_summary |> mutate(plot_stars = if_else(low_n, NA_real_, mean_stars))) +
  geom_sf(aes(fill = plot_stars), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "C", na.value = "grey85", name = "Mean\nstars") +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"])) +
  facet_wrap(~ layer) +
  labs(title = "Average Yelp rating by area",
       subtitle = "Grey = fewer than 5 restaurants (rating not shown)") +
  theme_minimal() +
  theme(axis.text = element_blank())

# (b) Same map using reviewer-adjusted rating
# ggplot(area_summary |> mutate(plot_adj = if_else(low_n, NA_real_, mean_adj))) +
#   geom_sf(aes(fill = plot_adj), color = "white", linewidth = 0.1) +
#   scale_fill_viridis_c(option = "C", na.value = "grey85", name = "Adjusted\nrating") +
#   coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"])) +
#   facet_wrap(~ layer) +
#   labs(title = "Reviewer-adjusted rating by area",
#        subtitle = "Grey = fewer than 5 restaurants") +
#   theme_minimal() +
#   theme(axis.text = element_blank())

# (c) Comparison plot: how thin is the data at each geography?
area_summary |>
  st_drop_geometry() |>
  filter(n_restaurants > 0) |>
  ggplot(aes(x = n_restaurants)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red") +
  facet_wrap(~ layer, scales = "free") +
  labs(title = "Restaurants per area by geography",
       subtitle = "Dashed line = 5-restaurant threshold",
       x = "Restaurants in area", y = "Number of areas") +
  theme_minimal()
#creation of informational table
layer_labels <- c(
  "ZCTA"               = "ZIP/ZCTA",
  "Census tract"       = "Census tract",
  "Neighborhood (NSA)" = "Neighborhood Statistical Area",
  "Ward"               = "Ward"
)

summary_table <- area_summary |>
  st_drop_geometry() |>
  mutate(layer = as.character(layer)) |>
  filter(layer %in% names(layer_labels)) |>
  group_by(layer) |>
  summarise(
    `Number of areas`      = n(),
    `Restaurants assigned` = sum(n_restaurants),
    `Smallest area count`  = min(n_restaurants[n_restaurants > 0]),
    `Largest area count`   = max(n_restaurants),
    .groups = "drop"
  ) |>
  left_join(
    unmatched |> select(layer, `Unmatched restaurants` = unmatched_na),
    by = "layer"
  ) |>
  mutate(`Geographic system` = unname(layer_labels[layer])) |>
  arrange(match(layer, names(layer_labels))) |>
  select(
    `Geographic system`, `Number of areas`, `Restaurants assigned`,
    `Unmatched restaurants`, `Smallest area count`, `Largest area count`
  )

print(as.data.frame(summary_table), row.names = FALSE)