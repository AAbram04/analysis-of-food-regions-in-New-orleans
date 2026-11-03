#install.packages(c("jsonlite", "dplyr"))  # run once if needed

library(jsonlite)
library(dplyr)

# Paths (adjust if your raw files live elsewhere)
raw_dir <- "data/raw/yelp_dataset"
out_dir <- "data"
dir.create(out_dir, showWarnings = FALSE)

# 1. Businesses: filter to New Orleans
business <- stream_in(
  file(file.path(raw_dir, "yelp_academic_dataset_business.json"))
)

nola_business <- business |>
  filter(city == "New Orleans")

saveRDS(nola_business, file.path(out_dir, "nola_business.rds"))

# 2. Reviews: filter in chunks to keep memory use low
nola_ids <- nola_business$business_id
review_chunks <- list()

stream_in(
  file(file.path(raw_dir, "yelp_academic_dataset_review.json")),
  handler = function(chunk) {
    review_chunks[[length(review_chunks) + 1]] <<- chunk |>
      filter(business_id %in% nola_ids)
  },
  pagesize = 50000
)

nola_reviews <- bind_rows(review_chunks)

saveRDS(nola_reviews, file.path(out_dir, "nola_reviews.rds"))

# Quick check
nrow(nola_business)
nrow(nola_reviews)