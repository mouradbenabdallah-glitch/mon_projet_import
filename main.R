library(here)
source(here("R", "02_scrape_ins_trade.R"))

# Scrape all years (2020 to 2026), all 12 months, and both Export & Import flows
full_dataset <- scrape_full_ins_dataset(
  years = 2020:2026,
  months = 1:12,
  flows = c("Export", "Import")
)