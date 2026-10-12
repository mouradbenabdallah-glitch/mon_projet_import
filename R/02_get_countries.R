library(rvest)
library(dplyr)

# Function to scrape or fetch the list of countries
get_ins_country_list <- function(url = "http://www.ins.tn/ComExt") {
  webpage <- read_html(url)
  
  # Extracts all country options from the dropdown list on the site
  countries <- webpage %>%
    html_elements("select[name='pays'] option") %>%
    html_text() %>%
    trimws()
  
  # Remove blank or header choices
  countries <- countries[countries != "" & countries != "Tous les pays"]
  
  return(countries)
}

# Run to get the full vector of countries
all_countries <- get_ins_country_list()
print(head(all_countries, 20))