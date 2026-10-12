library(httr2)
library(rvest)
library(dplyr)
library(purrr)
library(here)

INS_URL <- "http://www.ins.tn/ComExt"

# 1. Fetch country list
get_ins_countries <- function() {
  tryCatch({
    resp <- request(INS_URL) %>%
      req_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)") %>%
      req_perform()
    
    html_doc <- read_html(resp_body_string(resp))
    nodes <- html_doc %>% html_elements("select[name='pays'] option")
    
    countries_df <- tibble(
      code = nodes %>% html_attr("value"),
      name = nodes %>% html_text() %>% trimws()
    ) %>%
      filter(!is.na(code) & code != "" & code != "0" & !grepl("Tous", name, ignore.case = TRUE)) %>%
      arrange(name)
    
    return(countries_df)
  }, error = function(e) {
    return(tibble(code = character(), name = character()))
  })
}

# 2. Fetch single query (Year, Month, Flow)
fetch_live_ins_data <- function(country_code, country_name, year, month, flow) {
  tryCatch({
    resp <- request(INS_URL) %>%
      req_body_form(
        pays = country_code,
        annee = as.character(year),
        mois = sprintf("%02d", as.numeric(month)),
        flux = ifelse(flow == "Export", "E", "I")
      ) %>%
      req_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)") %>%
      req_perform()
    
    html_doc <- read_html(resp_body_string(resp))
    tables <- html_doc %>% html_table(fill = TRUE)
    
    if (length(tables) > 0) {
      df <- tables[[1]]
      if (ncol(df) >= 3) {
        colnames(df)[1:3] <- c("Produit", "Quantité (KG)", "Valeur (TND)")
        
        df <- df %>%
          mutate(
            Pays = country_name,
            Flux = flow,
            Date = paste0(year, "-", sprintf("%02d", as.numeric(month))),
            `Quantité (KG)` = as.numeric(gsub("[^0-9]", "", as.character(`Quantité (KG)`))),
            `Valeur (TND)` = as.numeric(gsub("[^0-9]", "", as.character(`Valeur (TND)`)))
          ) %>%
          select(Produit, Pays, Flux, Date, `Quantité (KG)`, `Valeur (TND)`)
        
        return(df)
      }
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

# 3. Master function to loop over ALL years, months, and flows (Export + Import)
scrape_full_ins_dataset <- function(years = 2020:2026, 
                                    months = 1:12, 
                                    flows = c("Export", "Import"),
                                    country_codes = NULL) {
  
  all_countries <- get_ins_countries()
  
  if (!is.null(country_codes)) {
    target_countries <- all_countries %>% filter(code %in% country_codes | name %in% country_codes)
  } else {
    target_countries <- all_countries
  }
  
  results <- list()
  total_queries <- nrow(target_countries) * length(years) * length(months) * length(flows)
  message(paste("Starting full scrape across", total_queries, "combinations..."))
  
  for (i in seq_len(nrow(target_countries))) {
    c_code <- target_countries$code[i]
    c_name <- target_countries$name[i]
    
    for (yr in years) {
      for (m in months) {
        for (fl in flows) { # Iterates over BOTH "Export" and "Import"
          data <- fetch_live_ins_data(c_code, c_name, yr, m, fl)
          
          if (!is.null(data) && nrow(data) > 0) {
            results[[length(results) + 1]] <- data
          }
          Sys.sleep(0.1) # Brief delay between server requests
        }
      }
    }
  }
  
  if (length(results) > 0) {
    full_df <- bind_rows(results)
    
    # Save the consolidated raw data to data/raw/dataset_brut.csv
    dir.create(here("data", "raw"), recursive = TRUE, showWarnings = FALSE)
    write.csv(full_df, here("data", "raw", "dataset_brut.csv"), row.names = FALSE)
    
    message("Scraping complete! File saved to data/raw/dataset_brut.csv")
    return(full_df)
  } else {
    message("No data retrieved.")
    return(NULL)
  }
}