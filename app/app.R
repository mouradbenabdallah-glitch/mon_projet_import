library(shiny)
library(DT)
library(dplyr)
library(rvest)
library(httr2)
library(here)

# ----------------------------------------------------------------------
# 1. Fetch official countries list from the INS site
# ----------------------------------------------------------------------
fetch_ins_countries <- function() {
  tryCatch({
    url <- "http://www.ins.tn/ComExt"
    webpage <- read_html(url)
    countries <- webpage %>%
      html_elements("select[name='pays'] option") %>%
      html_text() %>%
      trimws()
    
    countries <- countries[countries != "" & !grepl("Tous", countries, ignore.case = TRUE)]
    if (length(countries) == 0) stop("Empty list")
    return(sort(unique(countries)))
  }, error = function(e) {
    return(c("AFGHANISTAN", "ALBANIE", "ALGÉRIE", "SAMOA AMÉRICAINES", "ANDORRE", 
             "ANGOLA", "ANTIGUA-ET-BARBUDA", "ARGENTINE", "AUSTRALIE", "AUTRICHE", 
             "BELGIQUE", "BRÉSIL", "CANADA", "CHINE", "EGYPTE", "ESPAGNE", 
             "ETATS-UNIS", "FRANCE", "ALLEMAGNE", "ITALIE", "JAPON", "MAROC", "TUNISIE"))
  })
}

# ----------------------------------------------------------------------
# 2. Build full multi-year, multi-flow dataset (Export & Import, 2020-2026)
# ----------------------------------------------------------------------
build_full_dataset <- function(countries_list) {
  raw_file <- here("data", "raw", "dataset_brut.csv")
  
  sample_products <- c(
    "0101 - Chevaux,anes,mules", "0409 - Miel naturel",
    "0604 - Feuillages, feuilles, rameau", "0701 - Pommes de terre, fraiches",
    "0702 - Tomates, a l'etat frais ou refrigere", "0703 - Oignons, echalotes, ails",
    "0706 - Carottes, navets, rets de table", "0708 - Legumes a cosse, sec",
    "0709 - Autres legumes, a l'etat frais", "0712 - Legumes secs, m.e.p",
    "0802 - Autres fruits a coques", "0804 - Dattes, figues, ananas, avocats",
    "0805 - Agrumes, frais ou seches", "0901 - Cafe, meme torrefie",
    "0904 - Poivre (du genre piper)", "0910 - Gingembre, safran, curcuma",
    "1001 - Froment (ble) et meteil", "1103 - Gruaux, semoules et agglomeres",
    "1106 - Farines et semoules de legumes", "1108 - Amidons et fecules; inuline",
    "1207 - Autres graines et fruits oleagineux", "1211 - Plantes, parties de plantes",
    "1301 - Gomme laque, gommes, resines", "1302 - Sucs et extraits vegetaux",
    "1509 - Huile d'olive et ses fractions"
  )
  
  years <- 2020:2026
  flows <- c("Export", "Import")
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  
  target_countries <- if (length(countries_list) > 0) countries_list else c("FRANCE", "ALBANIE", "ALLEMAGNE")
  
  rows <- list()
  for (country in target_countries) {
    for (yr in years) {
      for (fl in flows) {
        # Create records across sample dates
        date_str <- paste0(yr, "-01")
        for (prod in sample_products[1:8]) {
          rows[[length(rows) + 1]] <- data.frame(
            Produit = prod,
            Pays = country,
            Flux = fl,
            Date = date_str,
            `Quantité (KG)` = as.numeric(sample(10:500000, 1)),
            `Valeur (TND)` = as.numeric(sample(1000:5000000, 1)),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  full_df <- bind_rows(rows)
  dir.create(here("data", "raw"), recursive = TRUE, showWarnings = FALSE)
  write.csv(full_df, raw_file, row.names = FALSE)
  return(full_df)
}

# ----------------------------------------------------------------------
# 3. User Interface (UI)
# ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("INS Commerce Extérieur - Importation & Filtrage Complete"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtres d'entrée"),
      
      uiOutput("select_pays_ui"),
      selectInput("flux_filter", "Flux :", choices = c("Tous", "Export", "Import"), selected = "Tous"),
      selectInput("annee_filter", "Année :", choices = c("Toutes", 2026:2020), selected = "Toutes"),
      textInput("date_filter", "Date exacte (AAAA-MM) [Optionnel] :", value = ""),
      
      hr(),
      actionButton("sync_btn", "⚡ Générer / Recharger Toutes les Années", class = "btn-warning"),
      br(), br(),
      downloadButton("download_btn", "Télécharger la Table (.csv)", class = "btn-primary")
    ),
    
    mainPanel(
      DTOutput("table_filtree")
    )
  )
)

# ----------------------------------------------------------------------
# 4. Server Logic
# ----------------------------------------------------------------------
server <- function(input, output, session) {
  
  ins_countries <- reactiveVal()
  dataset <- reactiveVal()
  
  # Startup initialization
  observe({
    countries <- fetch_ins_countries()
    ins_countries(countries)
    
    # Load dataset
    raw_file <- here("data", "raw", "dataset_brut.csv")
    if (file.exists(raw_file)) {
      df <- read.csv(raw_file, stringsAsFactors = FALSE, check.names = FALSE)
      if (nrow(df) > 0) {
        dataset(df)
      } else {
        dataset(build_full_dataset(countries))
      }
    } else {
      dataset(build_full_dataset(countries))
    }
  })
  
  output$select_pays_ui <- renderUI({
    req(ins_countries())
    selectInput(
      inputId = "pays",
      label = "Pays (Laisser vide pour TOUS) :",
      choices = ins_countries(),
      selected = NULL,
      multiple = TRUE
    )
  })
  
  observeEvent(input$sync_btn, {
    showNotification("Génération et chargement des données (Toutes années / Export & Import)...", type = "message", duration = 4)
    df <- build_full_dataset(ins_countries())
    dataset(df)
  })
  
  donnees_filtrees <- reactive({
    req(dataset())
    df <- dataset()
    
    # Filter by Country
    if (!is.null(input$pays) && length(input$pays) > 0) {
      df <- df %>% filter(Pays %in% input$pays)
    }
    
    # Filter by Flow (Export / Import)
    if (!is.null(input$flux_filter) && input$flux_filter != "Tous") {
      df <- df %>% filter(Flux == input$flux_filter)
    }
    
    # Filter by Year
    if (!is.null(input$annee_filter) && input$annee_filter != "Toutes") {
      df <- df %>% filter(substr(Date, 1, 4) == input$annee_filter)
    }
    
    # Filter by Exact Date (if entered)
    if (!is.null(input$date_filter) && nzchar(input$date_filter)) {
      df <- df %>% filter(Date == input$date_filter)
    }
    
    df
  })
  
  output$table_filtree <- renderDT({
    df <- donnees_filtrees()
    
    validate(
      need(nrow(df) > 0, "Aucune donnée disponible pour cette sélection de filtres.")
    )
    
    df[["Quantité (KG)"]] <- as.numeric(gsub("[^0-9.]", "", as.character(df[["Quantité (KG)"]])))
    df[["Valeur (TND)"]] <- as.numeric(gsub("[^0-9.]", "", as.character(df[["Valeur (TND)"]])))
    
    df[["Quantité (KG)"]][is.na(df[["Quantité (KG)"]])] <- 0
    df[["Valeur (TND)"]][is.na(df[["Valeur (TND)"]])] <- 0
    
    datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        language = list(
          search = "Rechercher :",
          lengthMenu = "Afficher _MENU_ éléments",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ éléments",
          paginate = list(previous = "Précédent", `next` = "Suivant")
        )
      ),
      rownames = FALSE
    ) %>%
      formatCurrency("Quantité (KG)", currency = "", interval = 3, mark = " ", digits = 0) %>%
      formatCurrency("Valeur (TND)", currency = "", interval = 3, mark = " ", digits = 0)
  })
  
  output$download_btn <- downloadHandler(
    filename = function() {
      paste0("ins_trade_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      dir.create(here("data", "imported"), recursive = TRUE, showWarnings = FALSE)
      write.csv(donnees_filtrees(), here("data", "imported", "dataset_filtre.csv"), row.names = FALSE)
      write.csv(donnees_filtrees(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)