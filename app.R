library(shiny)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
library(plotly)
library(scales)
library(DT)
library(writexl)
library(thematic)
library(stringi)
library(R.utils)
source("magyar.R")
source("plot_demographic.R")
source("table_demographic.R")
source("calculation_demographic.R")
source("plot_map.R")
source("save_PDF.R")
source("calculation_pyramid.R")
source("table_pyramid.R")
source("plot_pyramid.R")

options(warn = -1)

#adatok letöltése
file_url1 <- "https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Demographic_Indicators_Medium.csv.gz"
file_url2 <- "https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationByAge5GroupSex_Medium.csv.gz"

compressed_file_path1 <- "adat/WPP2024_Demographic_Indicators_Medium.csv.gz"
csv_file_path1 <- "adat/WPP2024_Demographic_Indicators_Medium.csv"
compressed_file_path2 <- "adat/WPP2024_PopulationByAge5GroupSex_Medium.csv.gz"
csv_file_path2 <- "adat/WPP2024_PopulationByAge5GroupSex_Medium.csv"

# letöltés és kicsomagolás
if (!file.exists(csv_file_path1)) {
  download.file(file_url1, destfile = compressed_file_path1, mode = "wb")
  gunzip(compressed_file_path1, remove = TRUE)
}

if (!file.exists(csv_file_path2)) {
  download.file(file_url2, destfile = compressed_file_path2, mode = "wb")
  gunzip(compressed_file_path2, remove = TRUE)
}

# adatok beolvasása
demographic_data <- fread("adat/WPP2024_Demographic_Indicators_Medium.csv")
data_pyramid <- fread("adat/WPP2024_PopulationByAge5GroupSex_Medium.csv")
magyar_nev <- read.csv("names-independent.tsv", sep = "\t")
magyar_nev <- magyar_nev %>%
  select(Alpha.3, Short)

# térképek RDS file mappa
map_population <- readRDS("map_combined_Population.rds")
map_density <- readRDS("map_combined_density.rds")
map_pop_growth_rate <- readRDS("map_combined_PopGrowthRate.rds")
map_exp_life_time <- readRDS("map_combined_exp_life_time.rds")

# magyar nevek használata
demographic_country <- merge(
  x = demographic_data,
  y = magyar_nev,
  by.x = "ISO3_code",
  by.y = "Alpha.3",
  all = TRUE
)
colnames(demographic_country)[68] <- "Hungarian_name"

data_pyramid_country <- merge(
  x = data_pyramid,
  y = magyar_nev,
  by.x = "ISO3_code",
  by.y = "Alpha.3",
  all = TRUE
)
colnames(data_pyramid_country)[21] <- "Hungarian_name"

# régiók hozzáadása az ország nevekhez
demographic_other <- demographic_data
data_pyramid_other <- data_pyramid

demographic_other$Hungarian_name <- hungarian_dict[demographic_data$Location]
data_pyramid_other$Hungarian_name <- hungarian_dict[data_pyramid$Location]

demographic_data <- rbind(demographic_other, demographic_country)
data_pyramid <- rbind(data_pyramid_other, data_pyramid_country)

demographic_data <- demographic_data[!(demographic_data$Hungarian_name %in% c(NA)), ]
data_pyramid <- data_pyramid[!(data_pyramid$Hungarian_name %in% c(NA)), ]

# Adatok szűrése
demographic_data <- demographic_data %>%
  filter(LocTypeName %in% c("World", "Country/Area", "Geographic region", "Subregion")) %>%
  select(TPopulation1Jan,
         PopDensity,
         PopGrowthRate,
         LE15,
         Time,
         Hungarian_name)
# népesség szorozva 1000 -el.
demographic_data$TPopulation1Jan <- demographic_data$TPopulation1Jan * 1000

data_pyramid <- data_pyramid %>%
  filter(LocTypeName %in% c("World", "Country/Area", "Geographic region", "Subregion"))
# nő / férfi népesség szorozva 1000 -el.
data_pyramid$PopFemale <- data_pyramid$PopFemale * 1000
data_pyramid$PopMale <- data_pyramid$PopMale * 1000
data_pyramid$PopTotal <- data_pyramid$PopTotal * 1000

#  UI felület létrehozása
ui <- fluidPage(
  # theme = bslib::bs_theme(bootswatch = "united"),
  tags$head(tags$style(
    HTML(
      "
      .specific-table .odd {
        background-color: #f2f2f2;
      }
      .specific-table .even {
        background-color: #d9fdd9;
      }
      .specific-table {
        font-family: Arial, sans-serif;
        font-size: 16px;
        color: grey;
      }
      .specific-table th {
        background-color: #4CAF50;
        color: white;
        text-align: center;
        padding: 10px;
      }
      .specific-table td {
        padding: 10px;
        text-align: left;
      }
    "
    )
  )),
  
  titlePanel("Népességi adatok vizualizációja"),
  
  sidebarLayout(
    sidebarPanel(
      # Választó gombok
      radioButtons(
        "main_options",
        "Válassz:",
        choices = list("Demográfiai mutatók" = "line_plots", "Korfa" = "age_pyramid")
      ),
      
      # Korfa UI
      conditionalPanel(
        condition = "input.main_options == 'age_pyramid'",
        selectInput(
          "countries_pyramid",
          "Válassz egy országot vagy régiót",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        sliderInput(
          "years_pyramid",
          "Válasszd ki az évet:",
          min = 1950,
          max = 2100,
          value = 1950,
          step = 1,
          animate = animationOptions(interval = 500, loop = TRUE)
        ),
      ),
      
      # Vonal diagrammok UI
      conditionalPanel(
        condition = "input.main_options == 'line_plots'",
        #ország választó
        selectInput(
          "countries",
          "Válassz ki akár több országot vagy régiót",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        #év választó
        sliderInput(
          "years",
          "Válasszd ki a tartományt",
          min = 1950,
          max = 2100,
          value = c(1950, 2100),
          step = 1
        ),
        sliderInput(
          "years_interested",
          "Válasszd ki az évet aminek az adataira kíváncsi vagy",
          min = 1950,
          max = 2100,
          value = 2024,
          step = 1
        ),
        sliderInput(
          "years_map",
          "Válasszd ki az évet a térképhez:",
          min = 1950,
          max = 2100,
          value = 1950,
          step = 1,
          animate = animationOptions(interval = 1000, loop = TRUE)
        ),
        
        # Demográfiai mutató legördülő lista
        selectInput(
          "demographic_options",
          "Válasz egy demográfiai mutatót",
          choices = list(
            "Népesség" = "population",
            "Népsűrűség" = "density",
            "Népesség növekedési ráta" = "growth_rate",
            "Várható kor" = "exp_life_time"
          )
        )
      )
    ),
    mainPanel(uiOutput("selected_app_output"))
  )
)


# szerver oldal
server <- function(input, output, session) {
  # szép kinézet
  thematic::thematic_shiny()
  
  # ország dinamikus kiválasztása
  observe({
    updateSelectInput(session,
                      "countries",
                      choices = unique(demographic_data$Hungarian_name))
  })
  
  # vizsgálni kívánt év határainak dinamikus kiválasztása
  observe({
    updateSliderInput(
      session,
      "years_interested",
      min = input$years[1],
      max = input$years[2],
      value = max(input$years[1], min(
        input$years_interested, input$years[2]
      )),
      step = 1
    )
  })
  
  # Reaktív adatszűrés
  filtered_data_demographic <- reactive({
    req(input$countries, input$years)
    subset(
      demographic_data,
      Hungarian_name %in% input$countries &
        Time >= input$years[1] & Time <= input$years[2]
    )
  })
  
  
  filtered_year_interested <- reactive({
    req(input$years_interested)
    subset(
      demographic_data,
      Hungarian_name %in% input$countries &
        Time %in% input$years_interested
    )
  })
  
  
  
  #NÉPESSÉGI ADATOK
  
  # Népesség diagramm reaktív kiszámítása
  plot_population <- reactive({
    data_line <- filtered_data_demographic()
    data_point <- filtered_year_interested()
    plot_demographic(
      data_line = data_line,
      data_point = data_point,
      demographic_column = "TPopulation1Jan",
      y_label = "Népesség (fő)",
      x_label = "Év",
      title_ = "Népesség",
      year_input_min = input$years[1],
      year_input_max = input$years[2],
      year_interested = input$years_interested
    )
  })
  
  # A renderPlot a reaktív plot_population függvény alapján frissül
  output$population_diagram <- renderPlot({
    plot_population()
  })
  
  #Népesség ábra mentés
  output$downloadPopulationPlot <- downloadHandler(
    filename = function() {
      paste("népesség_diagram-",
            input$years[1],
            "-",
            input$years[2] ,
            ".pdf",
            sep = "")
    },
    content = function(file) {
      create_population_plot <- plot_population()
      save_pdf(create_population_plot , file)
    }
  )
  
  
  

  #szám adatok kiszámítása népességhez
  summary_stats_population <- reactive({
    calculation_demographic(
      data = filtered_data_demographic(),
      year_selected = input$years_interested,
      demographic_col =  "TPopulation1Jan",
      year_min = input$years[1],
      year_max = input$years[2]
    )
  })
  
  # Render the population summary table in the UI
  output$summaryTablePopulation <- renderUI({
    table_demographic(summary_stats_population)
  })
  
  # Táblázat mentése Excel file-ba
  output$downloadPopulationTable <- downloadHandler(
    filename = function() {
      paste("népesség_táblázat-",
            input$years[1],
            "-",
            input$years[2],
            ".xlsx",
            sep = "")
    },
    content = function(file) {
      table_data <- summary_stats_population()
      
      # Kiíratás excel file-ba
      write_xlsx(table_data, file)
    }
  )
  
  filtered_data_map_population <- reactive({
    req(input$years_map)
    map_population %>%
      filter(Time %in% input$years_map) %>%
      mutate(PopulationCategory = factor(
        PopulationCategory,
        levels = c(
          "0-2M",
          "2M-5M",
          "5M-10M",
          "10M-20M",
          "20M-70M",
          "70M-100M",
          "100M-400M",
          "400M-800M",
          "800M-"
        )
      ))
  })
  
  
  
  plot_map_population <- reactive({
    data_map <- filtered_data_map_population()
    plot_map(
      data_map = data_map,
      category = "PopulationCategory",
      year_input = input$years_map,
      label = "Népesség (fő)",
      title_ = "Népesség"
    )
  })
  
  # Népesség térkép kirajzolása
  output$map_population_diagram <- renderPlot({
    plot_map_population()
  })
  
  output$downloadPopulationMap <- downloadHandler(
    filename = function() {
      paste("népesség_térkép-", input$years_map , ".pdf", sep = "")
    },
    content = function(file) {
      create_population_map <- plot_map_population()
      save_pdf(create_population_map , file)
    }
  )
  
  # NÉPSÜRÜSÉG
  
  # Népsűrűség diagramm reaktív kiszámítása
  plot_density <- reactive({
    data_line <- filtered_data_demographic()
    data_point <- filtered_year_interested()
    plot_demographic(
      data_line = data_line,
      data_point = data_point,
      demographic_column = "PopDensity",
      y_label = bquote("Népsűrűség (fő/km" ^ 2 * ")"),
      x_label = "Év",
      title_ = "Népsűrűség",
      year_input_min = input$years[1],
      year_input_max = input$years[2],
      year_interested = input$years_interested
    )
  })
  
  # A renderPlot a reaktív plot_population függvény alapján frissül
  output$density_diagram <- renderPlot({
    plot_density()
  })
  
  #Népesség ábra mentés
  output$downloadDensityPlot <- downloadHandler(
    filename = function() {
      paste("népsürüség_diagram-",
            input$years[1],
            "-",
            input$years[2] ,
            ".pdf",
            sep = "")
    },
    content = function(file) {
      create_density_plot <- plot_density()
      save_pdf(create_density_plot , file)
    }
  )
  
  summary_stats_density <- reactive({
    calculation_demographic(
      data = filtered_data_demographic(),
      year_selected = input$years_interested,
      demographic_col =  "PopDensity",
      year_min = input$years[1],
      year_max = input$years[2]
    )
  })
  
  #Népsürüség szám adatok kíírása táblázatba
  output$summaryTableDensity <- renderUI({
    table_demographic(summary_stats_density)
  })
  
  # Táblázat mentése Excel file-ba
  output$downloadDensityTable <- downloadHandler(
    filename = function() {
      paste("népsűrűség_táblázat-",
            input$years[1],
            "-",
            input$years[2],
            ".xlsx",
            sep = "")
    },
    content = function(file) {
      table_data <- summary_stats_density()
      
      # Kiíratás excel file-ba
      write_xlsx(table_data, file)
    }
  )
  
  filtered_data_map_density <- reactive({
    req(input$years_map)
    map_density %>%
      filter(Time %in% input$years_map) %>%
      mutate(DensityCategory = factor(
        DensityCategory,
        levels = c(
          "0-10",
          "10-20",
          "20-50",
          "50-100",
          "100-200",
          "200-300",
          "300-500",
          "500-1000",
          "1000+"
        )
      ))
  })
  
  plot_map_density <- reactive({
    data_map <- filtered_data_map_density()
    plot_map(
      data_map = data_map,
      category = "DensityCategory",
      year_input = input$years_map,
      label = bquote("Népsűrűség (fő/km" ^ 2 * ")"),
      title_ = "Népsűrűség"
    )
  })
  
  
  # #népsűrűség térkép kirajzolása
  output$map_density_diagram <- renderPlot({
    plot_map_density()
  })
  
  output$downloadDensityMap <- downloadHandler(
    filename = function() {
      paste("népsűrűség_térkép-", input$years_map , ".pdf", sep = "")
    },
    content = function(file) {
      create_density_map <- plot_map_density()
      
      save_pdf(create_density_map , file)
    }
  )
  
  
  #NÖVEKEDÉSI RÁTA ADATOK
  
  # Népesség diagramm reaktív kiszámítása
  plot_growth_rate <- reactive({
    data_line <- filtered_data_demographic()
    data_point <- filtered_year_interested()
    plot_demographic(
      data_line = data_line,
      data_point = data_point,
      demographic_column = "PopGrowthRate",
      y_label = "Népesség növekedési ráta (%)",
      x_label = "Év",
      title_ = "Népesség növekedési ráta",
      year_input_min = input$years[1],
      year_input_max = input$years[2],
      year_interested = input$years_interested
    )
  })
  
  output$growth_rate_diagram <- renderPlot({
    plot_growth_rate()
  })
  
  #Népesség ábra mentés
  output$downloadGrowthRatePlot <- downloadHandler(
    filename = function() {
      paste(
        "népesség_növekedési_ráta_diagram-",
        input$years[1],
        "-",
        input$years[2] ,
        ".pdf",
        sep = ""
      )
    },
    content = function(file) {
      create_growth_rate_plot <- plot_growth_rate()
      save_pdf(create_growth_rate_plot , file)
    }
  )
  
  # szám adatok kiszámítása
  summary_stats_growth <- reactive({
    calculation_demographic(
      data = filtered_data_demographic(),
      year_selected = input$years_interested,
      demographic_col =  "PopGrowthRate",
      year_min = input$years[1],
      year_max = input$years[2]
    )
  })
  
  #Növekedési ráta adatok kíírása táblázatba
  output$summaryTableGrowth <- renderUI({
    table_demographic(summary_stats_growth)
  })
  
  # Táblázat mentése Excel file-ba
  output$downloadGrowthRateTable <- downloadHandler(
    filename = function() {
      paste(
        "népesség_növekedési_ráta_táblázat-",
        input$years[1],
        "-",
        input$years[2],
        ".xlsx",
        sep = ""
      )
    },
    content = function(file) {
      table_data <- summary_stats_growth()
      write_xlsx(table_data, file)
    }
  )
  
  # Növekedési ráta térkép - reactive filtered data
  filtered_data_map_pop_growth <- reactive({
    req(input$years_map)
    map_pop_growth_rate %>%
      filter(Time %in% input$years_map) %>%
      mutate(PopGrowthRateCategory = factor(
        PopGrowthRateCategory,
        levels = c(
          "-1-0",
          "0-0.2",
          "0.2-0.4",
          "0.4-0.6",
          "0.6-0.8",
          "0.8-1.0",
          "1-2",
          "2-3",
          "3-"
        )
      ))
  })
  
  plot_map_growth_rate <- reactive({
    data_map <- filtered_data_map_pop_growth()
    plot_map(
      data_map = data_map,
      category = "PopGrowthRateCategory",
      year_input = input$years_map,
      label = "Népesség növekedési ráta (%)",
      title_ = "Népesség növekedési ráta"
    )
  })
  
  
  # Várható kor térkép kirajzolása
  output$map_pop_growth_rate_diagram <- renderPlot({
    plot_map_growth_rate()
  })
  
  
  output$downloadGrowthRateMap <- downloadHandler(
    filename = function() {
      paste("népesség_növekedési_ráta_térkép-",
            input$years_map ,
            ".pdf",
            sep = "")
    },
    content = function(file) {
      create_growth_rate_map <- plot_map_growth_rate()
      
      save_pdf(create_growth_rate_map , file)
    }
  )
  
  #VÁRHATÓ KOR ADATOK
  
  # Várható kor diagramm reaktív kiszámítása
  plot_exp_LT <- reactive({
    data_line <- filtered_data_demographic()
    data_point <- filtered_year_interested()
    plot_demographic(
      data_line = data_line,
      data_point = data_point,
      demographic_column = "LE15",
      y_label = "Várható kor (év)",
      x_label = "Év",
      title_ = "Várható kor",
      year_input_min = input$years[1],
      year_input_max = input$years[2],
      year_interested = input$years_interested
    )
  })
  
  # A renderPlot a reaktív plot_population függvény alapján frissül
  output$exp_life_time_diagram <- renderPlot({
    plot_exp_LT()
  })
  
  #Várható kor ábra mentés
  output$downloadexpLTPlot <- downloadHandler(
    filename = function() {
      paste("várható_kor_diagram-",
            input$years[1],
            "-",
            input$years[2] ,
            ".pdf",
            sep = "")
    },
    content = function(file) {
      create_exp_LT_plot <- plot_exp_LT()
      save_pdf(create_exp_LT_plot , file)
    }
  )
  
  # szám adatok kiszámítása
  summary_stats_lifetime <- reactive({
    calculation_demographic(
      data = filtered_data_demographic(),
      year_selected = input$years_interested,
      demographic_col =  "LE15",
      year_min = input$years[1],
      year_max = input$years[2]
    )
  })
  
  #várható kor adatok kíírása táblázatba
  output$summaryTableLifetime <- renderUI({
    table_demographic(summary_stats_lifetime)
  })
  
  # Táblázat mentése Excel file-ba
  output$downloadExpLTTable <- downloadHandler(
    filename = function() {
      paste("várható_kor_táblázat-",
            input$years[1],
            "-",
            input$years[2],
            ".xlsx",
            sep = "")
    },
    content = function(file) {
      table_data <- summary_stats_lifetime()
      
      # Kiíratás excel file-ba
      write_xlsx(table_data, file)
    }
  )
  
  # Várható kor térkép adatszűrés
  filtered_data_map_exp_LT <- reactive({
    req(input$years_map)
    map_exp_life_time %>%
      filter(Time %in% input$years_map) %>%
      mutate(ExpLifeTimeCategory = factor(
        ExpLifeTimeCategory,
        levels = c(
          "<40",
          "40-45",
          "45-50",
          "50-55",
          "55-60",
          "60-65",
          "65-70",
          "70-75",
          "75<"
        )
      ))
  })
  
  plot_map_exp_LT <- reactive({
    data_map <- filtered_data_map_exp_LT()
    plot_map(
      data_map = data_map,
      category = "ExpLifeTimeCategory",
      year_input = input$years_map,
      label = "Várható kor (év)",
      title_ = "Várható kor"
    )
  })
  
  
  # Várható kor térkép kirajzolása
  output$map_exp_life_time_diagram <- renderPlot({
    plot_map_exp_LT()
  })
  
  output$downloadExpLTMap <- downloadHandler(
    filename = function() {
      paste("várható_kor_térkép-", input$years_map , ".pdf", sep = "")
    },
    content = function(file) {
      create_exp_LT_map <- plot_map_exp_LT()
      
      save_pdf(create_exp_LT_map , file)
    }
  )
  
  #KORFA ADATOK
  # korfa ország szűrése
  observe({
    updateSelectInput(session,
                      "countries_pyramid",
                      choices = unique(data_pyramid$Hungarian_name))
  })
  
  # korfa adatszűrés
  filtered_data_pyramid <- reactive({
    subset(
      data_pyramid,
      Hungarian_name == input$countries_pyramid &
        Time == input$years_pyramid
    )
  })
  
  # Korfa kirajzolása
  output$age_pyramid_diagram <- renderPlotly({
    data <- filtered_data_pyramid()
    plot_pyramid(data, years = input$years_pyramid, countries = input$countries_pyramid)
  })
  
  # szám adatok kiszámítása férfi korfához
  summary_stats_pyramid_male <- reactive({
    calculation_pyramid(
      filtered_data_pyramid = filtered_data_pyramid(),
      pyramid_col =  "PopMale"
    )
   })
  
  # szám adatok kiszámítása nöi korfához
  summary_stats_pyramid_female <- reactive({
    calculation_pyramid(
      filtered_data_pyramid = filtered_data_pyramid(),
      pyramid_col =  "PopFemale"
    )
  })
  
  combined_stats_pyramid <- reactive({
    rbind(summary_stats_pyramid_male(), summary_stats_pyramid_female())
  })
  
  #Korfa férfi adatok kíírása táblázatba
  output$summaryTablePyramid <- renderUI({
    table_pyramid(combined_stats_pyramid)
  })
  
  # Vizualizáció kiválasztása UI rádiógomb alapján
  output$selected_app_output <- renderUI({
    if (input$main_options == "line_plots" &
        input$demographic_options == "population") {
      tagList(
        plotOutput("population_diagram"),
        tableOutput("summaryTablePopulation"),
        plotOutput("map_population_diagram"),
        downloadButton("downloadPopulationPlot", "Népesség ábra mentése"),
        downloadButton("downloadPopulationMap", "Népesség térkép mentése"),
        downloadButton("downloadPopulationTable", "Népesség táblázat mentése")
      )
    } else if (input$main_options == "line_plots" &
               input$demographic_options == "density") {
      tagList(
        plotOutput("density_diagram"),
        tableOutput("summaryTableDensity"),
        plotOutput("map_density_diagram"),
        downloadButton("downloadDensityPlot", "Népsűrűsség ábra mentése"),
        downloadButton("downloadDensityMap", "Népsűrűség térkép mentése"),
        downloadButton("downloadDensityTable", "Népsűrűség táblázat mentése")
      )
    } else if (input$main_options == "line_plots" &
               input$demographic_options == "growth_rate") {
      tagList(
        plotOutput("growth_rate_diagram"),
        tableOutput("summaryTableGrowth"),
        plotOutput("map_pop_growth_rate_diagram"),
        downloadButton("downloadGrowthRatePlot", "Növekedési ráta ábra mentése"),
        downloadButton(
          "downloadGrowthRateMap",
          "Növekedési ráta térkép mentése"
        ),
        downloadButton(
          "downloadGrowthRateTable",
          "Növekedési ráta táblázat mentése"
        )
      )
    } else if (input$main_options == "line_plots" &
               input$demographic_options == "exp_life_time") {
      tagList(
        plotOutput("exp_life_time_diagram"),
        tableOutput("summaryTableLifetime"),
        plotOutput("map_exp_life_time_diagram"),
        downloadButton("downloadExpLTPlot", "Várható kor ábra mentése"),
        downloadButton("downloadExpLTMap", "Várható kor térkép mentése"),
        downloadButton("downloadExpLTTable", "Várható kor táblázat mentése")
      )
    } else if (input$main_options == "age_pyramid") {
      tagList(
        plotlyOutput("age_pyramid_diagram"),
        tableOutput("summaryTablePyramid")
      )
    }
  })
}

# program futtatása
shinyApp(ui = ui, server = server)