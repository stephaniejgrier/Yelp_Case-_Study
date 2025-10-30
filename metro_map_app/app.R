#-------------------------------------------------------------------------------
#   Map App
#-------------------------------------------------------------------------------
library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(sf)
library(leaflet)
library(scales)
if (!exists("map_data_prepped")) {
  if (file.exists("map_data_prepped.csv")) {
    map_data_prepped <- readr::read_csv("map_data_prepped.csv", show_col_types = FALSE)
  } else if (file.exists("map_data_prepped.xlsx") && requireNamespace("readxl", quietly = TRUE)) {
    map_data_prepped <- readxl::read_excel("map_data_prepped.xlsx")
  } else {
    stop("Put map_data_prepped.csv (or .xlsx) in the app folder.")
  }
}
df <- map_data_prepped %>%
  mutate(
    zip_code  = str_pad(as.character(zip_code), 5, pad = "0"),
    city_name = as.character(city_name),
    population = as.numeric(population),
    Total_QSRs = as.numeric(Total_QSRs),
    customer_satisfaction = as.numeric(customer_satisfaction),
    qsr_per_10k = if_else(!is.na(population) & population > 0,
                          (Total_QSRs / population) * 10000, NA_real_)
  )

# keep the 10 metros
city_order <- c("Philadelphia","Tampa","Indianapolis","St Louis","Nashville",
                "Tucson","New Orleans","Boise","Reno","Santa Barbara")
df <- df %>% filter(city_name %in% city_order)

zpath <- "zctas20.rds"  
if (!file.exists(zpath)) stop("zctas RDS not found at: ", zpath)

zctas20 <- readRDS(zpath)

# ensure types/CRS are what the app expects
zctas20 <- zctas20 %>%
  dplyr::mutate(zip_code = as.character(zip_code))

if (is.na(sf::st_crs(zctas20)) || sf::st_crs(zctas20)$epsg != 4326) {
  zctas20 <- sf::st_transform(zctas20, 4326)  # WGS84 for Leaflet
}

# --- Shiny UI -----------------------------------------------------------------
ui <- fluidPage(
  titlePanel("ZIP-level Market View — pick a metro & metric"),
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Metro area:", choices = city_order, selected = city_order[1]),
      radioButtons(
        "metric", "Metric",
        choices = list("QSRs per 10k residents" = "qsr_per_10k",
                    "Customer satisfaction"   = "customer_satisfaction"),
        selected = "qsr_per_10k"
      ),
      checkboxInput("quantile", "Use quantile color bins", TRUE),
      helpText("Tip: quantile bins balance color spread across ZIPs.")
    ),
    mainPanel(leafletOutput("map", height = "75vh"))
  )
)

# --- Server -------------------------------------------------------------------
server <- function(input, output, session) {
  
  city_df <- reactive({
    req(input$city)
    df %>% filter(city_name == input$city)
  })
  
  city_shapes <- reactive({
    zctas20 %>% semi_join(city_df() %>% select(zip_code), by = "zip_code")
  })
  
  shp <- reactive({
    left_join(city_shapes(), city_df(), by = "zip_code")
  })
  
  metric_vals <- reactive({
    s <- shp()
    if (input$metric == "qsr_per_10k") s$qsr_per_10k else s$customer_satisfaction
  })
  
  legend_title <- reactive({
    if (input$metric == "qsr_per_10k") "QSRs per 10k residents" else "Customer satisfaction"
  })
  
  pal <- reactive({
    vals <- metric_vals()
    if (isTRUE(input$quantile)) {
      # 5 classes; fallback to continuous if quantile fails
      brks <- tryCatch(unique(quantile(vals, probs = seq(0,1,length.out=6), na.rm = TRUE)),
                       error = function(e) NA)
      colorBin("YlGnBu", domain = vals, bins = brks, na.color = "#eeeeee")
    } else {
      colorNumeric("YlGnBu", domain = vals, na.color = "#eeeeee")
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -95, lat = 39, zoom = 4)
  })
  observe({
    s <- shp()
    req(nrow(s) > 0)
    
    # ensure valid geometries (safe no-op if already valid)
    s <- sf::st_make_valid(s)
    
    vals  <- metric_vals()
    pal_f <- pal()
    
    metric_label <- if (input$metric == "qsr_per_10k") {
      ifelse(is.na(s$qsr_per_10k), "—", scales::comma(round(s$qsr_per_10k, 1)))
    } else {
      ifelse(is.na(s$customer_satisfaction), "—", scales::number(s$customer_satisfaction, accuracy = 0.1))
    }
    
    lbl <- sprintf(
      "<b>ZIP:</b> %s<br/><b>%s:</b> %s<br/>
     <b>Population:</b> %s<br/><b>Total QSRs:</b> %s",
      s$zip_code, legend_title(), metric_label,
      scales::comma(s$population),
      ifelse(is.na(s$Total_QSRs), "0", as.character(s$Total_QSRs))
    ) |> lapply(htmltools::HTML)
    
    # draw polygons + legend
    proxy <- leafletProxy("map") %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(
        data = s,
        fillColor = pal_f(vals),
        fillOpacity = 0.85,
        color = "#666", weight = 0.6,
        label = lbl,
        highlightOptions = highlightOptions(weight = 2, color = "#000",
                                            fillOpacity = 0.95, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal_f, values = vals, title = legend_title(),
                position = "bottomright", opacity = 0.85)
    
    # --- AUTO-ZOOM  ---
    u   <- suppressWarnings(sf::st_union(sf::st_geometry(s)))
    bb  <- sf::st_bbox(u)
    pad <- 0.02
    
    lng1 <- as.numeric(bb["xmin"]) - pad
    lat1 <- as.numeric(bb["ymin"]) - pad
    lng2 <- as.numeric(bb["xmax"]) + pad
    lat2 <- as.numeric(bb["ymax"]) + pad
    
    # fresh proxy call for zoom (more reliable than reusing the chain)
    leafletProxy("map") %>% leaflet::fitBounds(lng1 = lng1, lat1 = lat1,
                                               lng2 = lng2, lat2 = lat2)
    
    # hard fallback: also setView to the centroid (in case fitBounds is ignored)
    ctr <- suppressWarnings(sf::st_coordinates(sf::st_centroid(u)))[1, ]
    if (length(ctr) == 2 && all(is.finite(ctr))) {
      leafletProxy("map") %>% leaflet::setView(lng = ctr[1], lat = ctr[2], zoom = 10)
    
    }
  })
}

shinyApp(ui, server)