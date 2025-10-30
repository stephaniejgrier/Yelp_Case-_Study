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

# keep your 10 metros
city_order <- c("Philadelphia","Tampa","Indianapolis","St Louis","Nashville",
                "Tucson","New Orleans","Boise","Reno","Santa Barbara")
df <- df %>% filter(city_name %in% city_order)

# ZCTA geometries (generalized; cached)
zctas20 <- zctas(year = 2020, cb = TRUE) %>%
  select(zip_code = ZCTA5CE20, geometry) %>%
  mutate(zip_code = as.character(zip_code))

# --- Shiny UI -----------------------------------------------------------------
ui <- fluidPage(
  titlePanel("ZIP-level Market View — pick a metro & metric"),
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Metro area:", choices = city_order, selected = city_order[1]),
      radioButtons(
        "metric", "Metric",
        choices = c("QSRs per 10k residents" = "qsr_per_10k",
                    "Customer satisfaction"   = "customer_satisfaction"),
        selected = "qsr_per_10k"
      ),
      checkboxInput("quantile", "Use quantile color bins", TRUE),
      helpText("Tip: quantile bins balance color spread across ZIPs.")
    ),
    mainPanel(leafletOutput("map", height = 720))
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
    vals <- metric_vals()
    pal_f <- pal()
    
    # popup-friendly labels
    metric_label <- if (input$metric == "qsr_per_10k") {
      ifelse(is.na(s$qsr_per_10k), "—", comma(round(s$qsr_per_10k, 1)))
    } else {
      ifelse(is.na(s$customer_satisfaction), "—", number(s$customer_satisfaction, accuracy = 0.1))
    }
    
    lbl <- sprintf(
      "<b>ZIP:</b> %s<br/><b>%s:</b> %s<br/>
       <b>Population:</b> %s<br/><b>Total QSRs:</b> %s",
      s$zip_code,
      legend_title(),
      metric_label,
      comma(s$population),
      ifelse(is.na(s$Total_QSRs), "0", as.character(s$Total_QSRs))
    ) %>% lapply(HTML)
    
    bb <- sf::st_bbox(s)
    
    leafletProxy("map") %>%
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
                position = "bottomright", opacity = 0.85) %>%
      fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
  })
}

shinyApp(ui, server)