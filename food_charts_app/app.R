#-------------------------------------------------------------------------------
#  Food App
#-------------------------------------------------------------------------------
library(rsconnect)
library(shiny)     # UI/server
library(readr)     # read_csv()
library(dplyr)     # filter(), mutate(), group_by(), summarise(), arrange()
library(tidyr)     # pivot_longer()/pivot_wider()
library(forcats)   # fct_reorder(), fct_relevel() for food_type/city ordering
library(ggplot2)   # plotting
library(scales)    # label_number(), comma(), percent_format() for axes/labels
library(stringr)   # minor label cleanup (e.g., str_to_title), if used
# ---------- Load data (prefers in-memory viz.data; else reads from folder) ----------
if (exists("viz.data")) {
  df_raw <- viz.data
} else if (exists("viz_data")) {
  df_raw <- viz_data
} else if (file.exists("viz_data.csv")) {
  df_raw <- readr::read_csv("viz_data.csv", show_col_types = FALSE)
} else if (file.exists("viz_data.xlsx") && requireNamespace("readxl", quietly = TRUE)) {
  df_raw <- readxl::read_excel("viz_data.xlsx")
} else {
  stop
}

# ---------- Clean with column names ----------
# (expects: city_name, food_type, residents_by_qsr, avg_satisfaction, qsr_count)
df <- df_raw %>%
  transmute(
    city_name         = stringr::str_squish(as.character(city_name)),
    food_type         = stringr::str_squish(as.character(food_type)),
    residents_by_qsr  = suppressWarnings(as.numeric(residents_by_qsr)),
    avg_satisfaction  = suppressWarnings(as.numeric(avg_satisfaction)),
    qsr_count         = suppressWarnings(as.numeric(qsr_count)),
    # original scaled
    food_scaled_res_qsr = residents_by_qsr / 1000,
    # ✅ yew scaled series: multiply original by 2
    scaled_res_per_QSR_food = (residents_by_qsr / 1000) * 2
  )

# ---------- Helper: drop "Other" (case-insensitive), but don't show that in titles ----------
drop_other <- function(d) {
  d %>% filter(!is.na(food_type), str_to_lower(food_type) != "other")
}

metro_choices <- df %>% pull(city_name) %>% unique() %>% sort()

# ================= UI =================
ui <- fluidPage(
  titlePanel("Food-type metrics by Metro"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metro", "Metro area:", choices = metro_choices, selected = metro_choices[1]),
      radioButtons(
        "chart", "Chart type",
        choices = c(
          "Stacked: Scaled residents/QSR + Customer satisfaction" = "stacked",
          "QSR count" = "qsrs",
          "Customer satisfaction" = "sat"
        ),
        selected = "stacked"
      ),
      width = 3
    ),
    mainPanel(
      plotOutput("p", height = "72vh")
    )
  )
)

# ================= Server =================
server <- function(input, output, session) {
  
  # Selected metro, "Other" removed
  sel <- reactive({
    df %>% filter(city_name == input$metro) %>% drop_other()
  })
  
  # ---------- Stacked chart data ----------
  # Uses scaled_res_per_QSR_food and sorts desc by that metric.
  overlay_df <- reactive({
    d <- sel() %>%
      mutate(
        # scaling: (residents/QSR ÷ 1000) × 2
        scaled_res_per_QSR_food = (residents_by_qsr / 1000) * 2,
        # order by satisfaction (ascending)
        food_type = forcats::fct_reorder(food_type, avg_satisfaction, .desc = FALSE)
      )
    
    # scale factor so blue bars (satisfaction) align with right axis
    num_max <- suppressWarnings(max(d$scaled_res_per_QSR_food, na.rm = TRUE))
    den_max <- suppressWarnings(max(d$avg_satisfaction,        na.rm = TRUE))
    sf <- if (is.finite(num_max) && is.finite(den_max) && den_max > 0) num_max / den_max else 1
    
    d %>% mutate(
      avg_sat_scaled = avg_satisfaction * sf,
      scale_factor   = sf
    )
  })
  
  output$p <- renderPlot({
    mode <- input$chart
    
    # Overlay data + scaling (not stacked)
    overlay_df <- reactive({
      d <- sel() %>%
        mutate(
          # scaling: (residents/QSR ÷ 1000) × 2
          scaled_res_per_QSR_food = (residents_by_qsr / 1000) * 2,
          # order by satisfaction (ascending)
          food_type = forcats::fct_reorder(food_type, avg_satisfaction, .desc = FALSE)
        )
      
      # scale factor so blue bars (satisfaction) align with right axis
      num_max <- suppressWarnings(max(d$scaled_res_per_QSR_food, na.rm = TRUE))
      den_max <- suppressWarnings(max(d$avg_satisfaction,        na.rm = TRUE))
      sf <- if (is.finite(num_max) && is.finite(den_max) && den_max > 0) num_max / den_max else 1
      
      d %>% mutate(
        avg_sat_scaled = avg_satisfaction * sf,
        scale_factor   = sf
      )
    })

  if (mode == "stacked") {   # keep the same switch; it's now the overlay style
    d  <- overlay_df()
    sf <- unique(d$scale_factor)
    
    # to leave headroom for labels
    max_y <- max(pmax(d$scaled_res_per_QSR_food, d$avg_sat_scaled), na.rm = TRUE) * 1.08
    
    ggplot(d, aes(x = food_type)) +
      # BACK layer: Residents/QSR (orange)
      geom_col(aes(y = scaled_res_per_QSR_food),
               fill = "#f39c12", width = 0.85) +
      # FRONT layer: Customer satisfaction (blue), slightly narrower so the back shows
      geom_col(aes(y = avg_sat_scaled),
               fill = "#2b6cb0", width = 0.60) +
      # labels (match each series' unit)
      geom_text(aes(y = scaled_res_per_QSR_food,
                    label = scales::number(scaled_res_per_QSR_food, accuracy = 0.01)),
                vjust = -0.35, color = "#f39c12", size = 3.2) +
      geom_text(aes(y = avg_sat_scaled,
                    label = scales::number(avg_satisfaction, accuracy = 0.01)),
                vjust = -0.35, color = "#2b6cb0", size = 3.2) +
      scale_y_continuous(
        name     = "Residents/QSR (in thousands)",
        sec.axis = sec_axis(~ . / sf, name = "Avg. Customer satisfaction score")
      ) +
      coord_cartesian(ylim = c(0, max_y)) +
      labs(x = "Food Type", title = paste("Metro:", input$metro)) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor   = element_blank(),
        axis.title.y.right = element_text(margin = margin(l = 10)),
        legend.position    = "none"  # no legend needed; colors are distinct
      )
      } else if (mode == "qsrs") {
      # Different color for each food type
      d <- sel()
      ggplot(d, aes(x = fct_reorder(food_type, qsr_count), y = qsr_count, fill = food_type)) +
        geom_col(width = 0.8) +
        scale_fill_brewer(palette = "Paired", guide = guide_legend(title = "Food type")) +
        labs(x = "Food Type", y = "QSR count",
             title = paste("QSR count by food type —", input$metro)) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank())
      
    } else { # "sat" — two colors for positive vs negative
      d <- sel() %>%
        mutate(sign = if_else(avg_satisfaction >= 0, "Positive", "Negative"))
      
      ggplot(d, aes(x = fct_reorder(food_type, avg_satisfaction),
                    y = avg_satisfaction,
                    fill = sign)) +
        geom_col(width = 0.8) +
        scale_fill_manual(
          values = c("Positive" = "deepskyblue",   # calm teal
                     "Negative" = "#fc8d62"),  # soft orange
          guide = guide_legend(title = NULL)
        ) +
        labs(x = "Food Type", y = "Customer satisfaction",
             title = paste("Customer satisfaction by food type —", input$metro)) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank())
    }
  })
}

shinyApp(ui, server)