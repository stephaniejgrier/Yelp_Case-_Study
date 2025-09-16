#-------------------------------------------------------------------------------
# Pearson Correlation app — All or select metro - business attributes selectable
#-------------------------------------------------------------------------------

library(shiny)
library(bs4Dash)
library(tidyverse)
library(shinyWidgets)
library(reshape2)
library(janitor)

# ---------- helpers ----------
to_num01 <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(x))
}

# coalesce multiple possible columns into one; drop originals
coalesce_alias <- function(df, aliases, new_name) {
  present <- intersect(aliases, names(df))
  if (length(present) == 0) return(df)
  if (!(new_name %in% names(df))) df[[new_name]] <- NA
  for (nm in present) df[[new_name]] <- dplyr::coalesce(df[[new_name]], df[[nm]])
  # drop originals except the new_name if it was one of them
  drop_these <- setdiff(present, new_name)
  df <- dplyr::select(df, -dplyr::any_of(drop_these))
  df
}

pair_counts <- function(d) {
  p <- ncol(d); nmat <- matrix(0L, p, p, dimnames = list(colnames(d), colnames(d)))
  for (i in seq_len(p)) for (j in seq_len(p))
    nmat[i, j] <- sum(stats::complete.cases(d[, c(i, j), drop = FALSE]))
  nmat
}

# ---------- data ----------
raw <- readr::read_csv("data2.csv", show_col_types = FALSE) %>% clean_names()

# Unify/guess metro column
metro_col <- c("metro_area", "city", "metro")[c("metro_area","city","metro") %in% names(raw)][1]
validate_needed <- function() {
  if (is.na(metro_col)) stop("No metro column named metro_area/city/metro found.")
}
validate_needed()

# Coalesce common aliases to stable internal names
# accepts_credit_cards
acc_aliases <- c(
  "businessacceptscreditcards", "accepts_credit_cards",
  "acceptscreditcards", "accepts_creditcard", "business_accepts_credit_cards",
  "accepts_credit_cards_", "accepts_credit_card", "accepts_credit_cards__1",
  "accepts_creditcards", "accepts_credit_cards_1", "acceptscreditcard",
  "accepts_credit", "accepts_creditcard_", "accepts_credit_cards__2"
)
raw <- coalesce_alias(raw, acc_aliases, "accepts_credit_cards")

# customer satisfaction
csat_aliases <- c("customer_satisfaction", "csat", "avg_customer_satisfaction")
raw <- coalesce_alias(raw, csat_aliases, "customer_satisfaction")

# Build available attribute list (internals)
candidate_order <- c(
  "customer_satisfaction",
  "accepts_credit_cards",
  "wheelchair_accessible",
  "outdoor_seating",
  "has_tv",
  "dogs_allowed",
  "good_for_kids",
  "restaurants_good_for_groups",
  "drive_thru",
  "restaurants_price_range2",
  "latenight",
  "lunch",
  "dinner",
  "breakfast",
  "has_free_wifi"
)
avail <- intersect(candidate_order, names(raw))

# Pretty labels (1:1 with internal names)
pretty <- c(
  customer_satisfaction      = "Customer Satisfaction",
  accepts_credit_cards       = "Accepts Credit Cards",
  wheelchair_accessible      = "Wheelchair Accessible",
  outdoor_seating            = "Outdoor Seating",
  has_tv                     = "Has TV",
  dogs_allowed               = "Dogs Allowed",
  good_for_kids              = "Good for Kids",
  restaurants_good_for_groups= "Restaurants Good for Groups",
  drive_thru                 = "Drive Thru",
  restaurants_price_range2   = "Restaurants Price Range2",
  latenight                  = "Latenight",
  lunch                      = "Lunch",
  dinner                     = "Dinner",
  breakfast                  = "Breakfast",
  has_free_wifi              = "Has Free WiFi"
)

# Metro choices
city_choices <- raw %>% dplyr::distinct(.data[[metro_col]]) %>% dplyr::pull(1)

# ---------- UI ----------
ui <- bs4DashPage(
  header  = bs4DashNavbar(disable = TRUE),
  sidebar = bs4DashSidebar(disable = TRUE),
  body = bs4DashBody(
    tags$head(
      tags$style(HTML(
        "html, body, .content-wrapper, .wrapper { height: 100%; margin: 0; padding: 0; }
         .box, .bs4-box { height: 100vh; }"
      ))
    ),
    fluidRow(
      box(
        title = "Controls", width = 3, status = "primary",
        selectInput("metro", "Choose a Metro area:", choices = c("All", city_choices), selected = "All"),
        pickerInput(
          inputId = "cols",
          label   = "Attributes to include",
          multiple = TRUE,
          options  = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count > 3"),
          # show pretty labels; values are INTERNAL names
          choices  = setNames(avail, unname(pretty[avail])),
          # default: everything EXCEPT accepts_credit_cards
          selected = setdiff(avail, "accepts_credit_cards")
        ),
        helpText("Notes: constant/empty columns are dropped; suspicious ±1 with very low pair counts are masked.")
      ),
      box(
        title = "Correlation Heatmap", width = 9, status = "primary",
        plotOutput("heat", height = "700px")
      )
    )
  ),
  footer = bs4DashFooter()
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  plot_dat <- reactive({
    req(input$cols)
    
    d <- raw %>% dplyr::select(all_of(c(metro_col, input$cols)))
    
    if (!is.null(input$metro) && input$metro != "All") {
      d <- d %>% dplyr::filter(.data[[metro_col]] == input$metro)
    }
    
    # If the user did NOT select accepts_credit_cards, drop any leftover alias just in case
    if (!("accepts_credit_cards" %in% input$cols)) {
      d <- d %>% dplyr::select(-dplyr::any_of(acc_aliases), -dplyr::any_of("accepts_credit_cards"))
    }
    
    d %>% dplyr::mutate(dplyr::across(-all_of(metro_col), to_num01))
  }) %>% debounce(250)
  
  output$heat <- renderPlot({
    d_all <- plot_dat()
    d_num <- d_all %>%
      dplyr::select(-all_of(metro_col)) %>%
      dplyr::select(where(is.numeric)) %>%
      dplyr::select(where(~ sum(!is.na(.)) > 1)) %>%
      dplyr::select(where(~ dplyr::n_distinct(.[!is.na(.)]) > 1))
    
    shiny::validate(
      shiny::need(ncol(d_num) >= 2, "Need Customer Satisfaction + at least one other attribute with usable data.")
    )
    # Correlation
    M <- suppressWarnings(cor(d_num, use = "pairwise.complete.obs", method = "pearson"))
    
    # Pairwise complete counts
    nmat <- pair_counts(d_num)
    
    # Hardened masking
    M_mask <- M
    # 1) Exact ±1 with small N: treat as unreliable -> mask
    M_mask[(M %in% c(-1, 1)) & (nmat < 20)] <- NA_real_
    # 2) Near perfect with very small N: mask
    M_mask[(abs(M) >= 0.995) & (nmat < 10)] <- NA_real_
    
    # Labels must follow the matrix column order (to keep the diagonal aligned)
    lvls <- colnames(M_mask)
    axis_labs <- setNames(unname(pretty[lvls]), lvls)
    
    df_long <- reshape2::melt(M_mask, na.rm = FALSE)  # keep NA for masking
    
    ggplot(df_long, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "brown1", high = "forestgreen", mid = "bisque",
        midpoint = 0, limits = c(-1, 1), name = "Pearson\nCorrelation", na.value = "grey90"
      ) +
      geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.2f", value))), size = 5) +
      labs(title = paste0("Pearson correlation — ", if (input$metro == "All") "All metros" else input$metro),
           x = NULL, y = NULL) +
      scale_x_discrete(labels = axis_labs) +
      scale_y_discrete(labels = axis_labs) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid  = element_blank(),
        plot.title  = element_text(hjust = 0.5)
      )
  })
}

shinyApp(ui, server)
