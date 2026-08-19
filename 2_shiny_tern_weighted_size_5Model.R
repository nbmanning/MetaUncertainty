# title: 2_shiny_tern_weighted_size_5Model.R
# purpose: import the blind reviewer + consensus CSV to count the number of disagreements
# created May 2026
# last edited: May 2026

# author: Nick Manning

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(shiny)
library(shinyBS)  # for tooltips
library(ggtern)

# UI
ui <- fluidPage(
  titlePanel("Model Approach Ternary Plot"),
  sidebarLayout(
    sidebarPanel(
      h4("Model 1"),
      sliderInput("ms", "How confident are you that your approach addresses Model Specification?:", 0, 3, 0),
      bsTooltip("ms", "Uncertainty from the choice of modeling technique", "right", options = list(container = "body")),
      
      sliderInput("ks", "How confident are you that your approach addresses Knowledge of the System?:", 0, 3, 0),
      bsTooltip("ks", "Uncertainty from an incomplete understanding of the system", "right", options = list(container = "body")),
      
      sliderInput("sts", "How confident are you that your approach addresses Spatial & Temporal Scale Issues?:", 0, 3, 0),
      bsTooltip("sts", "Uncertainty from mismatched spatial or temporal scales", "right", options = list(container = "body")),
      
      sliderInput("sdh", "How confident are you that your approach addresses Spatial Dependence & Heterogeneity Issues?:", 0, 3, 0),
      bsTooltip("sdh", "Uncertainty from spatial autocorrelation or heterogeneity", "right", options = list(container = "body")),
      
      sliderInput("edl", "How confident are you that your approach addresses Empirical Data Limitations?:", 0, 3, 0),
      bsTooltip("edl", "Uncertainty from missing, noisy, or biased data", "right", options = list(container = "body")),
      
      sliderInput("cl", "How confident are you that your approach addresses Computing Limitations?:", 0, 3, 0),
      bsTooltip("cl", "Uncertainty due to computational constraints", "right", options = list(container = "body")),
      
      tags$hr(),
      
      # --- Toggle for Model 2 ---
      checkboxInput("show2", "Show Model 2", value = FALSE),
      
      # Only show the second set of sliders when toggled on
      conditionalPanel(
        condition = "input.show2",
        h4("Model 2"),
        sliderInput("ms2", "Model Specification (Model 2):", 0, 3, 0),
        bsTooltip("ms2", "Uncertainty from the choice of modeling technique", "right", options = list(container = "body")),
        
        sliderInput("ks2", "Knowledge of the System (Model 2):", 0, 3, 0),
        bsTooltip("ks2", "Uncertainty from an incomplete understanding of the system", "right", options = list(container = "body")),
        
        sliderInput("sts2", "Spatial & Temporal Scale Issues (Model 2):", 0, 3, 0),
        bsTooltip("sts2", "Uncertainty from mismatched spatial or temporal scales", "right", options = list(container = "body")),
        
        sliderInput("sdh2", "Spatial Dependence & Heterogeneity Issues (Model 2):", 0, 3, 0),
        bsTooltip("sdh2", "Uncertainty from spatial autocorrelation or heterogeneity", "right", options = list(container = "body")),
        
        sliderInput("edl2", "Empirical Data Limitations (Model 2):", 0, 3, 0),
        bsTooltip("edl2", "Uncertainty from missing, noisy, or biased data", "right", options = list(container = "body")),
        
        sliderInput("cl2", "Computing Limitations (Model 2):", 0, 3, 0),
        bsTooltip("cl2", "Uncertainty due to computational constraints", "right", options = list(container = "body"))
      ),
      # --- Toggle for Model 3 ---
      checkboxInput("show3", "Show Model 3", value = FALSE),
      
      # Only show the second set of sliders when toggled on
      conditionalPanel(
        condition = "input.show3",
        h4("Model 3"),
        sliderInput("ms3", "Model Specification (Model 3):", 0, 3, 0),
        bsTooltip("ms3", "Uncertainty from the choice of modeling technique", "right", options = list(container = "body")),
        
        sliderInput("ks3", "Knowledge of the System (Model 3):", 0, 3, 0),
        bsTooltip("ks3", "Uncertainty from an incomplete understanding of the system", "right", options = list(container = "body")),
        
        sliderInput("sts3", "Spatial & Temporal Scale Issues (Model 3):", 0, 3, 0),
        bsTooltip("sts3", "Uncertainty from mismatched spatial or temporal scales", "right", options = list(container = "body")),
        
        sliderInput("sdh3", "Spatial Dependence & Heterogeneity Issues (Model 3):", 0, 3, 0),
        bsTooltip("sdh3", "Uncertainty from spatial autocorrelation or heterogeneity", "right", options = list(container = "body")),
        
        sliderInput("edl3", "Empirical Data Limitations (Model 3):", 0, 3, 0),
        bsTooltip("edl3", "Uncertainty from missing, noisy, or biased data", "right", options = list(container = "body")),
        
        sliderInput("cl3", "Computing Limitations (Model 3):", 0, 3, 0),
        bsTooltip("cl3", "Uncertainty due to computational constraints", "right", options = list(container = "body"))
      ),
    
    # --- Toggle for Model 4 ---
    checkboxInput("show4", "Show Model 4", value = FALSE),
    
    # Only show the second set of sliders when toggled on
    conditionalPanel(
      condition = "input.show4",
      h4("Model 4"),
      sliderInput("ms4", "Model Specification (Model 4):", 0, 3, 0),
      bsTooltip("ms4", "Uncertainty from the choice of modeling technique", "right", options = list(container = "body")),
      
      sliderInput("ks4", "Knowledge of the System (Model 4):", 0, 3, 0),
      bsTooltip("ks4", "Uncertainty from an incomplete understanding of the system", "right", options = list(container = "body")),
      
      sliderInput("sts4", "Spatial & Temporal Scale Issues (Model 4):", 0, 3, 0),
      bsTooltip("sts4", "Uncertainty from mismatched spatial or temporal scales", "right", options = list(container = "body")),
      
      sliderInput("sdh4", "Spatial Dependence & Heterogeneity Issues (Model 4):", 0, 3, 0),
      bsTooltip("sdh4", "Uncertainty from spatial autocorrelation or heterogeneity", "right", options = list(container = "body")),
      
      sliderInput("edl4", "Empirical Data Limitations (Model 4):", 0, 3, 0),
      bsTooltip("edl4", "Uncertainty from missing, noisy, or biased data", "right", options = list(container = "body")),
      
      sliderInput("cl4", "Computing Limitations (Model 4):", 0, 3, 0),
      bsTooltip("cl4", "Uncertainty due to computational constraints", "right", options = list(container = "body"))
    ),
    # --- Toggle for Model 5 ---
    checkboxInput("show5", "Show Model 5", value = FALSE),
    
    # Only show the second set of sliders when toggled on
    conditionalPanel(
      condition = "input.show5",
      h4("Model 5"),
      sliderInput("ms5", "Model Specification (Model 5):", 0, 3, 0),
      bsTooltip("ms5", "Uncertainty from the choice of modeling technique", "right", options = list(container = "body")),
      
      sliderInput("ks5", "Knowledge of the System (Model 5):", 0, 3, 0),
      bsTooltip("ks5", "Uncertainty from an incomplete understanding of the system", "right", options = list(container = "body")),
      
      sliderInput("sts5", "Spatial & Temporal Scale Issues (Model 5):", 0, 3, 0),
      bsTooltip("sts5", "Uncertainty from mismatched spatial or temporal scales", "right", options = list(container = "body")),
      
      sliderInput("sdh5", "Spatial Dependence & Heterogeneity Issues (Model 5):", 0, 3, 0),
      bsTooltip("sdh5", "Uncertainty from spatial autocorrelation or heterogeneity", "right", options = list(container = "body")),
      
      sliderInput("edl5", "Empirical Data Limitations (Model 5):", 0, 3, 0),
      bsTooltip("edl5", "Uncertainty from missing, noisy, or biased data", "right", options = list(container = "body")),
      
      sliderInput("cl5", "Computing Limitations (Model 5):", 0, 3, 0),
      bsTooltip("cl5", "Uncertainty due to computational constraints", "right", options = list(container = "body"))
    )
  ),
    
    mainPanel(
      plotOutput("plot", height = "700px", width = "110%")
    )
  )
)

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
    # Helper to compute ternary and display vars from six sliders
    compute_point <- function(ms, ks, sts, sdh, edl, cl, label){
      weights <- c(
        "Model Specification" = ms,
        "Knowledge of the System" = ks,
        "Spatial & Temporal Scale Issues" = sts,
        "Spatial Dependence & Heterogeneity Issues" = sdh,
        "Empirical Data Limitations" = edl,
        "Computing Limitations" = cl
      )
      
      system <- weights["Model Specification"] + weights["Knowledge of the System"]
      unit   <- weights["Spatial & Temporal Scale Issues"] + weights["Spatial Dependence & Heterogeneity Issues"]
      data   <- weights["Empirical Data Limitations"] + weights["Computing Limitations"]
      total  <- system + unit + data
      
      if (total == 0) return(NULL)
      
      df <- data.frame(
        x = system / total,   # top (R)
        y = unit   / total,   # left (B)
        z = data   / total,   # right (G)
        total = as.numeric(total),
        id = label,
        stringsAsFactors = FALSE
      )
      
      # Border group by total
      df$border_group <- cut(
        df$total,
        breaks = c(-Inf, 6, 12, Inf),
        labels = c("<=6", "6-12", "12-18"),
        include.lowest = TRUE,
        right = FALSE
      )
      df
    }
    
    # Model 1 is always computed (unless total==0)
    df1 <- compute_point(input$ms,  input$ks,  input$sts,  input$sdh,  input$edl,  input$cl,  "Model 1")
    
    # Model 2 only if toggled on
    df2 <- NULL
    if (isTRUE(input$show2)) {
      df2 <- compute_point(input$ms2, input$ks2, input$sts2, input$sdh2, input$edl2, input$cl2, "Model 2")
    }
    
    # Model 3 only if toggled on
    df3 <- NULL
    if (isTRUE(input$show3)) {
      df3 <- compute_point(input$ms3, input$ks3, input$sts3, input$sdh3, input$edl3, input$cl3, "Model 3")
    }
    
    # Model 4 only if toggled on
    df4 <- NULL
    if (isTRUE(input$show4)) {
      df4 <- compute_point(input$ms4, input$ks4, input$sts4, input$sdh4, input$edl4, input$cl4, "Model 4")
    }
    
    # Model 5 only if toggled on
    df5 <- NULL
    if (isTRUE(input$show5)) {
      df5 <- compute_point(input$ms5, input$ks5, input$sts5, input$sdh5, input$edl5, input$cl5, "Model 5")
    }
    
    # If neither point has a positive total, nothing to plot
    if (is.null(df1) && is.null(df2) && is.null(df3) && is.null(df4) && is.null(df5)) return(NULL)
    
    # Combine available points
    df <- do.call(rbind, Filter(Negate(is.null), list(df1, df2, df3, df4, df5)))
    
    # Size scale bounds (0–3 per slider * 6 = 0–18)
    total_min <- 0
    total_max <- 18
    
    print(
      ggtern(df, aes(x, y, z)) +
        # Use a filled shape (21) so fill mapping is visible
        geom_point(
          aes(
            size  = total,
            color = border_group,  # border color reflects total bins
            # shape = border_group
            fill  = id             # fill distinguishes Model 1 vs Model 2
          ),
          shape = 21,              # filled circle with border
          stroke = 2.0   
        ) +
        
        # Scales
        scale_size_continuous(
          range = c(1, 18),
          limits = c(total_min, total_max),
          guide = "none"
        ) +
        scale_color_manual(
          values = c("<=6" = "#E41A1C", "6-12" = "grey40", "12-18" = "#4DAF4A"),
          name = "Total (Border)"
        ) +
        scale_fill_manual(
          #values = c("Model 1" = "#1F78B4", "Model 2" = "#FDBF6F", "Model 3" = "#4ABF8F", "Model 4" = "purple", "Model 5" = "pink"),
          values = c("Model 1" = "#66c2a5", "Model 2" = "#fc8d62", "Model 3" = "#8da0cb", "Model 4" = "#e78ac3", "Model 5" = "#a6d854"),
          name   = "Point (Fill)",
          drop   = TRUE
        ) +

        theme_bw() +
        labs(
          title = "Ternary Plot of Meta-Uncertainty Categories",
          x = "System",
          y = "Unit of Analysis",
          z = "Data"
        ) +
        theme(
          plot.title    = element_text(hjust = 0.5),
          axis.title    = element_text(size = 14),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.position = "right"
        ) +
      guides(
        fill = guide_legend(
          title = "Point (Fill)",
          override.aes = list(shape = 21, size = 6, stroke = 1.5) # bigger glyph
        ),
        color = guide_legend(
          title = "Total (Border)",
          override.aes = list(shape = 21, size = 6, stroke = 1.5, fill = "white")
        ))
    )
  })
}

shinyApp(ui, server)
