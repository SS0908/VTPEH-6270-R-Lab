# ==============================================================================
# COVID-19 Vaccine Hesitancy & Social Vulnerability Index
# Shiny App - VTPEH 6270 Check Point 07
# ==============================================================================
# Research Question: Is COVID-19 vaccine hesitancy higher in counties with
#                    higher Social Vulnerability Index (SVI)?
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# ---- Data loading & cleaning ------------------------------------------------

hesitancy_raw <- read_csv("R Lab_Vaccine_Hesitancy copy.csv", show_col_types = FALSE)

# Clean percentage columns (stored as "18.06%") -> numeric
pct_to_num <- function(x) {
  if (is.numeric(x)) return(x)
  as.numeric(sub("%", "", x))
}

hesitancy <- hesitancy_raw %>%
  rename(
    fips            = `FIPS Code`,
    county          = `County Name`,
    state           = State,
    state_code      = `State Code`,
    est_hesitant    = `Estimated hesitant`,
    est_hes_unsure  = `Estimated hesitant or unsure`,
    est_strong_hes  = `Estimated strongly hesitant`,
    svi             = `Social Vulnerability Index (SVI)`,
    svi_cat         = `SVI Category`,
    cvac            = `CVAC level of concern for vaccination rollout`,
    cvac_cat        = `CVAC Level Of Concern`,
    pct_vax         = `Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`
  ) %>%
  mutate(
    across(c(est_hesitant, est_hes_unsure, est_strong_hes, pct_vax), pct_to_num),
    svi_cat = factor(
      svi_cat,
      levels = c("Very Low Vulnerability", "Low Vulnerability",
                 "Moderate Vulnerability", "High Vulnerability",
                 "Very High Vulnerability")
    ),
    state = tools::toTitleCase(tolower(state))
  ) %>%
  filter(!is.na(svi_cat))

# Ordered palette from low (cool) to high (warm) vulnerability
svi_palette <- c(
  "Very Low Vulnerability"  = "#2c7bb6",
  "Low Vulnerability"       = "#abd9e9",
  "Moderate Vulnerability"  = "#ffffbf",
  "High Vulnerability"      = "#fdae61",
  "Very High Vulnerability" = "#d7191c"
)

state_choices <- c("All states" = "ALL", sort(unique(hesitancy$state)))

hesitancy_var_choices <- c(
  "Estimated hesitant (%)"            = "est_hesitant",
  "Estimated hesitant or unsure (%)"  = "est_hes_unsure",
  "Estimated strongly hesitant (%)"   = "est_strong_hes"
)

# ---- UI ---------------------------------------------------------------------

ui <- page_navbar(
  title = "Vaccine Hesitancy & Social Vulnerability",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    base_font = font_google("Source Sans Pro")
  ),
  
  # ---- Tab 1: About -------------------------------------------------------
  nav_panel(
    title = "About",
    icon = icon("circle-info"),
    div(
      class = "container",
      style = "max-width: 900px; margin-top: 24px;",
      h2("Is COVID-19 vaccine hesitancy higher in counties with higher Social Vulnerability?"),
      p(class = "lead",
        "This app explores the relationship between county-level COVID-19 vaccine",
        "hesitancy and the CDC's Social Vulnerability Index (SVI) across the",
        "United States."),
      hr(),
      h4("App goals"),
      tags$ul(
        tags$li(strong("Describe"), " the distribution of estimated vaccine hesitancy across counties."),
        tags$li(strong("Compare"), " hesitancy across the five SVI categories (Very Low \u2192 Very High)."),
        tags$li(strong("Test"), " whether hesitancy differs significantly between SVI groups."),
        tags$li(strong("Explore"), " the underlying county-level data.")
      ),
      h4("Variables"),
      tags$ul(
        tags$li(strong("Outcome (continuous):"), " estimated % of adults hesitant toward COVID-19 vaccination."),
        tags$li(strong("Predictor (categorical):"), " SVI category \u2014 five ordered levels of social vulnerability.")
      ),
      h4("Data"),
      p("U.S. county-level estimates of COVID-19 vaccine hesitancy paired with the",
        "CDC/ATSDR Social Vulnerability Index. The dataset includes",
        strong(nrow(hesitancy)),
        "counties across",
        strong(length(unique(hesitancy$state))),
        "states / territories."),
      h4("How to use"),
      tags$ol(
        tags$li(em("Explore"), " \u2014 pick a plot type and optionally filter by state."),
        tags$li(em("Compare by SVI"), " \u2014 see hesitancy grouped by the five vulnerability categories."),
        tags$li(em("Statistical test"), " \u2014 run an ANOVA or Kruskal\u2013Wallis test on the selected data."),
        tags$li(em("Data table"), " \u2014 browse the raw county-level data.")
      ),
      p(em("Amandine Gamble \u2014 VTPEH 6270, Spring 2026"), style = "color: #7f8c8d;")
    )
  ),
  
  # ---- Tab 2: Explore -----------------------------------------------------
  nav_panel(
    title = "Explore",
    icon = icon("chart-column"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        title = "Controls",
        selectInput("explore_var", "Hesitancy measure:",
                    choices = hesitancy_var_choices,
                    selected = "est_hesitant"),
        selectInput("explore_state", "State filter:",
                    choices = state_choices, selected = "ALL"),
        radioButtons("explore_plot", "Plot type:",
                     choices = c("Histogram"    = "hist",
                                 "Density"      = "density",
                                 "Boxplot by SVI" = "box"),
                     selected = "hist"),
        sliderInput("explore_bins", "Histogram bins:",
                    min = 10, max = 80, value = 30, step = 5),
        hr(),
        helpText("Filters apply to this tab's plot and summary only.")
      ),
      card(
        card_header("Distribution of vaccine hesitancy"),
        plotOutput("explore_plot", height = "420px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Summary statistics"),
             verbatimTextOutput("explore_summary")),
        card(card_header("Counties included"),
             verbatimTextOutput("explore_n"))
      )
    )
  ),
  
  # ---- Tab 3: Compare by SVI ---------------------------------------------
  nav_panel(
    title = "Compare by SVI",
    icon = icon("layer-group"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        title = "Controls",
        selectInput("cmp_var", "Hesitancy measure:",
                    choices = hesitancy_var_choices,
                    selected = "est_hesitant"),
        selectInput("cmp_state", "State filter:",
                    choices = state_choices, selected = "ALL"),
        checkboxGroupInput("cmp_cats", "SVI categories to include:",
                           choices = levels(hesitancy$svi_cat),
                           selected = levels(hesitancy$svi_cat)),
        radioButtons("cmp_plot", "Plot type:",
                     choices = c("Boxplot"         = "box",
                                 "Violin"          = "violin",
                                 "Mean \u00b1 95% CI" = "mean"),
                     selected = "box"),
        hr(),
        helpText("Tip: the SVI categories are ordered from lowest (left) to highest (right) social vulnerability.")
      ),
      card(
        card_header(textOutput("cmp_title", inline = TRUE)),
        plotOutput("cmp_plot", height = "460px")
      ),
      card(
        card_header("Group means"),
        tableOutput("cmp_table")
      )
    )
  ),
  
  # ---- Tab 4: Statistical test -------------------------------------------
  nav_panel(
    title = "Statistical test",
    icon = icon("calculator"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        title = "Controls",
        selectInput("test_var", "Hesitancy measure:",
                    choices = hesitancy_var_choices,
                    selected = "est_hesitant"),
        selectInput("test_state", "State filter:",
                    choices = state_choices, selected = "ALL"),
        radioButtons("test_type", "Test:",
                     choices = c("One-way ANOVA"         = "anova",
                                 "Kruskal\u2013Wallis"   = "kw"),
                     selected = "anova"),
        numericInput("test_alpha", "Significance level (\u03b1):",
                     value = 0.05, min = 0.001, max = 0.2, step = 0.01),
        hr(),
        actionButton("run_test", "Run test", class = "btn-primary", width = "100%"),
        hr(),
        helpText("ANOVA tests whether group means differ;",
                 "Kruskal\u2013Wallis is a rank-based non-parametric alternative.")
      ),
      card(
        card_header("Test result"),
        uiOutput("test_interpretation"),
        hr(),
        verbatimTextOutput("test_output")
      ),
      card(
        card_header("Post-hoc pairwise comparisons"),
        helpText("Tukey HSD for ANOVA, pairwise Wilcoxon (Bonferroni) for Kruskal\u2013Wallis."),
        verbatimTextOutput("posthoc_output")
      )
    )
  ),
  
  # ---- Tab 5: Data --------------------------------------------------------
  nav_panel(
    title = "Data",
    icon = icon("table"),
    card(
      card_header("County-level data"),
      DTOutput("data_table")
    )
  ),
  
  nav_spacer(),
  nav_item(
    tags$a("Data source: CDC / ATSDR SVI",
           href = "https://www.atsdr.cdc.gov/placeandhealth/svi/",
           target = "_blank",
           style = "color: white;")
  )
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Helper: apply state filter
  filter_state <- function(df, state_sel) {
    if (is.null(state_sel) || state_sel == "ALL") df
    else df %>% filter(state == state_sel)
  }
  
  # --- Explore tab ---------------------------------------------------------
  
  explore_data <- reactive({
    filter_state(hesitancy, input$explore_state) %>%
      filter(!is.na(.data[[input$explore_var]]))
  })
  
  explore_var_label <- reactive({
    names(hesitancy_var_choices)[hesitancy_var_choices == input$explore_var]
  })
  
  output$explore_plot <- renderPlot({
    df <- explore_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    var  <- input$explore_var
    xlab <- explore_var_label()
    
    p <- ggplot(df, aes(x = .data[[var]]))
    
    if (input$explore_plot == "hist") {
      p <- p + geom_histogram(bins = input$explore_bins,
                              fill = "#2c7bb6", color = "white", alpha = 0.9) +
        labs(x = xlab, y = "Number of counties")
    } else if (input$explore_plot == "density") {
      p <- p + geom_density(fill = "#2c7bb6", alpha = 0.5, color = "#2c3e50") +
        labs(x = xlab, y = "Density")
    } else {
      p <- ggplot(df, aes(x = svi_cat, y = .data[[var]], fill = svi_cat)) +
        geom_boxplot(outlier.alpha = 0.3) +
        scale_fill_manual(values = svi_palette, guide = "none") +
        labs(x = "SVI Category", y = xlab) +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
    }
    
    p + theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))
  })
  
  output$explore_summary <- renderPrint({
    df  <- explore_data()
    var <- input$explore_var
    if (nrow(df) == 0) { cat("No data."); return() }
    vals <- df[[var]]
    cat(sprintf("Measure : %s\n", explore_var_label()))
    cat(sprintf("N       : %d counties\n", length(vals)))
    cat(sprintf("Mean    : %.2f%%\n", mean(vals, na.rm = TRUE)))
    cat(sprintf("Median  : %.2f%%\n", median(vals, na.rm = TRUE)))
    cat(sprintf("SD      : %.2f\n",   sd(vals, na.rm = TRUE)))
    cat(sprintf("Min     : %.2f%%\n", min(vals, na.rm = TRUE)))
    cat(sprintf("Max     : %.2f%%\n", max(vals, na.rm = TRUE)))
  })
  
  output$explore_n <- renderPrint({
    df <- explore_data()
    cat(sprintf("State filter : %s\n",
                if (input$explore_state == "ALL") "All states" else input$explore_state))
    cat(sprintf("Counties     : %d\n", nrow(df)))
    cat(sprintf("States       : %d\n", length(unique(df$state))))
  })
  
  # --- Compare tab --------------------------------------------------------
  
  cmp_data <- reactive({
    validate(need(length(input$cmp_cats) >= 1, "Select at least one SVI category."))
    filter_state(hesitancy, input$cmp_state) %>%
      filter(svi_cat %in% input$cmp_cats,
             !is.na(.data[[input$cmp_var]]))
  })
  
  cmp_var_label <- reactive({
    names(hesitancy_var_choices)[hesitancy_var_choices == input$cmp_var]
  })
  
  output$cmp_title <- renderText({
    paste0(cmp_var_label(), " by SVI category",
           if (input$cmp_state != "ALL") paste0(" \u2014 ", input$cmp_state) else "")
  })
  
  output$cmp_plot <- renderPlot({
    df  <- cmp_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    var <- input$cmp_var
    
    base <- ggplot(df, aes(x = svi_cat, y = .data[[var]], fill = svi_cat)) +
      scale_fill_manual(values = svi_palette, guide = "none") +
      labs(x = "SVI Category", y = cmp_var_label()) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 15, hjust = 1))
    
    if (input$cmp_plot == "box") {
      base + geom_boxplot(outlier.alpha = 0.3, alpha = 0.9)
    } else if (input$cmp_plot == "violin") {
      base + geom_violin(alpha = 0.85, color = "gray30") +
        geom_boxplot(width = 0.15, fill = "white", outlier.shape = NA)
    } else {
      df_sum <- df %>%
        group_by(svi_cat) %>%
        summarise(
          m = mean(.data[[var]], na.rm = TRUE),
          se = sd(.data[[var]], na.rm = TRUE) / sqrt(n()),
          lo = m - 1.96 * se,
          hi = m + 1.96 * se,
          .groups = "drop"
        )
      ggplot(df_sum, aes(x = svi_cat, y = m, color = svi_cat)) +
        geom_point(size = 4) +
        geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, linewidth = 1) +
        scale_color_manual(values = svi_palette, guide = "none") +
        labs(x = "SVI Category", y = paste("Mean ", cmp_var_label())) +
        theme_minimal(base_size = 14) +
        theme(panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 15, hjust = 1))
    }
  })
  
  output$cmp_table <- renderTable({
    df <- cmp_data()
    var <- input$cmp_var
    df %>%
      group_by(`SVI Category` = svi_cat) %>%
      summarise(
        N      = n(),
        Mean   = round(mean(.data[[var]], na.rm = TRUE), 2),
        Median = round(median(.data[[var]], na.rm = TRUE), 2),
        SD     = round(sd(.data[[var]], na.rm = TRUE), 2),
        .groups = "drop"
      )
  }, striped = TRUE, hover = TRUE)
  
  # --- Statistical test tab -----------------------------------------------
  
  # eventReactive with ignoreNULL = FALSE fires once on startup AND on each
  # button click; downstream outputs read input$test_var/test_type/test_state
  # via isolate() so changing them does NOT auto-rerun the test.
  test_result <- eventReactive(input$run_test, {
    df  <- isolate(filter_state(hesitancy, input$test_state)) %>%
      filter(!is.na(.data[[isolate(input$test_var)]]), !is.na(svi_cat))
    var <- isolate(input$test_var)
    df_renamed <- df
    names(df_renamed)[names(df_renamed) == var] <- "y"
    
    if (isolate(input$test_type) == "anova") {
      fit <- aov(y ~ svi_cat, data = df_renamed)
      list(type = "anova", fit = fit,
           p = summary(fit)[[1]][["Pr(>F)"]][1],
           df = df_renamed, n = nrow(df_renamed))
    } else {
      kw <- kruskal.test(y ~ svi_cat, data = df_renamed)
      list(type = "kw", fit = kw, p = kw$p.value,
           df = df_renamed, n = nrow(df_renamed))
    }
  }, ignoreNULL = FALSE)
  
  output$test_output <- renderPrint({
    res <- test_result()
    if (res$type == "anova") print(summary(res$fit)) else print(res$fit)
  })
  
  output$test_interpretation <- renderUI({
    res <- test_result()
    p   <- res$p
    alpha <- input$test_alpha
    verdict <- if (p < alpha) {
      tags$span(class = "badge bg-success",
                sprintf("p = %.4g < \u03b1 = %.3f \u2014 reject H\u2080", p, alpha))
    } else {
      tags$span(class = "badge bg-secondary",
                sprintf("p = %.4g \u2265 \u03b1 = %.3f \u2014 fail to reject H\u2080", p, alpha))
    }
    
    test_name <- if (res$type == "anova") "one-way ANOVA" else "Kruskal\u2013Wallis test"
    concl <- if (p < alpha) {
      paste0("There is statistically significant evidence that mean ",
             "vaccine hesitancy differs across SVI categories.")
    } else {
      paste0("There is not enough evidence to conclude that vaccine ",
             "hesitancy differs across SVI categories at this \u03b1.")
    }
    
    tagList(
      p(strong("Test: "), test_name, " \u00b7 ",
        strong("N: "), res$n, " counties"),
      p(verdict),
      p(concl)
    )
  })
  
  output$posthoc_output <- renderPrint({
    res <- test_result()
    if (res$type == "anova") {
      print(TukeyHSD(res$fit))
    } else {
      pw <- pairwise.wilcox.test(res$df$y, res$df$svi_cat,
                                 p.adjust.method = "bonferroni")
      print(pw)
    }
  })
  
  # --- Data table tab -----------------------------------------------------
  
  output$data_table <- renderDT({
    hesitancy %>%
      select(
        FIPS = fips, County = county, State = state,
        `Est. hesitant (%)`           = est_hesitant,
        `Est. hesitant or unsure (%)` = est_hes_unsure,
        `Est. strongly hesitant (%)`  = est_strong_hes,
        SVI = svi, `SVI Category` = svi_cat,
        `% fully vaccinated`          = pct_vax
      ) %>%
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 15, scrollX = TRUE)
      ) %>%
      formatRound(columns = c("Est. hesitant (%)",
                              "Est. hesitant or unsure (%)",
                              "Est. strongly hesitant (%)",
                              "% fully vaccinated"),
                  digits = 2)
  })
}

shinyApp(ui, server)