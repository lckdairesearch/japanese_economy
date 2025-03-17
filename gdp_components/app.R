library(dplyr)
library(plotly)
library(shiny)

# Read pre-aggregated CSV data (one row per country & GDP component for 2023)
gdp_2023 <- read.csv('gdp_2023.csv', header = TRUE, stringsAsFactors = FALSE)

# Convert Import values to negative
gdp_2023 <- gdp_2023 %>%
  mutate(
    Value = ifelse(GDPComponent == "Import", -Value, Value),
    ValueTrillion = ifelse(GDPComponent == "Import", -ValueTrillion, ValueTrillion)
  )

# Define flag emoji mapping for countries
flag_emoji <- c(
  "United Kingdom" = "\U0001F1EC\U0001F1E7",  
  "Netherlands"     = "\U0001F1F3\U0001F1F1",  
  "Canada"          = "\U0001F1E8\U0001F1E6",  
  "Spain"           = "\U0001F1EA\U0001F1F8",  
  "Italy"           = "\U0001F1EE\U0001F1F9",  
  "Israel"          = "\U0001F1EE\U0001F1F1",  
  "Finland"         = "\U0001F1EB\U0001F1EE",  
  "Chile"           = "\U0001F1E8\U0001F1F1",  
  "Colombia"        = "\U0001F1E8\U0001F1F4",  
  "United States"   = "\U0001F1FA\U0001F1F8",  
  "Sweden"          = "\U0001F1F8\U0001F1EA",  
  "Czechia"         = "\U0001F1E8\U0001F1FF",  
  "Türkiye"         = "\U0001F1F9\U0001F1F7",  
  "New Zealand"     = "\U0001F1F3\U0001F1FF",  
  "Mexico"          = "\U0001F1F2\U0001F1FD",  
  "Switzerland"     = "\U0001F1E8\U0001F1ED",  
  "Iceland"         = "\U0001F1EE\U0001F1F8",  
  "Denmark"         = "\U0001F1E9\U0001F1F0",  
  "Germany"         = "\U0001F1E9\U0001F1EA",  
  "France"          = "\U0001F1EB\U0001F1F7",  
  "Estonia"         = "\U0001F1EA\U0001F1EA",  
  "Lithuania"       = "\U0001F1F1\U0001F1F9",  
  "Slovak Republic" = "\U0001F1F8\U0001F1F0",  
  "Australia"       = "\U0001F1E6\U0001F1FA",  
  "Korea"           = "\U0001F1F0\U0001F1F7",  
  "Slovenia"        = "\U0001F1F8\U0001F1EE",  
  "Latvia"          = "\U0001F1F1\U0001F1FB",  
  "Hungary"         = "\U0001F1ED\U0001F1FA",  
  "Belgium"         = "\U0001F1E7\U0001F1EA",  
  "Austria"         = "\U0001F1E6\U0001F1F9",  
  "Norway"          = "\U0001F1F3\U0001F1F4",  
  "Luxembourg"      = "\U0001F1F1\U0001F1FA",  
  "Croatia"         = "\U0001F1ED\U0001F1F7",  
  "Greece"          = "\U0001F1EC\U0001F1F7",  
  "Ireland"         = "\U0001F1EE\U0001F1EA",  
  "Japan"           = "\U0001F1EF\U0001F1F5"   
)

# Define base colors for GDP components
base_gdp_colors <- c(
  "Investment" = "#1F77B4", 
  "GovernmentExpenditure" = "#FF7F0E", 
  "Consumption" = "#2CA02C", 
  "Import" = "#D62728", 
  "Export" = "#9467BD"
)

ui <- fluidPage(
  titlePanel("GDP Composition & OECD Ranking by GDP Component"),
  fluidRow(
    column(6,
           selectInput("selectedCountry", "Select Country", 
                       choices = sort(unique(gdp_2023$Country)), selected = "Japan"),
           plotlyOutput("pieChart"),
           br(),
           div("Click on a slice to see how that component compares with other OECD countries",
               style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px; text-align:center; font-size:16px;")
    ),
    column(6,
           radioButtons("selectedMetric", "Select Metric:",
                        choices = c("% of GDP", "USD Trillion"),
                        selected = "% of GDP", inline = TRUE),
           plotlyOutput("barChart", height = "600px"),
           br(),
           div("Click on a bar to see the country's GDP composition",
               style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px; text-align:center; font-size:16px;")
    )
  )
)

server <- function(input, output, session) {
  
  # Compute data for the selected country's GDP composition for 2023.
  # Order GDPComponent as: Consumption, Investment, GovernmentExpenditure, Export, Import.
  country_data <- reactive({
    req(input$selectedCountry)
    desired_order <- c("Consumption", "Investment", "GovernmentExpenditure", "Export", "Import")
    d <- gdp_2023 %>%
      filter(Country == input$selectedCountry, Year == 2023)
    d$GDPComponent <- factor(d$GDPComponent, levels = desired_order)
    d <- d[order(d$GDPComponent), ]
    # For pie chart, use absolute value for slice sizes (so Import is positive).
    d <- d %>% mutate(PlotValue = ifelse(GDPComponent == "Import", abs(Value), Value))
    # Compute real total as (C+I+G+EX-IM) using signed Value.
    real_total <- sum(d$Value)
    d <- d %>% mutate(RealPercent = (Value / real_total) * 100)
    list(data = d, real_total = real_total)
  })
  
  # Maintain selected GDP component; default to "Consumption" if available.
  selected_component <- reactiveVal(NULL)
  observe({
    d_res <- country_data()
    d <- d_res$data
    if(nrow(d) > 0 && is.null(selected_component())) {
      if("Consumption" %in% as.character(d$GDPComponent)) {
        selected_component("Consumption")
      } else {
        selected_component(as.character(d$GDPComponent[1]))
      }
    }
  })
  
  # Render the pie chart.
  output$pieChart <- renderPlotly({
    res <- country_data()
    d <- res$data
    if(nrow(d) == 0) return(NULL)
    
    # Create pie labels: wrap "Import" in parentheses and append percentage.
    pie_labels <- ifelse(as.character(d$GDPComponent) == "Import", 
                         paste0("(", d$GDPComponent, ")"), 
                         as.character(d$GDPComponent))
    pie_labels <- paste0(pie_labels, "\n", round(d$RealPercent, 1), "%")
    
    custom_dat <- d$ValueTrillion  # Hover custom data
    pull_vector <- ifelse(as.character(d$GDPComponent) == selected_component(), 0.1, 0)
    line_colors <- ifelse(as.character(d$GDPComponent) == selected_component(), base_gdp_colors[as.character(d$GDPComponent)], "transparent")
    line_widths <- ifelse(as.character(d$GDPComponent) == selected_component(), 2, 0)
    
    p <- plot_ly(
      data = d,
      labels = pie_labels,
      values = ~PlotValue,
      customdata = custom_dat,
      type = 'pie',
      direction = "clockwise",
      textposition = 'inside',
      textinfo = 'label',
      hovertemplate = "%{label}<br>%{customdata:.2f} Trillion USD<extra></extra>",
      marker = list(
        colors = adjustcolor(base_gdp_colors[as.character(d$GDPComponent)], alpha.f = 0.5),
        line = list(color = line_colors, width = line_widths)
      ),
      pull = pull_vector,
      source = "pieChart",
      sort = FALSE,  # Disable automatic sorting
      showlegend = FALSE
    )
    p <- event_register(p, 'plotly_click')
    p %>% layout(
      title = paste(input$selectedCountry, "GDP Composition (2023)"),
      margin = list(t = 50),
      dragmode = FALSE
    )
  })
  
  # Update selected component when a pie slice is clicked.
  observeEvent(event_data("plotly_click", source = "pieChart"), {
    click <- event_data("plotly_click", source = "pieChart")
    if (!is.null(click)) {
      idx <- click$pointNumber + 1
      comp <- as.character(country_data()$data$GDPComponent[idx])
      selected_component(comp)
    }
  })
  
  # Render the bar chart for OECD ranking.
  output$barChart <- renderPlotly({
    comp <- selected_component()
    req(comp)
    
    # Compute total GDP per country (for 2023)
    total_gdp <- gdp_2023 %>%
      filter(Year == 2023) %>%
      group_by(Country) %>%
      summarize(TotalGDP = sum(Value, na.rm = TRUE)) %>%
      ungroup()
    
    # Filter for the selected component.
    comp_data <- gdp_2023 %>%
      filter(GDPComponent == comp, Year == 2023)
    
    # Merge with total GDP and compute percentage contribution.
    oecd_data <- left_join(comp_data, total_gdp, by = "Country") %>%
      mutate(Percentage = (Value / TotalGDP) * 100) %>%
      # { if(comp == "Import") arrange(., Percentage) else arrange(., desc(Percentage)) }
      arrange(desc(Percentage))
    
    # Determine metric from radio buttons.
    metric <- input$selectedMetric
    if(metric == "% of GDP") {
      x_vals <- oecd_data$Percentage
      x_title <- "Percentage of GDP (%)"
      hover_template <- "%{y}: %{x:.2f}% of GDP<br>%{customdata:.2f} Trillion USD<extra></extra>"
      custom_data <- oecd_data$ValueTrillion
    } else {
      x_vals <- oecd_data$ValueTrillion
      x_title <- "USD Trillion"
      hover_template <- "%{y}: %{x:.2f} Trillion USD<br>%{customdata:.2f}% of GDP<extra></extra>"
      custom_data <- oecd_data$Percentage
    }
    
    selected_ctry <- input$selectedCountry
    bar_colors <- sapply(oecd_data$Country, function(ctry) {
      if(ctry == selected_ctry) {
        adjustcolor(base_gdp_colors[comp], alpha.f = 1)
      } else {
        adjustcolor(base_gdp_colors[comp], alpha.f = 0.1)
      }
    })
    
    oecd_data <- oecd_data %>%
      mutate(CountryLabel = ifelse(Country %in% names(flag_emoji),
                                   paste0(Country, " ", flag_emoji[Country]),
                                   Country))
    
    p <- plot_ly(
      data = oecd_data,
      x = x_vals,
      y = ~(if(comp == "Import") reorder(CountryLabel, -x_vals) else reorder(CountryLabel, x_vals)),
      type = 'bar',
      orientation = 'h',
      marker = list(color = bar_colors),
      customdata = custom_data,
      hovertemplate = hover_template,
      source = "barChart"
    )
    p <- event_register(p, 'plotly_click')
    p %>% layout(
      title = paste("OECD Ranking: \n", comp, ifelse(metric == "% of GDP", "(as % of GDP)", " (in Trillion USD)")),
      xaxis = list(title = x_title, fixedrange = TRUE),
      yaxis = list(title = "Country", tickfont = list(size = 10), fixedrange = TRUE),
      margin = list(t = 50)
    )
  })
  
  # When a bar is clicked, update the selected country (preserving the selected component).
  observeEvent(event_data("plotly_click", source = "barChart"), {
    click <- event_data("plotly_click", source = "barChart")
    if (!is.null(click)) {
      clicked_label <- click$y
      new_country <- strsplit(clicked_label, " ")[[1]][1]
      updateSelectInput(session, "selectedCountry", selected = new_country)
    }
  })
}

shinyApp(ui, server)