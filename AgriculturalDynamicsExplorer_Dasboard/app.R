# title: "FIT5147 DVP Assignment"
# author: "Divya Harishchandra Shetty"
# Student ID: 33496366

# Required libraries for the Shiny app
# install.packages(c("shiny", "leaflet", "ggplot2", "dplyr", "plotly", "shinyjs", "rnaturalearth", "rnaturalearthdata", "sf", "htmltools"))
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyjs)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(htmltools)

# Load the crop yield dataset
data <- read.csv("merged_data.csv")

# Load government investment dataset
government_investment <- read.csv("merged_data_byinvestment.csv")

# Get country geographic data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge the datasets to include geographic information
data_geo <- merge(data, world, by.x = "Country", by.y = "name")

# Select relevant columns
data_geo <- data_geo %>% 
  select(Country, Year, Yield, geometry)

# Define the mapping of input values to more readable labels
climate_labels <- c(
  "Avg_Temperature" = "Temperature (degree Celsius)",
  "Avg_CO2_Emissions" = "CO2 Emissions",
  "Avg_Sea_Level_Rise" = "Sea Level Rise",
  "Avg_Precipitation" = "Precipitation (mm)",
  "Avg_Humidity" = "Humidity",
  "Avg_Wind_Speed" = "Wind Speed"
)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet"), 
    tags$script(HTML("
      Shiny.addCustomMessageHandler('scrollToCharts', function(message) {
        var element = document.getElementById('charts-section');
        if (element) {
          element.scrollIntoView({ behavior: 'smooth' });
        }
      });
    ")),
    tags$style(HTML("
      body {
        background-color: #DFF0D8; /* Light green background */
        margin: 0;
        padding: 0;
      }
      h1 {
        max-width: 100%; 
        margin: 2rem auto;
        text-align: center;
        font-family: 'Canela Web', serif;
        font-size: 60px;
        color: #403a30;
        font-weight: bold;
        line-height: 1.2;
      }
      .subtitle {
        max-width: 100rem;
        margin: 1rem auto;
        text-align: left;
        font-family: 'Canela Web', serif;
        font-size: 25px; /* Adjust font size as needed */
        font-weight: bold;
        padding: 10px;
        color: #403a30;
      }
     .description {
        max-width: 100rem;
        margin: 1rem auto;
        text-align: justify;
        font-family: 'Canela Web', serif;
        font-size: 20px;
        color: #403a30;
        padding: 10px;
        background-color: #DFF0D8; /* Light beige background for description */
        border-radius: 1px; /* Rounded corners */
      }
       .subdescription {
        max-width: 100rem;
        margin: 1rem auto;
        text-align: justify;
        font-family: 'Canela Web', serif;
        font-size: 20px;
        color: #403a30;
        padding: 10px;
        background-color: #DFF0D8; /* Light beige background for description */
      }
      .image-container {
        text-align: center;
        margin: 2rem auto;
      }
      .image-container img {
        max-width: 40%; 
        height: auto; /* Maintain aspect ratio */
        animation: dropIn 0.8s ease-out forwards; /* Apply drop-in animation */
      }
      @keyframes dropIn {
        0% {
          transform: translateY(-100vh); /* Start above the viewport */
          opacity: 0; /* Start invisible */
        }
        100% {
          transform: translateY(0); /* End at the original position */
          opacity: 1; /* End fully visible */
        }
      }
      .content-box {
          max-width: 100rem;
          margin: 2rem auto;
          padding: 20px;
          background-color: white; /* Match the main background color or make it transparent */
          border: 2px solid #027148; /* Add a colored border */
          border-radius: 10px; /* Rounded corners */
          box-shadow: 0 4px 8px rgba(0,0,0,0.1); /* Subtle shadow for depth */
        }
      .sidebar {
        background-color: #DFF0D8;
        padding: 20px;
        border-radius: 10px;
        box-shadow: none; /* Remove default shadow */
        border: none; /* Remove default border */
      }
      .well {
        background-color: #DFF0D8; /* Match the background color */
        border: none; /* Remove border */
        box-shadow: none; /* Remove shadow */
      }
      /* Custom cursor style for Plotly hover */
      .plotly .cursor-crosshair {
        cursor: default !important;
      }
      /* Animation for the hover info */
      @keyframes blink {
        0% { opacity: 1; }
        50% { opacity: 0; }
        100% { opacity: 1; }
      }
      .hover-info {
        background-color: #f0f0f0; /* Light grey background */
        border: 2px solid #027148; /* Border color */
        border-radius: 5px; /* Rounded corners */
        padding: 10px; /* Padding */
        display: flex; /* Flexbox layout */
        align-items: center; /* Center items vertically */
        justify-content: space-between; /* Space between items */
        width: fit-content; /* Fit content width */
        margin-top: 10px; /* Margin above */
        animation: blink 2s infinite;
      }
      .info-text {
        margin: 0; /* Remove margin */
        font-size: 14px; /* Font size */
      }
      .close-btn {
        background-color: #027148; /* Background color */
        color: #fff; /* Text color */
        border: none; /* No border */
        border-radius: 50%; /* Rounded corners */
        width: 15px; /* Width */
        height: 15px; /* Height */
        display: flex; /* Flexbox layout */
        align-items: center; /* Center items vertically */
        justify-content: center; /* Center items horizontally */
        cursor: pointer; /* Pointer cursor */
        font-size: 14px; /* Font size */
      }
      .center-content {
      display: flex;
      justify-content: center;
      align-items: center;
      flex-direction: column;
    }
    "))
  ),
  h1("Agricultural Dynamics Explorer"),
  div(class = "image-container",
      img(src = "newimage.png", alt = "Agricultural Image", style = "max-width: 65%; height:auto;") # Adjust max-width and height as needed
  ),
  div(class = "description", 
      "Welcome to the Agricultural Dynamics Explorer! Uncover the relationship between agriculture, climate, and economic resilience. By exploring these relationships, the goal is to answer critical questions such as: How do changes in temperature, precipitation, and other climate variables affect crop yields? and What role do economic factors play in supporting or hindering agricultural yield and agricultural contribution to economy?"),
  div(class = "description", 
      "The mission is to provide a comprehensive understanding of these dynamics to individuals who may not have extensive knowledge of agriculture or government investments, making complex data engaging and understandable."),
  div(class = "subtitle", 
      "Understanding the Impact of Climate on Crop Yield"),
  div(class = "subdescription",
      "Climate plays a crucial role in determining crop yield. Variations in temperature, precipitation, humidity, and other climate variables significantly influence the growth and productivity of crops. Different crops respond uniquely to these variables, with optimal conditions varying for each type. "),
  div(class = "subdescription",
      "Higher temperatures can enhance crop growth in some regions but may lead to heat stress and reduced yields in others. Precipitation patterns, including the amount and timing of rainfall, are critical for providing necessary water for crops, with both droughts and excessive rainfall posing risks. Humidity levels can affect the prevalence of pests and diseases, further influencing crop health and yields. "),
  div(class = "subdescription",
      HTML("<strong>Note:</strong> This analysis explores the relationship between climate variables and crop yields, the specific effects can vary widely based on the type of crop and soil conditions. These additional factors, which are not covered in this analysis also play a crucial role in shaping agricultural outcomes.")
  ),
  div(class = "subdescription",
      HTML("The <strong>size</strong> of each bubble on the scatter plot below represents the crop yield, with larger bubbles indicating higher yields.")),
  div(class = "content-box",
      sidebarLayout(
        sidebarPanel(
          div(class = "sidebar",
              selectInput("country", "Select Country", choices = unique(data$Country)),
              selectInput("climate", "Select Climate Variable", 
                          choices = c("Temperature" = "Avg_Temperature", 
                                      "CO2 Emissions" = "Avg_CO2_Emissions", 
                                      "Sea Level Rise" = "Avg_Sea_Level_Rise", 
                                      "Precipitation" = "Avg_Precipitation", 
                                      "Humidity" = "Avg_Humidity", 
                                      "Wind Speed" = "Avg_Wind_Speed"))
          ),
          div(id = "hover-info1", class = "hover-info",
              span("Hover over the bubbles for more information", class = "info-text"),
              actionButton("closeBtn1", "✖", class = "close-btn")
          ),
          div(id = "hover-info2", class = "hover-info",
              span("Click on the bubble to view the distribution of area harvested and production", class = "info-text"),
              actionButton("closeBtn2", "✖", class = "close-btn")
          )
        ),
        mainPanel(
          plotlyOutput("bubbleChart"),
          uiOutput("detailsChartUI")
        )
      )
  ),
  div(class = "subtitle", 
      "Crop Yield, Government Investment, and Economic Contribution"),
  div(class = "subdescription",
      "The visualizations presented aim to illustrate the relationships between crop yield, government investment, economic contribution and provide insights into how they collectively influence agricultural productivity and economic stability."),
  div(class = "subdescription",
      HTML(" The types of investments include:
      <ul>
        <li><b>Economic Contribution:</b> Net output of the agriculture sector, contributing significantly to the overall economy.</li>
        <li><b>Investment in Assets:</b> Investments in fixed assets like machinery and agricultural infrastructure</li>
        <li><b>Loans to Agriculture:</b> Loans and financial credits provided to agricultural sectors.</li>
        <li><b>Development Aid:</b> Development Financial Assistance provided by government or international organizations.</li>
        <li><b>Foreign Investments:</b> Foreign Direct Investment from international entities, fostering technology transfer and skill development.</li>
        <li><b>Research and Development:</b> Investments in research and development of agriculture, crops, and farming.</li>
      </ul>
    ")
  ),
  div(class = "center-content",
      div(id = "map-info", class = "hover-info",
          span("Click each country to discover detailed insights into government investments in agriculture.", class = "info-text"),
          actionButton("closeMapInfoBtn", "✖", class = "close-btn")
      ),
      leafletOutput("worldMap", height = 550, width = "67%"),
      div(id = "slider-info", class = "hover-info",
          span("Drag the slider below to select year.", class = "info-text"),
          actionButton("closeSliderInfoBtn", "✖", class = "close-btn")
      ),
      sliderInput("year", "Select Year:",
                  min = min(data$Year), max = max(data$Year),
                  value = min(data$Year), step = 1, sep = "",  width = "70%"),
      div(id = "charts-section", class ="content-box", style = "width: 80%;",
          plotlyOutput("investmentBarChart"),
          textOutput("cropYieldInfo")
      ),
      div(class = "subdescription",
          HTML("<strong>Note:</strong> Some investment charts may appear empty due to a lack of available data for those years.")
      ),
      div(class = "content-box",style = "width: 80%;",
          plotlyOutput("investmentTimeSeries")
      ),
      div(class = "subdescription",
          HTML("The above <strong>area chart</strong> presents the relationship between economic contribution by agriculture and government investments over time. This chart helps to visualize how the economic value generated by agriculture (through crop yields) correlates with the sum of various government investments.")
      )
  ),
  div(class = "subtitle", 
      "Conclusion"),
  div(class = "subdescription",
      "By exploring how climate variables and government investments influence crop yields, this tool aims to enhance understanding among these factors and analyze trends. Despite some limitations in data availability, the visualizations and analyses presented valuable perspectives. "),
  div(class = "subdescription",
      HTML("<strong>Data Source:</strong> <a href='https://www.fao.org/faostat/en/#data' target='_blank'>https://www.fao.org/faostat/en/#data</a>")
  ),
  div(class = "subdescription",style = "font-size: 15px;",
      "By Divya Harishchandra Shetty (33496366)")
)

# Define server logic
server <- function(input, output, session) {
  filteredData <- reactive({
    data %>%
      filter(Country == input$country) %>%
      select(Year, Yield, ClimateValue = !!sym(input$climate), Production, Area.harvested)
  })
  
  output$bubbleChart <- renderPlotly({
    plot_ly(
      data = filteredData(),
      x = ~Year,
      y = ~ClimateValue,
      size = ~Yield,
      color = ~Yield,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        opacity = 0.7, 
        line = list(width = 2.5, color = 'rgba(0,0,0,0.5)'),
        sizeref = 0.3  # Adjust this value to increase or decrease the size of the bubbles
      ),
      text = ~paste('Year:', Year,
                    '<br>Area Harvested:', Area.harvested,'ha',
                    '<br>Production:', Production,'t',
                    '<br>Yield:', Yield, '100g/ha'),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = paste("Impact of", climate_labels[input$climate], "on Crop Yield in", input$country),
        xaxis = list(title = "Year"),
        yaxis = list(title = climate_labels[input$climate]),
        colorbar = list(title = "Yield"),
        dragmode = FALSE  # Disable drag mode
      ) %>%
      config(
        displayModeBar = TRUE,  # Enable the mode bar
        modeBarButtonsToRemove = list(
          'sendDataToCloud',  # Remove unnecessary buttons
          'toggleSpikelines',
          'autoScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'zoom2d',
          'pan2d',
          'select2d',
          'lasso2d',
          'resetScale2d',
          'toImage'
        ),
        displaylogo = FALSE
      )
  })
  
  
  output$areaChart <- renderPlotly({
    plot_ly(
      data = filteredData(),
      x = ~Year,
      y = ~Area.harvested,
      type = 'scatter',
      mode = 'none',
      fill = 'tozeroy',
      name = 'Area Harvested',
      fillcolor = 'rgba(51, 153, 255, 0.9)'  # Adjust opacity here
    ) %>%
      add_trace(
        y = ~Production,
        fill = 'tozeroy',
        name = 'Production',
        fillcolor = 'rgba(255, 153, 51, 0.5)'  # Adjust opacity here
      ) %>%
      layout(
        title = paste("Area Harvested and Production over Time in", input$country),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Value"),
        showlegend = TRUE
      ) %>%
      config(
        displayModeBar = FALSE  # disable the mode bar
      )
  })
  
  observeEvent(event_data("plotly_click"), {
    showModal(modalDialog(
      title = "Distribution of Area harvested and Production",
      tags$div(
        plotlyOutput("areaChart"),
        div(id = "hover-info3", class = "hover-info",
            span("You can click on the legend to show or hide different parts of the chart.", class = "info-text"),
            actionButton("closeBtn3", "✖", class = "close-btn")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  observeEvent(input$closeBtn1, {
    shinyjs::hide(id = "hover-info1")
  })
  observeEvent(input$closeBtn2, {
    shinyjs::hide(id = "hover-info2")
  })
  observeEvent(input$closeBtn3, {
    shinyjs::hide(id = "hover-info3")
  })
  observeEvent(input$closeMapInfoBtn, {
    shinyjs::hide(id = "map-info")
  })
  observeEvent(input$closeSliderInfoBtn, {
    shinyjs::hide(id = "slider-info")
  })
  
  # Set default values
  default_country <- "Australia"
  default_year <- 2001
  
  # Update the default values for the input selectors
  updateSelectInput(session, "country", selected = default_country)
  updateSliderInput(session, "year", value = default_year)
  
  # Reactive data filtered by year
  data_year <- reactive({
    data %>% filter(Year == input$year)
  })
  
  # Render leaflet map initially
  output$worldMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 133.7751, lat = -25.2744, zoom = 4) # Centered on Australia
  })
  
  selectedCountry <- reactiveVal()
  
  observe({
    year_selected <- input$year
    crop_yield_year <- data %>% filter(Year == year_selected)
    data_geo <- merge(world, crop_yield_year, by.x = "name", by.y = "Country")
    
    leafletProxy("worldMap", data = data_geo) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~colorBin("YlOrRd", domain = data_geo$Yield, bins = 6)(Yield),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.5,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~lapply(paste("Country:", name, "<br>Yield:", Yield), htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = ~name
      ) %>%
      addLegend(pal = colorBin("YlOrRd", domain = data_geo$Yield, bins = 6), values = ~Yield, opacity = 0.7, title = "Crop Yield", position = "bottomright")
  })
  output$worldMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  # Reactive values to store selected country
  selectedCountry <- reactiveVal(default_country)
  
  # Update selected country on map click
  observeEvent(input$worldMap_shape_click, {
    selectedCountry(input$worldMap_shape_click$id)
  })
  
  # Reactive data for the selected country and year for investments
  filtered_investment_data <- reactive({
    req(selectedCountry())
    req(input$year)
    
    government_investment %>%
      filter(Country == selectedCountry(), Year == input$year) %>%
      select(Item, Value)
  })
  
  observeEvent(input$worldMap_shape_click, {
    selectedCountry(input$worldMap_shape_click$id)
    session$sendCustomMessage(type = 'scrollToCharts', message = list())
  })
  
  
  output$investmentBarChart <- renderPlotly({
    req(selectedCountry())
    req(input$year)
    
    selected_data <- government_investment %>%
      filter(Country == selectedCountry(), Year == input$year, Item != "Economic Contribution") %>%
      select(Item, Value)
    
    
    plot_ly(
      selected_data, 
      x = ~Item, 
      y = ~Value, 
      type = 'bar', 
      name = 'Investment', 
      marker = list(color = '#027148'),
      text = ~paste(
        'Investment Type:', Item,
        '<br>Value:', format(Value, big.mark = ",", scientific = FALSE), 'USD'
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = paste("Government Investment in", selectedCountry(), "for the Year", input$year),
        xaxis = list(title = "Investment Type"),
        yaxis = list(title = "Value in million USD"),
        barmode = 'group'
      ) %>%
      config(
        displayModeBar = FALSE  # disable the mode bar
      )
  })
  
  
  # Reactive text for crop yield information
  output$cropYieldInfo <- renderText({
    req(selectedCountry())
    req(input$year)
    
    crop_yield <- data %>%
      filter(Country == selectedCountry(), Year == input$year) %>%
      select(Yield) %>%
      .$Yield
    
    paste("The crop yield for", selectedCountry(), "in the year", input$year, "is", crop_yield, "100g/ha.")
  })
  
  
  # Additional chart showing Economic Contribution and Sum of Other Investments
  output$investmentTimeSeries <- renderPlotly({
    req(selectedCountry())
    
    # Summarize data to get Economic Contribution and Sum of Other Investments
    summarized_data <- government_investment %>%
      filter(Country == selectedCountry()) %>%
      group_by(Year) %>%
      summarize(
        Economic_Contribution = sum(Value[Item == "Economic Contribution"]),
        Other_Investments = sum(Value[Item != "Economic Contribution"])
      )
    
    plot_ly(summarized_data, x = ~Year) %>%
      add_trace(y = ~Economic_Contribution, name = 'Economic Contribution', type = 'scatter', mode = 'lines', fill = 'tozeroy', line = list(color = 'blue')) %>%
      add_trace(y = ~Other_Investments, name = 'Agricultural Investments', type = 'scatter', mode = 'lines', fill = 'tozeroy', line = list(color = 'red')) %>%
      layout(
        title = paste("Distribution of Government investment and the Economic contribution of Agriculture in", selectedCountry()),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Value in million USD")
      )%>%
      config(
        displayModeBar = FALSE  # disable the mode bar
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)