#
# This is the server logic of a Shiny web application. You can run the
# application by clicking "Run App" above.

suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(htmltools)
  library(sf)
  library(stringr)
  library(shinythemes)
  library(shinyjs)  # For JavaScript operations
  library(lubridate)  # Add this for year() function
  library(memoise)  # Added for caching
})

# Source page UIs
source("about_page.R")
source("forecasts_page.R")
source("prediction_page.R")
source("functions.R")
source("load_data.R")

# Define valid credentials
valid_credentials <- list(
  user1 = "pass1",
  user2 = "pass2"
)

# Add near the top of the file
options(shiny.maxRequestSize = 30*1024^2)  # Increase max request size to 30MB
options(shiny.reactlog = FALSE)  # Disable reactive log in production
options(shiny.autoreload = FALSE)  # Disable auto-reload in production

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  theme = shinytheme("readable"),
  tags$head(
    tags$style(HTML("
      #loading-content {
        position: absolute;
        background: #000000;
        opacity: 0.9;
        z-index: 100;
        left: 0;
        right: 0;
        height: 100%;
        text-align: center;
        color: #FFFFFF;
      }
    "))
  ),
  div(id = "loading-content", "Loading..."),
  # Navbar to each page
  navbarPage(
    "Everglades Wading Birds",
    tabPanel("Species Detection", uiOutput("predicted")),
    tabPanel("Species Forecasts", uiOutput("forecasts")),
    tabPanel("About", uiOutput("about")),
    tabPanel("Current Season",  # Changed from "Current Data"
      uiOutput("login_or_data")  # This will show either login form or data
    )
  )
)

server <- function(input, output, session) {
  # Get current year
  current_year <- reactive({
    format(Sys.Date(), "%Y")
  })

  # Authentication state
  is_authenticated <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  
  # Initialize colonies data
  colonies_data <- reactive({
    # Get the raw colonies data
    data <- colonies  # directly use the colonies data frame
    
    # Ensure coordinates are numeric and in the correct format
    if (inherits(data, "sf")) {
      coords <- st_coordinates(data)
      data$longitude <- coords[,1]
      data$latitude <- coords[,2]
    }
    data
  })
  
  # Login UI
  output$login_ui <- renderUI({
    if (!is_authenticated()) {
      div(
        style = "max-width: 300px; margin: 0 auto; padding: 20px;",
        h3("Login Required"),
        p("Please log in to view current season information."),
        textInput("username", "Username"),
        passwordInput("password", "Password"),
        actionButton("login", "Login", class = "btn-primary"),
        tags$br(),
        tags$br(),
        textOutput("login_message")
      )
    }
  })
  
  # Login message
  output$login_message <- renderText({
    if (!is.null(input$login) && input$login > 0) {
      if (!is_authenticated()) {
        "Invalid username or password"
      }
    }
  })
  
  # Handle login
  observeEvent(input$login, {
    if (!is.null(input$username) && !is.null(input$password)) {
      if (!is.null(valid_credentials[[input$username]]) &&
          valid_credentials[[input$username]] == input$password) {
        is_authenticated(TRUE)
        current_user(input$username)
      }
    }
  })
  
  # Render either login form or protected content
  output$login_or_data <- renderUI({
    req(colonies_data())
    if (!is_authenticated()) {
      uiOutput("login_ui")
    } else {
      fluidRow(
        column(12,
          div(
            style = "float: right;",
            actionButton("logout", "Logout", class = "btn-danger")
          ),
          h3(paste("Current Season Analysis (", current_year(), ")")),
          fluidRow(
            column(4,
              h4("Select Location"),
              selectInput("current_season_site", "Select Site",
                         choices = c("All", unique(colonies_data()$site)),
                         selected = "All"),
              leafletOutput("current_season_map", height = "400px"),
              uiOutput("current_season_date_slider")
            ),
            column(8,
              h4("Detected Birds"),
              plotOutput("current_season_plot"),
              div(
                style = "text-align: center; color: #666; margin-top: 20px;",
                textOutput("no_data_message")
              )
            )
          )
        )
      )
    }
  })
  
  # Handle logout
  observeEvent(input$logout, {
    is_authenticated(FALSE)
    current_user(NULL)
  })
  
  # Filter for current season data (current year)
  filtered_site_data <- reactive({
    req(is_authenticated())
    req(input$current_season_site)
    
    tryCatch({
      data <- df %>% 
        filter(lubridate::year(as.Date(event)) == current_year())
      
      if (input$current_season_site != "All") {
        data <- data %>% filter(site == input$current_season_site)
      }
      data
    }, error = function(e) {
      # Return empty dataframe with same structure as df
      df[0,]
    })
  })

  # Current Season Map
  output$current_season_map <- renderLeaflet({
    req(is_authenticated())
    data <- isolate(colonies_data())
    
    leaflet(data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~site,
        label = ~site,
        layerId = ~site
      ) %>%
      setView(
        lng = mean(data$longitude, na.rm = TRUE),
        lat = mean(data$latitude, na.rm = TRUE),
        zoom = 8
      )
  })

  # Date slider for current season
  output$current_season_date_slider <- renderUI({
    req(is_authenticated())
    
    tryCatch({
      # Get available dates for current year
      available_dates <- df %>%
        filter(lubridate::year(as.Date(event)) == current_year()) %>%
        pull(event) %>%
        unique() %>%
        sort()
      
      if (length(available_dates) > 0) {
        sliderTextInput(
          inputId = "current_season_date",
          label = "Select Date",
          choices = available_dates,
          selected = available_dates[1]
        )
      } else {
        div(
          style = "padding: 10px; margin-top: 10px; background-color: #f8f9fa; border-radius: 4px;",
          paste("No data available for", current_year(), "yet")
        )
      }
    }, error = function(e) {
      div(
        style = "padding: 10px; margin-top: 10px; background-color: #f8f9fa; border-radius: 4px;",
        paste("No data available for", current_year(), "yet")
      )
    })
  })

  # Plot for current season
  output$current_season_plot <- renderPlot({
    req(is_authenticated())
    
    data <- filtered_site_data()
    
    if (nrow(data) > 0) {
      tryCatch({
        time_predictions(
          data,
          site_name = input$current_season_site,
          selected_species = "All",
          selected_event = input$current_season_date
        )
      }, error = function(e) {
        # Create an empty plot with a message if there's an error
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = paste("No detection data available for", current_year(), "season yet"),
                  size = 6) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      })
    } else {
      # Create an empty plot with a message
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                label = paste("No detection data available for", current_year(), "season yet"),
                size = 6) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    }
  })

  # Message when no data is available
  output$no_data_message <- renderText({
    req(is_authenticated())
    
    if (nrow(filtered_site_data()) == 0) {
      paste("No bird detection data is available yet for the", current_year(), 
            "season. Please check back later for updates on current season observations.")
    }
  })

  # Update map markers
  observe({
    req(is_authenticated())
    req(input$current_season_site)
    data <- colonies_data()
    
    selected_site <- input$current_season_site
    
    if (selected_site != "All") {
      site_data <- data %>% 
        filter(site == selected_site)
      
      leafletProxy("current_season_map") %>%
        clearMarkers() %>%
        addMarkers(
          data = site_data,
          lng = ~longitude,
          lat = ~latitude,
          popup = ~site,
          label = ~site,
          layerId = ~site
        )
    } else {
      leafletProxy("current_season_map") %>%
        clearMarkers() %>%
        addMarkers(
          data = data,
          lng = ~longitude,
          lat = ~latitude,
          popup = ~site,
          label = ~site,
          layerId = ~site
        )
    }
  })

  # Add an input handler for map clicks
  observeEvent(input$current_season_map_marker_click, {
    click <- input$current_season_map_marker_click
    if (!is.null(click)) {
      # Update the selected site based on map click
      updateSelectInput(session, "current_season_site",
                       selected = click$id)
    }
  })

  output$zooniverse_anotation <- renderPlot(zooniverse_complete())

  # Set mapbox key
  if (file.exists("source_token.txt"))
    readRenviron("source_token.txt")
  MAPBOX_ACCESS_TOKEN = Sys.getenv("MAPBOX_ACCESS_TOKEN")
  if (is.na(MAPBOX_ACCESS_TOKEN) || MAPBOX_ACCESS_TOKEN == "")
    paste("Set MAPBOX ACCESS TOKEN,", "Refer to the README.")

  # Create pages
  output$about <- about_page()
  output$predicted <- predicted_page(df)
  output$forecasts <- forecasts_page()

  #### Sidebar Map###
  output$map <- renderLeaflet({
    data <- colonies_data()
    
    leaflet(data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~site,
        label = ~site,
        layerId = ~site
      ) %>%
      setView(
        lng = mean(data$longitude, na.rm = TRUE),
        lat = mean(data$latitude, na.rm = TRUE),
        zoom = 8
      )
  })

  site_name_filter <- reactive({
    return(as.character(input$prediction_site))
  })

  species_name_filter <- reactive({
    if ("All" %in% input$prediction_species) {
      return("All")
    } else {
      return(input$prediction_species)
    }
  })

  map_filter <- reactive({
    if (is.null(input$prediction_site)) {
      return(colonies_data())
    }
    map_data <- colonies_data() %>% filter(site == input$prediction_site)
    return(map_data)
  })

  ## Prediction panel ##
  prediction_filter <- reactive({
    if (is.null(input$mapbox_date)) {
      mapbox_date <- "2020-02-24"
    } else {
      mapbox_date <- input$mapbox_date
    }

    # filter based on selection
    print(paste("mapbox date is:", mapbox_date))
    print(paste("selected site is:", site_name_filter()))

    selected_species <- species_name_filter()
    if ("All" %in% selected_species) {
      to_plot <-
        df %>% filter(site == site_name_filter(), event == mapbox_date)
    } else {
      to_plot <-
        df %>% filter(
          site == site_name_filter(),
          event == mapbox_date,
          label %in% species_name_filter()
        )
    }
    return(to_plot)
  })

  output$date_slider <- renderUI({
    selected_site <- site_name_filter()
    selected_df <- df %>% filter(site == selected_site)
    available_dates <- sort(unique(selected_df$event))

    # Check if the selected date is in the available dates
    selected_date <- input$mapbox_date
    if (!is.null(selected_date) && !(selected_date %in% available_dates)) {
      # If the selected date is not in the available dates,
      # set it to the first available date
      selected_date <- available_dates[1]
    }

    # Check if the selected site is "All", don't render the slider
    if (selected_site == "All") {
      return(NULL)
    } else {
      # Otherwise, render the slider
      sliderTextInput(
        inputId = "mapbox_date",
        label = "Select Date",
        choices = available_dates,
        selected = selected_date
      )
    }
  })

  output$predicted_time_plot <-
    renderPlot(
      time_predictions(
        df,
        site_name_filter(),
        selected_species = species_name_filter(),
        selected_event = input$mapbox_date
      )
    )

  output$sample_prediction_map <-
    renderLeaflet(plot_predictions(df = prediction_filter(), MAPBOX_ACCESS_TOKEN))

  output$pred_obs_Image <- renderImage({
    filename <- normalizePath(file.path(
      "./forecasts",
      paste0("nb_origin_", input$forecast_origin, ".png")
    ))
    # Return a list containing the filename and alt text
    list(
      src = filename,
      alt = paste("Observed as a function of predicted for ", input$origin)
    )
  }, deleteFile = FALSE)

  output$greg_Image <- renderImage({
    filename <- normalizePath(file.path(
      "./forecasts",
      paste0("greg_nb_origin_", input$forecast_origin, ".png")
    ))
    # Return a list containing the filename and alt text
    list(
      src = filename,
      alt = paste("Time series for GREG since ",  input$forecast_origin)
    )
  }, deleteFile = FALSE)

  output$wost_Image <- renderImage({
    filename <- normalizePath(file.path(
      "./forecasts",
      paste0("wost_nb_origin_", input$forecast_origin, ".png")
    ))
    # Return a list containing the filename and alt text
    list(
      src = filename,
      alt = paste("Time series for WOST since ",  input$forecast_origin)
    )
  }, deleteFile = FALSE)

  output$whib_Image <- renderImage({
    filename <- normalizePath(file.path(
      "./forecasts",
      paste0("whib_nb_origin_", input$forecast_origin, ".png")
    ))
    # Return a list containing the filename and alt text
    list(
      src = filename,
      alt = paste("Time series for WHIB since ",  input$forecast_origin)
    )
  }, deleteFile = FALSE)

  output$greg_title <- renderText({
    "GREG Counts"
  })

  output$wost_title <- renderText({
    "WOST Counts"
  })

  output$whib_title <- renderText({
    "WHIB Counts"
  })

  output$pred_obs_title <- renderText({
    "Observed vs. Predicted Counts"
  })

  # Hide loading message on app start
  observe({
    shinyjs::hide("loading-content")
  })
  
  # Show/hide loading message during map updates
  observeEvent(input$current_season_site, {
    shinyjs::show("loading-content")
    shinyjs::delay(300, shinyjs::hide("loading-content"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
