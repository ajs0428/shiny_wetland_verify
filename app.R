# Wetland Patch Verification App
# Validates deep learning training data for wetland classification

library(shiny)
library(leaflet)
library(terra)
library(sf)
library(stringr)

# --- Configuration ---
PATCHES_DIR <- "Data/R_Patches_Labels"
REVIEW_LOG_DIR <- "Data"

# MOD_CLASS color palette
CLASS_COLORS <- c(
  "0" = "#00FFFF",  # EMW (Emergent) - Cyan
  "1" = "#006400",  # FSW (Forested) - Dark Green
  "2" = "#0000FF",  # OWW (Open Water) - Blue
  "3" = "#FFA500",  # SSW (Shrub-Scrub) - Orange
  "4" = "#808080"   # UPL (Upland) - Gray
)

CLASS_LABELS <- c(
  "0" = "EMW (Emergent Wetland)",
  "1" = "FSW (Forested Wetland)",
  "2" = "OWW (Open Water Wetland)",
  "3" = "SSW (Shrub-Scrub Wetland)",
  "4" = "UPL (Upland)"
)

# --- Helper Functions ---

#' Parse patch filename to extract metadata
parse_patch_filename <- function(filename) {
  # Pattern: cluster_<NUM>_huc_<CODE>_patch_<NUM>.tif
  pattern <- "cluster_(\\d+)_huc_(\\d+)_patch_(\\d+)\\.tif"
  matches <- regmatches(filename, regexec(pattern, filename))[[1]]
  if (length(matches) == 4) {
    list(
      file = filename,
      name = str_remove(filename, "labels_only_"),
      cluster = as.integer(matches[2]),
      huc = matches[3],
      patch_num = as.integer(matches[4])
    )
  } else {
    NULL
  }
}

#' Scan patches directory and build metadata dataframe
scan_patches <- function(dir_path) {
  files <- list.files(dir_path, pattern = "\\.tif$", full.names = FALSE)

  patch_list <- lapply(files, parse_patch_filename)
  patch_list <- Filter(Negate(is.null), patch_list)

  if (length(patch_list) == 0) {
    return(data.frame(
      file = character(),
      name = character(),
      cluster = integer(),
      huc = character(),
      patch_num = integer(),
      stringsAsFactors = FALSE
    ))
  }

  df <- do.call(rbind, lapply(patch_list, as.data.frame, stringsAsFactors = FALSE))
  df <- df[order(df$cluster, df$huc, df$patch_num), ]
  rownames(df) <- NULL
  df
}

#' Load review log or create empty one
load_review_log <- function(path) {
  if (file.exists(path)) {
    df <- read.csv(path, stringsAsFactors = FALSE)
    if (!"comment" %in% names(df)) {
      df$comment <- ""
    }
    if (!"reviewer" %in% names(df)) {
      df$reviewer <- ""
    }
    df
  } else {
    data.frame(
      patch_file = character(),
      cluster = integer(),
      huc = character(),
      patch_num = integer(),
      status = character(),
      comment = character(),
      reviewer = character(),
      timestamp = character(),
      stringsAsFactors = FALSE
    )
  }
}

#' Save review log to CSV
save_review_log <- function(log_df, path) {
  # Ensure directory exists
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.csv(log_df, path, row.names = FALSE)
}

#' Load and process raster for display
load_patch_raster <- function(file_path) {
  r <- rast(file_path)

  # Find MOD_CLASS band
  band_names <- names(r)
  mod_class_idx <- which(band_names == "MOD_CLASS")

  if (length(mod_class_idx) == 0) {
    # Try last band if MOD_CLASS not found by name
    mod_class_idx <- nlyr(r)
  }

  mod_class <- r[[mod_class_idx]]

  # Reproject to WGS84 for Leaflet
  mod_class_wgs84 <- project(mod_class, "EPSG:4326", method = "near")

  mod_class_wgs84
}

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .sidebar {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        height: calc(100vh - 40px);
        overflow-y: auto;
      }
      .btn-valid {
        background-color: #28a745;
        color: white;
        width: 31%;
        margin-right: 2%;
      }
      .btn-valid:hover {
        background-color: #218838;
        color: white;
      }
      .btn-uncertain {
        background-color: #ffc107;
        color: #212529;
        width: 31%;
        margin-right: 2%;
      }
      .btn-uncertain:hover {
        background-color: #e0a800;
        color: #212529;
      }
      .btn-invalid {
        background-color: #dc3545;
        color: white;
        width: 31%;
      }
      .btn-invalid:hover {
        background-color: #c82333;
        color: white;
      }
      .nav-btn {
        width: 48%;
      }
      .progress-text {
        font-size: 1.2em;
        font-weight: bold;
        text-align: center;
        margin: 10px 0;
      }
      .patch-info {
        background-color: #e9ecef;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 15px;
      }
      .legend-box {
        background-color: #fff;
        border: 1px solid #ddd;
        padding: 10px;
        border-radius: 5px;
        margin-top: 15px;
      }
      .legend-item {
        display: flex;
        align-items: center;
        margin: 5px 0;
      }
      .legend-color {
        width: 20px;
        height: 20px;
        margin-right: 10px;
        border: 1px solid #333;
      }
    "))
  ),

  titlePanel("Wetland Patch Verification"),

  fluidRow(
    # Sidebar
    column(3,
      div(class = "sidebar",
        # Reviewer name
        textInput("reviewer_name", "Reviewer Name:", value = "",
                  width = "100%", placeholder = "Enter your name"),

        hr(),

        # Filter controls
        h4("Filters"),
        selectInput("filter_cluster", "Cluster:", choices = c("All" = ""), width = "100%"),
        selectInput("filter_huc", "HUC:", choices = c("All" = ""), width = "100%"),

        hr(),

        # Progress indicator
        div(class = "progress-text",
          textOutput("progress_text")
        ),

        # Current patch info
        div(class = "patch-info",
          h5("Current Patch"),
          textOutput("patch_name"),
          textOutput("patch_cluster"),
          textOutput("patch_huc"),
          textOutput("patch_status")
        ),

        # Basemap selection
        radioButtons("basemap", "Basemap:",
                     choices = c("ESRI World Imagery", "NYS Hillshade", "NAIP"),
                     selected = "ESRI World Imagery", inline = FALSE),

        # Overlay controls
        checkboxInput("show_overlay", "Show Classification Overlay", value = TRUE),
        sliderInput("overlay_opacity", "Overlay Opacity:", min = 0, max = 1,
                    value = 0.7, step = 0.1, width = "100%"),

        hr(),

        # Comments
        textAreaInput("comment_box", "Comments (optional):", value = "", rows = 3,
                      width = "100%", placeholder = "Add notes about this patch..."),

        # Valid/Invalid buttons
        h5("Mark Patch As:"),
        div(style = "display: flex; margin-bottom: 15px;",
          actionButton("btn_valid", "Valid", class = "btn-valid"),
          actionButton("btn_uncertain", "Uncertain", class = "btn-uncertain"),
          actionButton("btn_invalid", "Invalid", class = "btn-invalid")
        ),

        # Navigation
        h5("Navigation"),
        div(style = "display: flex; margin-bottom: 10px;",
          actionButton("btn_prev", "Previous", class = "nav-btn", style = "margin-right: 4%;"),
          actionButton("btn_next", "Next", class = "nav-btn")
        ),
        actionButton("btn_next_unreviewed", "Jump to Next Unreviewed",
                     class = "btn-info", style = "width: 100%; margin-bottom: 15px;"),

        hr(),

        # Export
        downloadButton("export_csv", "Export Review Log", style = "width: 100%;"),

        # Legend
        div(class = "legend-box",
          h5("Classification Legend"),
          div(class = "legend-item",
            div(class = "legend-color", style = "background-color: #00FFFF;"),
            span("EMW (Emergent)")
          ),
          div(class = "legend-item",
            div(class = "legend-color", style = "background-color: #006400;"),
            span("FSW (Forested)")
          ),
          div(class = "legend-item",
            div(class = "legend-color", style = "background-color: #0000FF;"),
            span("OWW (Open Water)")
          ),
          div(class = "legend-item",
            div(class = "legend-color", style = "background-color: #FFA500;"),
            span("SSW (Shrub-Scrub)")
          ),
          div(class = "legend-item",
            div(class = "legend-color", style = "background-color: #808080;"),
            span("UPL (Upland)")
          )
        )
      )
    ),

    # Map
    column(9,
      leafletOutput("map", height = "calc(100vh - 80px)")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    all_patches = NULL,       # All patches metadata
    filtered_patches = NULL,  # Filtered patches based on selections
    current_index = 1,        # Current patch index in filtered list
    review_log = NULL,        # Review log dataframe
    current_raster = NULL     # Current loaded raster
  )

  # Reactive path based on reviewer name
  review_log_path <- reactive({
    req(input$reviewer_name, nchar(trimws(input$reviewer_name)) > 0)
    sanitized <- gsub("[^A-Za-z0-9_-]", "_", trimws(input$reviewer_name))
    file.path(REVIEW_LOG_DIR, paste0("review_log_", sanitized, ".csv"))
  })

  # Scan patches on startup
  observe({
    rv$all_patches <- scan_patches(PATCHES_DIR)
    rv$filtered_patches <- rv$all_patches

    if (nrow(rv$all_patches) > 0) {
      clusters <- sort(unique(rv$all_patches$cluster))
      hucs <- sort(unique(rv$all_patches$huc))

      updateSelectInput(session, "filter_cluster",
                        choices = c("All" = "", clusters))
      updateSelectInput(session, "filter_huc",
                        choices = c("All" = "", hucs))
    }
  })

  # Load reviewer's log when name changes
  observeEvent(input$reviewer_name, {
    if (nchar(trimws(input$reviewer_name)) == 0) {
      rv$review_log <- NULL
      return()
    }

    path <- review_log_path()
    rv$review_log <- load_review_log(path)

    # Auto-resume: jump to first unreviewed patch
    if (!is.null(rv$all_patches) && nrow(rv$all_patches) > 0) {
      reviewed_files <- rv$review_log$patch_file
      unreviewed_idx <- which(!rv$all_patches$name %in% reviewed_files)

      if (length(unreviewed_idx) > 0) {
        rv$current_index <- unreviewed_idx[1]
      } else {
        rv$current_index <- 1
      }
    }
  })

  # Filter patches when selections change
  observeEvent(list(input$filter_cluster, input$filter_huc), {
    req(rv$all_patches)

    filtered <- rv$all_patches

    if (!is.null(input$filter_cluster) && input$filter_cluster != "") {
      filtered <- filtered[filtered$cluster == as.integer(input$filter_cluster), ]
    }

    if (!is.null(input$filter_huc) && input$filter_huc != "") {
      filtered <- filtered[filtered$huc == input$filter_huc, ]
    }

    rv$filtered_patches <- filtered
    rv$current_index <- 1
  }, ignoreInit = TRUE)

  # Current patch reactive
  current_patch <- reactive({
    req(rv$filtered_patches, rv$current_index)

    if (nrow(rv$filtered_patches) == 0 || rv$current_index > nrow(rv$filtered_patches)) {
      return(NULL)
    }

    rv$filtered_patches[rv$current_index, ]
  })

  # Load raster when patch changes
  observe({
    patch <- current_patch()
    req(patch)

    file_path <- file.path(PATCHES_DIR, patch$file)

    if (file.exists(file_path)) {
      rv$current_raster <- tryCatch(
        load_patch_raster(file_path),
        error = function(e) {
          showNotification(paste("Error loading raster:", e$message), type = "error")
          NULL
        }
      )
    }
  })

  # Progress text
  output$progress_text <- renderText({
    req(rv$all_patches, rv$review_log)

    total <- nrow(rv$all_patches)
    reviewed <- nrow(rv$review_log)

    paste0(reviewed, " / ", total, " reviewed")
  })

  # Patch info outputs
  output$patch_name <- renderText({
    patch <- current_patch()
    if (is.null(patch)) return("No patches found")
    paste("File:", patch$name)
  })

  output$patch_cluster <- renderText({
    patch <- current_patch()
    if (is.null(patch)) return("")
    paste("Cluster:", patch$cluster)
  })

  output$patch_huc <- renderText({
    patch <- current_patch()
    if (is.null(patch)) return("")
    paste("HUC:", patch$huc)
  })

  output$patch_status <- renderText({
    patch <- current_patch()
    req(patch, rv$review_log)

    status_row <- rv$review_log[rv$review_log$patch_file == patch$name, ]

    if (nrow(status_row) > 0) {
      paste("Status:", toupper(status_row$status[1]))
    } else {
      "Status: PENDING"
    }
  })

  # Pre-populate comment box when navigating to a reviewed patch
  observe({
    patch <- current_patch()
    req(patch, rv$review_log)

    existing <- rv$review_log[rv$review_log$patch_file == patch$name, ]

    if (nrow(existing) > 0 && !is.null(existing$comment[1]) && !is.na(existing$comment[1])) {
      updateTextAreaInput(session, "comment_box", value = existing$comment[1])
    } else {
      updateTextAreaInput(session, "comment_box", value = "")
    }
  })

  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = -85, lat = 45, zoom = 5)
  })

  # Switch basemap when selection changes
  observeEvent(input$basemap, {
    proxy <- leafletProxy("map") %>% clearTiles()

    if (input$basemap == "ESRI World Imagery") {
      proxy %>% addProviderTiles(providers$Esri.WorldImagery)
    } else if (input$basemap == "NYS Hillshade") {
      proxy %>% addWMSTiles(
        baseUrl = "https://elevation.its.ny.gov/arcgis/services/NYS_Statewide_Hillshade/MapServer/WMSServer",
        layers = "0,1,2",
        options = WMSTileOptions(format = "image/png", transparent = FALSE),
        attribution = "NYS ITS GIS"
      )
    } else if (input$basemap == "NAIP") {
      proxy %>% addTiles(
        urlTemplate = "https://naip.maptiles.arcgis.com/arcgis/rest/services/NAIP/MapServer/tile/{z}/{y}/{x}",
        attribution = "USDA NAIP, Esri"
      )
    }
  }, ignoreInit = TRUE)

  # Update map when raster changes, overlay toggled, opacity adjusted, or basemap switched
  observe({
    raster_data <- rv$current_raster
    show_overlay <- input$show_overlay
    opacity <- input$overlay_opacity
    basemap <- input$basemap  # re-render overlay after basemap switch

    req(raster_data)

    # Get raster extent for map bounds
    raster_ext <- ext(raster_data)
    bounds <- c(
      xmin(raster_ext),
      xmax(raster_ext),
      ymin(raster_ext),
      ymax(raster_ext)
    )

    # Convert to matrix for leaflet
    vals <- values(raster_data, mat = TRUE)

    # Create color mapping
    unique_vals <- sort(unique(vals[!is.na(vals)]))

    # Build color palette
    pal <- colorFactor(
      palette = unname(CLASS_COLORS[as.character(unique_vals)]),
      domain = unique_vals,
      na.color = "transparent"
    )

    leafletProxy("map") %>%
      clearImages() %>%
      clearShapes() %>%
      fitBounds(
        lng1 = bounds[1],
        lat1 = bounds[3],
        lng2 = bounds[2],
        lat2 = bounds[4]
      ) %>%
      addRectangles(
        lng1 = bounds[1],
        lat1 = bounds[3],
        lng2 = bounds[2],
        lat2 = bounds[4],
        color = "#FF0000",
        weight = 2,
        fillOpacity = 0
      )

    if (show_overlay) {
      leafletProxy("map") %>%
        addRasterImage(
          raster_data,
          colors = pal,
          opacity = opacity,
          project = FALSE
        )
    }
  })

  # Navigation: Previous
  observeEvent(input$btn_prev, {
    if (rv$current_index > 1) {
      rv$current_index <- rv$current_index - 1
    } else {
      showNotification("Already at first patch", type = "warning")
    }
  })

  # Navigation: Next
  observeEvent(input$btn_next, {
    if (rv$current_index < nrow(rv$filtered_patches)) {
      rv$current_index <- rv$current_index + 1
    } else {
      showNotification("Already at last patch", type = "warning")
    }
  })

  # Navigation: Jump to next unreviewed
  observeEvent(input$btn_next_unreviewed, {
    req(rv$filtered_patches, rv$review_log)

    reviewed_files <- rv$review_log$patch_file
    unreviewed_idx <- which(!rv$filtered_patches$name %in% reviewed_files)

    if (length(unreviewed_idx) > 0) {
      # Find next unreviewed from current position
      future_unreviewed <- unreviewed_idx[unreviewed_idx > rv$current_index]

      if (length(future_unreviewed) > 0) {
        rv$current_index <- future_unreviewed[1]
      } else {
        # Wrap around to beginning
        rv$current_index <- unreviewed_idx[1]
        showNotification("Wrapped to beginning", type = "message")
      }
    } else {
      showNotification("All patches in current filter have been reviewed!", type = "message")
    }
  })

  # Mark as Valid
  observeEvent(input$btn_valid, {
    patch <- current_patch()
    req(patch)

    log_review(patch, "valid")
  })

  # Mark as Uncertain
  observeEvent(input$btn_uncertain, {
    patch <- current_patch()
    req(patch)

    log_review(patch, "uncertain")
  })

  # Mark as Invalid
  observeEvent(input$btn_invalid, {
    patch <- current_patch()
    req(patch)

    log_review(patch, "invalid")
  })

  # Helper function to log review
  log_review <- function(patch, status) {
    # Require reviewer name
    if (nchar(trimws(input$reviewer_name)) == 0) {
      showNotification("Please enter your name before reviewing.", type = "error")
      return()
    }

    # Check if already reviewed
    existing_idx <- which(rv$review_log$patch_file == patch$name)

    new_entry <- data.frame(
      patch_file = patch$name,
      cluster = patch$cluster,
      huc = patch$huc,
      patch_num = patch$patch_num,
      status = status,
      comment = input$comment_box,
      reviewer = trimws(input$reviewer_name),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )

    if (length(existing_idx) > 0) {
      # Update existing entry
      rv$review_log[existing_idx, ] <- new_entry
    } else {
      # Add new entry
      rv$review_log <- rbind(rv$review_log, new_entry)
    }

    # Auto-save
    save_review_log(rv$review_log, review_log_path())

    showNotification(paste("Marked as", toupper(status)), type = "message", duration = 2)

    # Clear comment box
    updateTextAreaInput(session, "comment_box", value = "")

    # Auto-advance to next patch
    if (rv$current_index < nrow(rv$filtered_patches)) {
      rv$current_index <- rv$current_index + 1
    }
  }

  # Export CSV download
  output$export_csv <- downloadHandler(
    filename = function() {
      reviewer <- gsub("[^A-Za-z0-9_-]", "_", trimws(input$reviewer_name))
      paste0("review_log_", reviewer, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(rv$review_log, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
