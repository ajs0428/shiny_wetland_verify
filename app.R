# Wetland Patch Verification App
# Validates deep learning training data for wetland classification

library(shiny)
library(shinyjs)
library(leaflet)
library(leafpm)
library(terra)
library(sf)
library(stringr)

# --- Configuration ---
VECTOR_DIR         <- "Data/R_Patches_Vector"
RASTER_DIR         <- "Data/R_Patches_Labels"
ALTERED_VECTOR_DIR <- "Data/Altered_R_Patches_Vector"
REVIEW_LOG_DIR     <- "Data"

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

#' Parse vector patch filename to extract metadata
parse_vector_filename <- function(filename) {
  pattern <- "NHP_cluster_(\\d+)_huc_(\\d+)_patch_(\\d+)\\.gpkg"
  matches <- regmatches(filename, regexec(pattern, filename))[[1]]
  if (length(matches) == 4) {
    list(
      file_vector = filename,
      cluster     = as.integer(matches[2]),
      huc         = matches[3],
      patch_num   = as.integer(matches[4])
    )
  } else {
    NULL
  }
}

#' Find matching raster file for a given cluster/huc/patch_num
#' Matches any filename ending in _cluster_<N>_huc_<CODE>_patch_<N>.tif
find_raster_match <- function(cluster, huc, patch_num) {
  pattern <- sprintf("_cluster_%d_huc_%s_patch_%d\\.tif$", cluster, huc, patch_num)
  matches <- list.files(RASTER_DIR, pattern = pattern, full.names = TRUE)
  if (length(matches) > 0) matches[1] else NULL
}

#' Scan vector patches directory; return only pairs with a matching raster
scan_patches <- function(vector_dir, raster_dir) {
  files <- list.files(vector_dir, pattern = "\\.gpkg$", full.names = FALSE)

  patch_list <- lapply(files, parse_vector_filename)
  patch_list <- Filter(Negate(is.null), patch_list)

  if (length(patch_list) == 0) {
    return(data.frame(
      file_vector = character(),
      file_raster = character(),
      cluster     = integer(),
      huc         = character(),
      patch_num   = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # Add raster match; drop unpaired
  patch_list <- lapply(patch_list, function(p) {
    raster_path <- find_raster_match(p$cluster, p$huc, p$patch_num)
    if (is.null(raster_path)) return(NULL)
    p$file_raster <- basename(raster_path)
    p
  })
  patch_list <- Filter(Negate(is.null), patch_list)

  if (length(patch_list) == 0) {
    return(data.frame(
      file_vector = character(),
      file_raster = character(),
      cluster     = integer(),
      huc         = character(),
      patch_num   = integer(),
      stringsAsFactors = FALSE
    ))
  }

  df <- do.call(rbind, lapply(patch_list, as.data.frame, stringsAsFactors = FALSE))
  df <- df[order(df$cluster, df$huc, df$patch_num), ]
  rownames(df) <- NULL
  df
}

#' Load review log or create empty one with new schema
load_review_log <- function(path) {
  if (file.exists(path)) {
    df <- read.csv(path, stringsAsFactors = FALSE)
    # Ensure all expected columns exist
    expected <- c("patch_file_vector", "cluster", "huc", "patch_num",
                  "status", "altered", "confidence", "comment", "reviewer", "timestamp")
    for (col in expected) {
      if (!col %in% names(df)) df[[col]] <- NA
    }
    df[, expected]
  } else {
    data.frame(
      patch_file_vector = character(),
      cluster           = integer(),
      huc               = character(),
      patch_num         = integer(),
      status            = character(),
      altered           = logical(),
      confidence        = integer(),
      comment           = character(),
      reviewer          = character(),
      timestamp         = character(),
      stringsAsFactors  = FALSE
    )
  }
}

#' Save review log to CSV
save_review_log <- function(log_df, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.csv(log_df, path, row.names = FALSE)
}

#' Load raster patch: extract MOD_CLASS band, reproject to WGS84
load_patch_raster <- function(file_path) {
  r          <- rast(file_path)
  band_names <- names(r)
  idx        <- which(band_names == "MOD_CLASS")
  if (length(idx) == 0) idx <- nlyr(r)
  mod_class  <- r[[idx]]
  project(mod_class, "EPSG:4326", method = "near")
}

#' Load vector patch: return list with WGS84 sf and original CRS object
load_patch_vector <- function(file_path) {
  sf_obj   <- st_read(file_path, quiet = TRUE)
  orig_crs <- st_crs(sf_obj)
  sf_wgs84 <- st_transform(sf_obj, 4326)
  list(sf_wgs84 = sf_wgs84, orig_crs = orig_crs)
}

# --- UI ---
ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$style(HTML("
      .sidebar {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        height: calc(100vh - 40px);
        overflow-y: auto;
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
      #btn_submit {
        margin-bottom: 15px;
      }
    ")),
    # Custom JS: extract all current Leaflet.PM layers and send to Shiny
    tags$script(HTML("
      Shiny.addCustomMessageHandler('getFinalFeatures', function(msg) {
        var leafletMap = HTMLWidgets.find('#map').getMap();
        var features = [];
        leafletMap.pm.getGeomanLayers().forEach(function(layer) {
          features.push(layer.toGeoJSON());
        });
        Shiny.setInputValue('final_vector_geojson', JSON.stringify({
          type: 'FeatureCollection',
          features: features
        }), {priority: 'event'});
      });
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
        checkboxInput("show_raster", "Show Raster Classification Overlay", value = TRUE),
        sliderInput("overlay_opacity", "Raster Opacity:", min = 0, max = 1,
                    value = 0.7, step = 0.1, width = "100%"),
        checkboxInput("show_vector", "Show Vector Overlay", value = TRUE),

        hr(),

        # Confidence and comment
        selectInput("confidence", "Confidence (1\u201310):",
                    choices = c("Select..." = "", as.character(1:10)),
                    selected = "", width = "100%"),
        textAreaInput("comment_box", "Comments (optional):", value = "", rows = 3,
                      width = "100%", placeholder = "Add notes about this patch..."),

        # Submit button — disabled until confidence is selected
        disabled(
          actionButton("btn_submit", "Submit Review",
                       class = "btn-primary", style = "width: 100%;")
        ),

        hr(),

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
    all_patches          = NULL,   # paired patch metadata df
    filtered_patches     = NULL,
    current_index        = 1,
    review_log           = NULL,
    current_raster       = NULL,   # SpatRaster in WGS84
    current_vector_wgs84 = NULL,   # sf in WGS84
    vector_orig_crs      = NULL,   # CRS object of original vector projection
    vector_altered       = FALSE   # TRUE if any PM edit event fired
  )

  # Reactive review log file path based on reviewer name
  review_log_path <- reactive({
    req(input$reviewer_name, nchar(trimws(input$reviewer_name)) > 0)
    sanitized <- gsub("[^A-Za-z0-9_-]", "_", trimws(input$reviewer_name))
    file.path(REVIEW_LOG_DIR, paste0("review_log_", sanitized, ".csv"))
  })

  # Scan patches on startup
  observe({
    rv$all_patches      <- scan_patches(VECTOR_DIR, RASTER_DIR)
    rv$filtered_patches <- rv$all_patches

    if (nrow(rv$all_patches) > 0) {
      clusters <- sort(unique(rv$all_patches$cluster))
      hucs     <- sort(unique(rv$all_patches$huc))
      updateSelectInput(session, "filter_cluster", choices = c("All" = "", clusters))
      updateSelectInput(session, "filter_huc",     choices = c("All" = "", hucs))
    }
  })

  # Load reviewer's log when name changes
  observeEvent(input$reviewer_name, {
    if (nchar(trimws(input$reviewer_name)) == 0) {
      rv$review_log <- NULL
      return()
    }
    path          <- review_log_path()
    rv$review_log <- load_review_log(path)

    # Auto-resume: jump to first unreviewed patch
    if (!is.null(rv$all_patches) && nrow(rv$all_patches) > 0) {
      reviewed_files  <- rv$review_log$patch_file_vector
      unreviewed_idx  <- which(!rv$all_patches$file_vector %in% reviewed_files)
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
    rv$current_index    <- 1
  }, ignoreInit = TRUE)

  # Current patch reactive
  current_patch <- reactive({
    req(rv$filtered_patches, rv$current_index)
    if (nrow(rv$filtered_patches) == 0 || rv$current_index > nrow(rv$filtered_patches)) {
      return(NULL)
    }
    rv$filtered_patches[rv$current_index, ]
  })

  # Load raster and vector when patch changes
  observe({
    patch <- current_patch()
    req(patch)

    # Load raster
    raster_path <- file.path(RASTER_DIR, patch$file_raster)
    rv$current_raster <- tryCatch(
      load_patch_raster(raster_path),
      error = function(e) {
        showNotification(paste("Error loading raster:", e$message), type = "error")
        NULL
      }
    )

    # Load vector
    vector_path <- file.path(VECTOR_DIR, patch$file_vector)
    result <- tryCatch(
      load_patch_vector(vector_path),
      error = function(e) {
        showNotification(paste("Error loading vector:", e$message), type = "error")
        NULL
      }
    )
    if (!is.null(result)) {
      rv$current_vector_wgs84 <- result$sf_wgs84
      rv$vector_orig_crs      <- result$orig_crs
    }

    # Reset altered flag on new patch
    rv$vector_altered <- FALSE
  })

  # --- Progress and patch info outputs ---

  output$progress_text <- renderText({
    req(rv$all_patches, rv$review_log)
    total    <- nrow(rv$all_patches)
    reviewed <- nrow(rv$review_log)
    paste0(reviewed, " / ", total, " reviewed")
  })

  output$patch_name <- renderText({
    patch <- current_patch()
    if (is.null(patch)) return("No patches found")
    paste("File:", patch$file_vector)
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
    row <- rv$review_log[rv$review_log$patch_file_vector == patch$file_vector, ]
    if (nrow(row) > 0) "Status: REVIEWED" else "Status: PENDING"
  })

  # Pre-populate confidence and comment when navigating to a reviewed patch
  observe({
    patch <- current_patch()
    req(patch, rv$review_log)
    existing <- rv$review_log[rv$review_log$patch_file_vector == patch$file_vector, ]
    if (nrow(existing) > 0) {
      conf_val <- existing$confidence[1]
      updateSelectInput(session, "confidence",
                        selected = if (!is.na(conf_val)) as.character(conf_val) else "")
      updateTextAreaInput(session, "comment_box",
                          value = if (!is.na(existing$comment[1])) existing$comment[1] else "")
    } else {
      updateSelectInput(session, "confidence", selected = "")
      updateTextAreaInput(session, "comment_box", value = "")
    }
  })

  # Enable/disable Submit based on confidence selection
  observe({
    if (!is.null(input$confidence) && input$confidence != "") {
      enable("btn_submit")
    } else {
      disable("btn_submit")
    }
  })

  # --- Leaflet map ---

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = -76, lat = 43, zoom = 8) %>%
      addPmToolbar(
        toolbarOptions = pmToolbarOptions(
          drawMarker      = FALSE,
          drawPolyline    = FALSE,
          drawCircle      = FALSE,
          drawCircleMarker = FALSE,
          drawRectangle   = FALSE,
          drawPolygon     = TRUE,
          editMode        = TRUE,
          dragMode        = TRUE,
          cutPolygon      = FALSE,
          removalMode     = TRUE
        )
      )
  })

  # Switch basemap
  observeEvent(input$basemap, {
    proxy <- leafletProxy("map") %>% clearTiles()
    if (input$basemap == "ESRI World Imagery") {
      proxy %>% addProviderTiles(providers$Esri.WorldImagery)
    } else if (input$basemap == "NYS Hillshade") {
      proxy %>% addWMSTiles(
        baseUrl = "https://elevation.its.ny.gov/arcgis/services/NYS_Statewide_Hillshade/MapServer/WMSServer",
        layers  = "0,1,2",
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

  # Update map when patch data, overlay settings, or basemap changes
  observe({
    raster_data  <- rv$current_raster
    vector_data  <- rv$current_vector_wgs84
    show_raster  <- input$show_raster
    show_vector  <- input$show_vector
    opacity      <- input$overlay_opacity
    input$basemap  # trigger on basemap switch

    req(vector_data)

    # Get bounds from vector for fitBounds
    bbox <- st_bbox(vector_data)

    proxy <- leafletProxy("map") %>%
      clearImages() %>%
      clearGroup("raster_overlay") %>%
      clearGroup("vector_layer") %>%
      fitBounds(
        lng1 = as.numeric(bbox["xmin"]),
        lat1 = as.numeric(bbox["ymin"]),
        lng2 = as.numeric(bbox["xmax"]),
        lat2 = as.numeric(bbox["ymax"])
      )

    # Raster overlay
    if (show_raster && !is.null(raster_data)) {
      vals         <- values(raster_data, mat = TRUE)
      unique_vals  <- sort(unique(vals[!is.na(vals)]))
      pal          <- colorFactor(
        palette  = unname(CLASS_COLORS[as.character(unique_vals)]),
        domain   = unique_vals,
        na.color = "transparent"
      )
      proxy <- proxy %>%
        addRasterImage(raster_data, colors = pal, opacity = opacity,
                       project = FALSE, group = "raster_overlay")
    }

    # Vector overlay (editable via leafpm)
    if (show_vector) {
      proxy <- proxy %>%
        addFeatures(data = vector_data, group = "vector_layer",
                    style = list(color = "#FF4500", weight = 2, fillOpacity = 0.2))
    }
  })

  # --- PM edit event tracking ---

  observeEvent(input$map_pm_draw_new_feature, { rv$vector_altered <- TRUE })
  observeEvent(input$map_pm_edit_feature,     { rv$vector_altered <- TRUE })
  observeEvent(input$map_pm_remove_feature,   { rv$vector_altered <- TRUE })

  # --- Navigation ---

  observeEvent(input$btn_prev, {
    if (rv$current_index > 1) {
      rv$current_index <- rv$current_index - 1
    } else {
      showNotification("Already at first patch", type = "warning")
    }
  })

  observeEvent(input$btn_next, {
    if (rv$current_index < nrow(rv$filtered_patches)) {
      rv$current_index <- rv$current_index + 1
    } else {
      showNotification("Already at last patch", type = "warning")
    }
  })

  observeEvent(input$btn_next_unreviewed, {
    req(rv$filtered_patches, rv$review_log)
    reviewed_files <- rv$review_log$patch_file_vector
    unreviewed_idx <- which(!rv$filtered_patches$file_vector %in% reviewed_files)

    if (length(unreviewed_idx) > 0) {
      future <- unreviewed_idx[unreviewed_idx > rv$current_index]
      if (length(future) > 0) {
        rv$current_index <- future[1]
      } else {
        rv$current_index <- unreviewed_idx[1]
        showNotification("Wrapped to beginning", type = "message")
      }
    } else {
      showNotification("All patches in current filter have been reviewed!", type = "message")
    }
  })

  # --- Submit: step 1 — trigger JS to collect current PM layer features ---

  observeEvent(input$btn_submit, {
    if (nchar(trimws(input$reviewer_name)) == 0) {
      showNotification("Please enter your name before submitting.", type = "error")
      return()
    }
    req(input$confidence != "")
    session$sendCustomMessage("getFinalFeatures", list())
  })

  # --- Submit: step 2 — receive GeoJSON from JS, log review, save if altered ---

  observeEvent(input$final_vector_geojson, {
    patch <- current_patch()
    req(patch)
    if (is.null(input$confidence) || input$confidence == "") return()

    altered <- rv$vector_altered

    if (altered) {
      geojson_str <- input$final_vector_geojson
      edited_sf_wgs84 <- tryCatch(
        st_read(dsn = geojson_str, quiet = TRUE),
        error = function(e) {
          showNotification(paste("Error reading edited geometry:", e$message), type = "error")
          NULL
        }
      )

      if (!is.null(edited_sf_wgs84) && nrow(edited_sf_wgs84) > 0) {
        edited_sf_orig <- st_transform(edited_sf_wgs84, rv$vector_orig_crs)
        out_path       <- file.path(ALTERED_VECTOR_DIR, patch$file_vector)
        dir.create(ALTERED_VECTOR_DIR, showWarnings = FALSE, recursive = TRUE)
        tryCatch(
          st_write(edited_sf_orig, out_path, delete_layer = TRUE, quiet = TRUE),
          error = function(e) {
            showNotification(paste("Error saving altered vector:", e$message), type = "error")
          }
        )
      }
    }

    new_entry <- data.frame(
      patch_file_vector = patch$file_vector,
      cluster           = patch$cluster,
      huc               = patch$huc,
      patch_num         = patch$patch_num,
      status            = "reviewed",
      altered           = altered,
      confidence        = as.integer(input$confidence),
      comment           = input$comment_box,
      reviewer          = trimws(input$reviewer_name),
      timestamp         = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors  = FALSE
    )

    existing_idx <- which(rv$review_log$patch_file_vector == patch$file_vector)
    if (length(existing_idx) > 0) {
      rv$review_log[existing_idx, ] <- new_entry
    } else {
      rv$review_log <- rbind(rv$review_log, new_entry)
    }

    save_review_log(rv$review_log, review_log_path())

    msg <- paste0("Submitted \u2014 confidence: ", input$confidence,
                  if (altered) " | vector altered & saved" else "")
    showNotification(msg, type = "message", duration = 3)

    # Reset and advance
    updateSelectInput(session, "confidence", selected = "")
    updateTextAreaInput(session, "comment_box", value = "")
    rv$vector_altered <- FALSE

    if (rv$current_index < nrow(rv$filtered_patches)) {
      rv$current_index <- rv$current_index + 1
    }
  })

  # --- Export CSV ---

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
