# ============================================================
# VIT CAMPUS MOBILITY DASHBOARD
# Save this file as app.R and click Run App in RStudio.
# ============================================================

# 1. INSTALL AND LOAD PACKAGES
required <- c(
  "shiny", "dplyr", "ggplot2", "tidyr",
  "DT", "leaflet", "scales", "htmltools", "reticulate"
)

missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
invisible(lapply(required, library, character.only = TRUE))

# 2. REFERENCE LOCATIONS
# These are approximate reference points for demonstration.
vit_places <- data.frame(
  location = c(
    "Main Gate", "Academic Blocks", "Library Walkway",
    "Cafeteria Junction", "SJT Ground", "Hostel Road",
    "Technology Tower"
  ),
  latitude = c(12.9699, 12.9711, 12.9704, 12.9687, 12.9720, 12.9680, 12.9717),
  longitude = c(79.1551, 79.1582, 79.1565, 79.1590, 79.1603, 79.1575, 79.1537),
  stringsAsFactors = FALSE
)

# 3. DEMO DATA
make_demo <- function() {
  set.seed(42)
  x <- expand.grid(
    date = seq.Date(Sys.Date() - 6, Sys.Date(), by = "day"),
    hour = 6:22,
    location = vit_places$location,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  x <- tibble::as_tibble(x) |>
    dplyr::mutate(
      time = sprintf("%02d:00", hour),
      day_type = ifelse(
        weekdays(date) %in% c("Saturday", "Sunday"),
        "Weekend", "Weekday"
      ),
      pedestrians = round(
        runif(dplyr::n(), 100, 420) *
          ifelse(hour %in% 9:13, 1.65, 1) *
          ifelse(location == "Library Walkway", 1.45, 1)
      ),
      vehicles = round(
        runif(dplyr::n(), 25, 170) *
          ifelse(hour %in% c(8, 9, 16, 17, 18), 1.5, 1) *
          ifelse(location == "Main Gate", 1.5, 1)
      )
    )
  dplyr::left_join(x, vit_places, by = "location") |>
    dplyr::select(
      date, time, hour, location, day_type,
      pedestrians, vehicles, latitude, longitude
    ) |>
    dplyr::mutate(address = NA_character_)
}

empty_data <- function() {
  data.frame(
    date = as.Date(character()), time = character(), hour = integer(),
    location = character(), day_type = character(),
    pedestrians = numeric(), vehicles = numeric(),
    latitude = numeric(), longitude = numeric(),
    address = character(), stringsAsFactors = FALSE
  )
}

# 4. CSV CLEANING AND COLUMN RECOGNITION
clean_data <- function(x) {
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    stop("The CSV file contains no data rows.")
  }
  
  normalize_name <- function(z) {
    z <- tolower(trimws(z))
    z <- gsub("[^a-z0-9]+", "_", z)
    gsub("^_+|_+$", "", z)
  }
  names(x) <- make.unique(normalize_name(names(x)))
  
  aliases <- list(
    date = c("date", "day", "record_date", "timestamp", "datetime", "date_time"),
    time = c("time", "record_time", "time_of_day"),
    hour = c("hour", "hour_of_day"),
    location = c("location", "area", "place", "campus_location",
                 "location_name", "site", "block", "terminal"),
    day_type = c("day_type", "daytype", "day_category"),
    pedestrians = c("pedestrians", "pedestrian", "people", "people_count",
                    "persons", "footfall", "pedestrian_count",
                    "pedestrians_count", "pedestrian_volume", "pedestrian_traffic"),
    vehicles = c("vehicles", "vehicle", "vehicle_count", "cars",
                 "traffic_count", "vehicle_volume", "vehicles_count", "vehicle_traffic"),
    latitude = c("latitude", "lat"),
    longitude = c("longitude", "lon", "lng", "long"),
    address = c("address", "full_address", "location_address")
  )
  
  for (target in names(aliases)) {
    if (!target %in% names(x)) {
      found <- intersect(aliases[[target]], names(x))
      if (length(found) > 0) names(x)[match(found[1], names(x))] <- target
    }
  }
  
  has_pedestrians <- "pedestrians" %in% names(x)
  has_vehicles <- "vehicles" %in% names(x)
  if (!has_pedestrians && !has_vehicles) {
    stop(paste(
      "No pedestrian or vehicle count column was found.",
      "Include pedestrians/pedestrian_count or vehicles/vehicle_count."
    ))
  }
  
  if (!"location" %in% names(x)) x$location <- paste0("Location ", seq_len(nrow(x)))
  if (!"date" %in% names(x)) x$date <- as.character(Sys.Date())
  if (!"time" %in% names(x)) {
    if ("hour" %in% names(x)) x$time <- as.character(x$hour) else x$time <- "12:00"
  }
  if (!"pedestrians" %in% names(x)) x$pedestrians <- 0
  if (!"vehicles" %in% names(x)) x$vehicles <- 0
  if (!"latitude" %in% names(x)) x$latitude <- NA_real_
  if (!"longitude" %in% names(x)) x$longitude <- NA_real_
  if (!"address" %in% names(x)) x$address <- NA_character_
  
  raw_date <- trimws(as.character(x$date))
  parsed_date <- suppressWarnings(as.Date(
    substr(raw_date, 1, 10),
    tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y",
                   "%d-%m-%Y", "%Y/%m/%d", "%d-%b-%Y",
                   "%b %d, %Y", "%d %B %Y")
  ))
  if (all(is.na(parsed_date))) {
    stop("Could not parse dates. Use YYYY-MM-DD or DD/MM/YYYY.")
  }
  x$date <- parsed_date
  
  raw_time <- trimws(as.character(x$time))
  extracted <- suppressWarnings(as.integer(sub("^([0-9]{1,2}).*$", "\\1", raw_time)))
  valid_format <- grepl("^[0-9]{1,2}([:.].*)?$", raw_time)
  extracted[!valid_format] <- NA_integer_
  extracted[is.na(extracted) | extracted < 0 | extracted > 23] <- NA_integer_
  
  if ("hour" %in% names(x)) {
    separate_hour <- suppressWarnings(as.integer(as.character(x$hour)))
    use_separate <- is.na(extracted) & !is.na(separate_hour) &
      separate_hour >= 0 & separate_hour <= 23
    extracted[use_separate] <- separate_hour[use_separate]
  }
  extracted[is.na(extracted)] <- 12L
  x$hour <- extracted
  x$time <- sprintf("%02d:00", x$hour)
  
  x$location <- trimws(as.character(x$location))
  x$location[is.na(x$location) | x$location == ""] <- "Unknown Location"
  
  if (!"day_type" %in% names(x)) {
    x$day_type <- ifelse(weekdays(x$date) %in% c("Saturday", "Sunday"),
                         "Weekend", "Weekday")
  } else {
    x$day_type <- trimws(as.character(x$day_type))
    missing_day <- is.na(x$day_type) | x$day_type == ""
    x$day_type[missing_day] <- ifelse(
      weekdays(x$date[missing_day]) %in% c("Saturday", "Sunday"),
      "Weekend", "Weekday"
    )
  }
  
  x$pedestrians <- suppressWarnings(as.numeric(gsub(",", "", as.character(x$pedestrians))))
  x$vehicles <- suppressWarnings(as.numeric(gsub(",", "", as.character(x$vehicles))))
  x$pedestrians[!is.finite(x$pedestrians) | is.na(x$pedestrians)] <- 0
  x$vehicles[!is.finite(x$vehicles) | is.na(x$vehicles)] <- 0
  x$latitude <- suppressWarnings(as.numeric(as.character(x$latitude)))
  x$longitude <- suppressWarnings(as.numeric(as.character(x$longitude)))
  x$address <- as.character(x$address)
  
  vit_match <- match(tolower(x$location), tolower(vit_places$location))
  valid_lat <- is.finite(x$latitude) & x$latitude >= -90 & x$latitude <= 90
  valid_lon <- is.finite(x$longitude) & x$longitude >= -180 & x$longitude <= 180
  use_vit <- !is.na(vit_match) & (!valid_lat | !valid_lon)
  x$latitude[use_vit] <- vit_places$latitude[vit_match[use_vit]]
  x$longitude[use_vit] <- vit_places$longitude[vit_match[use_vit]]
  
  x
}

fmt <- function(x) {
  format(round(sum(x, na.rm = TRUE)), big.mark = ",", scientific = FALSE)
}

empty_plot <- function(title) {
  ggplot() +
    annotate("text", x = 1, y = 1,
             label = "No records match the selected filters",
             colour = "#eaf3ff", size = 4) +
    xlim(0, 2) + ylim(0, 2) +
    labs(title = title) + theme_void() +
    theme(
      plot.background = element_rect(fill = "#071629", colour = NA),
      plot.title = element_text(colour = "#f3f7ff", face = "bold")
    )
}

dark_plot_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background = element_rect(fill = "#071629", colour = NA),
      panel.background = element_rect(fill = "#071629", colour = NA),
      panel.grid.major = element_line(colour = "#19334f", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = "#e1edfb"),
      axis.title = element_text(colour = "#d4e5f8"),
      plot.title = element_text(colour = "#f3f7ff", face = "bold", size = 13),
      legend.background = element_rect(fill = "#071629", colour = NA),
      legend.key = element_rect(fill = "#071629", colour = NA),
      legend.text = element_text(colour = "#e1edfb"),
      legend.title = element_text(colour = "#e1edfb")
    )
}

# 5. USER INTERFACE
ui <- fluidPage(
  tags$head(
    tags$title("VIT Campus Mobility Dashboard"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$script(HTML("
      (function() {
        let aiStream = null, aiTimer = null, aiLastSend = 0;
        function stopAiCamera() {
          if (aiTimer) { clearInterval(aiTimer); aiTimer = null; }
          if (aiStream) { aiStream.getTracks().forEach(function(t){t.stop();}); aiStream = null; }
          const v = document.getElementById('ai_camera_video');
          if (v) v.srcObject = null;
        }
        document.addEventListener('click', function(e) {
          if (e.target && (e.target.id === 'start_ai_camera' || e.target.closest('#start_ai_camera'))) {
            const video = document.getElementById('ai_camera_video');
            if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
              alert('Camera access requires localhost or HTTPS and a supported browser.'); return;
            }
            stopAiCamera();
            navigator.mediaDevices.getUserMedia({video: true, audio: false}).then(function(stream) {
              aiStream = stream; video.srcObject = stream; video.play();
              if (window.Shiny) Shiny.setInputValue('ai_camera_started', Date.now(), {priority:'event'});
              aiTimer = setInterval(function() {
                if (!video.videoWidth || !video.videoHeight || !window.Shiny) return;
                const el = document.getElementById('detection_interval');
                const seconds = el ? Math.max(1, Number(el.value) || 2) : 2;
                if (Date.now() - aiLastSend < seconds * 1000) return;
                aiLastSend = Date.now();
                const canvas = document.getElementById('ai_camera_canvas');
                canvas.width = 640; canvas.height = Math.round(640 * video.videoHeight / video.videoWidth);
                canvas.getContext('2d').drawImage(video, 0, 0, canvas.width, canvas.height);
                Shiny.setInputValue('ai_camera_frame', canvas.toDataURL('image/jpeg', 0.72), {priority:'event'});
              }, 300);
            }).catch(function(err) {
              alert('Could not open camera: ' + err.message + '. Check browser camera permissions.');
            });
          }
          if (e.target && (e.target.id === 'stop_ai_camera' || e.target.closest('#stop_ai_camera'))) {
            stopAiCamera();
            if (window.Shiny) Shiny.setInputValue('ai_camera_stopped', Date.now(), {priority:'event'});
          }
        });
        window.addEventListener('beforeunload', stopAiCamera);
        if (window.Shiny) {
          Shiny.addCustomMessageHandler('ai-alert-sound', function(m) {
            try {
              const a = new (window.AudioContext || window.webkitAudioContext)();
              const o = a.createOscillator(), g = a.createGain();
              o.connect(g); g.connect(a.destination); o.frequency.value = 880;
              g.gain.value = 0.08; o.start(); o.stop(a.currentTime + 0.25);
            } catch(e) {}
            alert(m.text + '. Check the camera feed and verify.');
          });
        }
      })();
    ")),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');
      html,body{background:#030d1b;color:#eaf3ff;font-family:Inter,Arial,sans-serif}
      .container-fluid{padding:10px 16px 22px}
      .topbar{display:flex;align-items:center;gap:18px;padding:10px 18px;margin:-10px -16px 12px;background:linear-gradient(100deg,#06172b,#0a2548 55%,#041326);border-bottom:1px solid #17416a}
      .vit-mark{width:66px;height:66px;flex:0 0 66px;border-radius:50%;border:2px solid #a9c9ed;display:flex;align-items:center;justify-content:center;color:#fff;font-family:Georgia,serif;font-size:22px;font-weight:800;background:radial-gradient(circle,#214d79,#06172b)}
      .brand{font-family:Georgia,serif;font-size:25px;font-weight:700;color:#fff;line-height:1.05}
      .brand-sub{font-size:12px;color:#b9d8fa;margin-top:4px}
      .heading{font-size:24px;font-weight:800;color:#8ebeff;line-height:1.15}
      .subheading{font-size:13px;color:#c2d9f4;margin-top:5px}
      .live-pill{margin-left:auto;border:1px solid #164f77;background:#061a30;border-radius:12px;padding:12px 14px;white-space:nowrap;font-size:12px}
      .live-dot{display:inline-block;width:9px;height:9px;border-radius:50%;background:#00e676;margin-right:8px;box-shadow:0 0 10px #00e676}
      .side{background:linear-gradient(180deg,#071a30,#061426);border:1px solid #16466c;border-radius:13px;padding:14px;margin-bottom:12px;box-shadow:0 8px 22px #0004}
      .side h4{font-size:15px;font-weight:700;color:#f2f7ff;margin:0 0 12px}
      .control-label{color:#c9def5;font-size:12px;font-weight:600}
      .form-control,.selectize-input{background:#102943!important;color:#f4f8ff!important;border:1px solid #244c70!important;border-radius:8px!important}
      .selectize-dropdown{background:#102943;color:#fff}
      .btn{border-radius:8px;font-weight:700}
      .btn-primary{background:#0878f9;border-color:#1685ff}
      .btn-default{background:#15334f;color:#eaf3ff;border-color:#244d70}
      .btn-success{background:#087c59;border-color:#00a875}
      .metric{min-height:112px;padding:14px 15px;border:1px solid #19456b;border-radius:14px;background:linear-gradient(145deg,#0a2039,#071629);box-shadow:0 7px 20px #0003;position:relative;overflow:hidden;margin-bottom:10px}
      .metric:after{content:'';position:absolute;inset:0 auto 0 0;width:4px;background:var(--accent,#1688ff)}
      .metric-label{font-size:12px;color:#d5e6fa;font-weight:600}
      .metric-value{font-size:26px;font-weight:800;color:#fff;line-height:1.2;margin-top:7px;overflow-wrap:anywhere}
      .metric-foot{font-size:11px;color:#a9c1dc;margin-top:6px}
      .metric-icon{font-size:22px;float:right;color:#d6eaff}
      .panel-card{background:linear-gradient(150deg,#071b30,#061426);border:1px solid #16466c;border-radius:13px;padding:12px;margin-bottom:12px;box-shadow:0 8px 22px #0003}
      .panel-title{font-size:15px;font-weight:700;color:#edf6ff;margin:0 0 9px}
      .nav-tabs{border-bottom:1px solid #16466c;background:#071a2e;border-radius:12px 12px 0 0;padding:5px 5px 0;margin:2px 0 12px}
      .nav-tabs>li>a{color:#c2dcf7!important;font-weight:600;border-radius:9px 9px 0 0;padding:11px 13px}
      .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover{background:#0878f9!important;color:white!important;border:1px solid #1685ff!important}
      /* HIGH-CONTRAST DATA TABLES: force readable cells and controls */
      .dataTables_wrapper,.dataTables_wrapper label,.dataTables_info,
      .dataTables_length,.dataTables_filter,.dataTables_paginate,
      .dataTables_wrapper .dataTables_paginate .paginate_button{color:#f4f8ff!important}
      .dataTables_wrapper input,.dataTables_wrapper select{
        background:#102943!important;color:#ffffff!important;
        border:1px solid #4a7198!important;border-radius:5px!important;padding:5px!important
      }
      table.dataTable,table.dataTable tbody,table.dataTable tbody tr,
      table.dataTable tbody td,table.dataTable tfoot th{
        background:#0b2037!important;color:#ffffff!important;border-color:#315170!important
      }
      table.dataTable thead th,table.dataTable thead td{
        background:#16416a!important;color:#ffffff!important;border-color:#47749b!important
      }
      table.dataTable.stripe tbody tr.odd,table.dataTable.display tbody tr.odd{background:#102943!important}
      table.dataTable.stripe tbody tr.even,table.dataTable.display tbody tr.even{background:#0b2037!important}
      table.dataTable tbody tr:hover td{background:#1a4268!important;color:#ffffff!important}
      table.dataTable tbody td{padding:8px 10px!important;font-size:12px!important}
      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover{
        background:#0878f9!important;color:#fff!important;border:1px solid #1685ff!important
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover{
        background:#16416a!important;color:#fff!important;border:1px solid #47749b!important
      }
      .leaflet-container{background:#102943!important;border-radius:9px;min-height:300px}
      .shiny-plot-output{background:#071629;min-height:240px;width:100%!important}
      .shiny-plot-output img{max-width:100%}
      .tab-content{padding-top:8px}
      .form-control::placeholder{color:#d7e7f8!important;opacity:1}
      ::selection{background:#168bff;color:#ffffff}
      .panel-card{min-width:0;overflow:hidden}
      .metric-value,.metric-label,.metric-foot{color:#ffffff!important}
      .small-note{font-size:11px;color:#d2e4f8!important;line-height:1.5}
      .shiny-notification{background:#102943;color:#fff;border:1px solid #1685ff}
      @media(max-width:900px){.topbar{flex-wrap:wrap}.heading{font-size:20px}.live-pill{margin-left:0}}
    "))
  ),
  
  div(class = "topbar",
      div(class = "vit-mark", "VIT"),
      div(style = "min-width:160px;",
          div(class = "brand", "VIT"),
          div(class = "brand-sub", "Vellore Institute of Technology")),
      div(style = "flex:1;min-width:210px;margin-left:8px;",
          div(class = "heading", "Campus Mobility Dashboard"),
          div(class = "subheading", "Pedestrian & Vehicle Traffic Analytics")),
      div(class = "live-pill", span(class = "live-dot"), "Dashboard Active"),
      div(style = "font-size:12px;color:#d2e4f8;white-space:nowrap;",
          textOutput("clock", inline = TRUE), br(), textOutput("today", inline = TRUE))
  ),
  
  fluidRow(
    column(
      2,
      div(class = "side",
          h4("Data & Filters"),
          radioButtons("source", "Data source",
                       choices = c("Upload CSV" = "upload", "Use Demo Data" = "demo"),
                       selected = "demo"),
          conditionalPanel(
            condition = "input.source == 'upload'",
            fileInput("csv", "Upload traffic CSV", accept = c(".csv", "text/csv"))
          ),
          uiOutput("upload_status"),
          div(class = "small-note",
              "Supported fields: date, time/hour, location, pedestrians, vehicles, latitude, longitude and address."),
          tags$hr(style = "border-color:#1a3d5e;"),
          h4("Date Range"),
          dateRangeInput("dates", NULL, start = Sys.Date() - 6, end = Sys.Date(),
                         format = "yyyy-mm-dd", separator = " to "),
          h4("Campus Location"),
          uiOutput("location_ui"),
          h4("Day Type"),
          uiOutput("day_ui"),
          h4("Measure"),
          selectInput("measure", NULL,
                      choices = c("Both", "Pedestrians", "Vehicles"),
                      selected = "Both"),
          actionButton("apply", "Apply Filters", icon = icon("filter"),
                       class = "btn-primary", width = "100%"),
          br(), br(),
          actionButton("reset", "Reset Filters", icon = icon("refresh"),
                       class = "btn-default", width = "100%"),
          br(), br(),
          div(class = "small-note",
              "After changing filters, click Apply Filters. A newly uploaded CSV loads automatically.")
      ),
      div(class = "side",
          h4("Download Data"),
          downloadButton("download", "Download Filtered CSV",
                         class = "btn-success", width = "100%")
      ),
      div(class = "side",
          h4("Live Camera Monitoring"),
          p(class = "small-note",
            "This app does not connect to a physical camera or process live video."),
          tags$div(
            style = "border:1px dashed #275277;border-radius:8px;padding:20px;text-align:center;color:#a9c1dc;",
            icon("video-camera", "fa-2x"), br(), "Camera preview placeholder"
          )
      )
    ),
    
    column(
      10,
      fluidRow(
        column(2, div(class = "metric", style = "--accent:#16a8ff;",
                      span(class = "metric-icon", "♟"),
                      div(class = "metric-label", "Total Pedestrians"),
                      div(class = "metric-value", textOutput("ped_total", inline = TRUE)),
                      div(class = "metric-foot", "Across filtered records"))),
        column(2, div(class = "metric", style = "--accent:#ff5268;",
                      span(class = "metric-icon", "V"),
                      div(class = "metric-label", "Total Vehicles"),
                      div(class = "metric-value", textOutput("veh_total", inline = TRUE)),
                      div(class = "metric-foot", "Across filtered records"))),
        column(2, div(class = "metric", style = "--accent:#aa7cff;",
                      span(class = "metric-icon", "⌖"),
                      div(class = "metric-label", "Campus Locations"),
                      div(class = "metric-value", textOutput("loc_total", inline = TRUE)),
                      div(class = "metric-foot", "Distinct locations"))),
        column(3, div(class = "metric", style = "--accent:#ffb43b;",
                      span(class = "metric-icon", "P"),
                      div(class = "metric-label", "Highest Pedestrian Area"),
                      div(class = "metric-value", style = "font-size:16px;",
                          textOutput("top_ped", inline = TRUE)),
                      div(class = "metric-foot", textOutput("top_ped_value", inline = TRUE)))),
        column(3, div(class = "metric", style = "--accent:#00d99a;",
                      span(class = "metric-icon", "V"),
                      div(class = "metric-label", "Highest Vehicle Area"),
                      div(class = "metric-value", style = "font-size:16px;",
                          textOutput("top_veh", inline = TRUE)),
                      div(class = "metric-foot", textOutput("top_veh_value", inline = TRUE))))
      ),
      
      tabsetPanel(
        id = "tabs",
        tabPanel("Overview",
                 fluidRow(
                   column(8, div(class = "panel-card",
                                 div(class = "panel-title", "Campus Map: Traffic View"),
                                 leafletOutput("map_overview", height = "380px", width = "100%"))),
                   column(4, div(class = "panel-card",
                                 div(class = "panel-title", "Hourly Traffic Trend"),
                                 plotOutput("hourly", height = "300px")))
                 ),
                 fluidRow(
                   column(4, div(class = "panel-card",
                                 div(class = "panel-title", "Location-wise Traffic"),
                                 plotOutput("area", height = "280px"))),
                   column(4, div(class = "panel-card",
                                 div(class = "panel-title", "Campus Traffic Heatmap"),
                                 plotOutput("heat", height = "280px"))),
                   column(4, div(class = "panel-card",
                                 div(class = "panel-title", "Daily Trend"),
                                 plotOutput("daily", height = "280px")))
                 )
        ),
        tabPanel("Campus Map",
                 div(class = "panel-card",
                     div(class = "panel-title", "Interactive Traffic Map"),
                     p(class = "small-note",
                       "Switch between satellite, street and dark layers. Uploaded coordinates are used when valid."),
                     leafletOutput("map_full", height = "650px", width = "100%"))
        ),
        tabPanel("Area Analysis",
                 fluidRow(
                   column(6, div(class = "panel-card",
                                 div(class = "panel-title", "Pedestrians vs Vehicles"),
                                 plotOutput("scatter", height = "360px"))),
                   column(6, div(class = "panel-card",
                                 div(class = "panel-title", "Traffic by Location"),
                                 plotOutput("area_full", height = "360px")))
                 ),
                 div(class = "panel-card",
                     div(class = "panel-title", "Location Summary"),
                     DTOutput("summary_table"))
        ),
        tabPanel("Heatmaps",
                 fluidRow(
                   column(6, div(class = "panel-card",
                                 div(class = "panel-title", "Pedestrian Activity by Hour"),
                                 plotOutput("heat_ped", height = "400px"))),
                   column(6, div(class = "panel-card",
                                 div(class = "panel-title", "Vehicle Activity by Hour"),
                                 plotOutput("heat_veh", height = "400px")))
                 )
        ),
        tabPanel("Data",
                 div(class = "panel-card",
                     div(class = "panel-title", "Filtered CSV Records"),
                     DTOutput("data_table"))
        ),
        tabPanel("CCTV Monitor",
                 fluidRow(
                   column(4,
                          div(class = "panel-card",
                              div(class = "panel-title", "Camera Connection"),
                              p(class = "small-note",
                                "Use a browser-compatible stream URL. RTSP cameras usually need a secure gateway that converts RTSP to HLS or WebRTC before a browser can play the stream."),
                              textInput("camera_name", "Camera name / area",
                                        value = "Main Gate Camera"),
                              selectInput("camera_type", "Stream type",
                                          choices = c("MJPEG image stream" = "mjpeg",
                                                      "HLS stream (.m3u8)" = "hls",
                                                      "MP4 stream" = "mp4",
                                                      "RTSP camera (gateway required)" = "rtsp"),
                                          selected = "mjpeg"),
                              textInput("camera_url", "Stream URL",
                                        value = "", placeholder = "https://your-camera-host/stream"),
                              actionButton("connect_camera", "Load Camera Preview",
                                           icon = icon("video-camera"),
                                           class = "btn-primary", width = "100%"),
                              br(), br(),
                              uiOutput("camera_status")
                          )
                   ),
                   column(8,
                          div(class = "panel-card",
                              div(class = "panel-title", "Live Camera Preview"),
                              uiOutput("camera_preview"),
                              p(class = "small-note",
                                "This page displays a stream only when the URL is reachable from the browser and the camera server permits access. Use HTTPS for a dashboard hosted over HTTPS. Do not paste passwords or private access tokens into the URL.")
                          )
                   )
                 ),
                 div(class = "panel-card",
                     div(class = "panel-title", "Camera Setup Notes"),
                     tags$ul(
                       tags$li("MJPEG: commonly displayed as an image stream when the camera provides a browser-accessible MJPEG endpoint."),
                       tags$li("HLS: use a .m3u8 URL. Playback depends on browser support; Safari usually supports HLS natively."),
                       tags$li("MP4: requires a reachable video file or compatible live MP4 endpoint."),
                       tags$li("RTSP: browsers do not play RTSP directly. Configure a trusted gateway such as an HLS/WebRTC service, then use its browser-compatible URL here."),
                       tags$li("For a real deployment, protect the stream with authentication, network restrictions, HTTPS and the camera owner's permission.")
                     )
                 )
        ),
        tabPanel("Advanced Analytics",
                 fluidRow(
                   column(6, div(class = "panel-card",
                                 div(class = "panel-title", "Traffic Composition"),
                                 plotOutput("composition", height = "330px"))),
                   column(6, div(class = "panel-card",
                                 div(class = "panel-title", "Traffic Distribution by Day Type"),
                                 plotOutput("day_boxplot", height = "330px")))
                 ),
                 fluidRow(
                   column(6, div(class = "panel-card",
                                 div(class = "panel-title", "Weekday and Weekend Comparison"),
                                 plotOutput("day_comparison", height = "330px"))),
                   column(6, div(class = "panel-card",
                                 div(class = "panel-title", "Pedestrian-Vehicle Relationship"),
                                 plotOutput("relationship", height = "330px")))
                 )
        ),
        tabPanel("AI Traffic Monitoring",
                 fluidRow(
                   column(4,
                          div(class = "panel-card",
                              div(class = "panel-title", "Live Laptop Camera"),
                              p(class = "small-note", "Allow camera permission. AI detection requires Python and the Ultralytics YOLO model."),
                              tags$video(id = "ai_camera_video", autoplay = NA, muted = NA, playsinline = NA,
                                         style = "width:100%;max-height:260px;background:#000;border-radius:8px;"),
                              tags$canvas(id = "ai_camera_canvas", style = "display:none;"),
                              fluidRow(
                                column(6, actionButton("start_ai_camera", "Start Camera", class = "btn-primary", width = "100%")),
                                column(6, actionButton("stop_ai_camera", "Stop Camera", class = "btn-default", width = "100%"))
                              ),
                              br(),
                              checkboxInput("enable_ai_detection", "Enable AI detection", TRUE),
                              numericInput("people_limit", "People alert threshold", 20, min = 1, max = 10000),
                              numericInput("vehicle_limit", "Vehicle alert threshold", 10, min = 1, max = 10000),
                              numericInput("detection_interval", "Detection interval (seconds)", 2, min = 1, max = 10),
                              actionButton("manual_accident_alert", "Report Suspected Accident",
                                           icon = icon("exclamation-triangle"), class = "btn-danger", width = "100%"),
                              br(), br(), uiOutput("ai_status")
                          )
                   ),
                   column(8,
                          div(class = "panel-card",
                              div(class = "panel-title", "Detection Results"),
                              fluidRow(
                                column(4, div(class = "metric", style = "--accent:#168bff;",
                                              div(class = "metric-label", "People detected"),
                                              div(class = "metric-value", textOutput("ai_people", inline = TRUE)))),
                                column(4, div(class = "metric", style = "--accent:#ff8b1f;",
                                              div(class = "metric-label", "Vehicles detected"),
                                              div(class = "metric-value", textOutput("ai_vehicles", inline = TRUE)))),
                                column(4, div(class = "metric", style = "--accent:#ff5268;",
                                              div(class = "metric-label", "Latest alert"),
                                              div(class = "metric-value", style = "font-size:16px;",
                                                  textOutput("ai_latest_alert", inline = TRUE))))
                              ),
                              uiOutput("ai_alert_banner"),
                              p(class = "small-note", "Counts represent objects in the sampled frame, not unique people or total traffic flow."),
                              tags$hr(style = "border-color:#1a3d5e;"),
                              div(class = "panel-title", "Analyze a Recorded Video"),
                              fileInput("ai_video_file", "Upload MP4 / AVI video",
                                        accept = c(".mp4", ".avi", ".mov", ".mkv")),
                              actionButton("analyze_video", "Analyze Uploaded Video",
                                           icon = icon("play"), class = "btn-primary"),
                              br(), br(), verbatimTextOutput("video_analysis_status"),
                              DTOutput("video_results_table")
                          )
                   )
                 ),
                 div(class = "panel-card",
                     div(class = "panel-title", "Accident Alert Limitations"),
                     p("The overlap heuristic can flag possible person-vehicle or vehicle-vehicle overlap, but it cannot confirm a crash and may produce false alarms. Treat alerts as preliminary and verify the footage. Reliable automatic accident detection needs a dedicated video action-recognition model tested on accident footage.")
                 )
        ),
        tabPanel("Findings",
                 div(class = "panel-card",
                     div(class = "panel-title", "Automated Findings"),
                     uiOutput("findings"))
        )
      )
    )
  )
)

# 6. SERVER
server <- function(input, output, session) {
  
  output$clock <- renderText({
    invalidateLater(1000, session)
    format(Sys.time(), "%I:%M:%S %p")
  })
  output$today <- renderText(format(Sys.Date(), "%b %d, %Y"))
  
  source_data <- reactive({
    if (identical(input$source, "upload")) {
      req(input$csv)
      tryCatch(
        read.csv(input$csv$datapath, stringsAsFactors = FALSE,
                 check.names = FALSE, na.strings = c("", "NA", "N/A")),
        error = function(e) {
          showNotification(paste("Unable to read CSV:", conditionMessage(e)),
                           type = "error", duration = NULL)
          empty_data()
        }
      )
    } else {
      make_demo()
    }
  })
  
  cleaned <- reactive({
    d <- source_data()
    if (nrow(d) == 0) return(empty_data())
    tryCatch(
      clean_data(d),
      error = function(e) {
        showNotification(paste("CSV processing error:", conditionMessage(e)),
                         type = "error", duration = 8)
        empty_data()
      }
    )
  })
  
  output$upload_status <- renderUI({
    if (identical(input$source, "demo")) {
      return(tags$p(class = "small-note", "Using generated demo data. Counts are simulated."))
    }
    if (is.null(input$csv)) {
      return(tags$p(class = "small-note", "Please select a CSV file."))
    }
    d <- cleaned()
    if (nrow(d) == 0) {
      return(tags$p(style = "color:#ff7585;font-size:12px;",
                    "No valid records loaded. Check the date and traffic count columns."))
    }
    tags$div(
      style = "background:#0b352c;border:1px solid #087c59;padding:8px;border-radius:8px;font-size:11px;",
      tags$b("CSV loaded successfully"), tags$br(),
      paste("Rows:", format(nrow(d), big.mark = ",")), tags$br(),
      paste("Locations:", length(unique(d$location)))
    )
  })
  
  output$location_ui <- renderUI({
    d <- cleaned()
    locations <- if (nrow(d) > 0) sort(unique(d$location)) else character()
    selectInput("location", NULL,
                choices = c("All Locations", locations),
                selected = "All Locations")
  })
  
  output$day_ui <- renderUI({
    d <- cleaned()
    days <- if (nrow(d) > 0) sort(unique(d$day_type)) else character()
    selectInput("day", NULL, choices = c("All Days", days), selected = "All Days")
  })
  
  # Store the last applied filter settings. Uploading a new file resets them.
  filter_state <- reactiveVal(NULL)
  
  apply_filters_to_data <- function(d, state) {
    if (nrow(d) == 0 || is.null(state)) return(d)
    if (!is.null(state$dates) && length(state$dates) == 2 &&
        all(!is.na(state$dates))) {
      start_date <- as.Date(state$dates[1])
      end_date <- as.Date(state$dates[2])
      d <- d[!is.na(d$date) & d$date >= start_date & d$date <= end_date,
             , drop = FALSE]
    }
    if (!is.null(state$location) && state$location != "All Locations") {
      d <- d[d$location == state$location, , drop = FALSE]
    }
    if (!is.null(state$day) && state$day != "All Days") {
      d <- d[d$day_type == state$day, , drop = FALSE]
    }
    d
  }
  
  observeEvent(list(input$source, input$csv), {
    d <- cleaned()
    if (nrow(d) == 0) return()
    valid_dates <- d$date[!is.na(d$date)]
    if (length(valid_dates) > 0) {
      updateDateRangeInput(session, "dates",
                           start = min(valid_dates), end = max(valid_dates),
                           min = min(valid_dates), max = max(valid_dates))
    }
    updateSelectInput(session, "location", selected = "All Locations")
    updateSelectInput(session, "day", selected = "All Days")
    updateSelectInput(session, "measure", selected = "Both")
    filter_state(list(
      dates = if (length(valid_dates) > 0) c(min(valid_dates), max(valid_dates)) else NULL,
      location = "All Locations", day = "All Days"
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$apply, {
    filter_state(list(
      dates = input$dates,
      location = input$location,
      day = input$day
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$reset, {
    d <- cleaned()
    if (nrow(d) == 0) return()
    valid_dates <- d$date[!is.na(d$date)]
    if (length(valid_dates) > 0) {
      updateDateRangeInput(session, "dates",
                           start = min(valid_dates), end = max(valid_dates),
                           min = min(valid_dates), max = max(valid_dates))
    }
    updateSelectInput(session, "location", selected = "All Locations")
    updateSelectInput(session, "day", selected = "All Days")
    updateSelectInput(session, "measure", selected = "Both")
    filter_state(list(
      dates = if (length(valid_dates) > 0) c(min(valid_dates), max(valid_dates)) else NULL,
      location = "All Locations", day = "All Days"
    ))
  })
  
  current <- reactive({
    d <- cleaned()
    state <- filter_state()
    if (is.null(state)) {
      valid_dates <- d$date[!is.na(d$date)]
      state <- list(
        dates = if (length(valid_dates) > 0) c(min(valid_dates), max(valid_dates)) else NULL,
        location = "All Locations", day = "All Days"
      )
    }
    apply_filters_to_data(d, state)
  })
  
  # The measure selection controls all measures consistently in charts and KPI values.
  measure_data <- reactive({
    d <- current()
    if (input$measure == "Pedestrians") d$vehicles <- 0
    if (input$measure == "Vehicles") d$pedestrians <- 0
    d
  })
  
  output$ped_total <- renderText(fmt(measure_data()$pedestrians))
  output$veh_total <- renderText(fmt(measure_data()$vehicles))
  output$loc_total <- renderText(length(unique(current()$location)))
  
  top_location <- function(d, column) {
    if (nrow(d) == 0) return(NA_character_)
    sums <- d |>
      dplyr::group_by(location) |>
      dplyr::summarise(value = sum(.data[[column]], na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(value))
    if (nrow(sums) == 0) NA_character_ else sums$location[1]
  }
  
  output$top_ped <- renderText({
    loc <- top_location(current(), "pedestrians")
    if (is.na(loc)) "No data" else loc
  })
  output$top_veh <- renderText({
    loc <- top_location(current(), "vehicles")
    if (is.na(loc)) "No data" else loc
  })
  output$top_ped_value <- renderText({
    d <- current()
    if (nrow(d) == 0) return("0 pedestrians")
    loc <- top_location(d, "pedestrians")
    paste(fmt(d$pedestrians[d$location == loc]), "pedestrians")
  })
  output$top_veh_value <- renderText({
    d <- current()
    if (nrow(d) == 0) return("0 vehicles")
    loc <- top_location(d, "vehicles")
    paste(fmt(d$vehicles[d$location == loc]), "vehicles")
  })
  
  map_data <- reactive({
    d <- measure_data()
    if (nrow(d) == 0) return(data.frame())
    d |>
      dplyr::group_by(location) |>
      dplyr::summarise(
        pedestrians = sum(pedestrians, na.rm = TRUE),
        vehicles = sum(vehicles, na.rm = TRUE),
        latitude = if (all(is.na(latitude))) NA_real_ else mean(latitude, na.rm = TRUE),
        longitude = if (all(is.na(longitude))) NA_real_ else mean(longitude, na.rm = TRUE),
        address = dplyr::first(address),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        total = pedestrians + vehicles,
        level = dplyr::case_when(
          total >= stats::quantile(total, 0.75, na.rm = TRUE) ~ "High Traffic",
          total >= stats::quantile(total, 0.40, na.rm = TRUE) ~ "Medium Traffic",
          TRUE ~ "Low Traffic"
        )
      )
  })
  
  make_map <- function(output_id) {
    output[[output_id]] <- renderLeaflet({
      d <- map_data()
      m <- leaflet(options = leafletOptions(zoomControl = TRUE, preferCanvas = TRUE)) |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
        addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Street map") |>
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark map") |>
        addLayersControl(
          baseGroups = c("Satellite", "Street map", "Dark map"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
      if (nrow(d) == 0) {
        return(m |> addControl(
          html = "<div style='background:#071629;color:white;padding:10px;border-radius:8px;'>No records match the selected filters.</div>",
          position = "topright"
        ))
      }
      
      valid <- d[
        is.finite(d$latitude) & is.finite(d$longitude) &
          d$latitude >= -90 & d$latitude <= 90 &
          d$longitude >= -180 & d$longitude <= 180,
        , drop = FALSE
      ]
      if (nrow(valid) == 0) {
        return(m |> addControl(
          html = paste0(
            "<div style='background:#071629;color:white;padding:10px;border-radius:8px;max-width:280px;'>",
            "No valid coordinates found. Add latitude and longitude columns to your CSV. ",
            "Reference coordinates are supplied only for recognized VIT locations.</div>"
          ),
          position = "topright"
        ))
      }
      
      if (max(valid$longitude) == min(valid$longitude) &&
          max(valid$latitude) == min(valid$latitude)) {
        m <- m |> setView(lng = valid$longitude[1], lat = valid$latitude[1], zoom = 16)
      } else {
        m <- m |> fitBounds(
          min(valid$longitude), min(valid$latitude),
          max(valid$longitude), max(valid$latitude)
        )
      }
      
      valid$radius <- scales::rescale(valid$total, to = c(7, 18))
      valid$marker_color <- ifelse(
        valid$level == "High Traffic", "#ff3b52",
        ifelse(valid$level == "Medium Traffic", "#ff9d24", "#00d084")
      )
      
      m |> addCircleMarkers(
        data = valid, lng = ~longitude, lat = ~latitude,
        radius = ~radius, stroke = TRUE, weight = 1.5,
        color = ~marker_color, fillColor = ~marker_color, fillOpacity = 0.82,
        popup = ~paste0(
          "<b>", htmltools::htmlEscape(location), "</b><br>",
          "Pedestrians: ", scales::comma(pedestrians), "<br>",
          "Vehicles: ", scales::comma(vehicles), "<br>",
          "Traffic level: ", level
        )
      )
    })
  }
  make_map("map_overview")
  make_map("map_full")
  
  output$hourly <- renderPlot({
    d <- measure_data()
    if (nrow(d) == 0) return(empty_plot("Hourly Traffic Trend"))
    z <- d |>
      dplyr::group_by(hour) |>
      dplyr::summarise(
        Pedestrians = sum(pedestrians, na.rm = TRUE),
        Vehicles = sum(vehicles, na.rm = TRUE), .groups = "drop"
      ) |>
      tidyr::pivot_longer(-hour, names_to = "Measure", values_to = "Count")
    ggplot(z, aes(hour, Count, color = Measure, group = Measure)) +
      geom_line(linewidth = 1.2) + geom_point(size = 2) +
      scale_color_manual(values = c(Pedestrians = "#168bff", Vehicles = "#ff8b1f")) +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Hour of day", y = "Count", color = NULL) + dark_plot_theme()
  })
  
  make_location_plot <- function(d, title = "Location-wise Traffic") {
    if (nrow(d) == 0) return(empty_plot(title))
    z <- d |>
      dplyr::group_by(location) |>
      dplyr::summarise(
        Pedestrians = sum(pedestrians, na.rm = TRUE),
        Vehicles = sum(vehicles, na.rm = TRUE), .groups = "drop"
      ) |>
      tidyr::pivot_longer(-location, names_to = "Measure", values_to = "Count")
    ggplot(z, aes(x = reorder(location, Count), y = Count, fill = Measure)) +
      geom_col(position = position_dodge(width = 0.82), width = 0.72) +
      coord_flip() +
      scale_fill_manual(values = c(Pedestrians = "#168bff", Vehicles = "#ff8b1f")) +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.12))) +
      geom_text(aes(label = ifelse(Count > 0, scales::comma(Count), "")),
                position = position_dodge(width = 0.82), hjust = -0.08,
                size = 3, colour = "#ffffff") +
      labs(title = title, x = NULL, y = "Count", fill = NULL) + dark_plot_theme()
  }
  
  output$area <- renderPlot(make_location_plot(measure_data()))
  output$area_full <- renderPlot(make_location_plot(measure_data(), "Traffic by Location"))
  
  output$heat <- renderPlot({
    d <- measure_data()
    if (nrow(d) == 0) return(empty_plot("Campus Traffic Heatmap"))
    z <- d |>
      dplyr::group_by(location, hour) |>
      dplyr::summarise(Count = sum(pedestrians + vehicles, na.rm = TRUE), .groups = "drop")
    ggplot(z, aes(hour, location, fill = Count)) +
      geom_tile(color = "#102944") +
      scale_fill_gradientn(colours = c("#0752cf", "#00c9d7", "#b8f32f", "#ff9d24", "#ff3e48")) +
      scale_x_continuous(breaks = seq(0, 23, 4)) +
      labs(x = "Hour of day", y = NULL, fill = "Count") + dark_plot_theme()
  })
  
  output$daily <- renderPlot({
    d <- measure_data()
    if (nrow(d) == 0) return(empty_plot("Daily Trend"))
    z <- d |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        Pedestrians = sum(pedestrians, na.rm = TRUE),
        Vehicles = sum(vehicles, na.rm = TRUE), .groups = "drop"
      ) |>
      tidyr::pivot_longer(-date, names_to = "Measure", values_to = "Count")
    ggplot(z, aes(date, Count, color = Measure, group = Measure)) +
      geom_line(linewidth = 1.2) + geom_point(size = 2) +
      scale_color_manual(values = c(Pedestrians = "#168bff", Vehicles = "#ff8b1f")) +
      scale_x_date(date_labels = "%b %d") +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Count", color = NULL) + dark_plot_theme()
  })
  
  output$scatter <- renderPlot({
    d <- measure_data()
    if (nrow(d) == 0) return(empty_plot("Pedestrians vs Vehicles"))
    ggplot(d, aes(pedestrians, vehicles, color = day_type)) +
      geom_point(alpha = 0.8, size = 2.5) +
      scale_color_brewer(palette = "Set1") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Pedestrians per record", y = "Vehicles per record", color = "Day type") +
      dark_plot_theme()
  })
  
  make_heat <- function(output_id, measure) {
    output[[output_id]] <- renderPlot({
      d <- current()
      if (nrow(d) == 0) return(empty_plot(paste(tools::toTitleCase(measure), "Activity by Hour")))
      z <- d |>
        dplyr::group_by(location, hour) |>
        dplyr::summarise(Count = sum(.data[[measure]], na.rm = TRUE), .groups = "drop")
      colors <- if (measure == "pedestrians") {
        c("#083fc7", "#087cf7", "#00d5c7", "#e3f34a")
      } else {
        c("#5b1a85", "#dc426d", "#ffb347")
      }
      ggplot(z, aes(hour, location, fill = Count)) +
        geom_tile(color = "#102944") +
        scale_fill_gradientn(colours = colors, labels = scales::comma) +
        scale_x_continuous(breaks = seq(0, 23, 4)) +
        labs(x = "Hour", y = NULL, fill = "Count") + dark_plot_theme()
    })
  }
  make_heat("heat_ped", "pedestrians")
  make_heat("heat_veh", "vehicles")
  
  # AI monitoring requires Python packages: ultralytics, pillow, opencv-python, numpy.
  ai_counts <- reactiveVal(list(people = 0L, vehicles = 0L, detail = "Waiting for camera frame"))
  ai_latest <- reactiveVal("No alert")
  ai_alerts <- reactiveVal(data.frame(time = character(), alert = character(),
                                      detail = character(), stringsAsFactors = FALSE))
  video_results <- reactiveVal(data.frame())
  video_status <- reactiveVal("Upload a video, then click Analyze Uploaded Video.")
  ai_last_alert_time <- reactiveVal(as.POSIXct("2000-01-01", tz = "UTC"))
  ai_python_ready <- reactiveVal(FALSE)
  
  init_ai_python <- function() {
    if (isTRUE(ai_python_ready())) return(TRUE)
    if (!requireNamespace("reticulate", quietly = TRUE)) {
      stop("Install the R package reticulate with install.packages('reticulate').")
    }
    reticulate::py_run_string("
import base64, io
from PIL import Image
from ultralytics import YOLO
import numpy as np
_ai_model = YOLO('yolov8n.pt')
_ai_names = _ai_model.names
def _ai_detect_b64(data_url):
    raw = data_url.split(',', 1)[1] if ',' in data_url else data_url
    im = Image.open(io.BytesIO(base64.b64decode(raw))).convert('RGB')
    result = _ai_model.predict(np.array(im), imgsz=640, conf=0.35, verbose=False)[0]
    people, vehicles, detected = 0, 0, []
    possible_overlap = False
    vehicle_names = ['bicycle', 'car', 'motorcycle', 'bus', 'truck']
    if result.boxes is not None:
        boxes = result.boxes.xyxy.tolist()
        names = [_ai_names[int(c)] for c in result.boxes.cls.tolist()]
        for name in names:
            if name == 'person': people += 1
            if name in vehicle_names:
                vehicles += 1
                detected.append(name)
        # Box overlap is only a warning heuristic, not crash confirmation.
        for i in range(len(boxes)):
            for j in range(i + 1, len(boxes)):
                ni, nj = names[i], names[j]
                relevant = ((ni == 'person' and nj in vehicle_names) or
                            (nj == 'person' and ni in vehicle_names) or
                            (ni in vehicle_names and nj in vehicle_names))
                if not relevant: continue
                a, b = boxes[i], boxes[j]
                iw = max(0, min(a[2], b[2]) - max(a[0], b[0]))
                ih = max(0, min(a[3], b[3]) - max(a[1], b[1]))
                inter = iw * ih
                area_a = max(1, (a[2]-a[0]) * (a[3]-a[1]))
                area_b = max(1, (b[2]-b[0]) * (b[3]-b[1]))
                if inter / min(area_a, area_b) >= 0.30:
                    possible_overlap = True
    return {'people': int(people), 'vehicles': int(vehicles),
            'vehicle_types': ', '.join(sorted(set(detected))) if detected else 'none',
            'possible_overlap': bool(possible_overlap)}
def _ai_detect_file(video_path, sample_every=15):
    import cv2
    cap = cv2.VideoCapture(video_path)
    if not cap.isOpened(): raise RuntimeError('OpenCV could not open this video file.')
    fps = cap.get(cv2.CAP_PROP_FPS)
    if not fps or fps < 1: fps = 25
    step = max(1, int(fps * sample_every))
    frame_no, rows = 0, []
    while True:
        ok, frame = cap.read()
        if not ok: break
        if frame_no % step == 0:
            rgb = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
            result = _ai_model.predict(rgb, imgsz=640, conf=0.35, verbose=False)[0]
            people, vehicles = 0, 0
            if result.boxes is not None:
                for cls in result.boxes.cls.tolist():
                    name = _ai_names[int(cls)]
                    if name == 'person': people += 1
                    if name in ['bicycle', 'car', 'motorcycle', 'bus', 'truck']: vehicles += 1
            rows.append({'time_seconds': round(frame_no / fps, 1),
                         'people': int(people), 'vehicles': int(vehicles)})
        frame_no += 1
    cap.release()
    return rows
")
    ai_python_ready(TRUE)
    TRUE
  }
  
  append_ai_alert <- function(alert_text, detail_text) {
    now <- Sys.time()
    previous <- ai_last_alert_time()
    if (as.numeric(difftime(now, previous, units = "secs")) < 30 &&
        identical(ai_latest(), alert_text)) return(invisible(NULL))
    ai_alerts(dplyr::bind_rows(
      data.frame(time = format(now, "%Y-%m-%d %H:%M:%S"),
                 alert = alert_text, detail = detail_text, stringsAsFactors = FALSE),
      ai_alerts()
    ))
    ai_latest(alert_text)
    ai_last_alert_time(now)
    showNotification(paste(alert_text, detail_text, sep = ": "),
                     type = "warning", duration = 10)
    session$sendCustomMessage("ai-alert-sound", list(text = alert_text))
  }
  
  observeEvent(input$ai_camera_started, {
    ai_counts(list(people = 0L, vehicles = 0L, detail = "Camera started; waiting for detection"))
  })
  observeEvent(input$ai_camera_stopped, {
    ai_counts(list(people = 0L, vehicles = 0L, detail = "Camera stopped"))
  })
  
  observeEvent(input$ai_camera_frame, {
    if (!isTRUE(input$enable_ai_detection)) return()
    tryCatch({
      init_ai_python()
      detected <- reticulate::py$`_ai_detect_b64`(input$ai_camera_frame)
      people <- as.integer(detected$people)
      vehicles <- as.integer(detected$vehicles)
      ai_counts(list(people = people, vehicles = vehicles,
                     detail = paste("Vehicle types:", detected$vehicle_types)))
      if (isTRUE(detected$possible_overlap)) {
        append_ai_alert("POSSIBLE COLLISION / OBJECT OVERLAP",
                        "Overlapping person/vehicle or vehicle boxes detected. This may be normal traffic; verify the video.")
      }
      if (people >= input$people_limit) {
        append_ai_alert("PEOPLE THRESHOLD EXCEEDED",
                        paste(people, "people detected; configured limit:", input$people_limit))
      }
      if (vehicles >= input$vehicle_limit) {
        append_ai_alert("VEHICLE THRESHOLD EXCEEDED",
                        paste(vehicles, "vehicles detected; configured limit:", input$vehicle_limit))
      }
    }, error = function(e) {
      ai_counts(list(people = 0L, vehicles = 0L,
                     detail = paste("AI error:", conditionMessage(e))))
      showNotification(paste(
        "AI detection failed. Install Python packages ultralytics, pillow, opencv-python and numpy.",
        conditionMessage(e)), type = "error", duration = 12)
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$manual_accident_alert, {
    append_ai_alert("SUSPECTED ACCIDENT: MANUAL REPORT",
                    "A user reported a possible incident. Verify footage and contact emergency services if needed.")
  })
  
  output$ai_people <- renderText(as.character(ai_counts()$people))
  output$ai_vehicles <- renderText(as.character(ai_counts()$vehicles))
  output$ai_latest_alert <- renderText(ai_latest())
  output$ai_status <- renderUI({
    c <- ai_counts()
    tags$div(style = "padding:9px;border-radius:8px;background:#102943;border:1px solid #244c70;color:#eaf3ff;font-size:12px;",
             tags$b("Detection status: "), c$detail,
             tags$br(), tags$span(class = "small-note",
                                  "First startup may download the YOLO model and can take several minutes."))
  })
  output$ai_alert_banner <- renderUI({
    logs <- ai_alerts()
    if (nrow(logs) == 0) {
      return(tags$div(style = "padding:12px;border-radius:8px;background:#0b352c;border:1px solid #087c59;color:#d9ffec;",
                      "No alerts recorded yet."))
    }
    tags$div(style = "padding:12px;border-radius:8px;background:#49202a;border:1px solid #ff5268;color:#fff;",
             tags$b(logs$alert[1]), tags$br(), logs$detail[1], tags$br(), tags$small(logs$time[1]))
  })
  
  observeEvent(input$analyze_video, {
    req(input$ai_video_file)
    video_status("Starting video analysis. The first run may download the model.")
    video_results(data.frame())
    tryCatch({
      init_ai_python()
      result <- reticulate::py$`_ai_detect_file`(input$ai_video_file$datapath, 15L)
      result_df <- if (length(result) == 0) {
        data.frame(time_seconds = numeric(), people = integer(), vehicles = integer())
      } else {
        as.data.frame(do.call(rbind, lapply(result, function(row) {
          data.frame(time_seconds = as.numeric(row$time_seconds),
                     people = as.integer(row$people), vehicles = as.integer(row$vehicles))
        })))
      }
      video_results(result_df)
      if (nrow(result_df) == 0) {
        video_status("No frames were sampled. Check the video format.")
      } else {
        max_people <- max(result_df$people, na.rm = TRUE)
        max_vehicles <- max(result_df$vehicles, na.rm = TRUE)
        video_status(paste0("Analysis complete. Sampled ", nrow(result_df),
                            " frames. Peak people: ", max_people, ". Peak vehicles: ", max_vehicles, "."))
        if (max_people >= input$people_limit) {
          append_ai_alert("VIDEO: PEOPLE THRESHOLD EXCEEDED",
                          paste("Peak sampled count:", max_people))
        }
        if (max_vehicles >= input$vehicle_limit) {
          append_ai_alert("VIDEO: VEHICLE THRESHOLD EXCEEDED",
                          paste("Peak sampled count:", max_vehicles))
        }
      }
    }, error = function(e) {
      video_status(paste("Video analysis failed:", conditionMessage(e),
                         "| Install Python packages ultralytics, pillow, opencv-python and numpy."))
      showNotification(video_status(), type = "error", duration = 12)
    })
  })
  output$video_analysis_status <- renderText(video_status())
  output$video_results_table <- renderDT({
    datatable(video_results(), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  
  # CCTV preview: the app does not discover cameras automatically.
  # The user must supply a reachable browser-compatible stream URL.
  camera_connected <- reactiveVal(FALSE)
  
  observeEvent(input$connect_camera, {
    url <- if (is.null(input$camera_url)) "" else trimws(input$camera_url)
    if (identical(input$camera_type, "rtsp")) {
      camera_connected(FALSE)
      showNotification(
        "RTSP cannot play directly in a browser. Convert it to HLS or WebRTC using a trusted camera gateway, then enter that URL.",
        type = "warning", duration = 8
      )
    } else if (!nzchar(url) || !grepl("^https?://", url, ignore.case = TRUE)) {
      camera_connected(FALSE)
      showNotification(
        "Enter a valid http:// or https:// stream URL.",
        type = "error", duration = 8
      )
    } else {
      camera_connected(TRUE)
      showNotification(
        "Preview requested. If it stays blank, check the stream URL, camera permissions, network access and browser compatibility.",
        type = "message", duration = 6
      )
    }
  })
  
  output$camera_status <- renderUI({
    if (!isTRUE(camera_connected())) {
      return(tags$p(class = "small-note",
                    "Status: not connected. Enter a stream URL and select Load Camera Preview."))
    }
    tags$p(style = "color:#7ff0b0;font-size:12px;",
           paste("Preview requested for:", input$camera_name))
  })
  
  output$camera_preview <- renderUI({
    if (!isTRUE(camera_connected())) {
      return(tags$div(
        style = "min-height:300px;display:flex;align-items:center;justify-content:center;border:1px dashed #275277;border-radius:10px;color:#a9c1dc;text-align:center;padding:24px;",
        tags$div(icon("video-camera", "fa-3x"), tags$br(),
                 "Camera preview will appear here after you enter a supported stream URL.")
      ))
    }
    
    url <- trimws(input$camera_url)
    camera_name <- htmltools::htmlEscape(if (is.null(input$camera_name) || !nzchar(input$camera_name)) "Camera" else input$camera_name)
    if (identical(input$camera_type, "mjpeg")) {
      tags$div(
        tags$h4(camera_name, style = "color:#eaf3ff;"),
        tags$img(src = url, alt = "Live MJPEG camera stream",
                 style = "width:100%;max-height:520px;object-fit:contain;background:#000;border-radius:8px;")
      )
    } else if (identical(input$camera_type, "hls")) {
      tags$div(
        tags$h4(camera_name, style = "color:#eaf3ff;"),
        tags$video(src = url, controls = NA, autoplay = NA, muted = NA,
                   style = "width:100%;max-height:520px;background:#000;border-radius:8px;")
      )
    } else if (identical(input$camera_type, "mp4")) {
      tags$div(
        tags$h4(camera_name, style = "color:#eaf3ff;"),
        tags$video(src = url, controls = NA,
                   style = "width:100%;max-height:520px;background:#000;border-radius:8px;")
      )
    } else {
      tags$p(class = "small-note",
             "RTSP needs a gateway. Enter the gateway's HLS or WebRTC-compatible URL instead.")
    }
  })
  
  # Advanced visualizations use the same active filters as the rest of the dashboard.
  output$composition <- renderPlot({
    d <- measure_data()
    if (nrow(d) == 0) return(empty_plot("Traffic Composition"))
    z <- data.frame(
      Measure = c("Pedestrians", "Vehicles"),
      Count = c(sum(d$pedestrians, na.rm = TRUE),
                sum(d$vehicles, na.rm = TRUE))
    )
    z <- z[z$Count > 0, , drop = FALSE]
    if (nrow(z) == 0) return(empty_plot("Traffic Composition"))
    ggplot(z, aes(x = "", y = Count, fill = Measure)) +
      geom_col(width = 1, colour = "#071629") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(scales::comma(Count), " (",
                                   scales::percent(Count / sum(Count), accuracy = 0.1), ")")),
                position = position_stack(vjust = 0.5), colour = "white", size = 4) +
      scale_fill_manual(values = c(Pedestrians = "#168bff", Vehicles = "#ff8b1f")) +
      labs(x = NULL, y = NULL, fill = NULL) +
      dark_plot_theme() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid = element_blank())
  })
  
  output$day_boxplot <- renderPlot({
    d <- current()
    if (nrow(d) == 0) return(empty_plot("Traffic Distribution by Day Type"))
    z <- d |>
      dplyr::mutate(Total_Traffic = pedestrians + vehicles)
    ggplot(z, aes(x = day_type, y = Total_Traffic, fill = day_type)) +
      geom_boxplot(alpha = 0.9, outlier.colour = "#ffb347") +
      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Day type", y = "Pedestrians + vehicles", fill = "Day type") +
      dark_plot_theme()
  })
  
  output$day_comparison <- renderPlot({
    d <- current()
    if (nrow(d) == 0) return(empty_plot("Weekday and Weekend Comparison"))
    z <- d |>
      dplyr::group_by(day_type) |>
      dplyr::summarise(
        Pedestrians = sum(pedestrians, na.rm = TRUE),
        Vehicles = sum(vehicles, na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(-day_type, names_to = "Measure", values_to = "Count")
    ggplot(z, aes(x = day_type, y = Count, fill = Measure)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = scales::comma(Count)),
                position = position_dodge(width = 0.8),
                vjust = -0.3, colour = "white", size = 3) +
      scale_fill_manual(values = c(Pedestrians = "#168bff", Vehicles = "#ff8b1f")) +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Day type", y = "Total count", fill = NULL) +
      dark_plot_theme()
  })
  
  output$relationship <- renderPlot({
    d <- current()
    if (nrow(d) == 0) return(empty_plot("Pedestrian-Vehicle Relationship"))
    ggplot(d, aes(x = pedestrians, y = vehicles, colour = day_type)) +
      geom_point(alpha = 0.65, size = 2) +
      geom_smooth(method = "lm", se = TRUE, colour = "#ffffff",
                  linewidth = 0.8, linetype = "dashed") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      scale_colour_brewer(palette = "Set1") +
      labs(x = "Pedestrians per record", y = "Vehicles per record", colour = "Day type") +
      dark_plot_theme()
  })
  
  summary_data <- reactive({
    d <- measure_data()
    if (nrow(d) == 0) return(data.frame())
    d |>
      dplyr::group_by(location) |>
      dplyr::summarise(
        Pedestrians = sum(pedestrians, na.rm = TRUE),
        Vehicles = sum(vehicles, na.rm = TRUE),
        Records = dplyr::n(),
        Avg_Pedestrians = round(mean(pedestrians, na.rm = TRUE), 1),
        Avg_Vehicles = round(mean(vehicles, na.rm = TRUE), 1),
        Traffic_Score = round(mean(pedestrians + vehicles, na.rm = TRUE), 1),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(Pedestrians + Vehicles))
  })
  
  output$summary_table <- renderDT({
    datatable(summary_data(), options = list(pageLength = 8, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$data_table <- renderDT({
    d <- current()
    if (nrow(d) == 0) {
      return(datatable(data.frame(Message = "No records match the selected filters."),
                       options = list(searching = FALSE, paging = FALSE), rownames = FALSE))
    }
    datatable(
      d |> dplyr::select(date, time, hour, location, day_type, pedestrians,
                         vehicles, latitude, longitude, address),
      options = list(pageLength = 12, scrollX = TRUE),
      rownames = FALSE, filter = "top"
    )
  })
  
  output$findings <- renderUI({
    d <- measure_data()
    if (nrow(d) == 0) return(tags$p("No records match the current filters."))
    
    byloc <- d |>
      dplyr::group_by(location) |>
      dplyr::summarise(
        ped = sum(pedestrians, na.rm = TRUE),
        veh = sum(vehicles, na.rm = TRUE), .groups = "drop"
      )
    peakp <- byloc[which.max(byloc$ped), , drop = FALSE]
    peakv <- byloc[which.max(byloc$veh), , drop = FALSE]
    peak_hour <- d |>
      dplyr::group_by(hour) |>
      dplyr::summarise(total = sum(pedestrians + vehicles, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(total))
    
    tags$div(
      tags$p(tags$b("Records analyzed: "), format(nrow(d), big.mark = ",")),
      tags$p(tags$b("Busiest pedestrian location: "), peakp$location, " (",
             format(round(peakp$ped), big.mark = ","), " pedestrians)."),
      tags$p(tags$b("Busiest vehicle location: "), peakv$location, " (",
             format(round(peakv$veh), big.mark = ","), " vehicles)."),
      tags$p(tags$b("Peak traffic hour: "), sprintf("%02d:00", peak_hour$hour[1]), "."),
      tags$p(tags$b("Average pedestrians per record: "),
             round(mean(d$pedestrians, na.rm = TRUE), 1)),
      tags$p(tags$b("Average vehicles per record: "),
             round(mean(d$vehicles, na.rm = TRUE), 1)),
      tags$p(class = "small-note",
             "Counts are only as reliable as the uploaded data. Demo data is simulated. Map accuracy depends on valid coordinates; reference points are used only for recognized VIT locations.")
    )
  })
  
  output$download <- downloadHandler(
    filename = function() paste0("VIT_Campus_Traffic_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(current(), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui = ui, server = server)

