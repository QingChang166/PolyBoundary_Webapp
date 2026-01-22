library(tidyverse)
library(shiny)
library(leaflet)
library(sf)
library(tigris)
library(shinymanager)
library(RPostgres) # Required for Supabase/Postgres connection
library(DBI)      # Sometimes required for database interaction functions
library(dbplyr)
# ============================================
# DATABASE CONNECTION CONFIGURATION
# ============================================
# We use a function to get a fresh connection for each transaction
#get_db_conn <- function() {
#  dbConnect(
#    Postgres(),
#    dbname   = "postgres",
#    host     = Sys.getenv("DB_HOST"),
#    port     = as.integer(Sys.getenv("DB_PORT")),
#    user     = Sys.getenv("DB_USER"),
#    password = Sys.getenv("DB_PASSWORD")
#  )
#}

# ============================================
# DATABASE CONNECTION CONFIGURATION
# ============================================

get_db_conn <- function() {
  # RECOMMENDED: Use Supabase Connection Pooler for cloud deployments
  # This resolves IPv6/network issues with Posit Connect Cloud
  
  pooler_host <- Sys.getenv("DB_POOLER_HOST", "aws-0-us-east-1.pooler.supabase.com")
  
  dbConnect(
    Postgres(),
    dbname   = Sys.getenv("DB_NAME", "postgres"),
    host     = pooler_host,  # Use pooler, not direct connection
    port     = as.integer(Sys.getenv("DB_PORT", "6543")),  # Pooler port
    user     = Sys.getenv("DB_USER", "postgres.cvzzrocsuglhumcqqykp"),
    password = Sys.getenv("DB_PASSWORD"),
    sslmode  = "require"  # Important for Supabase
  )
}

# ============================================
# Initialize credentials table if needed
# ============================================
initialize_credentials <- function() {
  tryCatch({
    con <- get_db_conn()
    on.exit(dbDisconnect(con))
    
    # Check if credentials table exists
    if (!dbExistsTable(con, "credentials")) {
      # Create credentials table
      create_db(
        credentials_data = data.frame(
          user = c("admin"),
          password = c("admin123"), # Change this!
          admin = c(TRUE),
          stringsAsFactors = FALSE
        ),
        sqlite_path = ":memory:", # Temporary, will be replaced
        passphrase = Sys.getenv("SHINY_MANAGER_PASSPHRASE", "default_passphrase")
      )
      
      # Then manually create in Postgres
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS credentials (
          user TEXT PRIMARY KEY,
          password TEXT,
          admin BOOLEAN,
          expire TEXT,
          applications TEXT
        )
      ")
      
      # Hash password (you should use bcrypt in production)
      dbExecute(con, "
        INSERT INTO credentials (user, password, admin)
        VALUES ('admin', '$2a$10$...', TRUE)
        ON CONFLICT (user) DO NOTHING
      ")
    }
    
    # Create responses table if not exists
    if (!dbExistsTable(con, "responses")) {
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS responses (
          unique_id INTEGER PRIMARY KEY,
          user_answer TEXT,
          note TEXT,
          timestamp TIMESTAMP,
          username TEXT,
          country TEXT,
          algo_decision TEXT,
          openai_decision TEXT
        )
      ")
    }
  }, error = function(e) {
    warning("Could not initialize database: ", e$message)
  })
}

# Initialize on startup
initialize_credentials()

# ============================================
# Load local RDS data
# ============================================
summary_table <- read_rds("boundary_change_verification_summary_results.rds")
gaul_geom <- read_rds("gaul_geometries_for_helpapp.rds")
gadm_geom <- read_rds("gadm_geometries_for_helpapp.rds")

summary_table <- summary_table %>%
  mutate(unique_id = row_number())

gaul_geom <- gaul_geom %>%
  st_transform(crs = 4326) %>% 
  st_make_valid()

gadm_geom <- gadm_geom %>%
  st_transform(crs = 4326) %>% 
  st_make_valid()

# ============================================
# UI
# ============================================
ui <- fluidPage(
  tags$style(HTML("
  #map { height: calc(100vh - 90px) !important; }
  .pf-btn { font-size: 10px !important; padding: 3px 6px !important; }
  .well { padding: 10px 12px; }
  .well h5 { font-size: 13px; margin: 6px 0; }
  .well p  { font-size: 12px; margin: 0 0 6px 0; line-height: 1.25; }
  .well hr { margin: 10px 0; }
  .well textarea { font-size: 10px; line-height: 1.25; }
  #agent_info ul { margin: 0 0 6px 16px; padding: 0; }
  #agent_info li { font-size: 11px; line-height: 1.2; margin-bottom: 4px; }
  
  .progress-bar-container { background-color: #e9ecef; border-radius: 4px; height: 20px; margin-bottom: 10px; overflow: hidden; }
  .progress-bar-fill { background-color: #28a745; height: 100%; transition: width 0.3s ease; display: flex; align-items: center; justify-content: center; color: white; font-size: 11px; font-weight: bold; }
  .progress-text { font-size: 12px; color: #666; margin-bottom: 5px; }
  .case-indicator { display: inline-block; padding: 2px 8px; border-radius: 3px; font-size: 11px; font-weight: bold; }
  .case-reviewed { background-color: #d4edda; color: #155724; }
  .case-current { background-color: #fff3cd; color: #856404; }
  .badge { display: inline-block; padding: 3px 8px; border-radius: 3px; font-weight: bold; }
  .badge-success { background-color: #28a745; color: white; }
  .badge-danger { background-color: #dc3545; color: white; }
  .badge-warning { background-color: #ffc107; color: #212529; }
  .badge-secondary { background-color: #6c757d; color: white; }
  .unanswered-warning { background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 10px; margin: 10px 0; border-radius: 4px; }
  .unanswered-warning-text { font-size: 12px; color: #856404; font-weight: bold; margin: 0; }
  .save-status { padding: 5px; margin: 5px 0; border-radius: 3px; font-size: 10px; text-align: center; }
  .save-success { background-color: #d4edda; color: #155724; }
  .save-error { background-color: #f8d7da; color: #721c24; }
  .connection-status { padding: 8px; margin: 10px 0; border-radius: 4px; font-size: 11px; text-align: center; }
  .status-connected { background-color: #d4edda; color: #155724; }
  .status-disconnected { background-color: #f8d7da; color: #721c24; }
")),
  
  titlePanel("Information Panel"),
  
  fluidRow(
    column(12, uiOutput("connection_status"), style = "padding: 5px 15px;")
  ),
  
  fluidRow(
    column(12, uiOutput("progress_info"), style = "padding: 5px 15px;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 5,
      h5("Predictions from AI and Algorithm"),
      uiOutput("agent_info"),
      hr(), 
      h5("Visualize Boundaries"),
      fluidRow(
        column(4, actionButton("before_only", "Before Only", width = "100%", class = "btn-sm pf-btn")),
        column(4, actionButton("after_only",  "After Only",  width = "100%", class = "btn-sm pf-btn")),
        column(4, actionButton("show_both",   "Both",        width = "100%", class = "btn-sm pf-btn"))
      ),
      hr(), 
      h5("Is the algorithm's answer correct?"),
      fluidRow(
        column(4, actionButton("correct",  "Yes",      width = "100%", class = "btn-sm pf-btn btn-outline-secondary")),
        column(4, actionButton("wrong",    "No",       width = "100%", class = "btn-sm pf-btn btn-outline-secondary")),
        column(4, actionButton("not_sure", "Not sure", width = "100%", class = "btn-sm pf-btn btn-outline-secondary"))
      ),
      uiOutput("save_status"),
      uiOutput("previous_answer_info"),
      hr(),
      h5("Your Note"),
      textAreaInput("note", label = NULL, value = "", placeholder = "Optional note...", width = "100%", height = "90px"),
      actionButton("save_note", "Save note", width = "100%", class = "btn-sm pf-btn btn-outline-secondary"),
      hr(),
      h5("Navigate to..."),
      fluidRow(
        column(6, actionButton("prev", "Previous", class = "btn-sm pf-btn",  width = "100%")),
        column(6, actionButton("next_butt", "Next", class = "btn-sm pf-btn",  width = "100%"))
      ), 
      hr(),
      h5("Export"),
      downloadButton("download_labels", "Download CSV", class = "btn-sm pf-btn")
    ),
    mainPanel(width = 7, leafletOutput("map"))
  )
)

# Secure UI
ui <- secure_app(ui, enable_admin = TRUE)

# ============================================
# SERVER
# ============================================
server <- function(input, output, session) {
  
  # Authentication - use SQLite database with hashed passwords
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "database.sqlite",
      passphrase = Sys.getenv("SHINY_MANAGER_PASSPHRASE")
    )
  )
  
  # Reactive state
  n <- nrow(summary_table)
  idx <- reactiveVal(1)
  mode <- reactiveVal("both")
  save_status <- reactiveVal("")
  reviewed_cases <- reactiveVal(numeric())
  db_cache <- reactiveVal(data.frame())
  just_answered <- reactiveVal(NULL)
  connection_ok <- reactiveVal(FALSE)
  
  # Initial Load from Supabase
  isolate({
    tryCatch({
      existing_data <- get_all_responses()
      
      if (!is.null(existing_data) && nrow(existing_data) > 0) {
        db_cache(existing_data)
        reviewed_ids <- as.numeric(existing_data$unique_id)
        reviewed_cases(reviewed_ids)
        
        unreviewed_ids <- setdiff(summary_table$unique_id, reviewed_ids)
        if (length(unreviewed_ids) > 0) {
          idx(which(summary_table$unique_id == min(unreviewed_ids))[1])
        }
        
        connection_ok(TRUE)
        showNotification("✓ Connected to Supabase", type = "message", duration = 3)
      } else {
        connection_ok(TRUE)
        showNotification("No existing data found", type = "message", duration = 3)
      }
      
    }, error = function(e) {
      connection_ok(FALSE)
      showNotification(
        paste("⚠️ Cannot connect to Supabase:", e$message, 
              "\nPlease check your SUPABASE_URL and SUPABASE_ANON_KEY environment variables."), 
        type = "error",
        duration = 10
      )
    })
  })
  
  # Connection status indicator
  output$connection_status <- renderUI({
    if (connection_ok()) {
      div(class = "connection-status status-connected", "✓ Connected to Supabase")
    } else {
      div(class = "connection-status status-disconnected", 
          "⚠️ Not connected to Supabase - data will not be saved")
    }
  })
  
  # Upsert function using REST API
  upsert_label <- function(answer = NA_character_, note = NA_character_) {
    row <- summary_table[idx(), , drop = FALSE]
    uid <- row$unique_id[1]
    current_user <- reactiveValuesToList(res_auth)$user %||% "unknown"
    
    current_cache <- db_cache()
    existing_row <- current_cache[current_cache$unique_id == uid, ]
    
    if (nrow(existing_row) > 0) {
      if (is.na(answer)) answer <- existing_row$user_answer[1]
      if (is.na(note))   note   <- existing_row$note[1]
    }
    
    success <- upsert_response(
      unique_id = uid,
      user_answer = answer,
      note = note,
      username = current_user,
      country = as.character(row$iso3c_prev[1]),
      algo_decision = as.character(row$type[1]),
      openai_decision = as.character(row$openai_assessment[1])
    )
    
    if (success) {
      save_status(paste("✓ Saved to Supabase", format(Sys.time(), "%H:%M:%S")))
      just_answered(uid)
      
      # Refresh cache
      new_data <- get_all_responses()
      if (!is.null(new_data) && nrow(new_data) > 0) {
        db_cache(new_data)
        reviewed_cases(as.numeric(new_data$unique_id))
      }
    } else {
      save_status("✗ Failed to save - check connection")
    }
  }
  
  output$save_status <- renderUI({
    status <- save_status()
    if (status == "") return(NULL)
    class_name <- if (grepl("^✓", status)) "save-success" else "save-error"
    div(class = paste("save-status", class_name), status)
  })
  
  output$progress_info <- renderUI({
    current_uid <- summary_table[idx(), ]$unique_id
    reviewed <- reviewed_cases()
    total_cases <- nrow(summary_table)
    progress_pct <- round((length(reviewed) / total_cases) * 100, 1)
    is_reviewed <- current_uid %in% reviewed
    
    tagList(
      div(class = "progress-text",
          paste0("Progress: ", length(reviewed), " / ", total_cases, " cases (", progress_pct, "%)"),
          span(class = paste("case-indicator", if(is_reviewed) "case-reviewed" else "case-current"),
               style = "margin-left: 10px;", if(is_reviewed) "✓ Reviewed" else "○ Not reviewed")
      ),
      div(class = "progress-bar-container", 
          div(class = "progress-bar-fill", 
              style = paste0("width: ", progress_pct, "%;"), 
              if(progress_pct > 10) paste0(progress_pct, "%") else ""))
    )
  })
  
  gaul_poly <- reactive({
    row <- summary_table[idx(), ]
    ids <- unlist(strsplit(as.character(row$old_id), ",\\s*"))
    gaul_geom %>% filter(change_level == row$change_level, old_id %in% ids)
  })
  
  gadm_poly <- reactive({
    row <- summary_table[idx(), ]
    ids <- unlist(strsplit(as.character(row$new_id), ",\\s*"))
    gadm_geom %>% filter(change_level == row$change_level, new_id %in% ids)
  })
  
  output$agent_info <- renderUI({
    infor <- summary_table[idx(), ]
    tagList(
      p(paste0("Algorithm: ", infor$type)),
      p(paste0("OpenAI Assessment: ", infor$openai_assessment)),
      p("Explanation:"),
      tags$ul(lapply(unlist(strsplit(as.character(infor$openai_explanation), "\\n|•")), 
                     function(x) if(nzchar(trimws(x))) tags$li(x)))
    )
  })
  
  observeEvent(input$before_only, { mode("before") })
  observeEvent(input$after_only,  { mode("after")  })
  observeEvent(input$show_both,   { mode("both")   })
  
  output$map <- renderLeaflet({ 
    leaflet() %>% 
      addTiles(
        urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
        attribution = 'Google',
        group = "base"
      )
  })
  
  observe({
    gaul <- gaul_poly(); gadm <- gadm_poly(); m <- mode()
    map_proxy <- leafletProxy("map") %>% clearShapes()
    
    if (m %in% c("before", "both") && nrow(gaul) > 0) {
      map_proxy <- map_proxy %>% addPolygons(data = gaul, color = "green", weight = 2, label = ~name_prev)
    }
    if (m %in% c("after", "both") && nrow(gadm) > 0) {
      map_proxy <- map_proxy %>% addPolygons(data = gadm, color = "red", weight = 2, label = ~name_curr)
    }
    
    all_bounds <- rbind(if(nrow(gaul)>0) st_bbox(gaul) else NULL, 
                        if(nrow(gadm)>0) st_bbox(gadm) else NULL)
    if (!is.null(all_bounds)) {
      map_proxy %>% fitBounds(min(all_bounds[,1]), min(all_bounds[,2]), 
                              max(all_bounds[,3]), max(all_bounds[,4]))
    }
  })
  
  # Navigation with skip warning
  observeEvent(input$prev, {
    current_uid <- summary_table[idx(), ]$unique_id
    cached <- db_cache()
    current_answer <- cached[cached$unique_id == current_uid, ]
    
    # Warn if current case is unanswered
    if (nrow(current_answer) == 0 || is.na(current_answer$user_answer[1]) || 
        current_answer$user_answer[1] == "") {
      showNotification(
        "⚠️ You haven't answered this case yet!",
        type = "warning",
        duration = 3
      )
    }
    
    i <- idx() - 1
    if(i < 1) i <- n
    idx(i)
    save_status("")
    just_answered(NULL)
  })
  
  observeEvent(input$next_butt, {
    current_uid <- summary_table[idx(), ]$unique_id
    cached <- db_cache()
    current_answer <- cached[cached$unique_id == current_uid, ]
    
    # Warn if current case is unanswered
    if (nrow(current_answer) == 0 || is.na(current_answer$user_answer[1]) || 
        current_answer$user_answer[1] == "") {
      showNotification(
        "⚠️ You haven't answered this case yet!",
        type = "warning",
        duration = 3
      )
    }
    
    i <- idx() + 1
    if(i > n) i <- 1
    idx(i)
    save_status("")
    just_answered(NULL)
  })
  
  observeEvent(input$correct,  { upsert_label(answer = "Yes", note = input$note) })
  observeEvent(input$wrong,    { upsert_label(answer = "No", note = input$note) })
  observeEvent(input$not_sure, { upsert_label(answer = "Not Sure", note = input$note) })
  observeEvent(input$save_note, { upsert_label(note = input$note) })
  
  observe({
    uid <- summary_table[idx(), ]$unique_id
    cached <- db_cache()
    existing <- cached[cached$unique_id == uid, ]
    
    note_val <- if(nrow(existing) > 0) existing$note[1] else ""
    updateTextAreaInput(session, "note", value = note_val)
  })
  
  output$previous_answer_info <- renderUI({
    uid <- summary_table[idx(), ]$unique_id
    if (!is.null(just_answered()) && just_answered() == uid) return(NULL)
    
    cached <- db_cache()
    prev <- cached[cached$unique_id == uid, ]
    
    if (nrow(prev) > 0 && !is.na(prev$user_answer[1]) && prev$user_answer[1] != "") {
      ans_class <- switch(prev$user_answer[1], 
                          "Yes"="badge-success", 
                          "No"="badge-danger", 
                          "Not Sure"="badge-warning", 
                          "badge-secondary")
      
      # Format timestamp
      timestamp_text <- ""
      if (!is.na(prev$timestamp[1]) && prev$timestamp[1] != "") {
        tryCatch({
          ts <- as.POSIXct(prev$timestamp[1])
          timestamp_text <- paste0(" • ", format(ts, "%Y-%m-%d %H:%M"))
        }, error = function(e) {
          timestamp_text <- ""
        })
      }
      
      div(style = "padding: 8px; background: #f8f9fa; border-left: 3px solid #007bff;",
          p(style="font-size:11px; font-weight:bold; margin-bottom: 5px;", "Previous Review:"),
          span(class=paste("badge", ans_class), prev$user_answer[1]),
          span(style="font-size:10px; color:#666;", 
               paste0(" by ", prev$username[1], timestamp_text))
      )
    } else {
      div(class="unanswered-warning", 
          p(class="unanswered-warning-text", "⚠️ Needs review"))
    }
  })
  
  output$download_labels <- downloadHandler(
    filename = function() paste0("user_labels_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      data <- get_all_responses()
      if (!is.null(data) && nrow(data) > 0) {
        write.csv(data, file, row.names = FALSE)
      } else {
        write.csv(data.frame(message = "No data available"), file, row.names = FALSE)
      }
    }
  )
}


shinyApp(ui, server)