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

get_db_conn <- function() {
 
  pooler_host <- Sys.getenv("DB_POOLER_HOST")
  
  dbConnect(
    Postgres(),
    dbname   = Sys.getenv("DB_NAME"),
    host     = pooler_host,  # Use pooler, not direct connection
    port     = as.integer(Sys.getenv("DB_PORT")),  # Pooler port
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    sslmode  = "require"  # Important for Supabase
  )
}


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
  
  .leaflet-tooltip { 
    background-color: rgba(255, 255, 255, 0.95); 
    border: 1px solid #999; 
    border-radius: 3px; 
    padding: 3px 6px; 
    font-size: 11px; 
    font-weight: bold;
    box-shadow: 0 1px 3px rgba(0,0,0,0.3);
  }
  
  .leaflet-tooltip.before-label { border-left: 3px solid green; }
  .leaflet-tooltip.after-label { border-left: 3px solid red; }
  
  #change_year, #change_month { height: 28px !important; font-size: 10px !important; padding: 2px 6px !important; }
  .form-group label { font-size: 10px !important; font-weight: bold !important; margin-bottom: 2px !important; }
  .form-group { margin-bottom: 8px !important; }
")),
  
  titlePanel("Information Panel"),
  
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
      uiOutput("date_inputs"),
      uiOutput("save_status"),
      uiOutput("previous_answer_info"),
      hr(),
      h5("Your Note"),
      textAreaInput("note", label = NULL, value = "", placeholder = "Optional note...", width = "100%", height = "90px"),
      actionButton("save_note", "Save Note & Date", width = "100%", class = "btn-sm pf-btn btn-outline-secondary"),
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


ui <- secure_app(ui, enable_admin = T)

# ============================================
# SERVER
# ============================================
server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "credentials.sqlite",
      passphrase = Sys.getenv("SHINY_MANAGER_PASSPHRASE")
    )
  )
  
  # Reactive state
  n <- nrow(summary_table)
  idx <- reactiveVal(1)
  mode <- reactiveVal("before")  # CHANGED: Default to "before" instead of "both"
  save_status <- reactiveVal("")
  reviewed_cases <- reactiveVal(numeric())
  db_cache <- reactiveVal(data.frame())
  just_answered <- reactiveVal(NULL)
  show_date_fields <- reactiveVal(FALSE)  # NEW: Track if date fields should be shown
  current_answer <- reactiveVal("")  # NEW: Track the current answer selection
  
  # Initial Load from Database
  isolate({
    tryCatch({
      con <- get_db_conn()
      on.exit(dbDisconnect(con))
      existing_data <- dbGetQuery(con, "SELECT * FROM responses")
      
      if (nrow(existing_data) > 0) {
        db_cache(existing_data)
        reviewed_ids <- as.numeric(existing_data$unique_id)
        reviewed_cases(reviewed_ids)
        
        unreviewed_ids <- setdiff(summary_table$unique_id, reviewed_ids)
        if (length(unreviewed_ids) > 0) {
          idx(which(summary_table$unique_id == min(unreviewed_ids))[1])
        }
      }
    }, error = function(e) {
      showNotification("Database connection failed or table empty", type = "error")
    })
  })
  
  # Database Insert Function - Always create new records
  insert_label <- function(answer = NA_character_, note = NA_character_, year = NA_character_, month = NA_character_) {
    row <- summary_table[idx(), , drop = FALSE]
    uid <- row$unique_id[1]
    current_user <- reactiveValuesToList(res_auth)$user %||% "unknown"
    
    # Get existing values if not provided
    current_cache <- db_cache()
    existing_row <- current_cache[current_cache$unique_id == uid, ]
    
    if (nrow(existing_row) > 0) {
      # Get the most recent entry for this unique_id
      existing_row <- existing_row[order(existing_row$timestamp, decreasing = TRUE), ][1, ]
      
      if (is.na(answer)) answer <- existing_row$user_answer[1]
      if (is.na(note))   note   <- existing_row$note[1]
      if (is.na(year))   year   <- if("change_year" %in% names(existing_row)) existing_row$change_year[1] else NA_character_
      if (is.na(month))  month  <- if("change_month" %in% names(existing_row)) existing_row$change_month[1] else NA_character_
    }
    
    con <- get_db_conn()
    on.exit(dbDisconnect(con))
    
    # Changed to INSERT only - no conflict resolution
    query <- "
      INSERT INTO responses (unique_id, user_answer, note, timestamp, username, country, algo_decision, openai_decision, change_year, change_month)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10);
    "
    
    tryCatch({
      dbExecute(con, query, list(
        uid, 
        ifelse(is.na(answer), "", as.character(answer)), 
        ifelse(is.na(note), "", as.character(note)), 
        as.character(Sys.time()), 
        current_user, 
        as.character(row$iso3c_prev[1]), 
        as.character(row$type[1]), 
        as.character(row$openai_assessment[1]),
        ifelse(is.na(year), "", as.character(year)),
        ifelse(is.na(month), "", as.character(month))
      ))
      
      save_status(paste("✓ Database Updated", format(Sys.time(), "%H:%M:%S")))
      just_answered(uid)
      
      # Reload all data from database
      new_data <- dbGetQuery(con, "SELECT * FROM responses")
      db_cache(new_data)
      reviewed_cases(unique(as.numeric(new_data$unique_id)))
      
    }, error = function(e) {
      save_status(paste("✗ Error:", substr(e$message, 1, 50)))
    })
  }
  
  output$save_status <- renderUI({
    status <- save_status()
    if (status == "") return(NULL)
    class_name <- if (grepl("^✓", status)) "save-success" else "save-error"
    div(class = paste("save-status", class_name), status)
  })
  
  # NEW: Render date input fields when answer is "Yes"
  output$date_inputs <- renderUI({
    if (show_date_fields()) {
      uid <- summary_table[idx(), ]$unique_id
      cached <- db_cache()
      existing <- cached[cached$unique_id == uid, ]
      
      year_val <- if(nrow(existing) > 0 && "change_year" %in% names(existing)) existing$change_year[1] else ""
      month_val <- if(nrow(existing) > 0 && "change_month" %in% names(existing)) existing$change_month[1] else ""
      
      div(
        style = "margin-top: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 4px;",
        fluidRow(
          column(6, 
                 textInput("change_year", "Year", value = year_val, 
                           placeholder = "e.g., 2020", width = "100%")
          ),
          column(6, 
                 textInput("change_month", "Month", value = month_val, 
                           placeholder = "1-12", width = "100%")
          )
        )
      )
    } else {
      NULL
    }
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
      div(class = "progress-bar-container", div(class = "progress-bar-fill", style = paste0("width: ", progress_pct, "%;"), if(progress_pct > 10) paste0(progress_pct, "%") else ""))
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
  
  
  # Display AI info
  output$agent_info <- renderUI({
    infor <- summary_table[idx(), , drop = FALSE]
    gaul <- gaul_poly()
    gadm <- gadm_poly()
    
    country <- if ("country" %in% names(infor)) infor$country[1] else infor$iso3c_prev[1]
    algo_decision <- infor$type[1]
    openai_decision <- infor$openai_assessment[1]
    openai_explanation <- infor$openai_explanation[1]
    openai_source <- infor$openai_sources[1]
    
    # Get polygon names
    before_names <- if(nrow(gaul) > 0) {
      paste(gaul$name_prev, collapse = ", ")
    } else {
      "N/A"
    }
    
    after_names <- if(nrow(gadm) > 0) {
      paste(gadm$name_curr, collapse = ", ")
    } else {
      "N/A"
    }
    
    split_bullets <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x)) return(character(0))
      x <- as.character(x)
      items <- unlist(strsplit(x, "\\r?\\n|•", perl = TRUE))
      items <- unlist(strsplit(paste(items, collapse = "\n"), "\\r?\\n", perl = TRUE))
      items <- trimws(items)
      items <- items[nzchar(items)]
      items <- sub("^[-*]+\\s*", "", items)
      items
    }
    
    split_lines_as_bullets <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x)) return(character(0))
      x <- as.character(x)
      items <- unlist(strsplit(x, "\\r?\\n", perl = TRUE))
      if (length(items) <= 1 && grepl("•", x, fixed = TRUE)) {
        items <- unlist(strsplit(x, "•", fixed = TRUE))
      }
      items <- trimws(items)
      items <- items[nzchar(items)]
      items <- sub("^[-*•]+\\s*", "", items)
      items
    }
    
    expl_items <- split_bullets(openai_explanation)
    src_items  <- split_lines_as_bullets(openai_source)
    
    tagList(
      p(paste0("Algorithm prediction: ", algo_decision)),
      p(paste0("Country: ", country)),
      p(HTML(paste0("<strong>Before:</strong> ", before_names))),
      p(HTML(paste0("<strong>After:</strong> ", after_names))),
      p(paste0("OpenAI assessment: ", openai_decision)),
      
      p("OpenAI explanation:"),
      if (length(expl_items) > 0) tags$ul(lapply(expl_items, tags$li)) else p("(none)"),
      
      p("OpenAI sources:"),
      if (length(src_items) > 0) tagList(lapply(src_items, function(s) p(paste0("• ", s)))) else p("(none)")
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
  
  # Update map
  observe({
    m <- mode()
    gaul <- gaul_poly()
    gadm <- gadm_poly()
    
    if (nrow(gaul) > 0) gaul <- sf::st_transform(gaul, 4326)
    if (nrow(gadm) > 0) gadm <- sf::st_transform(gadm, 4326)
    
    show_gaul <- (m %in% c("before", "both")) && nrow(gaul) > 0
    show_gadm <- (m %in% c("after",  "both")) && nrow(gadm) > 0
    
    bbs <- list()
    if (show_gaul) bbs <- c(bbs, list(sf::st_bbox(gaul)))
    if (show_gadm) bbs <- c(bbs, list(sf::st_bbox(gadm)))
    if (length(bbs) == 0) return(invisible(NULL))
    
    bb <- c(
      xmin = min(vapply(bbs, `[[`, numeric(1), "xmin")),
      ymin = min(vapply(bbs, `[[`, numeric(1), "ymin")),
      xmax = max(vapply(bbs, `[[`, numeric(1), "xmax")),
      ymax = max(vapply(bbs, `[[`, numeric(1), "ymax"))
    )
    
    gaul_label_col <- "name_prev"
    gadm_label_col <- "name_curr"
    
    map_proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers()
    
    # CHANGED: Remove permanent labels, keep hover-only labels
    if (show_gaul) {
      gaul$label_txt <- as.character(gaul[[gaul_label_col]])
      
      map_proxy <- map_proxy %>% 
        addPolygons(
          data = gaul, 
          color = "green", 
          weight = 2, 
          opacity = 1, 
          fill = FALSE,
          label = ~label_txt,
          highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE)
        )
    }
    
    # CHANGED: Remove permanent labels, keep hover-only labels
    if (show_gadm) {
      gadm$label_txt <- as.character(gadm[[gadm_label_col]])
      
      map_proxy <- map_proxy %>% 
        addPolygons(
          data = gadm, 
          color = "red", 
          weight = 2, 
          opacity = 1, 
          fill = FALSE,
          label = ~label_txt,
          highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE)
        )
    }
    
    map_proxy %>% fitBounds(
      unname(bb["xmin"]), unname(bb["ymin"]),
      unname(bb["xmax"]), unname(bb["ymax"])
    )
  })
  
  
  # Navigation with warning for unanswered cases
  observeEvent(input$prev, {
    current_uid <- summary_table[idx(), ]$unique_id
    
    # Check if current case has been answered
    if (!(current_uid %in% reviewed_cases())) {
      showModal(modalDialog(
        title = "⚠️ Unanswered Case",
        "You haven't answered this case yet. Are you sure you want to navigate away?",
        footer = tagList(
          actionButton("cancel_nav_prev", "Stay Here", class = "btn-primary"),
          actionButton("confirm_nav_prev", "Leave Anyway", class = "btn-secondary")
        ),
        easyClose = TRUE
      ))
    } else {
      # Navigate normally
      i <- idx() - 1
      if (i < 1) i <- n
      idx(i)
      save_status("")
      just_answered(NULL)  # Clear the "just answered" flag when navigating
    }
  })
  
  observeEvent(input$next_butt, {
    current_uid <- summary_table[idx(), ]$unique_id
    
    # Check if current case has been answered
    if (!(current_uid %in% reviewed_cases())) {
      showModal(modalDialog(
        title = "⚠️ Unanswered Case",
        "You haven't answered this case yet. Are you sure you want to navigate away?",
        footer = tagList(
          actionButton("cancel_nav_next", "Stay Here", class = "btn-primary"),
          actionButton("confirm_nav_next", "Leave Anyway", class = "btn-secondary")
        ),
        easyClose = TRUE
      ))
    } else {
      # Navigate normally
      i <- idx() + 1
      if (i > n) i <- 1
      idx(i)
      save_status("")
      just_answered(NULL)  # Clear the "just answered" flag when navigating
    }
  })
  
  # Handle modal responses for Previous
  observeEvent(input$cancel_nav_prev, {
    removeModal()
  })
  
  observeEvent(input$confirm_nav_prev, {
    removeModal()
    i <- idx() - 1
    if (i < 1) i <- n
    idx(i)
    save_status("")
    just_answered(NULL)  # Clear the "just answered" flag when navigating
  })
  
  # Handle modal responses for Next
  observeEvent(input$cancel_nav_next, {
    removeModal()
  })
  
  observeEvent(input$confirm_nav_next, {
    removeModal()
    i <- idx() + 1
    if (i > n) i <- 1
    idx(i)
    save_status("")
    just_answered(NULL)  # Clear the "just answered" flag when navigating
  })
  
  
  observeEvent(input$correct,  { 
    current_answer("Yes")
    show_date_fields(TRUE)  # Show date fields when user clicks "Yes"
  })
  observeEvent(input$wrong,    { 
    current_answer("No")
    show_date_fields(FALSE)  # Hide date fields for other answers
  })
  observeEvent(input$not_sure, { 
    current_answer("Not Sure")
    show_date_fields(FALSE)  # Hide date fields for other answers
  })
  
  # CHANGED: Single save button now saves everything
  observeEvent(input$save_note, { 
    year_input <- if(show_date_fields()) input$change_year else NA_character_
    month_input <- if(show_date_fields()) input$change_month else NA_character_
    insert_label(answer = current_answer(), note = input$note, year = year_input, month = month_input)
  })
  
  observe({
    uid <- summary_table[idx(), ]$unique_id
    cached <- db_cache()
    existing <- cached[cached$unique_id == uid, ]
    
    # Get the most recent entry for this unique_id
    if(nrow(existing) > 0) {
      existing <- existing[order(existing$timestamp, decreasing = TRUE), ][1, ]
      note_val <- existing$note[1]
      answer_val <- existing$user_answer[1]
      
      # Set the current answer
      current_answer(answer_val)
      
      # Show date fields if most recent answer was "Yes"
      if(answer_val == "Yes") {
        show_date_fields(TRUE)
        
        # Update year and month fields with most recent values
        year_val <- if("change_year" %in% names(existing) && !is.na(existing$change_year[1]) && existing$change_year[1] != "") {
          existing$change_year[1]
        } else {
          ""
        }
        
        month_val <- if("change_month" %in% names(existing) && !is.na(existing$change_month[1]) && existing$change_month[1] != "") {
          existing$change_month[1]
        } else {
          ""
        }
        
        updateTextInput(session, "change_year", value = year_val)
        updateTextInput(session, "change_month", value = month_val)
      } else {
        show_date_fields(FALSE)
      }
    } else {
      note_val <- ""
      current_answer("")
      show_date_fields(FALSE)
    }
    
    updateTextAreaInput(session, "note", value = note_val)
  })
  
  # CHANGED: Removed date info from Most Recent Review display
  output$previous_answer_info <- renderUI({
    uid <- summary_table[idx(), ]$unique_id
    if (!is.null(just_answered()) && just_answered() == uid) return(NULL)
    
    cached <- db_cache()
    prev <- cached[cached$unique_id == uid, ]
    
    if (nrow(prev) > 0) {
      # Get the most recent entry
      prev <- prev[order(prev$timestamp, decreasing = TRUE), ][1, ]
      
      ans_class <- switch(prev$user_answer[1], "Yes"="badge-success", "No"="badge-danger", "Not Sure"="badge-warning", "badge-secondary")
      
      div(style = "padding: 8px; background: #f8f9fa; border-left: 3px solid #007bff;",
          p(style="font-size:11px; font-weight:bold;", "Most Recent Review:"),
          span(class=paste("badge", ans_class), prev$user_answer[1]),
          span(style="font-size:10px; color:#666;", paste0(" by ", prev$username[1], " at ", format(as.POSIXct(prev$timestamp[1]), "%Y-%m-%d %H:%M")))
      )
    } else {
      div(class="unanswered-warning", p(class="unanswered-warning-text", "⚠️ Needs review"))
    }
  })
  
  output$download_labels <- downloadHandler(
    filename = function() paste0("user_labels_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      con <- get_db_conn()
      data <- dbGetQuery(con, "SELECT * FROM responses")
      dbDisconnect(con)
      write.csv(data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)