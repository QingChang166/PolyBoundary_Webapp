library(tidyverse)
library(shiny)
library(leaflet)
library(sf)
library(tigris)
library(shinymanager)
library(googlesheets4)
library(gargle)
library(jsonlite)
# ============================================
# CONFIGURATION
# ============================================
#GOOGLE_SHEET_URL <- Sys.getenv("GOOGLE_SHEET_URL", "YOUR_GOOGLE_SHEET_URL_HERE")
GOOGLE_SHEET_URL <- "https://docs.google.com/spreadsheets/d/1AjHdUdg_2u6Z7N_cVrU-xViIollf-SoWb1VsnwpNB1c/edit?gid=0#gid=0"  

SERVICE_ACCOUNT_KEY <- "~/Library/CloudStorage/Dropbox/PolBoundaryKG/R_Files/Help_App/service-account-key.json"

# Extract Sheet ID from URL
extract_sheet_id <- function(url) {
  # Extract ID from URL like: https://docs.google.com/spreadsheets/d/SHEET_ID/edit
  # Pattern matches alphanumeric, hyphens, and underscores
  id <- gsub(".*/spreadsheets/d/([a-zA-Z0-9_-]+).*", "\\1", url)
  return(id)
}

SHEET_ID <- extract_sheet_id(GOOGLE_SHEET_URL)
message("Google Sheet ID: ", SHEET_ID)

# ============================================
# Direct Google Sheets API Functions
# ============================================

# Global variable to store OAuth token
google_token <- NULL

# Authenticate and get OAuth token
setup_google_auth <- function() {
  if (file.exists(SERVICE_ACCOUNT_KEY)) {
    tryCatch({
      json_content <- jsonlite::fromJSON(SERVICE_ACCOUNT_KEY)
      message("Authenticating with service account: ", json_content$client_email)
      message("Project ID: ", json_content$project_id)
      
      # Verify all required fields exist
      required_fields <- c("type", "project_id", "private_key_id", "private_key", 
                           "client_email", "client_id", "token_uri")
      missing_fields <- setdiff(required_fields, names(json_content))
      
      if (length(missing_fields) > 0) {
        stop("Missing required fields in JSON: ", paste(missing_fields, collapse = ", "))
      }
      
      # Use jose package for JWT creation (more reliable than httr for service accounts)
      # Create JWT manually
      header <- list(
        alg = "RS256",
        typ = "JWT"
      )
      
      now <- as.numeric(Sys.time())
      claim <- list(
        iss = json_content$client_email,
        scope = paste(c(
          "https://www.googleapis.com/auth/spreadsheets",
          "https://www.googleapis.com/auth/drive"
        ), collapse = " "),
        aud = json_content$token_uri,
        exp = now + 3600,  # 1 hour from now
        iat = now
      )
      
      # Encode header and claim
      header_json <- jsonlite::toJSON(header, auto_unbox = TRUE)
      claim_json <- jsonlite::toJSON(claim, auto_unbox = TRUE)
      
      header_b64 <- base64enc::base64encode(charToRaw(header_json))
      header_b64 <- gsub("=+$", "", gsub("/", "_", gsub("\\+", "-", header_b64)))
      
      claim_b64 <- base64enc::base64encode(charToRaw(claim_json))
      claim_b64 <- gsub("=+$", "", gsub("/", "_", gsub("\\+", "-", claim_b64)))
      
      # Create signature input
      signature_input <- paste(header_b64, claim_b64, sep = ".")
      
      # Sign with private key
      private_key <- openssl::read_key(json_content$private_key)
      signature_raw <- openssl::signature_create(charToRaw(signature_input), 
                                                 openssl::sha256, 
                                                 private_key)
      signature_b64 <- base64enc::base64encode(signature_raw)
      signature_b64 <- gsub("=+$", "", gsub("/", "_", gsub("\\+", "-", signature_b64)))
      
      # Create JWT
      jwt <- paste(signature_input, signature_b64, sep = ".")
      
      # Exchange JWT for access token
      response <- httr::POST(
        json_content$token_uri,
        body = list(
          grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
          assertion = jwt
        ),
        encode = "form"
      )
      
      if (httr::status_code(response) != 200) {
        content <- httr::content(response, "text", encoding = "UTF-8")
        stop("Token exchange failed (", httr::status_code(response), "): ", content)
      }
      
      token_data <- httr::content(response, "parsed")
      
      # Create httr token object
      google_token <<- httr::Token2.0$new(
        app = httr::oauth_app("google", key = "", secret = ""),
        endpoint = httr::oauth_endpoints("google"),
        credentials = list(access_token = token_data$access_token),
        params = list(scope = claim$scope),
        cache_path = FALSE
      )
      
      message("✓ Authentication successful")
      message("  Access token obtained (expires in ", token_data$expires_in, " seconds)")
      return(TRUE)
      
    }, error = function(e) {
      message("✗ Authentication failed: ", e$message)
      google_token <<- NULL
      return(FALSE)
    })
  } else {
    message("✗ service-account-key.json not found")
    return(FALSE)
  }
}

# Read data from sheet using direct API
read_sheet_direct <- function(sheet_id, range = "Sheet1") {
  tryCatch({
    url <- sprintf(
      "https://sheets.googleapis.com/v4/spreadsheets/%s/values/%s",
      sheet_id, range
    )
    
    if (!is.null(google_token)) {
      response <- httr::GET(url, httr::config(token = google_token))
    } else {
      response <- httr::GET(url)
    }
    
    if (httr::status_code(response) != 200) {
      message("✗ Read failed with status: ", httr::status_code(response))
      return(NULL)
    }
    
    content <- httr::content(response, "parsed")
    
    if (is.null(content$values) || length(content$values) == 0) {
      # Empty sheet, return empty data frame with headers
      return(data.frame(
        unique_id = character(),
        user_answer = character(),
        note = character(),
        timestamp = character(),
        username = character(),
        country = character(),
        algo_decision = character(),
        openai_decision = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    # First row is headers
    headers <- unlist(content$values[[1]])
    
    if (length(content$values) == 1) {
      # Only headers, no data
      return(data.frame(
        unique_id = character(),
        user_answer = character(),
        note = character(),
        timestamp = character(),
        username = character(),
        country = character(),
        algo_decision = character(),
        openai_decision = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    # Convert to data frame
    data_rows <- content$values[-1]  # Skip header row
    
    df <- do.call(rbind, lapply(data_rows, function(row) {
      # Pad with empty strings if row is shorter than headers
      while (length(row) < length(headers)) {
        row <- c(row, "")
      }
      as.data.frame(t(row), stringsAsFactors = FALSE)
    }))
    
    colnames(df) <- headers
    
    return(df)
    
  }, error = function(e) {
    message("✗ read_sheet_direct error: ", e$message)
    return(NULL)
  })
}

# Write entire sheet using direct API (overwrites all data)
write_sheet_direct <- function(sheet_id, data, range = "Sheet1") {
  tryCatch({
    # Convert data frame to values array
    headers <- list(colnames(data))
    data_rows <- lapply(1:nrow(data), function(i) {
      as.list(as.character(data[i, ]))
    })
    
    values <- c(headers, data_rows)
    
    body <- list(
      range = range,
      majorDimension = "ROWS",
      values = values
    )
    
    # Clear existing data first
    clear_url <- sprintf(
      "https://sheets.googleapis.com/v4/spreadsheets/%s/values/%s:clear",
      sheet_id, range
    )
    
    if (!is.null(google_token)) {
      httr::POST(clear_url, httr::config(token = google_token), body = list(), encode = "json")
    }
    
    # Write new data
    update_url <- sprintf(
      "https://sheets.googleapis.com/v4/spreadsheets/%s/values/%s?valueInputOption=RAW",
      sheet_id, range
    )
    
    if (!is.null(google_token)) {
      response <- httr::PUT(
        update_url,
        httr::config(token = google_token),
        httr::content_type_json(),
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "json"
      )
    } else {
      response <- httr::PUT(
        update_url,
        httr::content_type_json(),
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "json"
      )
    }
    
    if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
      message("✓ Successfully wrote data to sheet")
      return(TRUE)
    } else {
      message("✗ Write failed with status: ", httr::status_code(response))
      message("  Response: ", httr::content(response, "text", encoding = "UTF-8"))
      return(FALSE)
    }
    
  }, error = function(e) {
    message("✗ write_sheet_direct error: ", e$message)
    return(FALSE)
  })
}

# Initialize authentication
message("\n=== Starting Google Sheets Setup ===")
auth_success <- setup_google_auth()

if (!auth_success) {
  stop("Failed to authenticate with Google Sheets. Please check:\n",
       "1. service-account-key.json exists in app directory\n",
       "2. Service account has access to the Google Sheet\n",
       "3. Google Sheets API is enabled in Google Cloud project")
}

message("=== Google Sheets Setup Complete ===\n")

# ============================================
# Load data
# ============================================
# Check if running locally or on server
data_path <- if (dir.exists("data")) "data" else "."

summary_table <- read_rds(file.path(data_path, "boundary_change_verification_summary_results.rds"))
gaul_geom <- read_rds(file.path(data_path, "gaul_geometries_for_helpapp.rds"))
gadm_geom <- read_rds(file.path(data_path, "gadm_geometries_for_helpapp.rds"))

summary_table <- summary_table %>%
  mutate(unique_id = row_number())

gaul_geom <- gaul_geom %>%
  st_transform(crs = 4326) %>% 
  st_make_valid()

gadm_geom <- gadm_geom %>%
  st_transform(crs = 4326) %>% 
  st_make_valid()

# ============================================
# Create credentials database
# ============================================
# This creates the credentials database on first run
# After first run, credentials.sqlite will exist and this section is skipped

if (!file.exists("credentials.sqlite")) {
  message("Creating credentials database for first time...")
  
  # Get passphrase from environment variable (REQUIRED)
  passphrase <- Sys.getenv("SHINY_MANAGER_PASSPHRASE")
  if (identical(passphrase, "")) {
    stop("ERROR: SHINY_MANAGER_PASSPHRASE environment variable is not set. Please create a .Renviron file.")
  }
  
  # Get admin password from environment variable (REQUIRED)
  default_password <- Sys.getenv("DEFAULT_ADMIN_PASSWORD")
  if (identical(default_password, "")) {
    stop("ERROR: DEFAULT_ADMIN_PASSWORD environment variable is not set. Please add it to your .Renviron file.")
  }
  
  credentials <- data.frame(
    user = c("admin"),
    password = c(default_password),
    admin = c(TRUE),
    stringsAsFactors = FALSE
  )
  
  create_db(
    credentials_data = credentials,
    sqlite_path = "credentials.sqlite",
    passphrase = passphrase
  )
  
  message("✓ Credentials database created successfully")
} else {
  message("✓ Using existing credentials database")
}

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
  
  .progress-bar-container {
    background-color: #e9ecef;
    border-radius: 4px;
    height: 20px;
    margin-bottom: 10px;
    overflow: hidden;
  }
  .progress-bar-fill {
    background-color: #28a745;
    height: 100%;
    transition: width 0.3s ease;
    display: flex;
    align-items: center;
    justify-content: center;
    color: white;
    font-size: 11px;
    font-weight: bold;
  }
  .progress-text {
    font-size: 12px;
    color: #666;
    margin-bottom: 5px;
  }
  .case-indicator {
    display: inline-block;
    padding: 2px 8px;
    border-radius: 3px;
    font-size: 11px;
    font-weight: bold;
  }
  .case-reviewed {
    background-color: #d4edda;
    color: #155724;
  }
  .case-current {
    background-color: #fff3cd;
    color: #856404;
  }
  .badge {
    display: inline-block;
    padding: 3px 8px;
    border-radius: 3px;
    font-weight: bold;
  }
  .badge-success {
    background-color: #28a745;
    color: white;
  }
  .badge-danger {
    background-color: #dc3545;
    color: white;
  }
  .badge-warning {
    background-color: #ffc107;
    color: #212529;
  }
  .badge-secondary {
    background-color: #6c757d;
    color: white;
  }
  .unanswered-warning {
    background-color: #fff3cd;
    border-left: 4px solid #ffc107;
    padding: 10px;
    margin: 10px 0;
    border-radius: 4px;
  }
  .unanswered-warning-text {
    font-size: 12px;
    color: #856404;
    font-weight: bold;
    margin: 0;
  }
  
  .save-status { 
    padding: 5px; 
    margin: 5px 0; 
    border-radius: 3px; 
    font-size: 10px;
    text-align: center;
  }
  .save-success { background-color: #d4edda; color: #155724; }
  .save-error { background-color: #f8d7da; color: #721c24; }
")),
  
  titlePanel("Information Panel"),
  
  # Progress indicator
  fluidRow(
    column(12, 
           uiOutput("progress_info"),
           style = "padding: 5px 15px;"
    )
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
      
      # Previous answer indicator
      uiOutput("previous_answer_info"),
      
      hr(),
      
      h5("Your Note (DON'T FORGET TO SAVE YOUR NOTE!)"),
      textAreaInput(
        "note",
        label = NULL,
        value = "",
        placeholder = "Write a short note about what you found (optional). Click save note button to save",
        width = "100%",
        height = "90px"
      ),
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
    
    mainPanel(
      width = 7,
      leafletOutput("map")
    )
  )
)

ui <- secure_app(ui, enable_admin = TRUE)

# ============================================
# SERVER
# ============================================
server <- function(input, output, session) {
  
  # Password authentication
  passphrase <- Sys.getenv("SHINY_MANAGER_PASSPHRASE")
  if (identical(passphrase, "")) stop("Missing env var: SHINY_MANAGER_PASSPHRASE")
  
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "credentials.sqlite",
      passphrase = passphrase
    )
  )
  
  # App state
  n <- nrow(summary_table)
  idx <- reactiveVal(1)
  mode <- reactiveVal("both")
  save_status <- reactiveVal("")
  
  # Initialize to first unreviewed case
  # (This is now handled in the isolate block above where reviewed_cases is initialized)
  
  user_labels <- reactiveVal(
    data.frame(
      unique_id = integer(),
      user_answer = character(),
      note = character(),
      timestamp = as.POSIXct(character()),
      username = character(),
      country = character(),
      algo_decision = character(),
      openai_decision = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Track reviewed cases from Google Sheets
  reviewed_cases <- reactiveVal(numeric())
  sheet_data_cache <- reactiveVal(data.frame())
  just_answered <- reactiveVal(NULL)
  
  # Load reviewed cases and set starting position on startup
  isolate({
    tryCatch({
      # Read existing data from Google Sheets using direct API
      existing_data <- read_sheet_direct(SHEET_ID)
      
      if (!is.null(existing_data) && nrow(existing_data) > 0 && "unique_id" %in% names(existing_data)) {
        # Cache the sheet data
        sheet_data_cache(existing_data)
        
        # Get list of reviewed unique_ids
        reviewed_ids <- as.numeric(existing_data$unique_id)
        reviewed_cases(reviewed_ids)
        
        # Find first case that hasn't been reviewed
        all_ids <- summary_table$unique_id
        unreviewed_ids <- setdiff(all_ids, reviewed_ids)
        
        if (length(unreviewed_ids) > 0) {
          first_unreviewed <- min(unreviewed_ids)
          first_unreviewed_idx <- which(summary_table$unique_id == first_unreviewed)[1]
          idx(first_unreviewed_idx)
          
          showNotification(
            paste0("Starting at first unreviewed case (#", first_unreviewed_idx, ")"),
            type = "message",
            duration = 4
          )
        } else {
          showNotification(
            "All cases have been reviewed! Starting from beginning.",
            type = "message",
            duration = 4
          )
        }
      } else {
        showNotification(
          "Starting from the beginning",
          type = "message",
          duration = 3
        )
      }
    }, error = function(e) {
      message("Could not load existing data: ", e$message)
      showNotification(
        "Starting from the beginning",
        type = "message",
        duration = 3
      )
    })
  })
  
  # Save to Google Sheets (with upsert logic)
  upsert_label <- function(answer = NA_character_, note = NA_character_) {
    row <- summary_table[idx(), , drop = FALSE]
    uid <- row$unique_id[1]
    
    current_user <- reactiveValuesToList(res_auth)$user
    if (is.null(current_user)) current_user <- "unknown"
    
    country <- if ("country" %in% names(row)) row$country[1] else row$iso3c_prev[1]
    algo_decision <- row$type[1]
    openai_decision <- row$openai_assessment[1]
    
    df <- user_labels()
    existing <- df[df$unique_id == uid, , drop = FALSE]
    
    if (nrow(existing) > 0) {
      if (is.na(answer)) answer <- existing$user_answer[1]
      if (is.na(note))   note   <- existing$note[1]
    }
    
    new_row <- data.frame(
      unique_id = uid,
      user_answer = ifelse(is.na(answer), "", as.character(answer)),
      note = ifelse(is.na(note), "", as.character(note)),
      timestamp = Sys.time(),
      username = current_user,
      country = country,
      algo_decision = algo_decision,
      openai_decision = openai_decision,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      # Read current sheet data
      existing_data <- tryCatch({
        read_sheet(GOOGLE_SHEET_URL, col_types = "c")  # Read all as character to avoid type issues
      }, error = function(e) {
        data.frame(
          unique_id = character(),
          user_answer = character(),
          note = character(),
          timestamp = character(),
          username = character(),
          country = character(),
          algo_decision = character(),
          openai_decision = character(),
          stringsAsFactors = FALSE
        )
      })
      
      # Convert new_row to match format
      new_row_formatted <- data.frame(
        unique_id = as.character(uid),
        user_answer = as.character(new_row$user_answer),
        note = as.character(new_row$note),
        timestamp = as.character(new_row$timestamp),
        username = as.character(new_row$username),
        country = as.character(new_row$country),
        algo_decision = as.character(new_row$algo_decision),
        openai_decision = as.character(new_row$openai_decision),
        stringsAsFactors = FALSE
      )
      
      if (nrow(existing_data) > 0) {
        # Remove any existing rows with this unique_id
        existing_data <- existing_data[existing_data$unique_id != as.character(uid), ]
      }
      
      # Add the new row
      updated_data <- rbind(existing_data, new_row_formatted)
      
      # Clear the sheet completely (including headers) and rewrite
      sheet_write(updated_data, ss = GOOGLE_SHEET_URL, sheet = 1)
      
      saved_items <- c()
      if (!is.na(answer) && answer != "") saved_items <- c(saved_items, "answer")
      if (!is.na(note) && note != "") saved_items <- c(saved_items, "note")
      
      if (length(saved_items) > 0) {
        save_status(paste("✓ Updated", paste(saved_items, collapse = " & "), 
                          "at", format(Sys.time(), "%H:%M:%S")))
      } else {
        save_status(paste("✓ Saved at", format(Sys.time(), "%H:%M:%S")))
      }
      
      showNotification("Updated in Google Sheets!", type = "message", duration = 2)
      
      # Update reviewed cases list
      current_reviewed <- reviewed_cases()
      if (!uid %in% current_reviewed) {
        reviewed_cases(c(current_reviewed, uid))
      }
      
      # Mark this case as "just answered" so we don't show "Previous Review" immediately
      just_answered(uid)
      
      # Update the cached sheet data
      cached_data <- sheet_data_cache()
      if (nrow(cached_data) > 0) {
        # Remove old entry for this unique_id
        cached_data <- cached_data[cached_data$unique_id != as.character(uid), ]
      }
      # Add new entry
      sheet_data_cache(rbind(cached_data, new_row_formatted))
      
    }, error = function(e) {
      save_status(paste("✗ Error:", substr(e$message, 1, 50)))
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
    
    df <- df[df$unique_id != uid, , drop = FALSE]
    user_labels(rbind(df, new_row))
  }
  
  output$save_status <- renderUI({
    status <- save_status()
    if (status == "") return(NULL)
    
    class_name <- if (grepl("^✓", status)) "save-success" else "save-error"
    div(class = paste("save-status", class_name), status)
  })
  
  # Progress indicator
  output$progress_info <- renderUI({
    current_idx <- idx()
    current_uid <- summary_table[current_idx, ]$unique_id
    reviewed <- reviewed_cases()
    
    total_cases <- nrow(summary_table)
    reviewed_count <- length(reviewed)
    progress_pct <- round((reviewed_count / total_cases) * 100, 1)
    
    is_current_reviewed <- current_uid %in% reviewed
    
    tagList(
      div(class = "progress-text",
          paste0("Progress: ", reviewed_count, " / ", total_cases, " cases reviewed (", progress_pct, "%)"),
          span(class = paste("case-indicator", if(is_current_reviewed) "case-reviewed" else "case-current"),
               style = "margin-left: 10px;",
               if(is_current_reviewed) "✓ Reviewed" else "○ Not reviewed"
          )
      ),
      div(class = "progress-bar-container",
          div(class = "progress-bar-fill", 
              style = paste0("width: ", progress_pct, "%;"),
              if(progress_pct > 10) paste0(progress_pct, "%") else ""
          )
      )
    )
  })
  
  # Prepare polygons
  gaul_poly <- reactive({
    selected_old_id <- summary_table[idx(), , drop = FALSE] %>%
      dplyr::pull(old_id) %>%
      stringr::str_split(",\\s*") %>%
      unlist() %>%
      unique()
    
    selected_level <- summary_table[idx(), , drop = FALSE] %>%
      dplyr::pull(change_level)
    
    gaul_geom %>%
      dplyr::filter(change_level == selected_level) %>%
      dplyr::filter(old_id %in% selected_old_id)
  })
  
  gadm_poly <- reactive({
    selected_new_id <- summary_table[idx(), , drop = FALSE] %>%
      dplyr::pull(new_id) %>%
      stringr::str_split(",\\s*") %>%
      unlist() %>%
      unique()
    
    selected_level <- summary_table[idx(), , drop = FALSE] %>%
      dplyr::pull(change_level)
    
    gadm_geom %>%
      dplyr::filter(change_level == selected_level) %>%
      dplyr::filter(new_id %in% selected_new_id)
  })
  
  # Display AI info
  output$agent_info <- renderUI({
    infor <- summary_table[idx(), , drop = FALSE]
    
    country <- if ("country" %in% names(infor)) infor$country[1] else infor$iso3c_prev[1]
    algo_decision <- infor$type[1]
    openai_decision <- infor$openai_assessment[1]
    openai_explanation <- infor$openai_explanation[1]
    openai_source <- infor$openai_sources[1]
    
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
      p(paste0("OpenAI assessment: ", openai_decision)),
      
      p("OpenAI explanation:"),
      if (length(expl_items) > 0) tags$ul(lapply(expl_items, tags$li)) else p("(none)"),
      
      p("OpenAI sources:"),
      if (length(src_items) > 0) tagList(lapply(src_items, function(s) p(paste0("• ", s)))) else p("(none)")
    )
  })
  
  # Visualization mode buttons
  observeEvent(input$before_only, { mode("before") })
  observeEvent(input$after_only,  { mode("after")  })
  observeEvent(input$show_both,   { mode("both")   })
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE))
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
    
    if (show_gaul) {
      gaul$label_txt <- as.character(gaul[[gaul_label_col]])
      map_proxy <- map_proxy %>% addPolygons(
        data = gaul, color = "green", weight = 2, opacity = 1, fill = FALSE,
        label = ~label_txt,
        highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE)
      )
    }
    
    if (show_gadm) {
      gadm$label_txt <- as.character(gadm[[gadm_label_col]])
      map_proxy <- map_proxy %>% addPolygons(
        data = gadm, color = "red", weight = 2, opacity = 1, fill = FALSE,
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
  
  # Answer buttons
  observeEvent(input$correct,  { upsert_label(answer = "Yes",      note = input$note) })
  observeEvent(input$wrong,    { upsert_label(answer = "No",       note = input$note) })
  observeEvent(input$not_sure, { upsert_label(answer = "Not Sure", note = input$note) })
  observeEvent(input$save_note, { upsert_label(note = input$note) })
  
  # Update note field when navigating - now includes previous answers
  observe({
    row <- summary_table[idx(), , drop = FALSE]
    uid <- row$unique_id[1]
    
    # First check local user_labels
    df <- user_labels()
    existing_note <- df$note[df$unique_id == uid]
    
    # If not in local, check cached sheet data
    if (length(existing_note) == 0 || existing_note[1] == "") {
      cached <- sheet_data_cache()
      if (nrow(cached) > 0) {
        sheet_row <- cached[cached$unique_id == as.character(uid), ]
        if (nrow(sheet_row) > 0) {
          existing_note <- sheet_row$note[1]
        }
      }
    }
    
    if (length(existing_note) == 0) existing_note <- ""
    
    updateTextAreaInput(session, "note", value = existing_note[1])
  })
  
  # Display previous answer indicator or unanswered warning
  output$previous_answer_info <- renderUI({
    row <- summary_table[idx(), , drop = FALSE]
    uid <- row$unique_id[1]
    
    # Don't show "Previous Review" if we just answered this case in this session
    if (!is.null(just_answered()) && just_answered() == uid) {
      return(NULL)
    }
    
    # Check if this case has been reviewed
    if (uid %in% reviewed_cases()) {
      # Get the previous answer from cache
      cached <- sheet_data_cache()
      if (nrow(cached) > 0) {
        prev_row <- cached[cached$unique_id == as.character(uid), ]
        if (nrow(prev_row) > 0) {
          prev_answer <- prev_row$user_answer[1]
          prev_user <- prev_row$username[1]
          prev_time <- prev_row$timestamp[1]
          
          answer_class <- switch(prev_answer,
                                 "Yes" = "badge-success",
                                 "No" = "badge-danger",
                                 "Not Sure" = "badge-warning",
                                 "badge-secondary"
          )
          
          return(
            div(style = "padding: 8px; margin: 8px 0; background-color: #f8f9fa; border-radius: 4px; border-left: 3px solid #007bff;",
                p(style = "margin: 0 0 4px 0; font-size: 11px; font-weight: bold; color: #666;",
                  "Previous Review:"
                ),
                p(style = "margin: 0;",
                  span(class = paste("badge", answer_class), 
                       style = "font-size: 10px; margin-right: 5px;",
                       prev_answer),
                  span(style = "font-size: 10px; color: #666;",
                       paste0("by ", prev_user, " at ", prev_time))
                )
            )
          )
        }
      }
    } else {
      # Show unanswered warning
      return(
        div(class = "unanswered-warning",
            p(class = "unanswered-warning-text",
              "⚠️ This case needs your review - please select Yes, No, or Not Sure"
            )
        )
      )
    }
    return(NULL)
  })
  
  # Download handler
  output$download_labels <- downloadHandler(
    filename = function() paste0("user_labels_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      write.csv(user_labels(), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)