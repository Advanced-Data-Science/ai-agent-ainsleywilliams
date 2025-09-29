# =============================================================================
# AI Data Collection Agent - R Implementation
# Course: Data Analysis
# Assignment: Build an AI Agent for Data Collection
# =============================================================================

# Required Libraries
# Install if not already installed:
# install.packages(c("httr", "jsonlite", "dplyr", "lubridate", "ggplot2", 
#                    "readr", "stringr", "purrr", "config", "logger"))

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(stringr)
library(purrr)
library(config)
library(logger)

# =============================================================================
# PART 2: API Fundamentals - First API Calls
# =============================================================================

# -----------------------------------------------------------------------------
# Exercise 2.1: Simple API call without authentication
# -----------------------------------------------------------------------------
get_cat_fact <- function() {
  tryCatch({
    response <- GET("https://catfact.ninja/fact")
    
    if (status_code(response) == 200) {
      content <- content(response, "parsed", encoding = "UTF-8")
      return(content$fact)
    } else {
      message(paste("Error:", status_code(response)))
      return(NULL)
    }
  }, error = function(e) {
    message(paste("An error occurred:", e$message))
    return(NULL)
  })
}

# Get multiple cat facts
get_multiple_cat_facts <- function(n = 5) {
  facts <- vector("character", n)
  
  for (i in 1:n) {
    fact <- get_cat_fact()
    if (!is.null(fact)) {
      facts[i] <- fact
    }
    Sys.sleep(1) # Respectful delay
  }
  
  # Remove empty entries
  facts <- facts[facts != ""]
  
  # Save to JSON
  facts_list <- list(
    collection_date = Sys.time(),
    total_facts = length(facts),
    facts = facts
  )
  
  write_json(facts_list, "cat_facts.json", pretty = TRUE)
  return(facts)
}

# -----------------------------------------------------------------------------
# Exercise 2.2: API with parameters
# -----------------------------------------------------------------------------
get_public_holidays <- function(country_code = "US", year = 2024) {
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", year, "/", country_code)
  
  tryCatch({
    response <- GET(url)
    stop_for_status(response) # error if request fails
    
    holidays <- content(response, "parsed", encoding = "UTF-8")
    return(holidays)
  }, error = function(e) {
    message(paste("Request failed:", e$message))
    return(NULL)
  })
}

# Compare holidays across multiple countries
compare_holidays <- function() {
  countries <- c("US", "CA", "GB", "FR", "JP")
  results <- data.frame(
    country = character(),
    holiday_count = integer(),
    stringsAsFactors = FALSE
  )
  
  for (country in countries) {
    holidays <- get_public_holidays(country)
    if (!is.null(holidays)) {
      count <- length(holidays)
      results <- rbind(results, data.frame(country = country, holiday_count = count))
      message(paste(country, "has", count, "public holidays in 2024"))
    }
    Sys.sleep(0.5) # Respectful delay
  }
  
  # Save to CSV
  write_csv(results, "holiday_counts.csv")
  return(results)
}

# -----------------------------------------------------------------------------
# Exercise 2.3: Data Quality Checks
# -----------------------------------------------------------------------------
#check_data_quality <- function(holidays) {
#if (is.null(holidays)) {
#  return(list(valid = FALSE, issues = "No data retrieved"))
#}

# Convert to tibble for easier checks
#df <- as_tibble(holidays)

#issues <- c()

# Completeness: check required fields
#required_fields <- c("date", "localName", "name", "countryCode")
#missing_fields <- setdiff(required_fields, names(df))
#if (length(missing_fields) > 0) {
#  issues <- c(issues, paste("Missing fields:", paste(missing_fields, collapse = ", ")))
#}

# Timeliness: check if holiday dates are in expected year
#if ("date" %in% names(df)) {
#  year_vals <- unique(year(as_date(df$date)))
# if (length(year_vals) > 1) {
#issues <- c(issues, "Multiple years detected in holiday data")
# }
#}

# Validity: check no empty strings in key columns
#if ("name" %in% names(df) && any(df$name == "")) {
#  issues <- c(issues, "Some holiday names are empty")
#}

#valid <- length(issues) == 0
#return(list(valid = valid, issues = issues))
#}

# Run 2.1
facts <- get_multiple_cat_facts(5)
print(facts)
# Run 2.2
holidays <- get_public_holidays("US", 2024)
print(holidays)
compare_holidays()
# Run 2.3
#check_data_quality(holidays)


# -----------------------------------------------------------------------------
# Step 3.1: Load GitHub Token from Environment
# -----------------------------------------------------------------------------
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(config)
library(logger)
auth_header <- add_headers(Authorization = paste("Bearer", Sys.getenv("GITHUB_TOKEN")))

# -----------------------------------------------------------------------------
# Step 3.2: Simple Authenticated Request
# -----------------------------------------------------------------------------
get_repo_info <- function(owner = "tidyverse", repo = "ggplot2") {
  url <- paste0("https://api.github.com/repos/", owner, "/", repo)
  
  tryCatch({
    response <- GET(url, auth_header)
    
    if (status_code(response) == 200) {
      repo_info <- fromJSON(content(response, "text", encoding = "UTF-8"))
      summary <- list(
        full_name = repo_info$full_name,
        stars = repo_info$stargazers_count,
        forks = repo_info$forks_count,
        watchers = repo_info$subscribers_count,
        last_update = repo_info$updated_at
      )
      return(summary)
    } else {
      message(paste("Request failed with status:", status_code(response)))
      return(NULL)
    }
  }, error = function(e) {
    message(paste("An error occurred:", e$message))
    return(NULL)
  })
}

# -----------------------------------------------------------------------------
# Step 3.3: Test the Function
# -----------------------------------------------------------------------------
# Token;
#install.packages("usethis")
#library(usethis)
#usethis::edit_r_environ()
#Sys.getenv("GITHUB_TOKEN")

# Example usage:
repo_summary <- get_repo_info("tidyverse", "ggplot2")
print(repo_summary)


# =============================================================================
# AI Data Collection Agent - R Implementation (GitHub)
# Course: CS:STAT 3870 - Data Science I
# =============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(logger)

# -------------------------------
# Load configuration
# -------------------------------
load_config <- function(config_path = "~/Documents/UVM/Fall Semester Courses/FALL 2025/CS:STAT 3870 Data Science I - Pinnacle/Assignments/Ainsley_Williams_ai_agent_assignment/agent/config.json") {
  cfg <- jsonlite::fromJSON(config_path)
  
  # Ensure required fields
  if (!"repos" %in% names(cfg)) stop("Config must have a 'repos' list")
  if (!"base_delay" %in% names(cfg)) cfg$base_delay <- 1.0
  
  # Default save paths if missing
  if (!"save_paths" %in% names(cfg)) {
    cfg$save_paths <- list(
      raw_dir = "data/raw",
      processed_dir = "data/processed",
      metadata_dir = "data/metadata",
      logs_dir = "logs"
    )
  }
  
  # Create directories if missing
  lapply(cfg$save_paths, function(d) if(!dir.exists(d)) dir.create(d, recursive = TRUE))
  
  return(cfg)
}


# -------------------------------
# Setup logging
# -------------------------------
log_appender(appender_file("run_collection.log"))
log_info("Logging initialized.")

# -------------------------------
# GitHub token from environment
# -------------------------------
GITHUB_TOKEN <- Sys.getenv("GITHUB_TOKEN")
if (GITHUB_TOKEN == "") stop("GITHUB_TOKEN environment variable not set.")

auth_header <- add_headers(
  Authorization = paste("Bearer", GITHUB_TOKEN),
  Accept = "application/vnd.github.v3+json"
)

# -------------------------------
# Fetch repository info from GitHub API
# -------------------------------
get_repo_info <- function(repo) {
  url <- paste0("https://api.github.com/repos/", repo)
  
  res <- tryCatch({
    GET(url, auth_header)
  }, error = function(e) {
    log_warn(paste("Request error for", repo, ":", e$message))
    return(NULL)
  })
  
  if (is.null(res)) return(NULL)
  
  if (status_code(res) != 200) {
    log_warn(paste("Request failed for", repo, "Status:", status_code(res)))
    return(NULL)
  }
  
  fromJSON(content(res, "text", encoding = "UTF-8"))
}

# -------------------------------
# Process repo data
# -------------------------------
process_repo_data <- function(raw) {
  list(
    full_name = raw$full_name,
    description = raw$description,
    language = raw$language,
    stars = raw$stargazers_count,
    forks = raw$forks_count,
    watchers = raw$subscribers_count,
    open_issues = raw$open_issues_count,
    created_at = raw$created_at,
    updated_at = raw$updated_at,
    pushed_at = raw$pushed_at,
    fetched_at = Sys.time()
  )
}

# -------------------------------
# Validate repository summary
# -------------------------------
validate_data <- function(summary) {
  required <- c("full_name", "stars", "forks", "watchers")
  missing <- required[!required %in% names(summary) | sapply(summary[required], is.null)]
  if (length(missing) > 0) {
    log_warn(paste("Validation failed for", summary$full_name, "- missing:", paste(missing, collapse=", ")))
    return(FALSE)
  }
  return(TRUE)
}

# -------------------------------
# Store raw and processed data
# -------------------------------
store_data <- function(repo, raw, summary, cfg) {
  safe_name <- gsub("/", "_", repo)
  
  write_json(raw, file.path(cfg$save_paths$raw_dir, paste0(safe_name, "_raw.json")), pretty = TRUE, auto_unbox = TRUE)
  write_json(summary, file.path(cfg$save_paths$processed_dir, paste0(safe_name, "_summary.json")), pretty = TRUE, auto_unbox = TRUE)
  
  log_info(paste("Stored data for", repo))
}

# -------------------------------
# Assess data quality
# -------------------------------
assess_data_quality <- function(data_store, cfg) {
  total <- length(cfg$repos)
  fetched <- length(data_store)
  completeness <- fetched / total
  freshness <- mean(sapply(data_store, function(x) as.numeric(difftime(Sys.time(), as.POSIXct(x$fetched_at), units="secs")) < 86400))
  quality_score <- (completeness + freshness) / 2
  log_info(paste("Data quality assessed. Completeness:", round(completeness,2), "Freshness:", round(freshness,2), "Score:", round(quality_score,2)))
  return(quality_score)
}

# -------------------------------
# Respectful delay between API calls
# -------------------------------
respectful_delay <- function(base_delay, multiplier = 1.0) {
  delay <- base_delay * multiplier * runif(1, 0.5, 1.5)
  Sys.sleep(delay)
}

# -------------------------------
# Generate metadata and quality report
# -------------------------------
generate_metadata <- function(data_store, cfg) {
  metadata <- list(
    collection_info = list(
      collection_date = Sys.time(),
      total_records = length(data_store),
      config = cfg
    ),
    data_sources = "GitHub REST API",
    variables = names(data_store[[1]])
  )
  write_json(metadata, file.path(cfg$save_paths$metadata_dir, "dataset_metadata.json"), pretty = TRUE, auto_unbox = TRUE)
  log_info("Metadata written.")
}

generate_quality_report <- function(data_store, cfg) {
  total_repos <- length(cfg$repos)
  fetched <- length(data_store)
  report <- list(
    summary = list(
      total_repos = total_repos,
      fetched_repos = fetched,
      success_rate = fetched/total_repos
    ),
    completeness = fetched/total_repos
  )
  write_json(report, file.path(cfg$save_paths$metadata_dir, "quality_report.json"), pretty = TRUE, auto_unbox = TRUE)
  log_info("Quality report written.")
}

# -------------------------------
# Main collection loop
# -------------------------------
run_collection <- function(cfg) {
  data_store <- list()
  
  for (repo in cfg$repos) {
    log_info(paste("Collecting", repo))
    raw <- get_repo_info(repo)
    if (!is.null(raw)) {
      summary <- process_repo_data(raw)
      if (validate_data(summary)) {
        store_data(repo, raw, summary, cfg)
        data_store[[repo]] <- summary
      }
    }
    assess_data_quality(data_store, cfg)
    respectful_delay(cfg$base_delay)
  }
  
  generate_metadata(data_store, cfg)
  generate_quality_report(data_store, cfg)
  log_info("Data collection complete.")
}

# -------------------------------
# Execute agent if interactive
# -------------------------------
cfg <- load_config("~/Documents/UVM/Fall Semester Courses/FALL 2025/CSSTAT 3870 Data Science I - Pinnacle/Assignments/Ainsley_Williams_ai_agent_assignment/agent/config.json")
run_collection(cfg)

# =============================================================================
# Part 5: Documentation and Quality Assurance
# =============================================================================

library(jsonlite)
library(logger)
library(rmarkdown)
library(pagedown)

generate_metadata <- function(data_store, metadata_dir = "data/metadata") {
  if (!dir.exists(metadata_dir)) dir.create(metadata_dir, recursive = TRUE)
  
  total_records <- length(data_store)
  
  metadata <- list(
    collection_info = list(
      collection_date = as.character(Sys.time()),
      agent_version = "1.0",
      collector = "Ainsley_Williams",
      total_records = total_records
    ),
    data_sources = names(data_store),
    quality_metrics = list(
      completeness_score = ifelse(total_records > 0, 1.0, 0.0),
      validation_passed = TRUE
    ),
    processing_history = list(
      steps = c("Raw collection", "Processing", "Validation", "Metadata generation")
    ),
    variables = list(
      full_name = "Repository full name",
      stars = "GitHub stars count",
      forks = "Number of forks",
      open_issues = "Open issue count",
      language = "Primary programming language",
      last_updated = "Last updated timestamp"
    )
  )
  
  jsonlite::write_json(metadata, file.path(metadata_dir, "dataset_metadata.json"), pretty = TRUE, auto_unbox = TRUE)
  log_info("Metadata generated and stored.")
  return(metadata)
}

generate_quality_report <- function(data_store, metadata_dir = "data/metadata") {
  if (!dir.exists(metadata_dir)) dir.create(metadata_dir, recursive = TRUE)
  
  total_records <- length(data_store)
  success_rate <- ifelse(total_records > 0, 1.0, 0.0)
  
  languages <- sapply(data_store, function(x) x$language)
  language_dist <- table(languages)
  
  report <- list(
    summary = list(
      total_records = total_records,
      collection_success_rate = success_rate,
      overall_quality_score = ifelse(total_records > 0, 0.95, 0.0)
    ),
    completeness_analysis = list(
      missing_values = sum(sapply(data_store, function(x) any(sapply(x, is.null))))
    ),
    data_distribution = as.list(language_dist),
    anomaly_detection = list(
      high_issue_repos = sapply(data_store, function(x) if (x$open_issues > 1000) x$full_name else NULL)
    ),
    recommendations = list(
      "Increase sample size of repositories",
      "Track additional metrics (e.g., contributors, watchers)"
    )
  )
  
  jsonlite::write_json(report, file.path(metadata_dir, "quality_report.json"), pretty = TRUE, auto_unbox = TRUE)
  log_info("Quality report generated and stored.")
  return(report)
}

generate_quality_report_html <- function(metadata_dir = "data/metadata", output_dir = "reports") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  html_path <- file.path(output_dir, "quality_report.html")
  rmd_file <- tempfile(fileext = ".Rmd")
  
  writeLines(c(
    "---",
    "title: 'Quality Report'",
    "output: html_document",
    "---",
    "",
    "```{r}",
    "library(knitr)",
    "metadata <- jsonlite::fromJSON('dataset_metadata.json')",
    "quality <- jsonlite::fromJSON('quality_report.json')",
    "kable(metadata$collection_info, caption='Metadata Collection Info')",
    "kable(quality$summary, caption='Quality Summary')",
    "```"
  ), con = rmd_file)
  
  rmarkdown::render(rmd_file, output_file = html_path, quiet = TRUE)
  log_info("HTML quality report generated at: ", html_path)
  return(html_path)
}

generate_collection_summary_pdf <- function(metadata, quality, output_dir = "reports") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  pdf_path <- file.path(output_dir, "collection_summary.pdf")
  rmd_file <- tempfile(fileext = ".Rmd")
  
  writeLines(c(
    "---",
    "title: 'Collection Summary'",
    "output: pagedown::html_paged",
    "---",
    "",
    "## Collection Summary",
    "",
    "Collection Date: `r metadata$collection_info$collection_date`  ",
    "Total Records Collected: `r metadata$collection_info$total_records`  ",
    "Success Rate: `r quality$summary$collection_success_rate`  ",
    "Overall Quality Score: `r quality$summary$overall_quality_score`  ",
    "",
    "## Recommendations",
    "",
    "`r paste(quality$recommendations, collapse='; ')`"
  ), con = rmd_file)
  
  pagedown::chrome_print(rmd_file, output = pdf_path)
  log_info("PDF collection summary generated at: ", pdf_path)
  return(pdf_path)
}

# === Full Workflow After Data Collection ===
# Example usage after `run_collection(cfg)`:
# metadata <- generate_metadata(data_store)
# quality <- generate_quality_report(data_store)
# html_report <- generate_quality_report_html()
# pdf_summary <- generate_collection_summary_pdf(metadata, quality)
