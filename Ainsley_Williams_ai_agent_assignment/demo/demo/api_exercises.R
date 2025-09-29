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
