# =============================================================================
# Run Collection Script with Documentation (Part 5 Integrated)
# =============================================================================
library(logger)
library(jsonlite)

# =============================================================================
# Run Collection Script - Full Workflow with Documentation
# =============================================================================
library(logger)
library(jsonlite)
library(rmarkdown)

# -------------------------------
# Initialize logging
# -------------------------------
if (!dir.exists("logs")) dir.create("logs", recursive = TRUE)
log_appender(appender_file("logs/collection.log"))
log_info("Logging initialized for data collection run.")

# -------------------------------
# Source main agent functions
# -------------------------------
source("~/Documents/UVM/Fall Semester Courses/FALL 2025/CSSTAT 3870 Data Science I - Pinnacle/Assignments/Ainsley_Williams_ai_agent_assignment/agent/Ainsley_Williams_ai_agent_assignment.R")

# -------------------------------
# Load configuration
# -------------------------------
cfg <- load_config("~/Documents/UVM/Fall Semester Courses/FALL 2025/CSSTAT 3870 Data Science I - Pinnacle/Assignments/Ainsley_Williams_ai_agent_assignment/agent/config.json")

# -------------------------------
# Run data collection
# -------------------------------
data_store <- run_collection(cfg)
log_info("Data collection completed successfully.")

# -------------------------------
# Documentation & QA
# -------------------------------
metadata <- generate_metadata(data_store, cfg)
quality  <- generate_quality_report(data_store, cfg)
summary_file <- generate_collection_summary(metadata, quality, output_dir = "reports")

                                            