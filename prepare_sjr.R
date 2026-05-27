# prepare_sjr.R ####
# =============================================================================
# ScholarPlot - SJR Data Preparation Script
# =============================================================================
# This script was developed with assistance from Anthropic's Claude (Sonnet)
# via the Spark interface, building on the original ScholarPlot tool by
# Alistair Legione (https://github.com/alegione/ScholarPlot).
#
# Purpose:
#   Reads the raw SCImago Journal Rankings (SJR) CSV file, cleans and parses
#   it into a structured tibble, expands the Categories column into individual
#   Q-value columns, and saves the result as a year-stamped .rda file for use
#   by the ScholarPlot Shiny application.
#
# Usage:
#   1. Download the latest SJR data from https://www.scimagojr.com/journalrank.php
#      (export as CSV, semicolon-separated)
#   2. Place the file in the same directory as this script
#   3. Run this script once — the output .rda file can then be uploaded to the
#      app or hosted remotely
# =============================================================================

library(tibble)

# Read raw SJR CSV ####
# -----------------------------------------------------------------------------
# The SJR export uses semicolons as delimiters and wraps some fields in quotes.
# Quoted fields may themselves contain semicolons (e.g. category lists), so
# quote = "\"" is set explicitly to protect those internal semicolons from
# being treated as delimiters. check.names = FALSE preserves the original
# column names exactly as exported, including spaces and special characters.
raw <- read.delim(
  file             = "sjr_data.csv",
  sep              = ";",
  header           = TRUE,
  quote            = "\"",
  stringsAsFactors = FALSE,
  check.names      = FALSE
)

# Strip residual quote characters and trim whitespace ####
# -----------------------------------------------------------------------------
# read.delim normally removes outer quotes, but this loop acts as a safety net
# for any edge cases. trimws() removes leading/trailing whitespace that can
# appear after quote removal.
for (col in names(raw)) {
  if (is.character(raw[[col]])) {
    raw[[col]] <- gsub('"', '', raw[[col]])
    raw[[col]] <- trimws(raw[[col]])
  }
}

# Convert to tibble and repair column names ####
# -----------------------------------------------------------------------------
# .name_repair = "unique" resolves the duplicate Publisher column (appears in
# both column 6 and column 23 of the SJR export) by appending position indices.
# We then manually rename all repaired columns to clean, consistent names that
# are safe to use as R object names without backticks.
sjr_tibble <- as_tibble(raw, .name_repair = "unique")

# Resolve the two Publisher columns — col 6 is the primary publisher,
# col 23 is a coverage/source publisher field
names(sjr_tibble)[names(sjr_tibble) == "Publisher...6"]  <- "Publisher"
names(sjr_tibble)[names(sjr_tibble) == "Publisher...23"] <- "Publisher_coverage"

# Rename remaining columns that contain spaces, punctuation, or special
# characters that would be problematic in R without backtick quoting
names(sjr_tibble)[names(sjr_tibble) == "Open Access"]               <- "Open.Access"
names(sjr_tibble)[names(sjr_tibble) == "Open Access Diamond"]       <- "Open.Access.Diamond"
names(sjr_tibble)[names(sjr_tibble) == "SJR Best Quartile"]         <- "SJR.Best.Quartile"
names(sjr_tibble)[names(sjr_tibble) == "H index"]                   <- "H.index"
names(sjr_tibble)[names(sjr_tibble) == "Total Docs. (2025)"]        <- "Total.Docs.2025"
names(sjr_tibble)[names(sjr_tibble) == "Total Docs. (3years)"]      <- "Total.Docs.3years"
names(sjr_tibble)[names(sjr_tibble) == "Total Refs."]               <- "Total.Refs"
names(sjr_tibble)[names(sjr_tibble) == "Total Citations (3years)"]  <- "Total.Citations.3years"
names(sjr_tibble)[names(sjr_tibble) == "Citable Docs. (3years)"]    <- "Citable.Docs.3years"
names(sjr_tibble)[names(sjr_tibble) == "Citations / Doc. (2years)"] <- "Citations.per.Doc.2years"
names(sjr_tibble)[names(sjr_tibble) == "Ref. / Doc."]               <- "Ref.per.Doc"
names(sjr_tibble)[names(sjr_tibble) == "%Female"]                   <- "Pct.Female"

# Parse Categories column into individual Q-value columns ####
# -----------------------------------------------------------------------------
# The Categories field contains entries like:
#   "Hematology (Q1); Oncology (Q1)"
# This section splits each entry on "; ", extracts the category name and its
# Q rating separately, then creates one new column per unique category across
# the entire dataset. Each row records the Q value for that category if the
# journal belongs to it, or NA if it does not.

# Pre-allocate a list to hold parsed category/Q-value pairs for each row
parsed_categories <- vector("list", nrow(sjr_tibble))

for (i in seq_len(nrow(sjr_tibble))) {
  
  cell <- sjr_tibble$Categories[i]
  
  # Skip rows with no category data
  if (is.na(cell) || trimws(cell) == "") {
    parsed_categories[[i]] <- setNames(character(0), character(0))
    next
  }
  
  # Split the cell on "; " to get individual category tokens
  tokens    <- strsplit(cell, "; ")[[1]]
  cat_names <- character(length(tokens))
  cat_qvals <- character(length(tokens))
  
  for (j in seq_along(tokens)) {
    token <- trimws(tokens[j])
    
    # Use regex to find the trailing "(Q1)" through "(Q4)" pattern
    q_match <- regmatches(token, regexpr("\\(Q[1-4]\\)$", token))
    
    if (length(q_match) == 1L) {
      # Remove parentheses to get just "Q1", "Q2", etc.
      cat_qvals[j] <- gsub("[()]", "", q_match)
      # Remove the Q suffix from the token to get the clean category name
      cat_names[j] <- trimws(sub("\\s*\\(Q[1-4]\\)$", "", token))
    } else {
      # Token present but no Q value found
      cat_qvals[j] <- NA_character_
      cat_names[j] <- token
    }
  }
  
  # Store as a named vector: names = category labels, values = Q ratings
  parsed_categories[[i]] <- setNames(cat_qvals, cat_names)
}

# Collect every unique category name seen across all rows, in first-seen order
all_category_names <- character(0)
for (i in seq_along(parsed_categories)) {
  new_names          <- names(parsed_categories[[i]])
  all_category_names <- c(
    all_category_names,
    new_names[!new_names %in% all_category_names]
  )
}

# Create one new NA-filled column per unique category
for (cat_name in all_category_names) {
  sjr_tibble[[cat_name]] <- NA_character_
}

# Populate each row's category columns with the appropriate Q value
for (i in seq_len(nrow(sjr_tibble))) {
  row_cats <- parsed_categories[[i]]
  if (length(row_cats) == 0L) next
  for (cat_name in names(row_cats)) {
    sjr_tibble[[cat_name]][i] <- row_cats[[cat_name]]
  }
}

# Save as year-stamped .rda file ####
# -----------------------------------------------------------------------------
# The output filename includes the current year so that multiple annual
# versions can coexist and the app can locate the most recent version
# automatically. The object saved inside the file is always named sjr_tibble
# regardless of the filename, so the app can load it consistently.
current_year <- format(Sys.Date(), "%Y")
output_filename <- paste0("sjr_tibble_", current_year, ".rda")

save(sjr_tibble, file = output_filename)

cat("Saved", output_filename, "with", nrow(sjr_tibble),
    "journals and", ncol(sjr_tibble), "columns.\n")
cat("Column names:\n")
print(names(sjr_tibble))

