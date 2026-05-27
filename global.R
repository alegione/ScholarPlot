# global.R ####
# =============================================================================
# ScholarPlot - Global Configuration and Helper Functions
# =============================================================================
# This script was developed with assistance from Anthropic's Claude (Sonnet)
# via the Spark interface, building on the original ScholarPlot tool by
# Alistair Legione (https://github.com/alegione/ScholarPlot).
#
# This file is sourced automatically by Shiny before the UI and server are
# initialised. It handles:
#   - Package installation and loading
#   - Remote SJR data loading with year-aware URL fallback
#   - String normalisation for journal name matching
#   - Preprint server detection
#   - Fuzzy journal matching with confidence scoring
#   - Research profile helper functions (area/category summaries, text
#     tokenisation, co-occurrence network construction)
# =============================================================================

# Package installation and loading ####
# -----------------------------------------------------------------------------
# All required packages are listed here. Any that are not yet installed will
# be installed automatically before loading. This ensures the app runs on a
# fresh R installation without manual setup.
list.of.packages <- c(
  "shiny",        # core Shiny framework
  "tidyverse",    # ggplot2, dplyr, tidyr, stringr, etc.
  "scholar",      # Google Scholar data retrieval
  "DT",           # interactive DataTables for Shiny
  "stringdist",   # fuzzy string distance metrics for journal matching
  "RColorBrewer", # colour palettes for plots
  "bslib",        # Bootstrap theming for Shiny UI
  "tibble",       # modern data frames
  "dplyr",        # data manipulation verbs
  "ggplot2",      # grammar of graphics plotting
  "scales",       # axis formatting (comma labels, pretty breaks, etc.)
  "wordcloud2",   # interactive HTML word cloud widget
  "igraph",       # graph/network data structures
  "ggraph",       # ggplot2-based network visualisation
  "tidytext",     # tidy text mining tools
  "stopwords",    # stop word lists for text cleaning
  "rcrossref"     # CrossRef API for full author list fallback
)

# Identify packages not yet installed
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[, "Package"])
]

# Install missing packages with their dependencies
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# Load all packages into the session
for (pkg in list.of.packages) {
  library(pkg, character.only = TRUE)
}

# Known preprint servers ####
# -----------------------------------------------------------------------------
# This vector lists domain names and common abbreviations associated with
# preprint servers. It is used during journal matching to flag papers that
# were posted to a preprint server rather than a peer-reviewed journal.
# Preprints will not exist in the SJR database and should not be matched —
# instead they receive an impact factor of 0 and no quartile assignment.
PREPRINT_SERVERS <- c(
  "arxiv",
  "biorxiv",
  "medrxiv",
  "chemrxiv",
  "psyarxiv",
  "socarxiv",
  "engrxiv",
  "techrxiv",
  "preprints",
  "ssrn",
  "researchsquare",
  "osf preprints",
  "zenodo",
  "eartharxiv",
  "authorea"
)

# Remote SJR data URL template ####
# -----------------------------------------------------------------------------
# The base URL points to the GitHub repository where the pre-built .rda files
# are hosted. The year is substituted in at load time so the most recent
# annual version is fetched first.
SJR_REMOTE_BASE <- "https://github.com/alegione/ScholarPlot/raw/master/sjr_tibble_"

# String normalisation helper ####
# -----------------------------------------------------------------------------
# Converts a character vector to lowercase, strips all non-alphanumeric
# characters (except spaces), and collapses multiple spaces to one.
# Applied to both Google Scholar journal names and SJR titles before any
# comparison so that punctuation and capitalisation differences do not
# prevent matches.
normalise_str <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", "", x)
  x <- trimws(gsub("\\s+", " ", x))
  x
}

# Preprint detection helper ####
# ----------------------------------------------------------------------------- #
# Returns TRUE if the normalised journal name matches any known preprint
# server. Uses partial matching (grepl) so that strings like
# "biorxiv preprint" or "posted on arxiv" are also caught.
is_preprint <- function(journal_name_norm) {
  if (is.na(journal_name_norm) || journal_name_norm == "") return(FALSE)
  any(sapply(PREPRINT_SERVERS, function(server) {
    grepl(server, journal_name_norm, fixed = TRUE)
  }))
}

# SJR data loader with year-aware fallback ####
# ----------------------------------------------------------------------------- #
# Attempts to load the SJR tibble in the following order:
#   1. User-uploaded file (if provided via the app's file input)
#   2. Remote .rda for the current year (e.g. sjr_tibble_2025.rda)
#   3. Remote .rda for the prior year (e.g. sjr_tibble_2024.rda)
#   4. Local copy of current year file (bundled with the app)
#   5. Local copy of prior year file
#   6. Error — no data available
# The loaded object is always named sjr_tibble inside the .rda file.
load_sjr_data <- function(local_path = NULL) {
  
  env <- new.env()
  
  # Priority 1: user-uploaded file
  if (!is.null(local_path)) {
    tryCatch(
      load(local_path, envir = env),
      error = function(e) stop("Could not load uploaded SJR file: ", e$message)
    )
    return(env$sjr_tibble)
  }
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  prior_year   <- current_year - 1L
  
  # Helper to attempt a remote download and load
  try_remote <- function(year) {
    url <- paste0(SJR_REMOTE_BASE, year, ".rda")
    tmp <- tempfile(fileext = ".rda")
    result <- tryCatch({
      download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
      load(tmp, envir = env)
      TRUE
    }, error = function(e) FALSE)
    result
  }
  
  # Helper to attempt loading a local file
  try_local <- function(year) {
    path <- paste0("sjr_tibble_", year, ".rda")
    if (file.exists(path)) {
      tryCatch({
        load(path, envir = env)
        TRUE
      }, error = function(e) FALSE)
    } else {
      FALSE
    }
  }
  
  # Priority 2: remote current year
  if (try_remote(current_year)) return(env$sjr_tibble)
  
  # Priority 3: remote prior year
  if (try_remote(prior_year))   return(env$sjr_tibble)
  
  # Priority 4: local current year
  if (try_local(current_year))  return(env$sjr_tibble)
  
  # Priority 5: local prior year
  if (try_local(prior_year))    return(env$sjr_tibble)
  
  # Priority 6: nothing worked
  stop(paste(
    "No SJR data could be loaded.",
    "Please upload an sjr_tibble_YYYY.rda file via the app sidebar."
  ))
}

# Author list truncation detector ####
# -----------------------------------------------------------------------------
# Google Scholar truncates long author lists with "..." or similar markers.
# This function returns TRUE if the author string appears to be truncated,
# indicating that a full author lookup should be attempted.
# Common truncation patterns include:
#   "Smith J, Jones B, ..."
#   "Smith J, Jones B, et al"
#   "Smith J, Jones B, … "  (unicode ellipsis)
is_truncated_authors <- function(author_string) {
  if (is.na(author_string) || author_string == "") return(FALSE)
  grepl(
    pattern = "(\\.\\.\\.)|( et al\\.?$)|\u2026",
    x       = author_string,
    ignore.case = TRUE
  )
}

# CrossRef full author fetcher ####
# -----------------------------------------------------------------------------
# Queries the CrossRef API using the paper title and first author as search
# terms. Returns a formatted author string if a match is found, or NULL if
# the lookup fails or returns no usable author data.
# CrossRef rate limit: ~1 request/second without an API key. A polite email
# address can be set via options(rcrossref.email = "you@example.com") to
# access the polite pool with higher limits.
fetch_authors_crossref <- function(title, fallback_authors) {
  
  query_result <- tryCatch(
    rcrossref::cr_works(
      query = title,
      limit = 1
    )$data,
    error   = function(e) NULL,
    warning = function(w) NULL
  )
  
  # Return fallback if CrossRef returned nothing
  if (is.null(query_result) || nrow(query_result) == 0) {
    return(fallback_authors)
  }
  
  authors_raw <- tryCatch(
    query_result$author[[1]],
    error = function(e) NULL
  )
  
  # Return fallback if no author data in the CrossRef record
  if (is.null(authors_raw) || nrow(authors_raw) == 0) {
    return(fallback_authors)
  }
  
  # Build "Given Family" strings, handling cases where either part is missing
  author_strings <- mapply(
    function(given, family) {
      given  <- if (is.na(given)  || given  == "") "" else trimws(given)
      family <- if (is.na(family) || family == "") "" else trimws(family)
      trimws(paste(given, family))
    },
    authors_raw$given,
    authors_raw$family,
    SIMPLIFY = TRUE
  )
  
  # Filter out any empty strings that result from missing name components
  author_strings <- author_strings[nchar(author_strings) > 0]
  
  if (length(author_strings) == 0) return(fallback_authors)
  
  paste(author_strings, collapse = ", ")
}



# Fuzzy journal matching with confidence scoring ####
# -----------------------------------------------------------------------------
# Matches each Google Scholar journal name against the SJR journal title list
# using the following strategy:
#
#   1. Preprint detection — flagged journals are assigned IF = 0, Q = NA,
#      match_type = "preprint" and skipped for SJR lookup
#   2. Exact match after normalisation — confidence = 100
#   3. Jaro-Winkler fuzzy match within a distance threshold — confidence
#      is a weighted combination of string similarity (80%) and field overlap
#      bonus (20%), capped at 99 to distinguish from exact matches
#   4. No match found — confidence = 0, match_type = "none"
#
# The field overlap bonus rewards fuzzy matches where the candidate SJR
# journal's research areas overlap with areas inferred from the author's
# exact-matched publications, providing a domain-aware tiebreaker.
match_publications_to_sjr <- function(gs_pubs,
                                      sjr_tibble,
                                      fuzzy_method       = "jw",
                                      fuzzy_threshold    = 0.20,
                                      string_weight      = 0.80,
                                      field_bonus_weight = 0.20) {
  
  # Normalise journal names for comparison
  gs_pubs$gs_journal_norm   <- normalise_str(gs_pubs$gs_journal)
  sjr_tibble$sjr_title_norm <- normalise_str(sjr_tibble$Title)
  sjr_titles_norm           <- sjr_tibble$sjr_title_norm
  
  # Infer author's research areas from exact-matched journals only.
  # This avoids circular reasoning — we only use confirmed matches to
  # build the prior, then apply it to score uncertain fuzzy matches.
  exact_match_idx <- match(gs_pubs$gs_journal_norm, sjr_titles_norm)
  author_areas    <- character(0)
  
  for (idx in exact_match_idx[!is.na(exact_match_idx)]) {
    areas_raw <- sjr_tibble$Areas[idx]
    if (!is.na(areas_raw) && trimws(areas_raw) != "") {
      split_areas  <- trimws(strsplit(areas_raw, ";")[[1]])
      author_areas <- c(author_areas, split_areas)
    }
  }
  
  # Rank areas by frequency — most common fields appear first
  author_top_areas <- if (length(author_areas) > 0) {
    names(sort(table(author_areas), decreasing = TRUE))
  } else {
    character(0)
  }
  
  # Initialise result columns on the publications tibble
  gs_pubs$sjr_match_title     <- NA_character_
  gs_pubs$match_type          <- NA_character_
  gs_pubs$string_similarity   <- NA_real_
  gs_pubs$field_overlap_bonus <- NA_real_
  gs_pubs$confidence_score    <- NA_real_
  gs_pubs$sjr_row_index       <- NA_integer_
  gs_pubs$is_preprint         <- FALSE
  
  for (i in seq_len(nrow(gs_pubs))) {
    
    query <- gs_pubs$gs_journal_norm[i]
    
    # Handle empty journal fields
    if (is.na(query) || query == "") {
      gs_pubs$match_type[i]       <- "none"
      gs_pubs$confidence_score[i] <- 0
      next
    }
    
    # Step 1: Preprint detection
    # Flag preprint server publications and skip SJR matching entirely.
    # These will appear in the publications table but are excluded from
    # quartile plots and receive no SJR metrics.
    if (is_preprint(query)) {
      gs_pubs$is_preprint[i]      <- TRUE
      gs_pubs$match_type[i]       <- "preprint"
      gs_pubs$confidence_score[i] <- 0
      next
    }
    
    # Step 2: Exact match after normalisation
    exact_idx <- which(sjr_titles_norm == query)
    if (length(exact_idx) >= 1L) {
      best_idx <- exact_idx[1]
      gs_pubs$sjr_match_title[i]     <- sjr_tibble$Title[best_idx]
      gs_pubs$match_type[i]          <- "exact"
      gs_pubs$string_similarity[i]   <- 1.0
      gs_pubs$field_overlap_bonus[i] <- 0.0
      gs_pubs$confidence_score[i]    <- 100.0
      gs_pubs$sjr_row_index[i]       <- best_idx
      next
    }
    
    # Step 3: Jaro-Winkler fuzzy match
    # Jaro-Winkler is preferred over Levenshtein for journal names because
    # it rewards shared prefixes (e.g. "Nature Reviews X" vs "Nature Reviews Y")
    # and is more robust to minor suffix differences.
    distances     <- stringdist::stringdist(query, sjr_titles_norm,
                                            method = fuzzy_method)
    best_dist_idx <- which.min(distances)
    best_dist     <- distances[best_dist_idx]
    
    # Reject matches beyond the distance threshold
    if (best_dist > fuzzy_threshold) {
      gs_pubs$match_type[i]       <- "none"
      gs_pubs$confidence_score[i] <- 0
      next
    }
    
    # Convert distance to similarity (Jaro-Winkler distance is in [0, 1])
    string_sim  <- 1 - best_dist
    field_bonus <- 0.0
    
    # Step 4: Field overlap bonus
    # Check whether the candidate SJR journal's research areas overlap with
    # the author's inferred top fields. More overlap = higher bonus.
    if (length(author_top_areas) > 0) {
      candidate_areas_raw <- sjr_tibble$Areas[best_dist_idx]
      if (!is.na(candidate_areas_raw) && trimws(candidate_areas_raw) != "") {
        candidate_areas <- trimws(strsplit(candidate_areas_raw, ";")[[1]])
        overlap_count   <- sum(candidate_areas %in% author_top_areas)
        field_bonus     <- min(
          overlap_count / max(length(candidate_areas), 1L), 1.0
        )
      }
    }
    
    # Step 5: Composite confidence score
    # Weighted sum of string similarity and field overlap, scaled to 0-99.
    # 100 is reserved exclusively for exact matches.
    composite_scaled <- min(
      round(
        (string_sim * string_weight + field_bonus * field_bonus_weight) * 100,
        1
      ),
      99.0
    )
    
    gs_pubs$sjr_match_title[i]     <- sjr_tibble$Title[best_dist_idx]
    gs_pubs$match_type[i]          <- "fuzzy"
    gs_pubs$string_similarity[i]   <- round(string_sim, 4)
    gs_pubs$field_overlap_bonus[i] <- round(field_bonus, 4)
    gs_pubs$confidence_score[i]    <- composite_scaled
    gs_pubs$sjr_row_index[i]       <- best_dist_idx
  }
  
  # Join SJR metadata onto the publications tibble ####
  # Add a row index key to sjr_tibble to enable joining by position
  sjr_tibble$sjr_row_index <- seq_len(nrow(sjr_tibble))
  
  # Left join preserves all Google Scholar publications, adding SJR columns
  # where a match was found and NA where no match exists
  final <- dplyr::left_join(
    gs_pubs, sjr_tibble,
    by     = "sjr_row_index",
    suffix = c("_gs", "_sjr")
  )
  
  # Remove internal working columns not needed downstream
  final$gs_journal_norm  <- NULL
  final$sjr_title_norm   <- NULL
  final$sjr_row_index    <- NULL
  
  final
}

# Research profile helper: summarise broad research areas ####
# -----------------------------------------------------------------------------
# Extracts the Areas field from matched publications, splits on ";",
# and returns a frequency-sorted tibble of area names and counts.
# Used by the Research Profile tab bar chart.
summarise_areas <- function(matched_pubs) {
  matched_pubs |>
    dplyr::filter(!is.na(Areas), Areas != "") |>
    dplyr::pull(Areas) |>
    strsplit(";") |>
    unlist() |>
    trimws() |>
    tibble::tibble(area = _) |>
    dplyr::filter(area != "") |>
    dplyr::count(area, sort = TRUE)
}

# Research profile helper: summarise journal subject categories ####
# -----------------------------------------------------------------------------
# Similar to summarise_areas but operates on the Categories field.
# Q-value suffixes (e.g. "(Q1)") are stripped so that the same category
# is not counted separately for different quartile ratings.
summarise_categories <- function(matched_pubs) {
  matched_pubs |>
    dplyr::filter(!is.na(Categories), Categories != "") |>
    dplyr::pull(Categories) |>
    strsplit(";") |>
    unlist() |>
    trimws() |>
    sub("\\s*\\(Q[1-4]\\)$", "", x = _) |>
    trimws() |>
    tibble::tibble(category = _) |>
    dplyr::filter(category != "") |>
    dplyr::count(category, sort = TRUE)
}

# Research profile helper: tokenise text for word cloud ####
# -----------------------------------------------------------------------------
# Converts a character vector (paper titles and/or abstracts) into a
# frequency-sorted tibble of individual words. Stop words (common English
# words with little semantic content) and short tokens are removed.
# Additional academic filler words are appended to the stop word list.
tokenise_text <- function(text_vector) {
  
  # Combine standard English stop words with domain-specific filler terms
  stop_words <- c(
    stopwords::stopwords("en", source = "snowball"),
    "using", "based", "study", "analysis", "novel", "new",
    "approach", "method", "results", "data", "paper", "research",
    "effect", "effects", "impact", "review", "case", "two",
    "three", "high", "low", "large", "small", "used", "use",
    "associated", "among", "across", "within", "without"
  )
  
  text_vector |>
    tolower() |>
    gsub("[^a-z ]", " ", x = _) |>      # remove punctuation and numbers
    strsplit("\\s+") |>
    unlist() |>
    trimws() |>
    (\(w) w[nchar(w) > 2])() |>         # discard very short tokens
    (\(w) w[!w %in% stop_words])() |>   # remove stop words
    tibble::tibble(word = _) |>
    dplyr::filter(word != "") |>
    dplyr::count(word, sort = TRUE)
}

# Research profile helper: build category co-occurrence network ####
# -----------------------------------------------------------------------------
# For each publication, extracts the list of subject categories and generates
# all pairwise combinations. Pairs that co-occur in at least min_edge_weight
# publications form edges in the network. Edge weight = co-occurrence count.
# Used by the Category Network view in the Research Profile tab.
build_category_cooccurrence <- function(matched_pubs, min_edge_weight = 2) {
  
  # Extract and clean category lists for each publication
  category_lists <- matched_pubs |>
    dplyr::filter(!is.na(Categories), Categories != "") |>
    dplyr::pull(Categories) |>
    lapply(function(x) {
      cats <- trimws(strsplit(x, ";")[[1]])
      cats <- sub("\\s*\\(Q[1-4]\\)$", "", cats)
      trimws(cats)
    })
  
  # Generate all within-publication category pairs
  edges <- lapply(category_lists, function(cats) {
    if (length(cats) < 2) return(NULL)
    # combn generates all unique pairs; sort() ensures consistent ordering
    # (A,B) and (B,A) are treated as the same edge
    pairs <- combn(sort(unique(cats)), 2, simplify = FALSE)
    do.call(rbind, lapply(pairs, function(p) {
      data.frame(from = p[1], to = p[2], stringsAsFactors = FALSE)
    }))
  })
  
  edges <- dplyr::bind_rows(edges)
  
  if (is.null(edges) || nrow(edges) == 0) return(NULL)
  
  # Count co-occurrences and filter by minimum edge weight
  edges |>
    dplyr::count(from, to, name = "weight") |>
    dplyr::filter(weight >= min_edge_weight)
}

# Filename generator for exports ####
# -----------------------------------------------------------------------------
# Constructs a standardised export filename in the format:
#   YYYY-MM-DD-Firstname-Surname-OutputType.ext
# The author name is extracted from the Google Scholar profile details list.
# Output type should be provided in CamelCase (e.g. "PublicationList").
make_export_filename <- function(details, output_type, ext) {
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  
  # Split the full name on whitespace and capitalise each part
  name_parts <- strsplit(trimws(details$name), "\\s+")[[1]]
  name_str   <- paste(name_parts, collapse = "-")
  
  paste0(date_str, "-", name_str, "-", output_type, ".", ext)
}
