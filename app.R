# app.R ####
# =============================================================================#
# ScholarPlot - Shiny Application
# =============================================================================#
# This script was developed with assistance from Anthropic's Claude (Sonnet)
# via the Spark interface, building on the original ScholarPlot tool by
# Alistair Legione (https://github.com/alegione/ScholarPlot).
#
# This file defines the full Shiny application. It sources global.R for
# package loading and helper functions, then defines the UI and server.
#
# Tabs (in order):
#   1. Publications Table  — sortable DT table with paper hiding/deletion
#   2. Citations & Papers  — yearly/cumulative plot with dual axis
#   3. Journal Quartiles   — stacked bar by SJR best quartile
#   4. Research Profile    — areas, categories, word cloud, network
#   5. Glossary            — definitions of all metrics and columns
# =============================================================================#

source("global.R")

# ══════════════════════════════════════════════════════════════════════════════#
# UI ####
# ══════════════════════════════════════════════════════════════════════════════#

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("ScholarPlot: visualise your research output"),
  
  tabsetPanel(
    
    # Tab 1: Publications Table ####
    # -------------------------------------------------------------------------#
    # The primary output table. Each row is one publication from Google
    # Scholar, enriched with SJR journal metrics where a match was found.
    # Users can hide individual rows from all downstream plots and outputs
    # without permanently deleting them, or restore hidden rows at any time.
    tabPanel(
      title = "Publications Table",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          # Scholar ID input and fetch trigger
          textInput(
            inputId     = "scholarId",
            label       = "Google Scholar ID",
            placeholder = "rW9T5f4AAAAJ",
            value       = "rW9T5f4AAAAJ"
          ),
          actionButton(
            inputId = "go",
            label   = "Fetch Data",
            class   = "btn-primary"
          ),
          
          hr(),
          
          # Full author list option
          # Exposed as a checkbox so the user can opt in to the slower but
          # more complete author fetching process. A warning is shown to
          # set expectations about the time required.
          h5("Author list options"),
          checkboxInput(
            inputId = "fetchFullAuthors",
            label   = "Fetch complete author lists",
            value   = FALSE
          ),
          conditionalPanel(
            condition = "input.fetchFullAuthors == true",
            tags$div(
              style = paste(
                "background:#fff3cd;",
                "border:1px solid #ffc107;",
                "border-radius:4px;",
                "padding:8px;",
                "margin-bottom:8px;",
                "font-size:12px;"
              ),
              tags$b("Note: "),
              "Fetching complete author lists requires one web request per
               paper and may take several minutes for large publication
               lists. Google Scholar rate limiting may cause additional
               delays. A CrossRef lookup is attempted as a fallback for
               any papers where the author list remains truncated."
            ),
            # Optional CrossRef polite pool email
            # Providing an email gives access to higher request rate limits
            # from the CrossRef API. Left blank, the default anonymous rate
            # limit applies (approximately 1 request per second).
            textInput(
              inputId     = "crossrefEmail",
              label       = "CrossRef contact email (optional)",
              placeholder = "your.email@institution.edu",
              value       = ""
            ),
            tags$p(
              style = "font-size:11px; color:#666;",
              "Providing an email address registers your requests with the ",
              tags$a(
                "CrossRef polite pool",
                href   = "https://github.com/CrossRef/rest-api-doc#etiquette",
                target = "_blank"
              ),
              " for higher rate limits. Your email is sent only to CrossRef
               and is not stored by this application."
            )
          ),
          
          hr(),
          
          # SJR database upload
          # If left blank the app attempts to load the pre-built .rda from
          # the repository (current year, then prior year, then local copy)
          h5("SJR Database"),
          p("Leave blank to use the bundled database, or upload a custom
             sjr_tibble_YYYY.rda file."),
          fileInput(
            inputId     = "sjrUpload",
            label       = NULL,
            accept      = ".rda",
            placeholder = "Upload sjr_tibble_YYYY.rda"
          ),
          
          hr(),
          
          # Row visibility controls
          # Hidden rows are excluded from all plots and the download but
          # remain in the session so they can be restored
          h5("Row visibility"),
          p("Select rows in the table then click Hide to exclude them from
             plots and exports. Restore All re-shows all hidden rows."),
          actionButton(
            inputId = "hideSelected",
            label   = "Hide selected rows",
            class   = "btn-warning"
          ),
          br(), br(),
          actionButton(
            inputId = "restoreAll",
            label   = "Restore all hidden rows",
            class   = "btn-success"
          ),
          
          hr(),
          
          # Download controls
          downloadButton(outputId = "downloadTable", "Save table"),
          
          NULL
        ),
        
        mainPanel(
          width = 9,
          
          # Profile summary strip — rendered once data is fetched
          uiOutput(outputId = "profileSummary"),
          br(),
          
          # Main publications DT table
          DT::dataTableOutput(outputId = "MetricsTable"),
          
          NULL
        )
      )
    ),
    
    # Tab 2: Citations & Papers ####
    # -------------------------------------------------------------------------#
    # Dual-axis plot of citations and papers per year (or cumulative).
    # Each series has its own yearly/cumulative toggle and show/hide checkbox
    # so they can be displayed independently in any combination.
    tabPanel(
      title = "Citations & Papers",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          # Year range slider — rendered dynamically once data is loaded
          uiOutput(outputId = "slider"),
          hr(),
          
          # Citations series controls
          h5("Citations series"),
          checkboxInput(
            inputId = "showCitations",
            label   = "Show citations",
            value   = TRUE
          ),
          radioButtons(
            inputId  = "citationMode",
            label    = NULL,
            choices  = c("Yearly" = "yearly", "Cumulative" = "cumulative"),
            selected = "yearly"
          ),
          
          # Papers series controls
          h5("Papers series"),
          checkboxInput(
            inputId = "showPapers",
            label   = "Show papers",
            value   = TRUE
          ),
          radioButtons(
            inputId  = "paperMode",
            label    = NULL,
            choices  = c("Yearly" = "yearly", "Cumulative" = "cumulative"),
            selected = "yearly"
          ),
          
          hr(),
          downloadButton(outputId = "downloadPlot", "Save plot"),
          NULL
        ),
        mainPanel(
          width = 9,
          plotOutput(outputId = "CitationPlot", height = "500px"),
          NULL
        )
      )
    ),
    
    # Tab 3: Journal Quartiles ####
    # -------------------------------------------------------------------------#
    # Stacked bar chart showing the number of publications per year broken
    # down by the best SJR quartile of the journal. Preprints and unmatched
    # journals are excluded. Year range is independently adjustable.
    tabPanel(
      title = "Journal Quartiles",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          p("Publications per year by best SJR quartile.
             Preprints and unmatched journals are excluded."),
          radioButtons(
            inputId  = "qPlotMode",
            label    = "Plot mode",
            choices  = c("Yearly" = "yearly", "Cumulative" = "cumulative"),
            selected = "yearly"
          ),
          hr(),
          uiOutput(outputId = "qYearSlider"),
          hr(),
          downloadButton(outputId = "downloadQPlot", "Save plot"),
          NULL
        ),
        mainPanel(
          width = 9,
          plotOutput(outputId = "QPlot", height = "500px"),
          NULL
        )
      )
    ),
    
    # Tab 4: Research Profile ####
    # -------------------------------------------------------------------------#
    # Visualisations of the researcher's field of expertise derived from the
    # SJR subject classifications of their matched journals, and from the
    # text of their paper titles and abstracts.
    tabPanel(
      title = "Research Profile",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          h5("Display options"),
          radioButtons(
            inputId  = "profileView",
            label    = "Primary view",
            choices  = c(
              "Broad research areas" = "areas",
              "Journal categories"   = "categories",
              "Word cloud"           = "wordcloud",
              "Category network"     = "network"
            ),
            selected = "areas"
          ),
          
          hr(),
          
          # Year range slider for research profile — rendered dynamically
          uiOutput(outputId = "profileYearSlider"),
          
          hr(),
          
          # Word cloud controls — only shown when word cloud is selected
          conditionalPanel(
            condition = "input.profileView == 'wordcloud'",
            radioButtons(
              inputId  = "wcSource",
              label    = "Text source",
              choices  = c(
                "Paper titles"             = "titles",
                "Abstracts (if available)" = "abstracts",
                "Titles + abstracts"       = "both"
              ),
              selected = "titles"
            ),
            sliderInput(
              inputId = "wcMaxWords",
              label   = "Max words",
              min     = 20,
              max     = 200,
              value   = 100,
              step    = 10
            )
          ),
          
          # Network controls — only shown when network is selected
          conditionalPanel(
            condition = "input.profileView == 'network'",
            sliderInput(
              inputId = "netMinEdge",
              label   = "Minimum co-occurrences",
              min     = 1,
              max     = 10,
              value   = 2,
              step    = 1
            ),
            sliderInput(
              inputId = "netTopN",
              label   = "Top N categories",
              min     = 5,
              max     = 40,
              value   = 20,
              step    = 5
            )
          ),
          
          # Bar chart controls — shown for areas and categories views
          conditionalPanel(
            condition = "input.profileView == 'areas' ||
                         input.profileView == 'categories'",
            sliderInput(
              inputId = "barTopN",
              label   = "Top N to display",
              min     = 5,
              max     = 40,
              value   = 15,
              step    = 5
            )
          ),
          
          hr(),
          
          # Download button hidden for word cloud (HTML widget, not a ggplot)
          conditionalPanel(
            condition = "input.profileView != 'wordcloud'",
            downloadButton(outputId = "downloadProfilePlot", "Save plot")
          ),
          
          NULL
        ),
        
        mainPanel(
          width = 9,
          
          # ggplot-based views (areas, categories, network)
          conditionalPanel(
            condition = "input.profileView != 'wordcloud'",
            plotOutput(outputId = "ProfilePlot", height = "600px")
          ),
          
          # Word cloud HTML widget with hover tip footnote
          conditionalPanel(
            condition = "input.profileView == 'wordcloud'",
            wordcloud2::wordcloud2Output(
              outputId = "WordCloud",
              height   = "550px"
            ),
            br(),
            tags$p(
              style = "color:#555; font-size:13px; font-style:italic;",
              tags$b("Tip:"),
              " Hover over any word to see how many times it appears
                across the selected text source."
            )
          ),
          
          NULL
        )
      )
    ),
    
    # Tab 5: Glossary ####
    # -------------------------------------------------------------------------#
    # Definitions of all metrics and columns used throughout the app,
    # including how composite scores are calculated and why they are useful.
    tabPanel(
      title = "Glossary",
      fluidRow(
        column(
          width = 10,
          offset = 1,
          br(),
          
          h3("Metric Definitions"),
          p("This glossary explains all columns and metrics used in the
             Publications Table and plots."),
          
          hr(),
          
          # ── Google Scholar metrics ────────────────────────────────────────
          h4("Google Scholar Metrics"),
          
          tags$dl(
            
            tags$dt("Citations"),
            tags$dd("Total number of times this paper has been cited by other
                     works, as recorded by Google Scholar."),
            
            tags$dt("Cites / Year"),
            tags$dd("Total citations divided by the number of years since
                     publication. For papers published in the current year,
                     the raw citation count is used. This normalises for the
                     fact that older papers have had more time to accumulate
                     citations."),
            
            tags$dt("h-index"),
            tags$dd("The author-level h-index: the largest number h such that
                     h papers have each been cited at least h times. A
                     researcher with h-index 20 has 20 papers each cited at
                     least 20 times."),
            
            tags$dt("i10-index"),
            tags$dd("The number of publications with at least 10 citations.
                     A simple measure of the breadth of an author's cited
                     output.")
          ),
          
          hr(),
          
          # ── SJR journal metrics ───────────────────────────────────────────
          h4("SJR Journal Metrics"),
          
          tags$dl(
            
            tags$dt("SJR Score"),
            tags$dd("The SCImago Journal Rank indicator. A measure of a
                     journal's prestige based on the number of citations
                     received and the prestige of the citing journals,
                     weighted by subject field. Higher values indicate
                     greater influence."),
            
            tags$dt("Best Quartile"),
            tags$dd("The best quartile ranking (Q1–Q4) achieved by the
                     journal across all subject categories it belongs to.
                     Q1 is the top 25% of journals in a field; Q4 is the
                     bottom 25%. A journal may be Q1 in one field and Q3
                     in another — Best Quartile records the highest
                     (best) ranking."),
            
            tags$dt("Impact Factor (2yr)"),
            tags$dd("The average number of citations received per article
                     published in the journal over the preceding two years.
                     This is the SJR 'Citations / Doc. (2years)' field and
                     is analogous to the traditional Journal Impact Factor
                     (JIF). It reflects how frequently recent articles in
                     the journal are cited on average."),
            
            tags$dt("Journal H-Index"),
            tags$dd("The h-index of the journal itself (not the author):
                     the largest number h such that h articles published in
                     the journal have each been cited at least h times.
                     Journals with a high h-index have a sustained record
                     of producing highly cited work.")
          ),
          
          hr(),
          
          # ── Composite and derived metrics ─────────────────────────────────
          h4("Composite and Derived Metrics"),
          
          tags$dl(
            
            tags$dt("Paper Score"),
            tags$dd(
              "A composite metric designed to identify an author's most
               impactful papers for use in grant applications, CVs, or
               promotion cases that ask for a researcher's 'top N papers'.",
              tags$br(), tags$br(),
              "Calculated as:",
              tags$br(),
              tags$code("Paper Score = Cites/Year + Impact Factor (2yr)"),
              tags$br(), tags$br(),
              "This rewards papers that are both actively accumulating
               citations (high Cites/Year) and were published in journals
               with a strong citation rate (high Impact Factor). The
               Impact Factor used is the journal-level two-year citation
               rate from the SJR database (average citations per article
               over the preceding two years), which is directly comparable
               to the traditional Journal Impact Factor. Papers with no
               SJR match contribute only their Cites/Year component.",
              
              tags$br(), tags$br(),
              "Why it is useful: raw citation counts favour older papers
               simply because they have had more time to be cited. By
               using Cites/Year instead, the score reflects current
               relevance rather than historical accumulation. Adding the
               journal SJR score incorporates venue prestige, which
               matters in fields where citation counts alone can be
               misleading (e.g. clinical vs. basic science)."
            ),
            
            tags$dt("Citation-to-Impact Ratio"),
            tags$dd(
              "The author-level citation rate of a paper relative to the
               average citation rate of the journal it was published in.",
              tags$br(), tags$br(),
              "Calculated as:",
              tags$br(),
              tags$code("Citation-to-Impact Ratio = Cites/Year ÷ Impact Factor (2yr)"),
              tags$br(), tags$br(),
              "Interpretation:",
              tags$ul(
                tags$li(tags$b("Ratio = 1.0"),
                        " — the paper is being cited at exactly the average
                          rate for its journal"),
                tags$li(tags$b("Ratio > 1.0"),
                        " — the paper is outperforming the journal average;
                          it is cited more frequently than a typical article
                          in that venue"),
                tags$li(tags$b("Ratio < 1.0"),
                        " — the paper is cited less frequently than the
                          journal average"),
                tags$li(tags$b("NA"),
                        " — the journal impact factor is unavailable
                          (no SJR match, preprint, or impact factor of zero)")
              ),
              tags$br(),
              "This metric is particularly useful for identifying papers
               that have had outsized impact relative to the expectations
               set by the journal — a paper in a modest journal with a
               ratio of 3.0 may be more noteworthy than a paper in a
               top journal with a ratio of 0.5."
            ),
            
            tags$dt("Normalised Citation Score (NCS)"),
            tags$dd(
              "A field-normalised citation metric that accounts for
               differences in citation practices between research areas.",
              tags$br(), tags$br(),
              "Calculated as:",
              tags$br(),
              tags$code("NCS = Cites/Year ÷ mean(Cites/Year) across all papers in the same Best Quartile group"),
              tags$br(), tags$br(),
              "A score above 1.0 means the paper is cited more frequently
               per year than the average paper published in journals of
               the same quartile tier in this author's output. This
               provides a rough within-dataset normalisation that adjusts
               for the fact that Q1 journals tend to attract more citations
               than Q4 journals regardless of individual paper quality.",
              tags$br(), tags$br(),
              "Note: this is calculated relative to the author's own
               publication set, not against a global database, so it
               should be interpreted in that context."
            )
          ),
          
          hr(),
          
          # ── Match quality indicators ──────────────────────────────────────
          h4("Journal Match Quality"),
          
          tags$dl(
            
            tags$dt("Match Type"),
            tags$dd(
              tags$ul(
                tags$li(tags$b("exact"),
                        " — the Google Scholar journal name matched an SJR
                          title exactly after normalisation (lowercasing and
                          punctuation removal). Confidence = 100."),
                tags$li(tags$b("fuzzy"),
                        " — the best available SJR title was within the
                          Jaro-Winkler distance threshold but was not an
                          exact match. Confidence = 0–99."),
                tags$li(tags$b("preprint"),
                        " — the journal field matched a known preprint
                          server (e.g. bioRxiv, arXiv). No SJR match is
                          attempted. Impact factor = 0, quartile = NA."),
                tags$li(tags$b("none"),
                        " — no sufficiently close SJR match was found.
                          All SJR metric columns will be NA.")
              )
            ),
            
            tags$dt("Match Confidence (%)"),
            tags$dd(
              "A 0–100 score indicating how reliable the journal match is.",
              tags$br(), tags$br(),
              tags$code(
                "Confidence = (String Similarity × 0.80 +
                 Field Overlap × 0.20) × 100"
              ),
              tags$br(), tags$br(),
              "String Similarity is 1 minus the Jaro-Winkler distance
               between the normalised journal names. Field Overlap is the
               proportion of the candidate journal's research areas that
               appear in the author's inferred top fields (derived from
               exact-matched publications). Exact matches always score
               100; fuzzy matches are capped at 99."
            )
          ),
          
          hr(),
          
          # ── Data sources ──────────────────────────────────────────────────
          h4("Data Sources"),
          
          tags$dl(
            tags$dt("Google Scholar"),
            tags$dd("Publication metadata (title, authors, journal, year,
                     citations) is retrieved via the scholar R package.
                     Data accuracy depends on Google Scholar's indexing."),
            tags$dt("SCImago Journal Rankings (SJR)"),
            tags$dd(
              "Journal metrics are sourced from the SCImago Journal
               Rankings database (",
              tags$a("www.scimagojr.com",
                     href   = "https://www.scimagojr.com",
                     target = "_blank"),
              "). The pre-built database file is updated annually."
            )
          ),
          
          br(), br()
        )
      )
    )
    
  ) # end tabsetPanel
) # end fluidPage

# SERVER ####
# =============================================================================#
# The server function receives input values from the UI, performs reactive
# computations, and sends rendered outputs back to the UI. All reactive
# expressions are defined here — they only re-execute when their dependencies
# change, which avoids redundant API calls to Google Scholar.
# =============================================================================#

server <- function(input, output, session) {
  
  # SJR data loader reactive ####
  # -------------------------------------------------------------------------#
  # Loads the SJR tibble either from a user-uploaded file or from the
  # remote/local fallback chain defined in load_sjr_data() in global.R.
  # Re-runs if the user uploads a new file during the session.
  sjr_data <- reactive({
    upload <- input$sjrUpload
    if (!is.null(upload)) {
      load_sjr_data(local_path = upload$datapath)
    } else {
      load_sjr_data()
    }
  })
  
  # Scholar ID reactive ####
  # -------------------------------------------------------------------------#
  # Captures the Google Scholar ID from the text input only when the
  # Fetch Data button is clicked. eventReactive prevents the app from
  # attempting to fetch data before the user has confirmed their ID.
  getid <- eventReactive(input$go, {
    input$scholarId
  })
  
  # Raw citation history reactive ####
  # -------------------------------------------------------------------------#
  # Fetches the per-year citation counts for the author from Google Scholar.
  # Returns a data frame with columns: year, cites.
  citations <- reactive({
    req(getid())
    scholar::get_citation_history(getid())
  })
  
  # Raw publications reactive ####
  # -------------------------------------------------------------------------#
  # Fetches the full list of publications for the author from Google Scholar.
  # Returns a data frame with one row per publication.
  papers <- reactive({
    req(getid())
    scholar::get_publications(getid())
  })
  
  # Author profile details reactive ####
  # -------------------------------------------------------------------------#
  # Fetches the author's profile summary from Google Scholar, including
  # their name, affiliation, total citations, h-index, and i10-index.
  # Stored as a named list for easy reference throughout the server.
  details <- reactive({
    req(getid())
    tmp <- scholar::get_profile(getid())
    list(
      name        = tmp$name,
      affiliation = tmp$affiliation,
      total_cites = tmp$total_cites,
      h_index     = tmp$h_index,
      i10_index   = tmp$i10_index
    )
  })
  
  # Full author list reactive ####
  # -------------------------------------------------------------------------#
  # Only runs when the user has opted in via the fetchFullAuthors checkbox.
  # If the checkbox is unchecked, returns the original truncated author
  # strings immediately without making any web requests.
  #
  # When opted in, attempts two-stage author retrieval:
  #   Stage 1 — scholar::get_complete_authors()
  #   Stage 2 — CrossRef fallback for still-truncated results
  #
  # If a CrossRef email is provided it is set via options() before the first
  # CrossRef request and cleared afterwards to avoid persisting between
  # sessions.
  full_authors <- reactive({
    req(papers(), getid())
    
    pubs <- papers()
    id   <- getid()
    
    # Return original author strings immediately if opt-in is not selected
    if (!isTRUE(input$fetchFullAuthors)) {
      return(pubs$author)
    }
    
    # Set CrossRef polite pool email if provided by the user
    # Store the previous value so it can be restored after fetching
    prev_email <- getOption("rcrossref.email", default = NULL)
    email_input <- trimws(input$crossrefEmail)
    
    if (nchar(email_input) > 0) {
      options(rcrossref.email = email_input)
    }
    
    # Ensure the CrossRef email option is restored when this reactive exits,
    # whether it exits normally or due to an error
    on.exit({
      if (is.null(prev_email)) {
        options(rcrossref.email = NULL)
      } else {
        options(rcrossref.email = prev_email)
      }
    }, add = TRUE)
    
    # Initialise result vector with original author strings as fallback
    result <- pubs$author
    
    # Open progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close(), add = TRUE)
    progress$set(
      message = "Fetching full author lists...",
      detail  = "This may take several minutes for large publication lists.",
      value   = 0
    )
    
    for (i in seq_len(nrow(pubs))) {
      
      # Update progress indicator with current paper title
      progress$set(
        value  = i / nrow(pubs),
        detail = paste0(
          "Paper ", i, " of ", nrow(pubs), ": ",
          substr(pubs$title[i], 1, 40),
          if (nchar(pubs$title[i]) > 40) "..." else ""
        )
      )
      
      # ── Stage 1: scholar::get_complete_authors() ──────────────────────────
      # Scrapes the individual Google Scholar page for this publication to
      # retrieve the full author list as displayed by Scholar
      stage1_result <- tryCatch(
        scholar::get_complete_authors(id, pubs$pubid[i]),
        error   = function(e) pubs$author[i],
        warning = function(w) pubs$author[i]
      )
      
      # Treat NULL, empty, or whitespace-only results as failures
      if (is.null(stage1_result)        ||
          length(stage1_result) == 0    ||
          trimws(stage1_result) == "") {
        stage1_result <- pubs$author[i]
      }
      
      # Polite delay after each Scholar request
      Sys.sleep(0.5)
      
      # ── Stage 2: CrossRef fallback ─────────────────────────────────────────
      # Only attempted if Stage 1 result still appears truncated
      if (is_truncated_authors(stage1_result)) {
        
        progress$set(
          value  = i / nrow(pubs),
          detail = paste0(
            "Paper ", i, " of ", nrow(pubs),
            " (CrossRef fallback)..."
          )
        )
        
        stage2_result <- fetch_authors_crossref(
          title            = pubs$title[i],
          fallback_authors = stage1_result
        )
        
        # Polite delay after CrossRef request
        Sys.sleep(1)
        
        result[i] <- stage2_result
        
      } else {
        result[i] <- stage1_result
      }
    }
    
    result
  })
  
  
  
  
  # Merged yearly citation and paper count data reactive ####
  # -------------------------------------------------------------------------#
  # Combines the citation history with a per-year paper count derived from
  # the publications list. The result is a single data frame with one row
  # per year containing both citation and paper counts, plus cumulative
  # versions of each for use in the cumulative plot mode.
  dat <- reactive({
    req(citations(), papers())
    
    ct <- citations()
    p  <- papers()
    
    # Remove publications with no year recorded
    p <- p[complete.cases(p$year), ]
    
    # Count publications per year using table(), then convert to data frame
    papersPerYear      <- as.data.frame(
      table(p$year),
      stringsAsFactors = FALSE
    )
    papersPerYear      <- dplyr::rename(papersPerYear, "year" = "Var1")
    papersPerYear$year <- as.numeric(as.character(papersPerYear$year))
    
    # Full join ensures years present in either source are retained,
    # with NA filled as 0 for years with no citations or no papers
    ct <- dplyr::full_join(ct, papersPerYear, by = "year")
    ct <- dplyr::arrange(ct, year)
    ct[is.na(ct)] <- 0
    
    # Add cumulative columns for use when cumulative plot mode is selected
    ct$cumulative_cites  <- cumsum(ct$cites)
    ct$cumulative_papers <- cumsum(ct$Freq)
    
    ct
  })
  
  # Enriched publications reactive ####
  # -------------------------------------------------------------------------#
  # The central data reactive for the app. Takes the raw Google Scholar
  # publications, renames columns for consistency, calculates derived
  # metrics, runs the SJR fuzzy matching, and adds composite scores.
  # All downstream outputs (table, plots, profile) draw from this reactive.
  enriched_papers <- reactive({
    req(papers(), sjr_data())
    
    p <- as_tibble(papers())
    
    # Remove publications with no year recorded
    p <- p[complete.cases(p$year), ]
    
    # Rename Google Scholar columns to avoid conflicts with SJR columns
    # after the join, and to make their source explicit
    names(p)[names(p) == "journal"] <- "gs_journal"
    names(p)[names(p) == "title"]   <- "gs_title"
    names(p)[names(p) == "cites"]   <- "gs_cites"
    names(p)[names(p) == "year"]    <- "gs_year"
    
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    
    # Ensure numeric types for calculation columns
    p <- dplyr::mutate(
      p,
      gs_cites = as.numeric(gs_cites),
      gs_year  = as.numeric(gs_year)
    )
    
    # Cites per year: for papers not published in the current year, divide
    # total citations by years since publication. For current-year papers,
    # use the raw citation count to avoid division by zero.
    p$cites_per_year <- ifelse(
      p$gs_year != current_year,
      p$gs_cites / (current_year - p$gs_year),
      p$gs_cites
    )
    
    # Run SJR fuzzy matching — adds match_type, confidence_score,
    # sjr_match_title, is_preprint, and all SJR metric columns
    matched <- match_publications_to_sjr(p, sjr_data())
    
    # Numeric best quartile for sorting (Q1=1, Q4=4, NA=5 sorts last)
    matched$best_q_numeric <- dplyr::case_when(
      matched$SJR.Best.Quartile == "Q1" ~ 1L,
      matched$SJR.Best.Quartile == "Q2" ~ 2L,
      matched$SJR.Best.Quartile == "Q3" ~ 3L,
      matched$SJR.Best.Quartile == "Q4" ~ 4L,
      TRUE ~ NA_integer_
    )
    
    # Replace truncated author strings with full author lists ####
    # -----------------------------------------------------------------------
    # full_authors() is a separate reactive that fetches complete author data
    # via Scholar and CrossRef. It returns a character vector of the same
    # length as the publications tibble. We check that the lengths match
    # before replacing to guard against any edge case where the two reactives
    # return different row counts (e.g. if papers() changes mid-session).
    fa <- tryCatch(
      full_authors(),
      error = function(e) NULL
    )
    
    if (!is.null(fa) && length(fa) == nrow(matched)) {
      matched$author <- fa
    }
    
    
    # Convert impact factor from European decimal format (comma) to numeric.
    # Citations.per.Doc.2years is the SJR field representing the average
    # number of citations received per article over the preceding two years —
    # this is the journal-level impact factor used in all downstream metrics.
    if_val <- suppressWarnings(
      as.numeric(gsub(",", ".", matched$Citations.per.Doc.2years))
    )
    
    # Store the converted numeric impact factor back onto the tibble so it
    # is available in the correct format for the display table
    matched$impact_factor_2yr <- if_val
    
    # Paper Score: citations per year + journal impact factor
    # Rewards papers that are both actively accumulating citations (high
    # cites_per_year) and were published in journals with a strong citation
    # rate (high impact factor). Papers with no SJR match contribute only
    # their cites_per_year component (impact factor treated as 0).
    matched$paper_score <- matched$cites_per_year + ifelse(
      is.na(if_val), 0, if_val
    )
    
    # Citation-to-Impact Ratio: author-level cites per year for this paper
    # divided by the journal-level impact factor.
    # Values above 1.0 indicate the paper is cited more frequently per year
    # than the average article in that journal.
    # NA when impact factor is unavailable or zero (avoids division by zero).
    matched$citation_impact_ratio <- ifelse(
      !is.na(if_val) & if_val > 0,
      round(matched$cites_per_year / if_val, 3),
      NA_real_
    )
    
    
    # Normalised Citation Score: cites per year relative to the mean for
    # papers in the same best quartile group within this author's output.
    # Scores above 1.0 indicate above-average performance within the tier.
    matched <- matched |>
      dplyr::group_by(SJR.Best.Quartile) |>
      dplyr::mutate(
        ncs_group_mean = mean(cites_per_year, na.rm = TRUE),
        normalised_citation_score = ifelse(
          !is.na(ncs_group_mean) & ncs_group_mean > 0,
          round(cites_per_year / ncs_group_mean, 3),
          NA_real_
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-ncs_group_mean)
    
    # Default sort: highest paper score first
    dplyr::arrange(matched, desc(paper_score))
  })
  
  # Hidden rows tracker ####
  # -------------------------------------------------------------------------#
  # Maintains a reactive vector of pubid values for rows the user has chosen
  # to hide. Hidden rows are excluded from all plots and the download but
  # remain in the session so they can be restored. Uses reactiveVal so it
  # can be updated by the hide and restore action buttons.
  hidden_pubids <- reactiveVal(character(0))
  
  # Hide selected rows when the hide button is clicked
  observeEvent(input$hideSelected, {
    req(input$MetricsTable_rows_selected)
    
    # Get the pubid values for the selected rows from the visible papers
    selected_idx  <- input$MetricsTable_rows_selected
    visible       <- visible_papers()
    selected_ids  <- visible$pubid[selected_idx]
    
    # Add newly hidden pubids to the existing hidden set (union avoids dupes)
    hidden_pubids(union(hidden_pubids(), selected_ids))
  })
  
  # Restore all hidden rows when the restore button is clicked
  observeEvent(input$restoreAll, {
    hidden_pubids(character(0))
  })
  
  # Visible papers reactive ####
  # -------------------------------------------------------------------------#
  # Returns enriched_papers() with hidden rows removed. All plots and the
  # download handler use this reactive rather than enriched_papers() directly
  # so that hiding a row propagates consistently everywhere.
  visible_papers <- reactive({
    req(enriched_papers())
    pubs <- enriched_papers()
    
    # Filter out any pubid recorded in the hidden set
    if (length(hidden_pubids()) > 0) {
      pubs <- dplyr::filter(pubs, !pubid %in% hidden_pubids())
    }
    
    pubs
  })
  
  # Dynamic year slider: Citations & Papers tab ####
  # -------------------------------------------------------------------------#
  # Rendered after data is fetched so the min/max range reflects the actual
  # span of the author's publication history.
  output$slider <- renderUI({
    req(dat())
    range <- dat()$year
    sliderInput(
      inputId = "yearRange",
      label   = "Year range",
      min     = min(range),
      max     = max(range),
      value   = c(min(range), max(range)),
      step    = 1,
      sep     = ""
    )
  })
  
  # Dynamic year slider: Journal Quartiles tab ####
  output$qYearSlider <- renderUI({
    req(enriched_papers())
    years <- enriched_papers()$gs_year
    years <- years[!is.na(years)]
    sliderInput(
      inputId = "qYearRange",
      label   = "Year range",
      min     = min(years),
      max     = max(years),
      value   = c(min(years), max(years)),
      step    = 1,
      sep     = ""
    )
  })
  
  # Dynamic year slider: Research Profile tab ####
  output$profileYearSlider <- renderUI({
    req(enriched_papers())
    years <- enriched_papers()$gs_year
    years <- years[!is.na(years)]
    sliderInput(
      inputId = "profileYearRange",
      label   = "Year range",
      min     = min(years),
      max     = max(years),
      value   = c(min(years), max(years)),
      step    = 1,
      sep     = ""
    )
  })
  
  # Profile summary strip ####
  # -------------------------------------------------------------------------#
  # Renders a summary bar above the publications table showing the author's
  # name, affiliation, and key Google Scholar metrics.
  output$profileSummary <- renderUI({
    req(details())
    d <- details()
    tags$div(
      style = "background:#f8f9fa; padding:10px; border-radius:6px;",
      tags$b(d$name), " \u2014 ", d$affiliation, tags$br(),
      tags$b("Total citations: "), d$total_cites, " | ",
      tags$b("h-index: "),         d$h_index,     " | ",
      tags$b("i10-index: "),       d$i10_index
    )
  })
  
  # Publications DT table ####
  # -------------------------------------------------------------------------#
  # Renders the interactive publications table using the DT package.
  # Displays visible papers only (hidden rows are excluded).
  # Columns are selected, converted, and renamed before passing to DT.
  # formatRound is applied separately for general metrics (2 dp) and the
  # impact factor (2 dp) to avoid integer rounding of decimal values.
  # Colour coding is applied to Match Type and Best Quartile columns.
  output$MetricsTable <- DT::renderDataTable({
    req(visible_papers())
    
    tbl <- visible_papers()
    
    # Select columns for display in the order they should appear
    display_cols <- c(
      "gs_title", "author", "gs_journal", "gs_year", "gs_cites",
      "cites_per_year", "paper_score",
      "citation_impact_ratio", "normalised_citation_score",
      "sjr_match_title", "match_type", "confidence_score",
      "SJR.Best.Quartile", "H.index",
      "Citations.per.Doc.2years",
      "Areas", "Categories"
    )
    
    # Retain only columns that exist in the joined tibble
    # (SJR columns will be absent for unmatched papers)
    display_cols <- display_cols[display_cols %in% names(tbl)]
    tbl          <- tbl[, display_cols]
    
    # Convert SJR comma-decimal columns to numeric before renaming
    # The SJR export uses European decimal format (e.g. "45,22" = 45.22)
    if ("Citations.per.Doc.2years" %in% names(tbl)) {
      tbl$Citations.per.Doc.2years <- as.numeric(
        gsub(",", ".", tbl$Citations.per.Doc.2years)
      )
    }
    
    # Map internal column names to display-friendly labels
    col_rename_map <- c(
      "gs_title"                 = "Title",
      "author"                   = "Authors",
      "gs_journal"               = "Journal (Scholar)",
      "gs_year"                  = "Year",
      "gs_cites"                 = "Citations",
      "cites_per_year"           = "Cites/Year",
      "paper_score"              = "Paper Score",
      "citation_impact_ratio"    = "Citation/Impact Ratio",
      "normalised_citation_score"= "Normalised Citation Score",
      "sjr_match_title"          = "Matched SJR Journal",
      "match_type"               = "Match Type",
      "confidence_score"         = "Match Confidence (%)",
      "SJR.Best.Quartile"        = "Best Quartile",
      "H.index"                  = "Journal H-Index",
      "Citations.per.Doc.2years" = "Impact Factor (2yr)",
      "Areas"                    = "Research Areas",
      "Categories"               = "Categories"
    )
    
    # Apply renames only for columns that are present
    for (old in names(col_rename_map)) {
      if (old %in% names(tbl)) {
        names(tbl)[names(tbl) == old] <- col_rename_map[[old]]
      }
    }
    
    # Build list of numeric columns to round to 2 dp, excluding impact factor
    # which is handled separately to guarantee decimal places are retained
    round_cols <- c(
      "Cites/Year", "Paper Score", "Match Confidence (%)",
      "Citation/Impact Ratio", "Normalised Citation Score"
    )
    round_cols <- round_cols[round_cols %in% names(tbl)]
    
    impact_col <- if ("Impact Factor (2yr)" %in% names(tbl)) {
      "Impact Factor (2yr)"
    } else {
      character(0)
    }
    
    DT::datatable(
      tbl,
      filter     = "top",      # per-column filter inputs above each column
      rownames   = FALSE,       # suppress row numbers in first column
      extensions = c("Buttons", "ColReorder"),
      options    = list(
        dom        = "Blfrtip",
        buttons    = c("copy", "csv", "excel", "pdf", "print"),
        pageLength = 25,
        lengthMenu = list(
          c(10, 25, 50, -1),
          c("10", "25", "50", "All")
        ),
        colReorder = TRUE,      # allow drag-to-reorder columns
        scrollX    = TRUE,      # horizontal scroll for wide tables
        autoWidth  = TRUE,
        order      = list(list(6, "desc"))  # default sort: Paper Score desc
      )
    ) |>
      DT::formatRound(columns = round_cols, digits = 2) |>
      DT::formatRound(columns = impact_col, digits = 2) |>
      DT::formatStyle(
        "Match Type",
        backgroundColor = DT::styleEqual(
          c("exact",   "fuzzy",   "preprint", "none"),
          c("#d4edda", "#fff3cd", "#d6eaf8",  "#f8d7da")
        )
      ) |>
      DT::formatStyle(
        "Best Quartile",
        backgroundColor = DT::styleEqual(
          c("Q1",     "Q2",     "Q3",     "Q4"),
          c("#d4edda","#cce5ff","#fff3cd","#f8d7da")
        )
      )
  })
  
  # Download handler: Publications table ####
  # -------------------------------------------------------------------------#
  # Saves the visible (non-hidden) enriched publications as a TSV file.
  # The filename follows the YYYY-MM-DD-Firstname-Surname-OutputType.ext
  # convention defined in make_export_filename() in global.R.
  output$downloadTable <- downloadHandler(
    filename = function() {
      make_export_filename(details(), "PublicationList", "tsv")
    },
    content = function(file) {
      write.table(
        visible_papers(),
        file,
        row.names = FALSE,
        sep       = "\t"
      )
    },
    contentType = "text/tab-separated-values"
  )
  
  # Citations & Papers plot builder ####
  # -------------------------------------------------------------------------#
  # Builds the dual-axis plot of citations and papers over time.
  # Papers are the primary (left) axis rendered as blue columns.
  # Citations are the secondary (right) axis rendered as a grey line.
  # Each series has its own yearly/cumulative mode and show/hide toggle.
  # The secondary axis transform scales citations onto the papers axis
  # so both series share the same plot area without distortion.
  plotInput <- function() {
    req(dat(), input$yearRange)
    
    # Filter to the selected year range
    plotdata <- dplyr::filter(
      dat(),
      year >= input$yearRange[1],
      year <= input$yearRange[2]
    )
    
    info <- details()
    
    # Select the appropriate column for each series based on mode
    cite_col  <- if (input$citationMode == "cumulative") {
      "cumulative_cites"
    } else {
      "cites"
    }
    paper_col <- if (input$paperMode == "cumulative") {
      "cumulative_papers"
    } else {
      "Freq"
    }
    
    # Axis labels reflect the selected mode for each series
    y_left  <- if (input$paperMode == "cumulative") {
      "Cumulative Papers"
    } else {
      "Annual Papers"
    }
    y_right <- if (input$citationMode == "cumulative") {
      "Cumulative Citations"
    } else {
      "Annual Citations"
    }
    
    max_cites  <- max(plotdata[[cite_col]],  na.rm = TRUE)
    max_papers <- max(plotdata[[paper_col]], na.rm = TRUE)
    
    # Transform scales citations down onto the papers (primary) axis.
    # The 2/3 factor prevents the citation line from dominating the plot.
    transform <- if (max_papers > 0 && max_cites > 0) {
      round(max_papers / max_cites * (2 / 3), digits = 4)
    } else {
      1
    }
    
    p <- ggplot(plotdata, aes(x = year))
    
    # Papers series: blue columns on the primary (left) axis
    if (isTRUE(input$showPapers)) {
      p <- p + geom_col(
        aes(y = .data[[paper_col]]),
        fill      = "white",
        colour    = "#2c7bb6",
        linewidth = 1
      )
    }
    
    # Citations series: grey line scaled onto the primary axis via transform
    if (isTRUE(input$showCitations)) {
      p <- p +
        geom_line(
          aes(y = .data[[cite_col]] * transform),
          linewidth = 2,
          colour    = "grey50"
        ) +
        geom_point(
          aes(y = .data[[cite_col]] * transform),
          size   = 4,
          colour = "black"
        )
    }
    
    # Secondary axis inverts the transform to show true citation values.
    # Only rendered if the citations series is visible.
    sec_axis_spec <- if (isTRUE(input$showCitations)) {
      sec_axis(
        trans  = ~ . / transform,
        name   = paste0(y_right, " \u2014 Grey Line"),
        labels = scales::label_comma()
      )
    } else {
      waiver()
    }
    
    p +
      scale_x_continuous(
        breaks = seq(min(plotdata$year), max(plotdata$year), by = 1)
      ) +
      scale_y_continuous(
        name   = paste0(y_left, " \u2014 Blue Columns"),
        breaks = function(limits) {
          raw <- scales::breaks_pretty(n = 6)(limits)
          raw[raw == floor(raw)]
        },
        labels   = scales::label_number(accuracy = 1),
        sec.axis = sec_axis_spec
      ) +
      xlab("Year") +
      ggtitle(
        label = paste0(
          info$name,
          " \u2014 Citations: ", info$total_cites,
          ", h-index: ",         info$h_index,
          ", i10-index: ",       info$i10_index
        ),
        subtitle = paste(
          "Data accessed:", format(Sys.Date(), "%b %d %Y"),
          "| github.com/alegione/ScholarPlot"
        )
      ) +
      theme_bw(base_size = 18) +
      theme(
        plot.title         = element_text(hjust  = 0.5, face = "bold",
                                          size   = 20,  colour = "black"),
        plot.subtitle      = element_text(hjust  = 0.5, size  = 13,
                                          colour = "black"),
        axis.title.x       = element_text(face   = "bold", size   = 18,
                                          colour = "black"),
        axis.text.x        = element_text(face   = "bold", size   = 16,
                                          colour = "black",
                                          angle  = 0,     hjust  = 0.5),
        axis.title.y       = element_text(face   = "bold", size   = 18,
                                          colour = "black",
                                          margin = margin(r = 10)),
        axis.title.y.right = element_text(face   = "bold", size   = 18,
                                          colour = "black",
                                          margin = margin(l = 10)),
        axis.text.y        = element_text(face   = "bold", size   = 16,
                                          colour = "black"),
        panel.grid.minor   = element_blank(),
        panel.border       = element_rect(colour = "black")
      )
  }
  
  output$CitationPlot <- renderPlot(height = 500, { plotInput() })
  
  # Download handler: Citations & Papers plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      make_export_filename(details(), "CitationPlot", "jpeg")
    },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "jpeg",
             width = 11.69, height = 8.27)
    }
  )
  # Q-value plot builder ####
  # -------------------------------------------------------------------------#
  # Builds the stacked bar chart of publications per year by SJR best
  # quartile. Preprints and hidden rows are excluded before plotting.
  # A complete year x quartile grid is constructed before counting to
  # ensure every quartile carries forward correctly in cumulative mode.
  qPlotInput <- function() {
    req(enriched_papers(), input$qPlotMode)
    
    # Start from visible (non-hidden) publications only
    qdata <- visible_papers()
    
    # Apply the quartile tab year range filter if the slider has rendered
    if (!is.null(input$qYearRange)) {
      qdata <- dplyr::filter(
        qdata,
        gs_year >= input$qYearRange[1],
        gs_year <= input$qYearRange[2]
      )
    }
    
    # Exclude preprints and unmatched journals — these have no quartile
    qdata <- dplyr::filter(
      qdata,
      !is.na(SJR.Best.Quartile),
      SJR.Best.Quartile %in% c("Q1", "Q2", "Q3", "Q4"),
      !is.na(gs_year)
    )
    
    # Return an informative empty plot if nothing remains after filtering
    if (nrow(qdata) == 0) {
      return(
        ggplot() +
          annotate(
            "text", x = 0.5, y = 0.5,
            label = paste(
              "No quartile data available.",
              "Ensure SJR data is loaded and publications are matched.",
              sep = "\n"
            ),
            size = 6, hjust = 0.5
          ) +
          theme_void()
      )
    }
    
    # Build a complete year x quartile grid so that missing combinations
    # are filled with zero rather than dropped. This prevents cumulative
    # sums from losing a quartile in years where no new papers were published
    # in that category.
    all_years     <- seq(min(qdata$gs_year), max(qdata$gs_year))
    all_quartiles <- c("Q1", "Q2", "Q3", "Q4")
    
    full_grid <- tidyr::expand_grid(
      year     = all_years,
      quartile = all_quartiles
    )
    
    # Count observed publications per year per quartile
    q_counts <- qdata |>
      dplyr::count(gs_year, SJR.Best.Quartile) |>
      dplyr::rename(year = gs_year, quartile = SJR.Best.Quartile)
    
    # Left join onto the full grid — missing combinations become NA then 0
    q_counts <- dplyr::left_join(
      full_grid, q_counts,
      by = c("year", "quartile")
    )
    q_counts$n[is.na(q_counts$n)] <- 0L
    
    # Apply cumulative sum per quartile if cumulative mode is selected
    if (input$qPlotMode == "cumulative") {
      q_counts <- q_counts |>
        dplyr::group_by(quartile) |>
        dplyr::arrange(year, .by_group = TRUE) |>
        dplyr::mutate(n = cumsum(n)) |>
        dplyr::ungroup()
      y_label <- "Cumulative Papers"
    } else {
      y_label <- "Papers per Year"
    }
    
    # Drop quartiles that are zero across all years to keep the plot clean
    # (e.g. an author with no Q3/Q4 papers will not show those in the legend)
    active_quartiles <- q_counts |>
      dplyr::group_by(quartile) |>
      dplyr::summarise(total = sum(n), .groups = "drop") |>
      dplyr::filter(total > 0) |>
      dplyr::pull(quartile)
    
    q_counts <- dplyr::filter(q_counts, quartile %in% active_quartiles)
    
    # Factor with Q4 first so that Q1 renders on top of the stacked bars
    # and the legend is reversed to match (Q1 at top of legend)
    q_counts$quartile <- factor(
      q_counts$quartile,
      levels = c("Q4", "Q3", "Q2", "Q1")
    )
    
    q_colours <- c(
      "Q1" = "#2c7bb6",
      "Q2" = "#abd9e9",
      "Q3" = "#fdae61",
      "Q4" = "#d7191c"
    )
    
    info <- details()
    
    ggplot(q_counts, aes(x = year, y = n, fill = quartile)) +
      geom_col(position = "stack", colour = "white", linewidth = 0.4) +
      scale_fill_manual(
        values = q_colours,
        name   = "Best Quartile",
        guide  = guide_legend(reverse = TRUE)
      ) +
      scale_x_continuous(
        breaks = seq(min(q_counts$year), max(q_counts$year), by = 1)
      ) +
      scale_y_continuous(
        breaks = function(limits) {
          raw <- scales::breaks_pretty(n = 6)(limits)
          raw[raw == floor(raw)]
        },
        labels = scales::label_number(accuracy = 1)
      ) +
      labs(
        title    = paste0(info$name,
                          " \u2014 Publications by Journal Quartile"),
        subtitle = paste(
          "Data accessed:", format(Sys.Date(), "%b %d %Y"),
          "| github.com/alegione/ScholarPlot"
        ),
        x = "Year",
        y = y_label
      ) +
      theme_bw(base_size = 18) +
      theme(
        plot.title    = element_text(hjust   = 0.5, face = "bold",
                                     size   = 20,  colour = "black"),
        plot.subtitle = element_text(hjust   = 0.5, size  = 13,
                                     colour = "black"),
        axis.title.x  = element_text(face    = "bold", size   = 18,
                                     colour = "black"),
        axis.text.x   = element_text(face    = "bold", size   = 16,
                                     colour = "black",
                                     angle  = 0,     hjust  = 0.5),
        axis.title.y  = element_text(face    = "bold", size   = 18,
                                     colour = "black"),
        axis.text.y   = element_text(face    = "bold", size   = 16,
                                     colour = "black"),
        legend.title  = element_text(face    = "bold", size   = 16,
                                     colour = "black"),
        legend.text   = element_text(size    = 15,    colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border     = element_rect(colour = "black")
      )
  }
  
  output$QPlot <- renderPlot(height = 500, { qPlotInput() })
  
  # Download handler: Quartile plot
  output$downloadQPlot <- downloadHandler(
    filename = function() {
      make_export_filename(details(), "QuartilePlot", "jpeg")
    },
    content = function(file) {
      ggsave(file, plot = qPlotInput(), device = "jpeg",
             width = 11.69, height = 8.27)
    }
  )
  
  # Research Profile: year-filtered publications reactive ####
  # -------------------------------------------------------------------------#
  # Filters the visible (non-hidden) enriched publications by the year range
  # selected in the Research Profile tab slider. All profile views (bar
  # charts, word cloud, network) draw from this reactive so the year filter
  # applies consistently across all of them.
  profile_pubs <- reactive({
    req(enriched_papers())
    pubs <- visible_papers()
    if (!is.null(input$profileYearRange)) {
      pubs <- dplyr::filter(
        pubs,
        gs_year >= input$profileYearRange[1],
        gs_year <= input$profileYearRange[2]
      )
    }
    pubs
  })
  
  # Abstract fetcher ####
  # -------------------------------------------------------------------------#
  # Attempts to retrieve the abstract for each publication via the scholar
  # package. get_publication_abstract() can return NULL, a zero-length
  # vector, or NA when an abstract is unavailable (e.g. the paper is not
  # indexed with an abstract, or the request fails). All failure modes are
  # caught and replaced with an empty string so the result vector always
  # has the same length as the publications tibble.
  abstracts <- reactive({
    req(enriched_papers(), getid())
    
    pubs   <- enriched_papers()
    id     <- getid()
    result <- vector("character", nrow(pubs))
    
    if ("pubid" %in% names(pubs)) {
      for (i in seq_len(nrow(pubs))) {
        raw <- tryCatch(
          scholar::get_publication_abstract(id, pubs$pubid[i]),
          error   = function(e) NULL,
          warning = function(w) NULL
        )
        # Assign empty string for any failure mode before assignment
        result[i] <- if (
          is.null(raw)       ||
          length(raw) == 0   ||
          !is.character(raw) ||
          all(is.na(raw))
        ) {
          ""
        } else {
          # Collapse in case multiple strings are returned by the API
          paste(raw, collapse = " ")
        }
      }
    }
    
    result
  })
  
  # Word frequency reactive ####
  # -------------------------------------------------------------------------#
  # Tokenises the selected text source (titles, abstracts, or both) after
  # applying the profile year filter. Returns a frequency-sorted tibble of
  # words and counts for use by the word cloud renderer.
  word_frequencies <- reactive({
    req(profile_pubs())
    
    pubs <- profile_pubs()
    
    text_to_use <- switch(input$wcSource,
                          "titles"    = pubs$gs_title,
                          "abstracts" = abstracts(),
                          "both"      = c(pubs$gs_title, abstracts())
    )
    
    # Remove NULL, NA, empty, and whitespace-only strings
    text_to_use <- text_to_use[
      !is.null(text_to_use) &
        !is.na(text_to_use)   &
        trimws(text_to_use) != ""
    ]
    
    # Return an empty tibble with correct column types if nothing survives
    # filtering — the word cloud renderer checks nrow == 0 and returns NULL
    if (length(text_to_use) == 0) {
      return(tibble::tibble(word = character(0), n = integer(0)))
    }
    
    tokenise_text(text_to_use)
  })
  
  # Research Profile plot builder ####
  # -------------------------------------------------------------------------#
  # Builds one of four views depending on input$profileView:
  #   "areas"      — horizontal bar chart of broad SJR research areas
  #   "categories" — horizontal bar chart of SJR subject categories
  #   "network"    — category co-occurrence force-directed network
  # The word cloud is handled separately by output$WordCloud below.
  profilePlotInput <- function() {
    req(profile_pubs(), input$profileView)
    
    pubs <- profile_pubs()
    info <- details()
    
    # Shared subtitle including data source watermark
    subtitle_str <- paste(
      "Data accessed:", format(Sys.Date(), "%b %d %Y"),
      "| github.com/alegione/ScholarPlot"
    )
    
    # Shared theme applied to all bar chart views
    profile_theme <- theme_bw(base_size = 18) +
      theme(
        plot.title    = element_text(hjust   = 0.5, face = "bold",
                                     size   = 20,  colour = "black"),
        plot.subtitle = element_text(hjust   = 0.5, size  = 13,
                                     colour = "black"),
        axis.text     = element_text(face    = "bold", size   = 15,
                                     colour = "black"),
        axis.title.x  = element_text(face    = "bold", size   = 18,
                                     colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border     = element_rect(colour = "black")
      )
    
    # Shared integer-only y axis breaks and labels
    integer_y <- list(
      scale_y_continuous(
        breaks = function(limits) {
          raw <- scales::breaks_pretty(n = 6)(limits)
          raw[raw == floor(raw)]
        },
        labels = scales::label_number(accuracy = 1)
      )
    )
    
    if (input$profileView == "areas") {
      
      # Broad research areas bar chart ####
      area_counts <- summarise_areas(pubs) |>
        dplyr::slice_head(n = input$barTopN)
      
      if (nrow(area_counts) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "No area data available.",
                     size = 6, hjust = 0.5) +
            theme_void()
        )
      }
      
      # Factor levels set in reverse so the most frequent area appears
      # at the top of the horizontal bar chart after coord_flip()
      area_counts$area <- factor(area_counts$area,
                                 levels = rev(area_counts$area))
      
      ggplot(area_counts, aes(x = area, y = n, fill = n)) +
        geom_col(colour = "black", linewidth = 0.4) +
        coord_flip() +
        scale_fill_gradientn(
          colours = RColorBrewer::brewer.pal(9, "Blues")[3:9],
          guide   = "none"
        ) +
        integer_y +
        labs(
          title    = paste0(info$name, " \u2014 Broad Research Areas"),
          subtitle = subtitle_str,
          x        = NULL,
          y        = "Number of publications"
        ) +
        profile_theme
      
    } else if (input$profileView == "categories") {
      
      # Journal subject categories bar chart ####
      cat_counts <- summarise_categories(pubs) |>
        dplyr::slice_head(n = input$barTopN)
      
      if (nrow(cat_counts) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "No category data available.",
                     size = 6, hjust = 0.5) +
            theme_void()
        )
      }
      
      cat_counts$category <- factor(cat_counts$category,
                                    levels = rev(cat_counts$category))
      
      ggplot(cat_counts, aes(x = category, y = n, fill = n)) +
        geom_col(colour = "black", linewidth = 0.4) +
        coord_flip() +
        scale_fill_gradientn(
          colours = RColorBrewer::brewer.pal(9, "Greens")[3:9],
          guide   = "none"
        ) +
        integer_y +
        labs(
          title    = paste0(info$name, " \u2014 Journal Subject Categories"),
          subtitle = subtitle_str,
          x        = NULL,
          y        = "Number of publications"
        ) +
        profile_theme
      
    } else if (input$profileView == "network") {
      
      # Category co-occurrence network ####
      edges <- build_category_cooccurrence(
        pubs,
        min_edge_weight = input$netMinEdge
      )
      
      if (is.null(edges) || nrow(edges) == 0) {
        return(
          ggplot() +
            annotate(
              "text", x = 0.5, y = 0.5,
              label = paste(
                "Insufficient co-occurrence data.",
                "Try lowering the minimum co-occurrences slider.",
                sep = "\n"
              ),
              size = 6, hjust = 0.5
            ) +
            theme_void()
        )
      }
      
      # Restrict to the top N most frequent categories to keep the
      # network readable — very large networks become uninterpretable
      top_cats <- summarise_categories(pubs) |>
        dplyr::slice_head(n = input$netTopN) |>
        dplyr::pull(category)
      
      edges <- dplyr::filter(
        edges,
        from %in% top_cats & to %in% top_cats
      )
      
      if (nrow(edges) == 0) {
        return(
          ggplot() +
            annotate(
              "text", x = 0.5, y = 0.5,
              label = paste(
                "No edges remain after filtering.",
                "Try increasing Top N or lowering minimum co-occurrences.",
                sep = "\n"
              ),
              size = 6, hjust = 0.5
            ) +
            theme_void()
        )
      }
      
      # Node strength = sum of all edge weights connected to that node
      # Used to scale node size so highly connected categories appear larger
      node_strength <- dplyr::bind_rows(
        dplyr::select(edges, node = from, weight),
        dplyr::select(edges, node = to,   weight)
      ) |>
        dplyr::group_by(node) |>
        dplyr::summarise(strength = sum(weight), .groups = "drop")
      
      g <- igraph::graph_from_data_frame(
        edges,
        directed = FALSE,
        vertices = node_strength
      )
      
      # Fruchterman-Reingold layout places strongly connected nodes closer
      # together, giving a natural clustering of related fields
      ggraph::ggraph(g, layout = "fr") +
        ggraph::geom_edge_link(
          aes(width = weight, alpha = weight),
          colour = "#2c7bb6"
        ) +
        ggraph::geom_node_point(
          aes(size = strength),
          colour = "#d7191c"
        ) +
        ggraph::geom_node_text(
          aes(label = name),
          repel    = TRUE,
          size     = 4.5,
          fontface = "bold",
          colour   = "black"
        ) +
        ggraph::scale_edge_width(range = c(0.5, 4),   guide = "none") +
        ggraph::scale_edge_alpha(range = c(0.3, 0.9), guide = "none") +
        scale_size(range = c(3, 12), guide = "none") +
        labs(
          title    = paste0(info$name,
                            " \u2014 Category Co-occurrence Network"),
          subtitle = subtitle_str
        ) +
        theme_void(base_size = 18) +
        theme(
          plot.title    = element_text(hjust   = 0.5, face = "bold",
                                       size   = 20,  colour = "black"),
          plot.subtitle = element_text(hjust   = 0.5, size  = 13,
                                       colour = "black")
        )
    }
  }
  
  output$ProfilePlot <- renderPlot(height = 600, { profilePlotInput() })
  
  # Download handler: Research Profile plot
  output$downloadProfilePlot <- downloadHandler(
    filename = function() {
      view_label <- switch(input$profileView,
                           "areas"      = "ResearchAreas",
                           "categories" = "SubjectCategories",
                           "network"    = "CategoryNetwork",
                           "ResearchProfile"
      )
      make_export_filename(details(), view_label, "jpeg")
    },
    content = function(file) {
      ggsave(file, plot = profilePlotInput(), device = "jpeg",
             width = 11.69, height = 8.27)
    }
  )
  
  # Word cloud renderer ####
  # -------------------------------------------------------------------------#
  # Renders the interactive wordcloud2 HTML widget. Word size is
  # proportional to frequency. Colours cycle through the Dark2 palette.
  # The widget supports hover tooltips natively — hovering over a word
  # displays its frequency count (noted in the UI footnote).
  output$WordCloud <- wordcloud2::renderWordcloud2({
    req(word_frequencies(), input$wcMaxWords)
    
    wf <- word_frequencies() |>
      dplyr::slice_head(n = input$wcMaxWords) |>
      dplyr::rename(word = word, freq = n)
    
    # Return NULL silently if no words remain after filtering
    if (nrow(wf) == 0) return(NULL)
    
    wordcloud2::wordcloud2(
      data            = wf,
      size            = 0.6,
      color           = rep_len(
        RColorBrewer::brewer.pal(8, "Dark2"),
        nrow(wf)
      ),
      backgroundColor = "white",
      fontFamily      = "sans-serif",
      fontWeight      = "bold"
    )
  })
  
} # end server

# ══════════════════════════════════════════════════════════════════════════════#
# Launch application ####
# ══════════════════════════════════════════════════════════════════════════════#
shinyApp(ui = ui, server = server)

