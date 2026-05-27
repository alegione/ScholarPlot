# ScholarPlot

## What is it?

ScholarPlot is an R Shiny tool for visualising and exporting Google Scholar
profile data, enriched with journal quality metrics from the SCImago Journal
Rankings (SJR) database.

Developed with assistance from Anthropic's Claude (Sonnet) via the Spark
interface, building on the original ScholarPlot tool by Alistair Legione.

---

## Inputs

- Your **Google Scholar ID**, found in your profile URL:
  `scholar.google.com/citations?user=XXXXXX`
- Optionally, an updated **SJR database** (`.rda` format — see below)

---

## Features

- **Publications Table** — the primary output; a sortable, filterable DT
  table with per-column search. Individual rows can be hidden from all
  plots and exports without being permanently deleted, and restored at
  any time. Includes:
  - Google Scholar metadata (title, authors, journal, year, citations)
  - SJR journal metrics (SJR score, best quartile, H-index, 2-year
    impact factor, research areas)
  - Fuzzy journal matching with match type indicator
    (`exact` / `fuzzy` / `preprint` / `none`) and confidence score (0-100)
  - Custom **Paper Score** (citations per year + SJR score)
  - **Citation-to-Impact Ratio** (citations per year divided by journal
    impact factor; values above 1.0 indicate above-average performance
    relative to the journal)
  - **Normalised Citation Score** (citations per year relative to the
    mean for papers in the same quartile tier)

- **Citations & Papers plot** — dual-axis plot with independently
  toggleable series; each series can be set to yearly or cumulative
  independently

- **Journal Quartiles plot** — publications per year stacked by SJR
  best quartile (Q1-Q4), yearly or cumulative, with adjustable year
  range; preprints and unmatched journals are excluded

- **Research Profile tab** — field-of-expertise visualisations:
  - Broad research areas bar chart
  - Journal subject categories bar chart
  - Category co-occurrence network plot
  - Word cloud from paper titles and/or abstracts (hover for counts)
  - All views filterable by year range

- **Glossary tab** — definitions of all metrics and columns, including
  how composite scores are calculated and how to interpret them

- **Export** — save any plot as JPEG or the full table as TSV; files are
  named `YYYY-MM-DD-Firstname-Surname-OutputType.ext` automatically

---

## SJR Database

The app loads a pre-built SJR dataset automatically from this repository,
trying the current year first, then the prior year, then a local copy.
To use a custom or updated version:

1. Download the latest data from
   [SCImago Journal Rankings](https://www.scimagojr.com/journalrank.php)
   (export as CSV, semicolon-separated)
2. Run `prepare_sjr.R` to parse and save a new `.rda` file
3. Upload it via the **SJR Database** file input in the app sidebar

The output file is named `sjr_tibble_YYYY.rda` where `YYYY` is the
current year at the time `prepare_sjr.R` is run.

---

## Running Locally

Install [R](https://cran.rstudio.com/) and
[RStudio](https://rstudio.com/products/rstudio/download/), then run:

    install.packages("shiny")
    library(shiny)
    runGitHub("ScholarPlot", "alegione")

---

## Dependencies

| Package | Purpose |
|---|---|
| `scholar` | Google Scholar data retrieval |
| `stringdist` | Fuzzy journal name matching |
| `DT` | Interactive sortable tables |
| `ggplot2` | Plots |
| `ggraph` / `igraph` | Category co-occurrence network plot |
| `wordcloud2` | Interactive word cloud |
| `tidytext` / `stopwords` | Text tokenisation and stop word removal |
| `scales` | Axis formatting |
| `dplyr` / `tibble` | Data wrangling |
| `bslib` | App theming |

---

## Thanks and References

Builds on the [scholar](https://cran.r-project.org/web/packages/scholar/)
package by Yu et al., and journal metrics from
[SCImago Journal Rankings](https://www.scimagojr.com).

Developed with assistance from Anthropic's Claude (Sonnet) via the Spark
interface.
