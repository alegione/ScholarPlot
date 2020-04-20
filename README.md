# ScholarPlot 

## What is it?
ScholarPlot is an R Shiny tool for visualising and exporting your google scholar data.

The input is your Google Scholar ID. You can find this in your Google Scholar web address.
If you go to your scholar profile, your individual ID can be found as part of the web address.

![Example URL](https://github.com/alegione/ScholarPlot/blob/master/ScholarPlot/images/ExampleURL.jpg)

In the above example, my user ID is highlighted (i.e. *OmIonF8AAAAJ*).

The current primary output is a plot of papers per year and citations per year, a secondary output is a table of the publications of the author to date, ordered by 'Paper Score': A custom metric that aims to take into account impact factor of the journal and the number of citations per year of the manuscript itself. This can be helpful for applications that ask for you to provide your 'top 10 papers'.

Below is an example of the ScholarPlot output of bioinformatics wizard Heng Li (Assistant professor of Biostatistics and Computational Biology at Dana-Farber and Harvard Medical School).

![Heng Li ScholarPlot Example](https://github.com/alegione/ScholarPlot/blob/master/ScholarPlot/images/ScholarPlotHengLi.jpg)

Plots and tables can be exported separately. Years of publication can be adjusted for the plot, these don't as of yet affect the table output.

## Run your own local version

You can run 'ScholarPlot' on your own computer.

To run, start by installing the programming language [R](https://cran.rstudio.com/) and the GUI interface [RStudio](https://rstudio.com/products/rstudio/download/)

Then run the below commands in the Rstudio console
```R
install.packages('shiny')
library(shiny)
runGitHub("ScholarPlot", "alegione")
```

This will download and run the Shiny application locally

## To Do List
- Word cloud pulling frequent terms from paper abstracts
- Convert output table to sortable DT type table for easy ordering by year, citations, first author, etc.
- Option for Papers/Year or Citations/Year or Both, rather than merging by default


## Thanks/References
The tool leans on the fantastic 'Scholar' package by Yu et al (https://cran.r-project.org/web/packages/scholar/)

