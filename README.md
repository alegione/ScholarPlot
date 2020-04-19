# ScholarPlot 

## What is it?
ScholarPlot is an R Shiny tool for visualising and exporting your google scholar data.

The input is your Google Scholar ID. You can find this in your Google Scholar web address.
If you go to your scholar profile, your individual ID can be found as part of the web address.

![Example URL](/images/ExampleURL.jpg)

In the above example, my user ID is highlighted (i.e. *OmIonF8AAAAJ*).

The current primary output is a plot of papers per year and citations per year, a secondary output is a table of the publications of the author to date, ordered by 'Paper Score': A custom metric that aims to take into account impact factor of the journal and the number of citations per year of the manuscript itself. This can be helpful for applications that ask for you to provide your 'top 10 papers'.

Below is an example of the ScholarPlot output of bioinformatics wizard Heng Li (Assistant professor of Biostatistics and Computational Biology at Dana-Farber and Harvard Medical School).

![Heng Li ScholarPlot Example](/images/ScholarPlotHengLi.jpg)

The tool leans on the fantastic 'Scholar' package by Yu et al (https://cran.r-project.org/web/packages/scholar/)

Unfortunately google appears to block scaping data when this tool is loaded onto the web (e.g. at shinyapps.io).

Therefore, you'll need to copy the script locally and launch it in Rstudio to use it.

Just download, open in Rstudio, and click the 'Run App' button

