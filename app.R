# ScholarPlot
# A tool to visualise a GoogleScholar profile
# https://github.com/alegione/ScholarPlot
# Working Version



#list of packages required
list.of.packages <- c("tidyverse", "scholar", "NLP", "RColorBrewer", "tm", "wordcloud", "qdap", "readtext", "xml2", "rvest")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

### Load packages
for (i in list.of.packages) {
  print(i)
  library(i, character.only = TRUE)
}


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("ScholarPlot: visualise your research output"),
  
  # Sidebar with a slider input for number of bins 
  tabsetPanel(
    tabPanel(title = "Citations", 
             sidebarLayout(
               sidebarPanel(
                 textInput(inputId = "scholarId",
                           label = "Enter GoogleScholar ID",
                           placeholder = "rW9T5f4AAAAJ",
                           value = "rW9T5f4AAAAJ"),
                 actionButton(inputId = "go", label = "Go"),
                 uiOutput(outputId = "slider"),
                 
                 downloadButton(outputId = "downloadPlot", "Save image"),
                 downloadButton(outputId = "downloadtable", "Save table"),
                 width = 2,
                 NULL
               ),
               # Show a plot of the generated distribution
               mainPanel(
                 verticalLayout(
                   plotOutput(outputId = "CitationPlot")
                 ),
                 verticalLayout(
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   tableOutput(outputId = "MetricsTable"),
                   #textOutput(outputId = "TextTest"),
                   NULL
                 )
               )
             )
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  # dat <- eventReactive(input$go,{
  #                      #(input$scholarId)
  #   id <- input$scholarId
  #   ct <- get_citation_history(id)
  #   p <- get_publications(id)
  #   p <- p[complete.cases(p$year),]
  #   papersPerYear <- as.data.frame(x = table(p$year),
  #                                  stringsAsFactors = FALSE
  #   )
  #   papersPerYear <- rename(papersPerYear, "year" = "Var1")
  #   papersPerYear$year <- as.numeric(as.character(papersPerYear$year))
  #   ct <- full_join(x = ct, y = papersPerYear, by = c("year"))
  #   ct <- arrange(ct, year)
  # 
  #   ct[is.na(ct)] <- 0
  # 
  #   ct$sum <- cumsum(ct$Freq)
  #   ct
  #   print(ct)
  #   }
  # )
  
  getid <- eventReactive(input$go,{
    (input$scholarId)
  })
  
  citations <- reactive({
    print("get citations")
    get_citation_history(getid())
    
  })
  
  papers <- reactive({
    print("get publications")
    
    get_publications(getid())
  })
  
  
  details <- reactive({
    tmp <- get_profile(getid())
    (c(tmp$name, tmp$affiliation, tmp$total_cites, tmp$h_index, tmp$i10_index))
  })
  
  
  
  
  dat <- reactive({
    print("get data")
    
    ct <- citations()
    print(ct)
    
    p <- papers()
    
    p <- p[complete.cases(p$year),]
    
    papersPerYear <- as.data.frame(x = table(p$year),
                                   stringsAsFactors = FALSE
    )
    papersPerYear <- dplyr::rename(papersPerYear, "year" = "Var1")
    papersPerYear$year <- as.numeric(as.character(papersPerYear$year))
    ct <- full_join(x = ct, y = papersPerYear, by = c("year"))
    ct <- arrange(ct, year)
    
    ct[is.na(ct)] <- 0
    
    ct$sum <- cumsum(ct$Freq)
    
    ct
  })
  
  #  IMF <- get_impactfactor(journals = p$journal, max.distance = 0.2)
  #  profile <- get_profile(id)
  output$slider <- renderUI({
    range <- dat()$year
    sliderInput(inputId = "yearRange",
                label = "Years",
                min = min(range),
                max = max(range),
                value = c(min(range), max(range)),
                step = 1)
  })
  
  plotInput <- function() {
    if (is.null(dat())) {
      return()
    }
    
    plotdata <- dat()
    plotdata <- filter(plotdata, year >= input$yearRange[1], year <= input$yearRange[2])
    titleInfo <- details()
    
    transform <- round(x = max(plotdata$cites)/max(plotdata$Freq) * (2/3), digits = 0)
    
    
    ggplot(data = plotdata) +
      geom_col(aes(x = year, y = Freq * transform), fill = "White", colour = "black", size = 1) + 
      # geom_line(aes(x = year, y = citesPerYear), size = 2, colour = "Purple", linetype = 2) +
      geom_line(aes(x = year, y = cites), size = 2, colour = "Grey") + 
      geom_point(aes(x = year, y = cites), size = 4, colour = "Black") +
      #  geom_point(aes(x = year, y = citesPerYear), size = 4, colour = "Black") +
      ylab("Annual Citations") + 
      #  ggtitle("Dr Alistair Legione") +
      xlab("Year") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
      theme(legend.title = element_text(face = "bold", size = 15)) +
      theme(legend.text = element_text(size = 14)) +
      theme(axis.title.x = element_text(face = "bold", size = 15, colour = "Black"),
            axis.text.x = element_text(face = "bold", size = 12, colour = "Black", angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(face = "bold",
                                        size = 15, colour = "Black",
                                        margin = margin(t = 0, r = 10, b = 0, l = 10)),
            axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
            axis.text.y = element_text(face = "bold", size = 12, colour = "Black")) +
      scale_x_continuous(breaks = c(seq(from = min(plotdata$year), to = max(plotdata$year, by = 1)))) + 
      scale_y_continuous(sec.axis = sec_axis(trans = ~. / transform, name = "Annual Papers")) + 
      ggtitle(label = paste0(titleInfo[1], " - Citations: ", titleInfo[3], ", h-index: ", titleInfo[4], ", i10-index: ", titleInfo[5]), subtitle = paste("Data accessed:", format(Sys.Date(), "%b %d %Y"))) +
      #scale_colour_manual("", breaks = c("Citations_per_year", "Projected_citations"), values = c("Citations_per_year" = "Cyan", "Projected_citations" = "Purple")) +
      NULL
  }
  
  # output$downloadplot <- downloadHandler(
  #   filename <- function() {
  #     paste0('CitationPlot', 'png', sep = ".")
  #   },
  #   content <- function(file) {
  #     png(filename = file, width = 1000, height = 800)
  #     
  #     plotInput()
  #     
  #     print(ImageSave)
  #     
  #     dev.off()
  #   },
  #   contentType = "image/png"
  # )
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("CitationPlot", '.jpeg', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "jpeg", width = 11.69, height = 8.27)
    }
  )
  
  output$CitationPlot <- renderPlot(height = 600, {
    plotInput()
  })
  
  Generate_MetricsTable <- reactive({
    if (is.null(dat())) {
      return()
    }
    papersTable <- papers()
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    papersTable$ImpactFactor <- get_impactfactor(papersTable$journal, max.distance = 0.20)$ImpactFactor

    papersTable$citesPerYear <- ifelse(papersTable$year != current_year, papersTable$cites/(current_year - papersTable$year), papersTable$cites)
    
    papersTable$PaperScore <- papersTable$citesPerYear + papersTable$ImpactFactor
    
    papersTable$cites <- sprintf('%1i', papersTable$cites)
    
    papersTable$year <- sprintf('%1i', papersTable$year)
    
    CiteTable <- filter(papersTable, PaperScore > 0) %>%
      arrange(desc(PaperScore)) %>%
      select(-cid, -pubid)
    
    (CiteTable <- add_column(.data = CiteTable, Rank = 1:nrow(CiteTable), .before = "title") %>%
        plyr::rename(c("Rank" = "Rank", "title" = "Title", "author" = "Authors", "journal" = "Journal", "number" = "Issue", "cites" = "Citations", "year" = "Year", "ImpactFactor" = "Impact Factor", "citesPerYear" = "Annual Citations", "PaperScore" = "Paper Score"))
    )
    
    
  })
  
  
  output$MetricsTable <- renderTable({
    Generate_MetricsTable()
  })
  
  output$downloadtable <- downloadHandler(
    filename = function() { paste0('MetricsTable', '.tsv', sep = "") },
    content = function(file) {
      write.table(x = Generate_MetricsTable(), file = file, row.names = FALSE, sep = "\t")
    },
    contentType = "text/csv"
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

