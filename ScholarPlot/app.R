# ScholarPlot
# A tool to visualise a GoogleScholar profile
# https://github.com/alegione/ScholarPlot
#



#list of packages required
list.of.packages <- c("tidyverse", "scholar")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

### Load packages
library(scholar)
library(tidyverse)




# Define UI for application
ui <- fluidPage(
  
   # Application title
   titlePanel("ScholarPlot: visualise your research output"),
   
   # Sidebar with a slider input for number of bins 
   tabsetPanel(
     tabPanel(title = "Citations", 
       sidebarLayout(
          sidebarPanel(
            textInput(inputId = "scholarId", label = "Enter GoogleScholar ID", placeholder = "OmIonF8AAAAJ", value = "OmIonF8AAAAJ"),
            actionButton("go", "Go"),
            uiOutput("slider"),
            
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
               #tableOutput(outputId = "MetricsTable")
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

  id <- eventReactive(input$go,{
    (input$scholarId)
  })
  
  citations <- reactive({
    print("get citations")
    get_citation_history(id())

  })

  papers <- reactive({
    print("get publications")
    
    get_publications(id())
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
    sliderInput(inputId = "yearRange", label = "Years", min = min(range), max = max(range), value = c(min(range),max(range)),step = 1)
  })
  
  plotInput <- function() {
    if (is.null(dat())) {
      return()
    }
    
    plotdata <- dat()
    plotdata <- filter(plotdata, year >= input$yearRange[1], year <= input$yearRange[2])
    
    
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
    
  
 
}

# Run the application 
shinyApp(ui = ui, server = server)

