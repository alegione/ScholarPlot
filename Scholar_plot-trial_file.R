
library(scholar)
library(tidyverse)
library(lubridate)

#Alistair: OmIonF8AAAAJ
#Mauricio: pPYRgUAAAAAJ
#Nadeeka: iiDz-skAAAAJ
#Jo Devlin: b_0bnnoAAAAJ
id <- "OmIonF8AAAAJ"
nToday <- yday(Sys.time())
ct <- get_citation_history(id)
ct$citesPerMonth <- ct$cites/12
finalRow <- nrow(ct)
ct$citesPerMonth[finalRow] <- ct$cites[finalRow]/7
ct$citesPerYear <- ct$cites
ct$citesPerYear[finalRow] <- ct$citesPerYear[finalRow]/nToday * 365

ct
# ct <- rbind(c(2006,0,0,0), ct)

p <- get_publications(id)

p <- p[complete.cases(p$year),]
#ct <- ct[1:13,1:4]
p

papersPerYear <- as.data.frame(x = table(p$year),
                                         stringsAsFactors = FALSE
                               )
papersPerYear <- rename(papersPerYear, "year" = "Var1")
papersPerYear$year <- as.numeric(as.character(papersPerYear$year))
ct <- full_join(x = ct, y = papersPerYear, by = c("year"))
ct <- arrange(ct, year)

ct[is.na(ct)] <- 0
  
ct$sum <- cumsum(ct$Freq)

papersPerYear
#rbind(papersPerYear, c(as.factor(2019),0,81))
# ct$papersPerYear <- papersPerYear$Freq
#ct$papersPerYear <- papersPerYear$Freq[2:9]
ct


ctMinus <- ct[1:nrow(ct)-1,] #saves citations less the current year to remove small values

transform <- round(x =max(ct$cites)/max(ct$Freq) * (2/3), digits = 0)

(citationsPlot <- ggplot(data = ct) +
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
        axis.text.x = element_text(face = "bold", size = 12, colour = "Black")) +
  theme(axis.title.y = element_text(face = "bold",
                                    size = 15, colour = "Black",
                                    margin = margin(t = 0, r = 10, b = 0, l = 10)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
        axis.text.y = element_text(face = "bold", size = 12, colour = "Black")) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~. / transform, name = "Annual Papers")) + 
    #scale_colour_manual("", breaks = c("Citations_per_year", "Projected_citations"), values = c("Citations_per_year" = "Cyan", "Projected_citations" = "Purple")) +
    NULL
)
(citationsPlot <- ggplot(data = ct) +
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
    scale_x_continuous(breaks = c(seq(from = min(ct$year), to = max(ct$year, by = 1)))) + 
    scale_y_continuous(sec.axis = sec_axis(trans = ~. / transform, name = "Annual Papers")) + 
    #scale_colour_manual("", breaks = c("Citations_per_year", "Projected_citations"), values = c("Citations_per_year" = "Cyan", "Projected_citations" = "Purple")) +
    NULL
)
ggplot(ct, aes(year, citesPerMonth)) + geom_line() + geom_point()

p <- get_publications(id)
papersPerYear <- as.data.frame(table((p$year)))
papersPerYear$sum <- cumsum(papersPerYear[,2])

papersPerYear

ct$papersPerYear <- papersPerYear$Freq
#ct$papersPerYear <- papersPerYear$Freq[2:9]


ctMinus <- ct[1:nrow(ct)-1,] #saves citations less the current year to remove small values

(citationsPlotMinus <- ggplot(ctMinus) +
    geom_col(aes(x = year, y = papersPerYear*20), fill = "White", colour = "black", size = 1) + 
    
  #  geom_line(aes(x = year, y = citesPerYear), size = 2, colour = "Purple", linetype = 2) +
    geom_line(aes(x = year, y = cites), size = 2, colour = "Grey") + 
    geom_point(aes(x = year, y = cites), size = 4, colour = "Black") +
  #  geom_point(aes(x = year, y = citesPerYear), size = 4, colour = "Black") +
    ylab("Annual Citations") + 
    #ggtitle("Citation metrics") +
    xlab("Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
    theme(legend.title = element_text(face = "bold", size = 15)) +
    theme(legend.text = element_text(size = 14)) +
    theme(axis.title.x = element_text(face = "bold",
                                      size = 15, colour = "Black"),
          axis.text.x = element_text(face = "bold",
                                     size = 12,
                                     colour = "Black")) +
    theme(axis.title.y = element_text(face = "bold",
                                      size = 15,
                                      colour = "Black", 
                                      margin = margin(t = 0, r = 10, b = 0, l = 10)),
          axis.text.y = element_text(face = "bold",
                                     size = 12,
                                     colour = "Black"),
          axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10))) +
    
    #scale_colour_manual("", breaks = c("Citations_per_year", "Projected_citations"), values = c("Citations_per_year" = "Cyan", "Projected_citations" = "Purple")) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./20, name = "Annual Papers")) + 
    NULL
)


ggplot() +
  geom_col(data = papersPerYear, aes(x = Var1, y = Freq), fill = "Navy") +
  NULL

#loop below, to produce a graph for every file, not working
for (i in 1:nrow(p)) {
  ach <- get_article_cite_history(id, p$pubid[i])
  
  ggplot(ach, aes(year, cites)) +
    geom_segment(aes(xend = year, yend = 0), size=1, color='darkgrey') +
    geom_point(size=3, color='firebrick') +
    ggtitle(p$title[i])
}
