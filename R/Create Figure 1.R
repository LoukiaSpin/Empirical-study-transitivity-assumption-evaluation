#*******************************************************************************
#*
#*
#*                      Create Figure 1
#*                                                                 
#* Authors: Loukia M. Spineli & Katerina Papadimitropoulou
#* Date: October 2022
#*******************************************************************************



## Load libraries ----
list.of.packages <- c("ggplot2", "plyr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load data ----
load("./data/dataset.RData")



## General information ----
year <- dataset[, "Year"]; table(year)



## Bubble plot: Year by healthcase field ----
counts <- ddply(dataset, .(dataset[, "Year"], dataset[, "Health-related field"]), nrow)
names(counts) <- c("Year", "Health", "Freq"); counts
#sort_field <- names(sort(table(field), decreasing = FALSE))

## Add radius as new variable to data frame 'counts'
counts$radius <- sqrt(counts$Freq/pi)

## Create the bubble plot ^_^
ggplot(counts, 
       aes(x = as.factor(Year), 
           y = factor(Health, levels = rev(sort(unique(Health)))), 
           fill = Freq)) +
  geom_point(aes(size = radius * 7.8), 
             shape = 21, 
             stroke = 1.2) +
  geom_text(aes(label = Freq),
            size = 4, 
            color="white") +
  labs(x = "Year of publication") + 
  labs(y = "Healthcare field") +
  scale_size_identity() +
  scale_fill_gradient(low = "#FC4E07", high = "#00AFBB") +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = 2, color = "grey"),
        axis.text.x      = element_text(size = 15),
        axis.text.y      = element_text(size = 15),
        axis.title       = element_text(size = 15, face = "bold"),
        legend.position = "none")
