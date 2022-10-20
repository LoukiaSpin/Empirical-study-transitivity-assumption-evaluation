#*******************************************************************************
#*
#*
#*                               Create Figure 2                                                          
#*             (Box plots on characteristics type & PICO elements)                               
#*                                                                 
#* Authors: Loukia M. Spineli & Katerina Papadimitropoulou
#* Date: October 2022
#*******************************************************************************

## Load libraries ----
list.of.packages <- c("ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)

## Load data ----
load("./data/dataset.RData")

## General information ----
year <- dataset[, "Year"]; table(year)

## Q: [16] A Table of Characteristics (ToC) is provided in the publication ----
table_reported <- factor(dataset[, 70], levels = c("Yes", "No"))

## Q: If [16] is 'Yes', the total number of characteristics per type 
q19_data <- dataset[, 73:75]

# Select based on the condition
q19_sub <- subset(q19_data, table_reported == "Yes")
year19 <- subset(year, table_reported == "Yes")

## If [16] is 'Yes', the characteristics refer to [multiple choice possible]
q20_data <- dataset[, c(76, 78, 80, 82)]

# Select based on the condition
q20_sub <- subset(q20_data, table_reported == "Yes")

## Box plots ----
# Dataset for the type of characteristics
level19 <- c("Quantitative", "Qualitative", "Combination")
q19_bind <- as.numeric(unlist(q19_sub))
q19_names <- rep(level19, each = length(q19_sub[, 1]))
q19_names_new <- factor(q19_names, levels = level19)
year19_new <- as.factor(rep(year19, length(level19)))
data19 <- data.frame(q19_bind, q19_names_new, year19_new)
colnames(data19) <- c("value", "levels", "year")

# Box plot for the type of characteristics 
g19 <- ggplot(data19, 
              aes(x = year, 
                  y = value, 
                  color = levels,
                  fill = levels)) + 
  geom_boxplot(aes(fill = after_scale(colorspace::lighten(fill, .4))),
               size = 1, 
               outlier.shape = NA) +
  geom_jitter(width = .1, 
              size = 1.3, 
              alpha = .5) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(y = "Number of characteristics", 
       x = "") +
  coord_cartesian(ylim = c(0, 34)) +
  theme_classic() + 
  theme(axis.text   = element_text(size = 15),
        axis.title  = element_text(size = 15, face = "bold"),
        legend.position = "bottom",
        legend.text  = element_text(size = 14), 
        legend.title = element_blank())


# Dataset for the PICO elements
level20 <- c("Participants", "Interventions", "Outcomes", "Designs")
q20_bind <- as.numeric(unlist(q20_sub))
q20_names <- rep(level20, each = length(q20_sub[, 1]))
q20_names_new <- factor(q20_names, levels = level20)
year20_new <- as.factor(rep(year19, length(level20)))
data20 <- data.frame(q20_bind, q20_names_new, year20_new)
colnames(data20) <- c("value", "levels", "year")

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

g20 <- ggplot(data20, 
              aes(x = year, 
                  y = value, 
                  color = levels,
                  fill = levels)) +
  scale_fill_manual(values = cbp1) +
  scale_color_manual(values = cbp1) + 
  geom_boxplot(aes(fill = after_scale(colorspace::lighten(fill, .4))),
               size = 1, 
               outlier.shape = NA) +
  geom_jitter(width = .1, 
              size = 1.3, 
              alpha = .5) +
  labs(y = "Number of characteristics", 
       x = "") +
  coord_cartesian(ylim = c(0, 34)) +
  theme_classic() + 
  theme(axis.text   = element_text(size = 15),
        axis.title  = element_text(size = 15, face = "bold"),
        legend.position = "bottom",
        legend.text  = element_text(size = 14), 
        legend.title = element_blank())

# Bring both box plots together (hooray!!!)
ggarrange(g19, g20, labels = c("A)", "B)"))

