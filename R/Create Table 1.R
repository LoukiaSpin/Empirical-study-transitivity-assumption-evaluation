#*******************************************************************************
#*
#*
#*                                Create Table 1                                                                                        
#*   (Pre-planned evaluation of the transitivity assumption in the protocol)   
#*                                                                 
#* Author: Loukia M. Spineli
#* Date: October 2022
#*******************************************************************************

## Load library
library(gmodels)

## Load data ----
load("./data/dataset.RData")

## General information ----
year <- dataset[, "Year"]; table(year)

## Q: [1] Whether the review provides a Protocol ----
protocol <- factor(dataset[, 6], 
                   levels = c("Registered", 
                              "Not registered but published", 
                              "Mentioned but not available", 
                              "Explicitly mentioned that there is no protocol", 
                              "Protocol not mentioned"))

#' NOTE: First row % is row-percentage (for the 'Total %' in the Table), 
#' and second row % is column-percentage (For the 'year %' in the Table)
CrossTable(protocol, year, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")

#* Q: [2] If [1] is 'Registered' or 'Not registered but published', whether the ----
#* authors defined the transitivity assumption in the Protocol 
trans_defined <- factor(subset(dataset[, 7], 
                               is.element(protocol, c("Registered", "Not registered but published"))),
                        levels = c("Not applicable", "Yes", "No"))
year2 <- subset(year, is.element(protocol, c("Registered", "Not registered but published")))

#' NOTE: First row % is row-percentage (for the 'Total %' in the Table), 
#' and second row % is column-percentage (For the 'year %' in the Table)
CrossTable(trans_defined, year2, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")

#* Q: [3] If [1] is 'Registered' or 'Not registered but published', whether the ----
#* authors mentioned in the Protocol how they plan to evaluate the transitivity 
#* assumption in the review [multiple choice possible]
q3_data <- dataset[, c(10, 11, 13, 15, 17, 19:22)]

# Select based on the condition
q3_sub <- subset(q3_data, is.element(protocol, c("Registered", "Not registered but published")))
for (i in 1:dim(q3_sub)[2]) {
  q3_sub[, i] <- factor(q3_sub[, i], levels = c("Not applicable", "Yes", "No"))
}
names(q3_sub) <- c("No plan", "Salanti 1", "Salanti 2&3", "Salanti 4", "Salanti 5",
                   "Sensitivity", "Subgroup", "Meta-regression", "Consistency")

# Obtain 'Total' and '% Total' per multiple-choice
total0 <- apply(q3_sub, 2, function(x) sum(x == "Yes"))
total <- data.frame(total0, round((total0 / sum(total0)) * 100, 1))
colnames(total) <- c("counts", "percent"); total

# Obtain count and % 'Per year' for each multiple-choice
q3_year <- list()
for (i in 1:dim(q3_sub)[2]) {
  q3_year[[i]] <- table(q3_sub[, i], year2)  # Per year, n
}
year_n0 <- do.call(rbind, q3_year)
year_n <- subset(year_n0, rownames(year_n0) == "Yes")
rownames(year_n) <- names(q3_sub); year_n                                # counts per year
round(t(apply(year_n, 1, function(x) x/apply(year_n, 2, sum))) * 100, 1) # % per year

