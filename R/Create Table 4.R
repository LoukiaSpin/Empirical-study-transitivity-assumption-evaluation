#*******************************************************************************
#*
#*
#*                                Create Table 4                                                                                        
#*                   (Reporting the table of characteristics)                                       
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

# Q: [16] A Table of Characteristics (ToC) is provided in the publication ----
table_reported <- factor(dataset[, 70], levels = c("Yes", "No"))

#' NOTE: See column 'Row Total for absolute (n) and relative Total (%) in the Table, 
#' and second row % is column-percentage for the 'year %' in the Table
CrossTable(table_reported, year, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")

# Q: [17] If [16] is 'Yes', this Table can be found in ----
q17 <- factor(dataset[, 71], levels = c("Not applicable", "Main article", "Supplementary", "Both"))

table_found_in <- subset(q17, table_reported == "Yes")
year17 <- subset(year, table_reported == "Yes")

#' NOTE: See column 'Row Total for absolute (n) and relative Total (%) in the Table, 
#' and second row % is column-percentage for the 'year %' in the Table
CrossTable(table_found_in, year17, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")

# Q: [21] If [16] is 'Yes', the Table presents the information at [ONE choice possible] ----
q21_data0 <- dataset[, c(80:86)]
q21_data0[, 1:4] <- ifelse(q21_data0[, 1:4] == "Yes", 1, 0)

#' Merge 'Trial-level with arms' with 'Trial-level w/o arms' into 'Trial-level', and 
#' 'Comparison-level with arms' with 'Comparison-level-level w/o arms' into 'Comparison-level'
q21_data <- data.frame(q21_data0[, 1] + q21_data0[, 2], q21_data0[, 3] + q21_data0[, 4], q21_data0[, 5:7])
q21_data[, 1:2] <- ifelse(q21_data[, 1:2] == 1, "Yes", "No")

# Select based on the condition
q21_sub <- subset(q21_data, table_reported == "Yes")
year21 <- subset(year, table_reported == "Yes")
for (i in 1:5) {
  q21_sub[, i] <- factor(q21_sub[, i], levels = c("Yes", "No"))
}
names(q21_sub) <- c("Characteristics at trial-level","At comparison-level with trials",
                    "Comparison-level with characteristics", 
                    "Intervention-level", "Descriptives per chracteristic")

# Obtain 'Total' and '% Total' per multiple-choice
total0 <- apply(q21_sub, 2, function(x) sum(x == "Yes"))
total21 <- data.frame(total0, round((total0 / sum(total0)) * 100, 1))
colnames(total21) <- c("counts", "percent"); total21

# Obtain count and % 'Per year' for each multiple-choice
q21_year <- list()
for (i in 1:dim(q21_sub)[2]) {
  q21_year[[i]] <- table(q21_sub[, i], year21)  # Per year, n
}
year_n0 <- do.call(rbind, q21_year)
year_n21 <- subset(year_n0, rownames(year_n0) == "Yes")
rownames(year_n21) <- names(q21_sub); year_n21                                # counts per year
round(t(apply(year_n21, 1, function(x) x/apply(year_n21, 2, sum))) * 100, 1)  # % per year

#* Q: [22] If [16] is 'Yes' and [21] is not 'Table with the descriptive statistics ----
#* for each characteristic', there is at least one trial (or comparison) with 
#* at least one missing characteristic
q22 <- factor(dataset[, 87], levels = c("Not applicable", "Yes", "No"))

missing_chars <- subset(q22, q22 != "Not applicable")
year22 <- subset(year, q22 != "Not applicable")

#' NOTE: See column 'Row Total for absolute (n) and relative Total (%) in the Table, 
#' and second row % is column-percentage for the 'year %' in the Table
CrossTable(missing_chars, year22, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")
