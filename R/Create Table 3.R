#*******************************************************************************
#*
#*
#*                                Create Table 3                                                                                        
#*       (Acknowledging the implications of the transitivity evaluation)                
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

#* Q: [11] Among the reviews with '[7] is Yes' or '[8] is Yes', what did ----
#* the authors onclude or imply regarding the plausibility of transitivity 
q11 <- factor(dataset[, 45],
              levels = c("Not applicable",
                         "Transitivity may be plausible",
                         "Transitivity may be questionable",
                         "Difficult to judge due to limited data",
                         "Nothing stated"))

plausibility <- subset(q11, dataset[, 29] == "No")
year11 <- subset(year, dataset[, 29] == "No")

#' NOTE: See column 'Row Total for absolute (n) and relative Total (%) in the Table, 
#' and second row % is column-percentage for the 'year %' in the Table
CrossTable(plausibility, year11, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")

#* Q: [12] If [11] is 'Transitivity may be questionable' or 'Difficult to judge ----
#* due to limited data', did the authors decide to abstain from NMA 
q12 <- factor(dataset[, 68], 
              levels = c("Not applicable", "Yes", "No, they performed NMA"))

abstain_from_nma <- subset(q12, is.element(q11, c("Transitivity may be questionable", 
                                                  "Difficult to judge due to limited data")))
year12 <- subset(year, is.element(q11, c("Transitivity may be questionable", 
                                         "Difficult to judge due to limited data")))

#' NOTE: See column 'Row Total for absolute (n) and relative Total (%) in the Table, 
#' and second row % is column-percentage for the 'year %' in the Table
CrossTable(abstain_from_nma, year12, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")

#* Q: [13] If [11] is not 'Nothing stated' or 'Not applicable', What method(s) ----
#* did the authors consider to conclude or imply about the plausibility of 
#* transitivity [multiple choice possible]
q13_data <- dataset[, 48:56]

# Select based on the condition
q13_sub <- subset(q13_data, !is.element(q11, c("Nothing stated", "Not applicable")))
year13 <- subset(year, !is.element(q11, c("Nothing stated", "Not applicable")))
for (i in 1:dim(q13_sub)[2]) {
  q13_sub[, i] <- factor(q13_sub[, i], levels = c("Not applicable", "Yes", "No"))
}
names(q13_sub) <- c("Limited data", "Salanti 1", "Salanti 2&3", "Salanti 4", "Salanti 5",
                    "Sensitivity", "Subgroup", "Meta-regression", "Consistency")

# Obtain 'Total' and '% Total' per multiple-choice
total_1 <- apply(q13_sub, 2, function(x) sum(x == "Yes"))
total13 <- data.frame(total_1, round((total_1 / sum(total_1)) * 100, 1))
colnames(total13) <- c("counts", "percent"); total13

# Obtain count and % 'Per year' for each multiple-choice
q13_year <- list()
for (i in 1:dim(q13_sub)[2]) {
  q13_year[[i]] <- table(q13_sub[, i], year13)  # Per year, n
}
year_n_1 <- do.call(rbind, q13_year)
year_n13 <- subset(year_n_1, rownames(year_n_1) == "Yes")
rownames(year_n13) <- names(q13_sub); year_n13                                # counts per year
round(t(apply(year_n13, 1, function(x) x/apply(year_n13, 2, sum))) * 100, 1)  # % per year

#* Q: [14] If [11] is not 'Nothing stated', this information was found in ----
#* [multiple choice possible]
q14_data <- dataset[, 58:61]

# Select based on the condition
q14_sub <- subset(q14_data, !is.element(q11, c("Nothing stated", "Not applicable")))
year14 <- subset(year, !is.element(q11, c("Nothing stated", "Not applicable")))
for (i in 1:dim(q14_sub)[2]) {
  q14_sub[, i] <- factor(q14_sub[, i], levels = c("Not applicable", "Yes", "No"))
}
names(q14_sub) <- c("Abstract", "Results", "Discussion", "Conclusions")

# Obtain 'Total' and '% Total' per multiple-choice
total_2 <- apply(q14_sub, 2, function(x) sum(x == "Yes"))
total14 <- data.frame(total_2, round((total_2 / sum(total_2)) * 100, 1))
colnames(total14) <- c("counts", "percent"); total14

# Obtain count and % 'Per year' for each multiple-choice
q14_year <- list()
for (i in 1:dim(q14_sub)[2]) {
  q14_year[[i]] <- table(q14_sub[, i], year14)  # Per year, n
}
year_n_2 <- do.call(rbind, q14_year)
year_n14 <- subset(year_n_2, rownames(year_n_2) == "Yes")
rownames(year_n14) <- names(q14_sub); year_n14                                # counts per year
round(t(apply(year_n14, 1, function(x) x/apply(year_n14, 2, sum))) * 100, 1)  # % per year

#* Q: [15] If [11] is not 'Nothing stated', implications were discussed or ----
#* implied in the context of which NMA component(s) [multiple choice possible]
q15_data <- dataset[, 63:67]

# Select based on the condition - Part I
q15_sub <- subset(q15_data, !is.element(q11, c("Nothing stated", "Not applicable")))
year15 <- subset(year, !is.element(q11, c("Nothing stated", "Not applicable")))

# Rename the levels of the choice 'Not mentioned'
q15_sub[, 1] <- factor(q15_sub[, 1], levels = c("Not applicable", "Yes", "No", "Unclear"))

# Each multiple-choice as a list
for (i in 2:dim(q15_sub)[2]) {
  q15_sub[, i] <- factor(q15_sub[, i], levels = c("Not applicable", "Yes", "No"))
}
names(q15_sub) <- c("Not mentioned", "Treatment effects", "Intervention ranking", 
                    "Statistical heterogeneity", "Evidence consistency")
levels(q15_sub[, 1])[levels(q15_sub[, 1]) == "Unclear"] <- "Yes"

# Obtain 'Total' and '% Total' per multiple-choice
total_3 <- apply(q15_sub, 2, function(x) sum(x == "Yes"))
total15 <- data.frame(total_3, round((total_3 / sum(total_3)) * 100, 1))
colnames(total15) <- c("counts", "percent"); total15

# Obtain count and % 'Per year' for each multiple-choice
q15_year <- list()
for (i in 1:dim(q15_sub)[2]) {
  q15_year[[i]] <- table(q15_sub[, i], year15)  # Per year, n
}
year_n_3 <- do.call(rbind, q15_year)
year_n15 <- subset(year_n_3, rownames(year_n_3) == "Yes")
rownames(year_n15) <- names(q15_sub); year_n15                                # counts per year
round(t(apply(year_n15, 1, function(x) x/apply(year_n15, 2, sum))) * 100, 1)  # % per year
