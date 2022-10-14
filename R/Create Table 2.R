#*******************************************************************************
#*
#*
#*                                Create Table 2                                                                                        
#*      (Awareness and methods of evaluating the transitivity assumption)         
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



# Q: [4] The authors defined transitivity assumption in the Review ----
trans_defined <- factor(dataset[, 23], levels = c("Yes", "No"))

#' NOTE: First row % is row-percentage (for the 'Total %' in the Table), 
#' and second row % is column-percentage (For the 'year %' in the Table)
CrossTable(trans_defined, year, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")



# Q: [5] If [4] is 'Yes', it is stated in ----
q5 <- factor(dataset[, 24], 
             levels = c("Not applicable", 
                        "Abstract", 
                        "Introduction", 
                        "Methods",
                        "Results", 
                        "Discussion", 
                        "Supplementary"))

definion_found_in <- subset(q5, trans_defined == "Yes")
year5 <- subset(year, trans_defined == "Yes")

#' NOTE: First row % is row-percentage (for the 'Total %' in the Table), 
#' and second row % is column-percentage (For the 'year %' in the Table)
CrossTable(definion_found_in, year5, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")



#* Q: [6] The authors explicitly stated in the Methods section of the Review ---- 
#* whether they evaluated transitivity
methods_trans <- factor(dataset[, 26], levels = c("Yes", "No"))

#' NOTE: First row % is row-percentage (for the 'Total %' in the Table), 
#' and second row % is column-percentage (For the 'year %' in the Table)
CrossTable(methods_trans, year, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")



#* Q: [7] If [6] is 'Yes', the authors reported the transitivity evaluation in the ----
#* Results or Discussion section
q7 <- factor(dataset[, 27], levels = c("Not applicable", "Yes", "No"))

reported_trans <- subset(q7, methods_trans == "Yes")
year7 <- subset(year, methods_trans == "Yes")

#' NOTE: First row % is row-percentage (for the 'Total %' in the Table), 
#' and second row % is column-percentage (For the 'year %' in the Table)
CrossTable(reported_trans, year7, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")



#* Q: [8] If [6] is 'No', the Results or Discussion section indicate that the ----
#* authors have performed transitivity evaluation
q8 <- factor(dataset[, 28], levels = c("Not applicable", "Yes", "No"))

indicated_trans <- subset(q8, methods_trans == "No")
year8 <- subset(year, methods_trans == "No")

#' NOTE: First row % is row-percentage (for the 'Total %' in the Table), 
#' and second row % is column-percentage (For the 'year %' in the Table)
CrossTable(indicated_trans, year8, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")



#* Q: [9] Among the reviews with '[7] is Yes' or '[8] is Yes', which method(s) ----
#* have been used [multiple choice possible]
q9_data <- dataset[, c(30, 32, 34, 38, 40, 41, 42, 43)]

# Select based on the condition
q9_sub <- subset(q9_data, dataset[, 29] == "No")
year9 <- subset(year, dataset[, 29] == "No")
for (i in 1:dim(q9_sub)[2]) {
  q9_sub[, i] <- factor(q9_sub[, i], levels = c("Not applicable", "Yes", "No"))
}
names(q9_sub) <- c("Salanti 1", "Salanti 2&3", "Salanti 4", "Salanti 5",
                   "Sensitivity", "Subgroup", "Meta-regression", "Consistency")

# Obtain 'Total' and '% Total' per multiple-choice
total0 <- apply(q9_sub, 2, function(x) sum(x == "Yes"))
total <- data.frame(total0, round((total0 / sum(total0)) * 100, 1))
colnames(total) <- c("counts", "percent"); total

# Obtain count and % 'Per year' for each multiple-choice
q9_year <- list()
for (i in 1:dim(q9_sub)[2]) {
  q9_year[[i]] <- table(q9_sub[, i], year9)  # Per year, n
}
year_n0 <- do.call(rbind, q9_year)
year_n <- subset(year_n0, rownames(year_n0) == "Yes")
rownames(year_n) <- names(q9_sub); year_n                                # counts per year
round(t(apply(year_n, 1, function(x) x/apply(year_n, 2, sum))) * 100, 1) # % per year



#* Q: [10] The authors planned to evaluate transitivity ([6] is 'Yes'), ----
#* but they did not perform any
q10 <- factor(dataset[, 44], 
              levels = c("Not applicable",
                         "They performed the evaluation exactly as planned",
                         "They performed some of the planned methods (in [9]) due to limited data",
                         "They could not perform the evaluation due to limited data",
                         "They did not report in the Results or Discussion section any transitivity evaluation"))  

performed_nma <- subset(q10, methods_trans == "Yes")
year10 <- subset(year, methods_trans == "Yes")

#' NOTE: First row % is row-percentage (for the 'Total %' in the Table), 
#' and second row % is column-percentage (For the 'year %' in the Table)
CrossTable(performed_nma, year10, digits = 1, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS")

