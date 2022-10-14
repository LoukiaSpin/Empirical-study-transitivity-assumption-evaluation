#*******************************************************************************
#*
#*
#*                      Descriptive Statistics & Graphs                                            
#*
#*
#*******************************************************************************



## Load libraries ----
list.of.packages <- c("readxl", "ggplot2", "gghalves", "plyr", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load GitHub repo for trend test
#remotes::install_github("raredd/rawr")
library(rawr)



## Load data ----
path <- "./30_Analysis - Extraction/Loukia Extraction Package/Extracted reviews_Transitivity_Loukia_Version4.xlsm"
dataset0 <- as.data.frame(read_excel(path, 
                                     sheet = "Original", 
                                     na = "NA"))[2:358, ]
dataset <- subset(dataset0, PMID != 24756870) # Remove SR from Pollock (not an NMA!)
colnames(dataset) <- c("Planned trans evaluation: Not applicable",
                       "Planned trans evaluation: No",
                       "Planned trans evaluation: Interpretation 1 Salanti", 
                       "Planned trans evaluation: Verbatim interpretation 1", 
                       "Planned trans evaluation: Interpretations 2&3 Salanti", 
                       "Planned trans evaluation: Verbatim interpretations 2&3",
                       "Planned trans evaluation: Interpretation 4 Salanti",
                       "Planned trans evaluation: Statistically or narratively",
                       "Planned trans evaluation: Interpretation 5 Salanti",
                       "Planned trans evaluation: Verbatim interpretation 5",
                       "Planned trans evaluation: Sensitivity analysis",
                       "Planned trans evaluation: Subgroup analysis",
                       "Planned trans evaluation: Meta-regression",
                       "Planned trans evaluation: Statistical or qualitative consistency evaluation",
                       "Trans evaluated: Not applicable",
                       "Trans evaluated: Interpretation 1 Salanti", 
                       "Trans evaluated: Verbatim interpretation 1", 
                       "Trans evaluated: Interpretations 2&3 Salanti", 
                       "Trans evaluated: Verbatim interpretations 2&3",
                       "Trans evaluated: Interpretation 4 Salanti",
                       "Trans evaluated: Statistically or narratively",
                       "Trans evaluated: Verbatim interpretation 4",
                       "Trans evaluated: Correct evaluation?",
                       "Trans evaluated: Interpretation 5 Salanti",
                       "Trans evaluated: Verbatim interpretation 5",
                       "Trans evaluated: Sensitivity analysis",
                       "Trans evaluated: Subgroup analysis",
                       "Trans evaluated: Meta-regression",
                       "Trans evaluated: Statistical or qualitative consistency evaluation",
                       "Discussed trans: Not applicable",
                       "Discussed trans: Limited available data",
                       "Discussed trans: Interpretation 1 Salanti", 
                       "Discussed trans: Interpretations 2&3 Salanti", 
                       "Discussed trans: Interpretation 4 Salanti", 
                       "Discussed trans: Interpretation 5 Salanti", 
                       "Discussed trans: Sensitivity analysis",
                       "Discussed trans: Subgroup analysis",
                       "Discussed trans: Meta-regression",
                       "Discussed trans: Statistical or qualitative consistency evaluation",
                       "Discussion found in: Not applicable",
                       "Discussion found in: Abstract",
                       "Discussion found in: Results",
                       "Discussion found in: Discussion",
                       "Discussion found in: Conclusions",
                       "Parameter discussed: Not applicable",
                       "Parameter discussed: Not mentioned",
                       "Parameter discussed: Treatment effects",
                       "Parameter discussed: Intervention ranking",
                       "Parameter discussed: Statistical heterogeneity",
                       "Parameter discussed: Evidence (in)consistency",
                       "Quantitative characteristics",
                       "Qualitative characteristics",
                       "Mixture characteristics",
                       "Participant characteristics",
                       "Report participant characteristics",
                       "Intervention characteristics",
                       "Report intervention characteristics",
                       "Outcomes characteristics",
                       "Report outcome characteristics",
                       "Design characteristics",
                       "Report design characteristics",
                       "Trial-level without arm-level",
                       "Trial-level with arm-level",
                       "Comparison-level with arm-level",
                       "minimum no trials",
                       "maximum no trials",
                       "Comparison-level without arm-level",
                       "minimum no trials",
                       "maximum no trials",
                       "Comparison-level characteristic summarised across trials",
                       "Intervention level",
                       "Descriptive statistics per characteristic")


## General information ----
year <- dataset[, "Year"]; table(year)
journal <- dataset[, "Journal"]
field <- dataset[, "Health-related field"]



## Protocol level ----
# G: [1] Whether the review provides a Protocol
q1 <- factor(dataset[, 7], 
             levels = c("Registered", 
                        "Not registered but published", 
                        "Mentioned but not available", 
                        "Explicitly mentioned that there is no protocol", 
                        "Protocol not mentioned"))

table(q1)                                     # Total, n
round(prop.table(table(q1))*100, 1)           # Total, %
table(q1, year)                               # Per year, n
round(prop.table(table(q1, year), 2)*100, 1)  # per year, %
#chisq.test(table(q1, year))
#kw.test(q1 ~ as.factor(year), simulate.p.value = TRUE)

#* H: [2] If [1] is 'Registered' or 'Not registered but published', whether the 
#* authors defined the transitivity assumption in the Protocol 
q2 <- factor(dataset[, 8], levels = c("Not applicable", "Yes", "No"))

q2_new <- subset(q2, is.element(q1, c("Registered", "Not registered but published")))
year2 <- subset(year, is.element(q1, c("Registered", "Not registered but published")))

sum(table(q2_new))                                 # Condition
table(q2_new)                                      # Total, n
round(prop.table(table(q2_new))*100, 1)            # Total, %
table(q2_new, year2)                               # Per year, n
round(prop.table(table(q2_new, year2), 2)*100, 1)  # per year, %
#chisq.test(table(q2_new, year2))

#* J-W: [3] If [1] is 'Registered' or 'Not registered but published', whether the 
#* authors mentioned in the Protocol how they plan to evaluate the transitivity 
#* assumption in the review [multiple choice possible]
q3_1 <- dataset[, 10]   # J: Not applicable
q3_2 <- dataset[, 11]   # K: No
q3_3 <- dataset[, 12]   # L: Salanti 1
q3_4 <- dataset[, 14]   # N: Salanti 2&3
q3_5 <- dataset[, 16]   # P: Salanti 4
q3_5_1 <- dataset[, 17] # Q: Statistical or narrative comparison?
q3_6 <- dataset[, 18]   # R: Salanti 5
q3_7 <- dataset[, 20]   # T: Sensitivity analysis
q3_8 <- dataset[, 21]   # U: Subgroup analysis
q3_9 <- dataset[, 22]   # V: Meta-regression analysis
q3_10 <- dataset[, 23]  # W: Statistical or qualitative methods for consistency

q3_data <- data.frame(q3_2, q3_3, q3_4, q3_5, q3_6, q3_7, q3_8, q3_9, q3_10)
q3_sub <- subset(q3_data, is.element(q1, c("Registered", "Not registered but published")))
for (i in 1:9) {
  q3_sub[, i] <- factor(q3_sub[, i], levels = c("Not applicable", "Yes", "No"))
}
names(q3_sub) <- c("No", "Salanti 1", "Salanti 2&3", "Salanti 4", "Salanti 5",
                   "Sensitivity", "Subgroup", "Meta-regression", "Consistency")

q3_total_n <- q3_year_n <- chisq3 <- list()
for (i in 1:9) {
  q3_total_n[[i]] <- table(q3_sub[, i])                                     # Total, n
  q3_year_n[[i]] <- table(q3_sub[, i], year2)                               # Per year, n
  q3_year_p[[i]] <- round(prop.table(table(q3_sub[, i], year2), 2)*100, 1)  # Per year, %
  chisq3[[i]] <- chisq.test(table(q3_sub[, i], year2))
}
#table(q3_1); round(prop.table(table(q3_1))*100, 1)                   # Refers to 'Not applicable'
#table(q3_1, year);  round(prop.table(table(q3_1, year), 2)*100, 1)   # Refers to 'Not applicable'
names(q3_total_n) <- names(q3_year_n)  <- names(q3_sub)
do.call(rbind, q3_total_n)
round((do.call(rbind, q3_total_n)/sum(do.call(rbind, q3_total_n)[, 2])) * 100, 1)
q3_year_n0 <- do.call(rbind, q3_year_n)
(q3_year_n <- subset(q3_year_n0, rownames(q3_year_n0) == "Yes"))
round(t(apply(q3_year_n, 1, function(x) x/apply(q3_year_n, 2, sum))) * 100, 1)

# q3_5_1: Report the % statistical comparisons out of total "YES"
q3_5_1_new <- subset(q3_5_1, q3_5 == "Yes")
table(q3_5_1_new)
round(prop.table(table(q3_5_1_new))*100, 1)



## Prevalence of transitivity assumption (Review) ----
# X: [4] The authors defined transitivity assumption in the Review
q4 <- factor(dataset[, 24], levels = c("Yes", "No"))

table(q4)                                  # Total, n
round(prop.table(table(q4))*100, 1)        # Total, %
table(q4, year)                            # Per year, n
round(prop.table(table(q4, year))*100, 1)  # Per year, %
#chisq.test(table(q4, year))

# Y: [5] If [4] is 'Yes', it is stated in
q5 <- factor(dataset[, 25], 
             levels = c("Not applicable", 
                        "Abstract", 
                        "Introduction", 
                        "Methods",
                        "Results", 
                        "Discussion", 
                        "Supplementary"))

q5_new <- subset(q5, q4 == "Yes")
year5 <- subset(year, q4 == "Yes")

table(q5_new)                                      # Total, n
round(prop.table(table(q5_new))*100, 1)            # Total, %
table(q5_new, year5)                               # Per year, n
round(prop.table(table(q5_new, year5), 2)*100, 1)  # per year, %


#* AA: [6] The authors explicitly stated in the Methods section of the Review 
#* whether they evaluated transitivity
q6 <- factor(dataset[, 27], levels = c("Yes", "No"))

table(q6)                                     # Total, n
round(prop.table(table(q6))*100, 1)           # Total, %
table(q6, year)                               # Per year, n
round(prop.table(table(q6, year), 2)*100, 1)  # per year, %
#chisq.test(table(q6, year))

#* AB: [7] If [6] is 'Yes', the authors reported the transitivity evaluation in the 
#* Results or Discussion section
q7 <- factor(dataset[, 28], levels = c("Not applicable", "Yes", "No"))

q7_new <- subset(q7, q6 == "Yes")
year7 <- subset(year, q6 == "Yes")

table(q7_new)                                      # Total, n
round(prop.table(table(q7_new))*100, 1)            # Total, %
table(q7_new, year7)                               # Per year, n
round(prop.table(table(q7_new, year7), 2)*100, 1)  # per year, %

#* AC: [8] If [6] is 'No', the Results or Discussion section indicate that the 
#* authors have performed transitivity evaluation
q8 <- factor(dataset[, 29], levels = c("Not applicable", "Yes", "No"))

q8_new <- subset(q8, q6 == "No")
year8 <- subset(year, q6 == "No")

table(q8_new)                                      # Total, n
round(prop.table(table(q8_new))*100, 1)            # Total, %
table(q8_new, year8)                               # Per year, n
round(prop.table(table(q8_new, year8), 2)*100, 1)  # per year, %



## Quality of assessing transitivity assumption (Review) ----
#* AD-AQ: [9] Among the reviews with '[7] is Yes' or '[8] is Yes', which method(s) 
#* have been used [multiple choice possible]
q9_1 <- dataset[, 30]   # AD: Not applicable
q9_2 <- dataset[, 31]   # AE: Salanti 1
q9_3 <- dataset[, 33]   # AG: Salanti 2&3
q9_4 <- dataset[, 35]   # AI: Salanti 4
q9_4_1 <- dataset[, 36] # AJ: Statistical or narrative comparison
q9_4_2 <- dataset[, 38] # AL: Correct evaluation? (They should compare whether comparisons are similar regarding the characteristics)
q9_5 <- dataset[, 39]   # AM: Salanti 5
q9_6 <- dataset[, 41]   # AO: Sensitivity analysis
q9_7 <- dataset[, 42]   # AP: Subgroup analysis
q9_8 <- dataset[, 43]   # AQ: Meta-regression analysis
q9_9 <- dataset[, 44]   # AR: Statistical or qualitative methods for consistency 

q9_data <- data.frame(q9_2, q9_3, q9_4, q9_5, q9_6, q9_7, q9_8, q9_9)
q9_sub <- subset(q9_data, q9_1 == "No")
year9 <- subset(year, q9_1 == "No")
for (i in 1:8) {
  q9_sub[, i] <- factor(q9_sub[, i], levels = c("Not applicable", "Yes", "No"))
}
names(q9_sub) <- names(q3_sub[-1])

q9_total_n <- q9_year_n <- list()
for (i in 1:8) {
  q9_total_n[[i]] <- table(q9_sub[, i])                                     # Total, n
  q9_year_n[[i]] <- table(q9_sub[, i], year9)                               # Per year, n
}
#table(q9_1); round(prop.table(table(q9_1))*100, 1)                         # Refers to 'Not applicable'
#table(q9_1, year9);  round(prop.table(table(q9_1, year9), 2)*100, 1)       # Refers to 'Not applicable'
names(q9_total_n) <- names(q9_year_n) <- names(q9_sub)
do.call(rbind, q9_total_n)
round((do.call(rbind, q9_total_n)/sum(do.call(rbind, q9_total_n)[, 2])) * 100, 1)
q9_year_n0 <- do.call(rbind, q9_year_n)
(q9_year_n <- subset(q9_year_n0, rownames(q9_year_n0) == "Yes"))
round(t(apply(q9_year_n, 1, function(x) x/apply(q9_year_n, 2, sum))) * 100, 1)


# q9_4_1: Report the % statistical comparisons out of total "YES"
q9_4_1_new <- subset(q9_4_1, q9_4 == "Yes")
table(q9_4_1_new)
round(prop.table(table(q9_4_1_new))*100, 1)

# q9_4_2: Report the % correct evaluations out of total "YES"
q9_4_2_new <- subset(q9_4_2, q9_4 == "Yes")
table(q9_4_2_new)
round(prop.table(table(q9_4_2_new))*100, 1)
#table(q9_4_2_new, q9_4_1_new)

#* AS: [10] The authors planned to evaluate transitivity ([6] is 'Yes'), 
#* but they did not perform any
q10 <- factor(dataset[, 45], 
              levels = c("Not applicable",
                         "They performed the evaluation exactly as planned",
                         "They performed some of the planned methods (in [9]) due to limited data",
                         "They could not perform the evaluation due to limited data",
                         "They did not report in the Results or Discussion section any transitivity evaluation"))  

q10_new <- subset(q10, q6 == "Yes")
year10 <- subset(year, q6 == "Yes")

table(q10_new)                                       # Total, n
round(prop.table(table(q10_new))*100, 1)             # Total, %
table(q10_new, year10)                               # Per year, n
round(prop.table(table(q10_new, year10), 2)*100, 1)  # per year, %



## Acknowledging the implication of transitivity evaluation ----
#* AT: [11] Among the reviews with '[7] is Yes' or '[8] is Yes', what did the authors
#* conclude or imply regarding the plausibility of transitivity 
q11 <- factor(dataset[, 46],
              levels = c("Not applicable",
                         "Transitivity may be plausible",
                         "Transitivity may be questionable",
                         "Difficult to judge due to limited data",
                         "Nothing stated"))

q11_new <- subset(q11, q9_1 == "No")
year11 <- subset(year, q9_1 == "No")

table(q11_new)                                       # Total, n
round(prop.table(table(q11_new))*100, 1)             # Total, %
table(q11_new, year11)                               # Per year, n
round(prop.table(table(q11_new, year11), 2)*100, 1)  # per year, %

#* AV-BE: [12] If [11] is not 'Nothing stated' or 'Not applicable', What method(s) 
#* did the authors consider to conclude or imply about the plausibility of 
#* transitivity [multiple choice possible]
q12_1 <- dataset[, 48]  # AV: Not applicable
q12_2 <- dataset[, 49]  # AW: Limited data
q12_3 <- dataset[, 50]  # AX: Salanti 1
q12_4 <- dataset[, 51]  # AY: Salanti 2&3
q12_5 <- dataset[, 52]  # AZ: Salanti 4
q12_6 <- dataset[, 53]  # BA: Salanti 5
q12_7 <- dataset[, 54]  # BB: Sensitivity analysis
q12_8 <- dataset[, 55]  # BC: Subgroup analysis
q12_9 <- dataset[, 56]  # BD: Meta-regression analysis
q12_10 <- dataset[, 57] # BE: Statistical or qualitative methods for consistency evaluation 

q12_data <- data.frame(q12_2, q12_3, q12_4, q12_5, q12_6, q12_7, q12_8, q12_9, q12_10)
q12_sub <- subset(q12_data, !is.element(q11, c("Nothing stated", "Not applicable")))
year12 <- subset(year, !is.element(q11, c("Nothing stated", "Not applicable")))
for (i in 1:9) {
  q12_sub[, i] <- factor(q12_sub[, i], levels = c("Not applicable", "Yes", "No"))
}
names(q12_sub) <- c("Limited data", names(q9_sub))

q12_total_n <- q12_year_n <- list()
for (i in 1:9) {
  q12_total_n[[i]] <- table(q12_sub[, i])                                      # Total, n
  q12_year_n[[i]] <- table(q12_sub[, i], year12)                               # Per year, n
}
#table(q12_1); round(prop.table(table(q12_1))*100, 1)                          # Refers to 'Not applicable'
#table(q12_1, year);  round(prop.table(table(q12_1, year), 2)*100, 1)          # Refers to 'Not applicable'
names(q12_total_n) <- names(q12_year_n) <- names(q12_sub)
do.call(rbind, q12_total_n)
round((do.call(rbind, q12_total_n)/sum(do.call(rbind, q12_total_n)[, 2])) * 100, 1)
q12_year_n0 <- do.call(rbind, q12_year_n)
(q12_year_n <- subset(q12_year_n0, rownames(q12_year_n0) == "Yes"))
round(t(apply(q12_year_n, 1, function(x) x/apply(q12_year_n, 2, sum))) * 100, 1)

#* BF-BJ: [13] If [11] is not 'Nothing stated', this information was found in 
#* [multiple choice possible]
q13_1 <- dataset[, 58]  # BF: Not applicable
q13_2 <- dataset[, 59]  # BG: Abstract
q13_3 <- dataset[, 60]  # BH: Results
q13_4 <- dataset[, 61]  # BI: Discussion
q13_5 <- dataset[, 62]  # BJ: Conclusions

q13_data <- data.frame(q13_2, q13_3, q13_4, q13_5)
q13_sub <- subset(q13_data, !is.element(q11, c("Nothing stated", "Not applicable")))
year13 <- subset(year, !is.element(q11, c("Nothing stated", "Not applicable")))
for (i in 1:4) {
  q13_sub[, i] <- factor(q13_sub[, i], levels = c("Not applicable", "Yes", "No"))
}
names(q13_sub) <- c("Abstract", "Results", "Discussion", "Conclusions")

q13_total_n <- q13_year_n <- list()
for (i in 1:4) {
  q13_total_n[[i]] <- table(q13_sub[, i])                                      # Total, n
  q13_year_n[[i]] <- table(q13_sub[, i], year13)                               # Per year, n
}
#table(q13_1); round(prop.table(table(q13_1))*100, 1)                          # Refers to 'Not applicable'
#table(q13_1, year);  round(prop.table(table(q13_1, year), 2)*100, 1)          # Refers to 'Not applicable'
names(q13_total_n) <- names(q13_year_n) <- names(q13_sub)
do.call(rbind, q13_total_n)
round((do.call(rbind, q13_total_n)/sum(do.call(rbind, q13_total_n)[, 2])) * 100, 1)
q13_year_n0 <- do.call(rbind, q13_year_n)
(q13_year_n <- subset(q13_year_n0, rownames(q13_year_n0) == "Yes"))
round(t(apply(q13_year_n, 1, function(x) x/apply(q13_year_n, 2, sum))) * 100, 1)

#* BK-BP: [14] If [11] is not 'Nothing stated', implications were discussed or 
#* implied in the context of which NMA component(s) [multiple choice possible]
q14_1 <- dataset[, 63]  # BK: Not applicable
q14_2 <- dataset[, 64]  # BL: Not mentioned
q14_3 <- dataset[, 65]  # BM: Treatment effects
q14_4 <- dataset[, 66]  # BN: Intervention ranking
q14_5 <- dataset[, 67]  # BO: Statistical heterogeneity
q14_6 <- dataset[, 68]  # BP: Evidence consistency

q14_data <- data.frame(q14_2, q14_3, q14_4, q14_5, q14_6)
q14_sub <- subset(q14_data, !is.element(q11, c("Nothing stated", "Not applicable")))
year14 <- subset(year, !is.element(q11, c("Nothing stated", "Not applicable")))
q14_sub[, 1] <- factor(q14_sub[, 1], levels = c("Not applicable", "Yes", "No", "Unclear"))
for (i in 2:5) {
  q14_sub[, i] <- factor(q14_sub[, i], levels = c("Not applicable", "Yes", "No"))
}
names(q14_sub) <- c("Not mentioned", "Treatment effects", "Intervention ranking", 
                    "Statistical heterogeneity", "Evidence consistency")
levels(q14_sub[, 1])[levels(q14_sub[, 1]) == "Unclear"] <- "Yes"

q14_total_n <- q14_year_n <- list()
for (i in 1:5) {
  q14_total_n[[i]] <- table(q14_sub[, i])                                      # Total, n
  q14_year_n[[i]] <- table(q14_sub[, i], year14)                               # Per year, n
}
#table(q14_1); round(prop.table(table(q14_1))*100, 1)                          # Refers to 'Not applicable'
#table(q14_1, year);  round(prop.table(table(q14_1, year), 2)*100, 1)          # Refers to 'Not applicable'
names(q14_total_n) <- names(q14_year_n) <- names(q14_sub)
do.call(rbind, q14_total_n)
round((do.call(rbind, q14_total_n)/sum(do.call(rbind, q14_total_n)[, 2])) * 100, 1)
q14_year_n0 <- do.call(rbind, q14_year_n)
(q14_year_n <- subset(q14_year_n0, rownames(q14_year_n0) == "Yes"))
round(t(apply(q14_year_n, 1, function(x) x/apply(q14_year_n, 2, sum))) * 100, 1)

#* BQ: [15] If [11] is 'Transitivity may be questionable' or 'Difficult to judge due to 
#* limited data', did the authors decide to abstain from NMA [Document verbatim]
q15 <- factor(dataset[, 69], 
              levels = c("Not applicable", "Yes", "No, they performed NMA"))

q15_new <- subset(q15, is.element(q11, c("Transitivity may be questionable", 
                                         "Difficult to judge due to limited data")))
year15 <- subset(year, is.element(q11, c("Transitivity may be questionable", 
                                         "Difficult to judge due to limited data")))

table(q15_new)                                       # Total, n
round(prop.table(table(q15_new))*100, 1)             # Total, %
table(q15_new, year15)                               # Per year, n
round(prop.table(table(q15_new, year15), 2)*100, 1)  # per year, %



## Reporting the Table of Characteristics ----
# BS: [16] A Table of Characteristics (ToC) is provided in the publication
q16 <- factor(dataset[, 71], levels = c("Yes", "No"))

table(q16)                                     # Total, n
round(prop.table(table(q16))*100, 1)           # Total, %
table(q16, year)                               # Per year, n
round(prop.table(table(q16, year), 2)*100, 1)  # per year, %
#chisq.test(table(q16, year))

# BT: [17] If [16] is 'Yes', this Table can be found in
q17 <- factor(dataset[, 72], levels = c("Not applicable",
                                        "Main article",
                                        "Supplementary",
                                        "Both"))

q17_new <- subset(q17, q16 == "Yes")
year17 <- subset(year, q16 == "Yes")

table(q17_new)                                       # Total, n
round(prop.table(table(q17_new))*100, 1)             # Total, %
table(q17_new, year17)                               # Per year, n
round(prop.table(table(q17_new, year17), 2)*100, 1)  # per year, %

# CG-CQ: [21] If [16] is 'Yes', the Table presents the information at [ONE choice possible]
q21_10 <- dataset[, 85]   # CG: Trial-level w/o arm-level information 
q21_1 <- ifelse(q21_10 == "Yes", 1, 0)
q21_20 <- dataset[, 86]   # CH: Trial-level with arm-level information 
q21_2 <- ifelse(q21_20 == "Yes", 1, 0)
q21_30 <- dataset[, 87]   # CI: Comparison-level with included trials and arm-level information 
q21_3 <- ifelse(q21_30 == "Yes", 1, 0)
q21_3_1 <- dataset[, 88] # CJ: minimum number of trials
q21_3_2 <- dataset[, 89] # CK: maximum number of trials
q21_40 <- dataset[, 90]   # CL: Comparison-level with included trials but w/o arm-level information
q21_4 <- ifelse(q21_40 == "Yes", 1, 0)
q21_4_1 <- dataset[, 91] # CM: minimum number of trials 
q21_4_2 <- dataset[, 92] # CN: maximum number of trials 
q21_5 <- dataset[, 93]   # CO: Comparison-level with characteristics summarised across trials 
q21_6 <- dataset[, 94]   # CP: Intervention-level summarised patient characteristics 
q21_7 <- dataset[, 95]   # CQ: Table with the descriptive statistics for each characteristic

q21_data <- data.frame(q21_1 + q21_2, q21_3 + q21_4, q21_5, q21_6, q21_7)
q21_data[, 1:2] <- ifelse(q21_data[, 1:2] == 1, "Yes", "No")
q21_sub <- subset(q21_data, q16 == "Yes")
year21 <- subset(year, q16 == "Yes")
for (i in 1:5) {
  q21_sub[, i] <- factor(q21_sub[, i], levels = c("Yes", "No"))
}
names(q21_sub) <- c("Characteristics at trial-level","At comparison-level with trials",
                    "Comparison-level with characteristics", 
                    "Intervention-level", "Descriptives per chracteristic")
#names(q21_sub) <- c("Trial-level w/o", "Trial-level with", "Comparison-level with",
#                    "Comparison-level w/o", "Comparison-level with characteristics", 
#                    "Intervention-level", "Descriptives per chracteristic")

q21_total_n <- q21_total_p <- q21_year_n <- q21_year_p <- list()
for (i in 1:5) {
  q21_total_n[[i]] <- table(q21_sub[, i])                                      # Total, n
  q21_total_p[[i]] <- round(prop.table(table(q21_sub[, i]))*100, 1)            # Total, %
  q21_year_n[[i]] <- table(q21_sub[, i], year21)                               # Per year, n
  q21_year_p[[i]] <- round(prop.table(table(q21_sub[, i], year21), 2)*100, 1)  # Per year, %
}
names(q21_total_n) <- names(q21_total_p) <- names(q21_sub)
names(q21_year_n) <- names(q21_year_p) <- names(q21_sub)
do.call(rbind, q21_total_n)
do.call(rbind, q21_total_p)
q21_year_n0 <- do.call(rbind, q21_year_n)
(q21_year_n <- subset(q21_year_n0, rownames(q21_year_n0) == "Yes"))
round(t(apply(q21_year_n, 1, function(x) x/apply(q21_year_n, 2, sum))) * 100, 1)

# q21_3_1: Report the range for 'min' and 'max' separately
q21_3_min <- as.numeric(subset(q21_3_1, q21_3_1 != "Not applicable"))
q21_3_max <- as.numeric(subset(q21_3_2, q21_3_2 != "Not applicable"))
q21_3_res <- rbind(range(q21_3_min), range(q21_3_max))
colnames(q21_3_res) <- c("min", "max")
rownames(q21_3_res) <- c("q21_3_min", "q21_3_max"); q21_3_res

# q21_4_1: Report the range for 'min' and 'max' separately
q21_4_min <- as.numeric(subset(q21_4_1, q21_4_1 != "Not applicable"))
q21_4_max <- as.numeric(subset(q21_4_2, q21_4_2 != "Not applicable"))
q21_4_res <- rbind(range(q21_4_min), range(q21_4_max))
colnames(q21_4_res) <- c("min", "max")
rownames(q21_4_res) <- c("q21_4_min", "q21_4_max"); q21_4_res

#* CR: [22] If [16] is 'Yes' and [21] is not 'Table with the descriptive statistics 
#* for each characteristic', there is at least one trial (or comparison) with 
#* at least one missing characteristic
q22 <- factor(dataset[, 96], levels = c("Not applicable", "Yes", "No"))

q22_new <- subset(q22, q22 != "Not applicable")
year22 <- subset(year, q22 != "Not applicable")

table(q22_new)                                       # Total, n
round(prop.table(table(q22_new))*100, 1)             # Total, %
table(q22_new, year22)                               # Per year, n
round(prop.table(table(q22_new, year22), 2)*100, 1)  # per year, %



## Bubble plot: Year by healthcase field ----
counts <- ddply(dataset, .(dataset[, "Year"], dataset[, "Health-related field"]), nrow)
names(counts) <- c("Year", "Health", "Freq"); counts
sort_field <- names(sort(table(field), decreasing = FALSE))

# Add radius as new variable to data frame 'counts'
counts$radius <- sqrt(counts$Freq/pi)

tiff("./31_Analysis - Descriptives/Figure 1.tiff", 
     height = 30, 
     width = 38, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(counts, aes(as.factor(Year), factor(Health, levels = sort_field))) +
  geom_point(aes(size = radius*7.8), 
             shape = 21, 
             fill = "white", 
             color = "black",
             stroke = 1.2) +
  geom_text(aes(label = Freq), size = 4) +
  labs(x = "Year of publication") + 
  labs(y = "Health specialty") +
  scale_size_identity() +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = 2, color = "grey"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"))
dev.off()

