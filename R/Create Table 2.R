#*******************************************************************************
#*
#*
#*                         Creating Table 2 (Main Text)     
#*                           (Using 'dataset.RData')                                                                                                                                                                                                         
#*
#*
#*******************************************************************************



## Load libraries ----
list.of.packages <- c("readxl", "plyr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load data ----
load("./31_Analysis - Descriptives/dataset.RData")



## General information ----
table(dataset[, "Year"])
year <- factor(ifelse(dataset[, "Year"] <= 2015, "Before PRISMA", "After PRISMA"),
               levels = c("Before PRISMA", "After PRISMA")); table(year)



## Protocol level ----
# G: [1] Whether the review provides a Protocol
q1 <- factor(dataset[, 6], levels = c("Available", "Not available"))

table(q1)                                     # Total, n
round(prop.table(table(q1))*100, 1)           # Total, %
table(q1, year)                               # Per year, n
round(prop.table(table(q1, year), 2)*100, 1)  # per year, %
# Logistic regression
q1_reg <- glm(ifelse(q1 == "Available", 1, 0) ~ year, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q1_reg))[2], 2); round(exp(confint(q1_reg))[2,], 2)

#* H: [2] If [1] is 'Available', whether the authors defined 
#* the transitivity assumption in the Protocol 
q2 <- factor(subset(dataset[, 7], q1 == "Available"), 
             levels = c("Yes", "No"))
year2 <- subset(year, q1 == "Available")

sum(table(q2))                                 # Condition
table(q2)                                      # Total, n
round(prop.table(table(q2))*100, 1)            # Total, %
table(q2, year2)                               # Per year, n
round(prop.table(table(q2, year2), 2)*100, 1)  # per year, %
# Logistic regression
q2_reg <- glm(ifelse(q2 == "Yes", 1, 0) ~ year2, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q2_reg))[2], 2); round(exp(confint(q2_reg))[2,], 2)

#* K: [3] If [1] is 'Available', whether the authors mentioned in the Protocol 
#* that they planned to evaluate the transitivity assumption in the review
q3 <- ifelse(subset(dataset[, c(11, 13, 19, 18, 22, 24, 26, 27)], 
                    q1 == "Available") == "Yes", 1, 0)

# Reviews that used at least one method for transitivity ('Yes')
q3_new <- factor(ifelse(rowSums(q3) > 0, "Yes", "No"),
                 levels = c("Yes", "No"))

sum(table(q3_new))                                 # Condition
table(q3_new)                                      # Total, n
round(prop.table(table(q3_new))*100, 1)            # Total, %
table(q3_new, year2)                               # Per year, n
round(prop.table(table(q3_new, year2), 2)*100, 1)  # per year, %
# Logistic regression
q3_reg <- glm(ifelse(q3_new == "Yes", 1, 0) ~ year2, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q3_reg))[2], 2); round(exp(confint(q3_reg))[2,], 2)

#* L-AB (Not V, X, Z!): [4] If [3] is 'Yes', which methods were reported to have been used
#q4 <- subset(q3, q3_new == "Yes")
#year4 <- subset(year2, q3_new == "Yes")

# Distinguish among reviews with 'Direct', 'Indirect', or 'Both'
#q4_new <- factor(ifelse(rowSums(q4[, 1:4]) > 0 & rowSums(q4[, 5:8]) == 0, "direct", 
#                        ifelse(rowSums(q4[, 1:4]) == 0 & rowSums(q4[, 5:8]) > 0, "indirect", "both")), 
#                 levels = c("direct", "indirect", "both"))
#
#sum(table(q4_new))                                      # Condition
#table(q4_new)                                           # Total, n
#round(prop.table(table(q4_new))*100, 1)                 # Total, %
#table(q4_new, year4)                                    # Per year, n
#round(prop.table(table(q4_new, year4), 2)*100, 1)       # per year, %
# Logistic regression
#q4_reg <- glm(ifelse(subset(q4_new, q4_new != "direct") == "indirect", 1, 0) ~ 
#                subset(year4, q4_new != "direct"), family = binomial)
# Odds ratio & 95% CI
#round(exp(coef(q4_reg))[2], 2); round(exp(confint(q4_reg))[2,], 2)



## Review level (definition & methods) ----
# AC: [5] The authors defined transitivity assumption in the Review
q5 <- factor(dataset[, 28], levels = c("Yes", "No"))

table(q5)                                     # Total, n
round(prop.table(table(q5))*100, 1)           # Total, %
table(q5, year)                               # Per year, n
round(prop.table(table(q5, year), 2)*100, 1)  # Per year, %
# Logistic regression
q5_reg <- glm(ifelse(q5 == "Yes", 1, 0) ~ year, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q5_reg))[2], 2); round(exp(confint(q5_reg))[2,], 2)

#* AG: [6] The authors explicitly stated in the methods that they planned to 
#* evaluate transitivity and reported the evaluation results 
q_cond <- ifelse(dataset[, c(35, 37, 43, 42, 46, 48, 50, 51)] == "Yes", 1, 0)

#' Reviews that used at least one method for transitivity ('Yes')
#' Note that some of the methods that used consistency may have also used the remaining indirect methods
#' but for statistical heterogeneity source.
q_cond_new <- ifelse(rowSums(q_cond) > 0, "Trans+", "Other") 
q_cond_fin <- ifelse(dataset[, 34] == "Yes", "Yes", q_cond_new)


# Correcting dataset[, 32] for indirect methods that were explicitly used for trans!
q6 <- factor(ifelse(dataset[, 32] == "Yes" & q_cond_fin == "Trans+", "Yes", "No"),
             levels = c("Yes", "No"))

table(q6)                                     # Total, n
round(prop.table(table(q6))*100, 1)           # Total, %
table(q6, year)                               # Per year, n
round(prop.table(table(q6, year), 2)*100, 1)  # per year, %
# Logistic regression
q6_reg <- glm(ifelse(q6 == "Yes", 1, 0) ~ year, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q6_reg))[2], 2); round(exp(confint(q6_reg))[2,], 2)

#* AH: [7] The authors did *not* state in the methods any plans for transitivity 
#* evaluation, but evaluation results were found in the manuscript
# Correcting dataset[, 33] for indirect methods that were explicitly used for trans!
q7 <- factor(ifelse(dataset[, 33] == "Yes" & q_cond_fin == "Trans+", "Yes", "No"),
             levels = c("Yes", "No"))

table(q7)                                     # Total, n
round(prop.table(table(q7))*100, 1)           # Total, %
table(q7, year)                               # Per year, n
round(prop.table(table(q7, year), 2)*100, 1)  # per year, %
# Logistic regression
q7_reg <- glm(ifelse(q7 == "Yes", 1, 0) ~ year, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q7_reg))[2], 2); round(exp(confint(q7_reg))[2,], 2)

#* AJ-AZ (Not AT, AV, AX!): [8] Among the reviews that evaluated transitivity, 
#* which methods were reported to have been used
#q8 <- subset(q_cond, q6 == "Yes" | q7 == "Yes")
#
# Distinguish among reviews with 'Direct', 'Indirect', or 'Both'
#q8_new <- factor(ifelse(rowSums(q8[, 1:4]) > 0 & rowSums(q8[, 5:8]) == 0, "direct", 
#                        ifelse(rowSums(q8[, 1:4]) == 0 & rowSums(q8[, 5:8]) > 0, "indirect", "both")), 
#                 levels = c("direct", "indirect", "both"))
#year8 <- subset(year, q6 == "Yes" | q7 == "Yes")

#sum(table(q8_new))                                        # Condition
#table(q8_new)                                              # Total, n
#round(prop.table(table(q8_new))*100, 1)                    # Total, %
#table(q8_new, year8)                                       # Per year, n
#round(prop.table(table(q8_new, year8), 2)*100, 1)          # per year, %
# Multinomial logistic regression
#q8_reg_indi <- multinom(relevel(q8_new, ref = "indirect") ~ year8)
#q8_reg_both <- multinom(relevel(q8_new, ref = "both") ~ year8)
#q8_reg_dire <- multinom(relevel(q8_new, ref = "direct") ~ year8)
# Odds ratio & 95% CI
#round(exp(coef(q8_reg_indi))[1,2], 2); round(exp(confint(q8_reg_indi))[2,,1], 2) # Odds of 'direct' in After- versus Before-PRISMA
#round(exp(coef(q8_reg_both))[1,2], 2); round(exp(confint(q8_reg_both))[2,,1], 2) # Odds of 'indirect' in After- versus Before-PRISMA
#round(exp(coef(q8_reg_dire))[2,2], 2); round(exp(confint(q8_reg_dire))[2,,2], 2) # Odds of 'both' in After- versus Before-PRISMA



## Acknowledging the implication of transitivity evaluation ----
# [9] The authors conclude or imply the (im)plausibility of transitivity 
q9 <- factor(ifelse(is.element(dataset[, 53], c("Not applicable", "Nothing stated")) | q_cond_fin != "Trans+", "No", "Yes"),
             levels = c("Yes", "No"))

sum(table(q9))                                # Condition
table(q9)                                     # Total, n
round(prop.table(table(q9))*100, 1)           # Total, %
table(q9, year)                               # Per year, n
round(prop.table(table(q9, year), 2)*100, 1)  # per year, %
# Logistic regression
q9_reg <- glm(ifelse(q9 == "Yes", 1, 0) ~ year, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q9_reg))[2], 2); round(exp(confint(q9_reg))[2,], 2)

#* BY: [10] Among the reviews with a conclusion about transitivity, 
#* some authors explicitly abstained from NMA
q10 <- factor(ifelse(subset(dataset[, 76], q9 == "Yes") == "Yes", "Yes", "No"), 
              levels = c("Yes", "No"))
year10 <- subset(year, q9 == "Yes")
 
sum(table(q10))                                 # Condition
table(q10)                                      # Total, n
round(prop.table(table(q10))*100, 1)            # Total, %
table(q10, year10)                              # Per year, n
round(prop.table(table(q10, year10), 2)*100, 1) # per year, %
# Logistic regression
q10_reg <- glm(ifelse(q10 == "Yes", 1, 0) ~ year10, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q10_reg))[2], 2); round(exp(confint(q10_reg))[2,], 2)

#* BU-BX: [11] Among the reviews with a conclusion about transitivity, 
#* implications were discussed or implied concerning at least one NMA parameter 
q11 <- ifelse(subset(dataset[, 72:75], q9 == "Yes") == "Yes", 1, 0)

# Reviews that used at least one NMA parameter
q11_new <- factor(ifelse(rowSums(q11) > 0, "Yes", "No"), levels = c("Yes", "No"))

sum(table(q11_new))                                 # Condition
table(q11_new)                                      # Total, n
round(prop.table(table(q11_new))*100, 1)            # Total, %
table(q11_new, year10)                              # Per year, n
round(prop.table(table(q11_new, year10), 2)*100, 1) # per year, %
# Logistic regression
q11_reg <- glm(ifelse(q11_new == "Yes", 1, 0) ~ year10, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q11_reg))[2], 2); round(exp(confint(q11_reg))[2,], 2)



## Reporting the Table of Characteristics ----
# CA: [12] A Table of Characteristics (ToC) is provided in the publication
q12 <- factor(dataset[, 78], levels = c("Yes", "No"))

# Exclude the systematic reviews without access to their supplementary material
q12_new <- subset(q12, dataset[, 80] != "No access")
year12 <- subset(year, dataset[, 80] != "No access")

table(q12_new)                                      # Total, n
round(prop.table(table(q12_new))*100, 1)            # Total, %
table(q12_new, year12)                              # Per year, n
round(prop.table(table(q12_new, year12), 2)*100, 1) # per year, %
# Logistic regression
q12_reg <- glm(ifelse(q12_new == "Yes", 1, 0) ~ year12, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q12_reg))[2], 2); round(exp(confint(q12_reg))[2,], 2)

# CO-CY: [13] If [12] is 'Yes', the Table presents the information at [ONE choice possible]
q13 <- ifelse(subset(dataset[, 88:94], q12 == "Yes" & dataset[, 80] != "No access") == "Yes", 1, 0)

# Distinguish between reviews with 'Proper' and 'Improper' ToC for transitivity evaluation
q13_new <- factor(ifelse(rowSums(q13[, 1:5]) == 1 & rowSums(q13[, 6:7]) == 0, "Proper", "Improper"), 
                 levels = c("Proper", "Improper"))
year13 <- subset(year, q12 == "Yes" & dataset[, 80] != "No access")

sum(table(q13_new))                                 # Condition
table(q13_new)                                      # Total, n
round(prop.table(table(q13_new))*100, 1)            # Total, %
table(q13_new, year13)                              # Per year, n
round(prop.table(table(q13_new, year13), 2)*100, 1) # per year, %
# Logistic regression
q13_reg <- glm(ifelse(q13_new == "Proper", 1, 0) ~ year13, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q13_reg))[2], 2); round(exp(confint(q13_reg))[2,], 2)

#* CR: [14] Among the reviews with a proper table structure, there is at least one missing 
#* characteristic across the trials or comparisons in the ToC
q12_cond <- ifelse(dataset[, 88:92] == "Yes", 1, 0)

# Keep only the reviews with proper ToC and access to supplementary material 
q14 <- factor(subset(dataset[, 95], q12 == "Yes" & dataset[, 80] != "No access" & rowSums(q12_cond) == 1),
              levels = c("Yes", "No"))
year14 <- subset(year, q12 == "Yes" & dataset[, 80] != "No access" & rowSums(q12_cond) == 1) 

sum(table(q14))                                 # Condition
table(q14)                                      # Total, n
round(prop.table(table(q14))*100, 1)            # Total, %
table(q14, year14)                              # Per year, n
round(prop.table(table(q14, year14), 2)*100, 1) # per year, %
# Logistic regression
q14_reg <- glm(ifelse(q14 == "Yes", 1, 0) ~ year14, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q14_reg))[2], 2); round(exp(confint(q14_reg))[2,], 2)
