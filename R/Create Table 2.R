#*******************************************************************************
#*
#*            
#*                         Create Table 2 (Main article)                                                                                                                                                                                                                                               
#*                                                                 
#* Author: Loukia M. Spineli 
#* Date: July 2023
#*******************************************************************************


## Load data ----
load("./data/Analysis dataset.RData")


## General information ----
table(dataset[, "Year"])
year <- factor(ifelse(dataset[, "Year"] <= 2015, "Before PRISMA", "After PRISMA"),
               levels = c("Before PRISMA", "After PRISMA")); table(year)


## Protocol level ----
# [1] Whether the review provides a Protocol
q1 <- factor(dataset[, 6], levels = c("Available", "Not available"))

table(q1)                                     # Total, n
round(prop.table(table(q1))*100, 1)           # Total, %
table(q1, year)                               # Per year, n
round(prop.table(table(q1, year), 2)*100, 1)  # per year, %
# Logistic regression
q1_reg <- glm(ifelse(q1 == "Available", 1, 0) ~ year, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q1_reg))[2], 2); round(exp(confint(q1_reg))[2,], 2)

#* [2] If [1] is 'Available', whether the authors defined 
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

#* [3] If [1] is 'Available', whether the authors mentioned in the Protocol 
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


## Review level (definition & methods) ----
# [4] The authors defined transitivity assumption in the Review
q4 <- factor(dataset[, 28], levels = c("Yes", "No"))

table(q4)                                     # Total, n
round(prop.table(table(q4))*100, 1)           # Total, %
table(q4, year)                               # Per year, n
round(prop.table(table(q4, year), 2)*100, 1)  # Per year, %
# Logistic regression
q4_reg <- glm(ifelse(q4 == "Yes", 1, 0) ~ year, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q4_reg))[2], 2); round(exp(confint(q4_reg))[2,], 2)

#* [5] The authors explicitly stated in the methods that they planned to 
#* evaluate transitivity and reported the evaluation results 
q_cond <- ifelse(dataset[, c(35, 37, 43, 42, 46, 48, 50, 51)] == "Yes", 1, 0)

#' Reviews that used at least one method for transitivity ('Yes')
#' Note that some of the methods that used consistency may have also used the remaining indirect methods
#' but for statistical heterogeneity source.
q_cond_new <- ifelse(rowSums(q_cond) > 0, "Trans+", "Other") 
#q_cond_fin <- ifelse(dataset[, 34] == "Yes", "Yes", q_cond_new)

# Correcting dataset[, 32] for indirect methods that were explicitly used for trans!
q5 <- factor(ifelse(dataset[, 32] == "Yes" & q_cond_new == "Trans+", "Yes", "No"), 
             levels = c("Yes", "No"))

table(q5)                                     # Total, n
round(prop.table(table(q5))*100, 1)           # Total, %
table(q5, year)                               # Per year, n
round(prop.table(table(q5, year), 2)*100, 1)  # per year, %
# Logistic regression
q5_reg <- glm(ifelse(q5 == "Yes", 1, 0) ~ year, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q5_reg))[2], 2); round(exp(confint(q5_reg))[2,], 2)

#* [6] The authors did *not* state in the methods any plans for transitivity 
#* evaluation, but evaluation results were found in the manuscript
# Correcting dataset[, 33] for indirect methods that were explicitly used for trans!
q6 <- factor(ifelse(dataset[, 33] == "Yes" & q_cond_new == "Trans+", "Yes", "No"),
             levels = c("Yes", "No"))

table(q6)                                     # Total, n
round(prop.table(table(q6))*100, 1)           # Total, %
table(q6, year)                               # Per year, n
round(prop.table(table(q6, year), 2)*100, 1)  # per year, %
# Logistic regression
q6_reg <- glm(ifelse(q6 == "Yes", 1, 0) ~ year, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q6_reg))[2], 2); round(exp(confint(q6_reg))[2,], 2)


## Acknowledging the implication of transitivity evaluation ----
# [7] The authors conclude or imply the (im)plausibility of transitivity 
q7 <- factor(ifelse(is.element(dataset[, 53], c("Not applicable", "Nothing stated")) | 
                      q_cond_new != "Trans+", "No", "Yes"),
             levels = c("Yes", "No"))

sum(table(q7))                                # Condition
table(q7)                                     # Total, n
round(prop.table(table(q7))*100, 1)           # Total, %
table(q7, year)                               # Per year, n
round(prop.table(table(q7, year), 2)*100, 1)  # per year, %
# Logistic regression
q7_reg <- glm(ifelse(q7 == "Yes", 1, 0) ~ year, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q7_reg))[2], 2); round(exp(confint(q7_reg))[2,], 2)

#* [8] Among the reviews with a conclusion about transitivity, 
#* some authors explicitly refrained from NMA
q8 <- factor(ifelse(subset(dataset[, 76], q7 == "Yes") == "Yes", "Yes", "No"), 
              levels = c("Yes", "No"))
year8 <- subset(year, q7 == "Yes")
 
sum(table(q8))                                 # Condition
table(q8)                                      # Total, n
round(prop.table(table(q8))*100, 1)            # Total, %
table(q8, year8)                               # Per year, n
round(prop.table(table(q8, year8), 2)*100, 1)  # per year, %
# Logistic regression
q8_reg <- glm(ifelse(q8 == "Yes", 1, 0) ~ year8, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q8_reg))[2], 2); round(exp(confint(q8_reg))[2,], 2)

#* [9] Among the reviews with a conclusion about transitivity, 
#* implications were discussed or implied concerning at least one NMA parameter 
q9 <- ifelse(subset(dataset[, 72:75], q7 == "Yes") == "Yes", 1, 0)

# Reviews that used at least one NMA parameter
q9_new <- factor(ifelse(rowSums(q9) > 0, "Yes", "No"), levels = c("Yes", "No"))

sum(table(q9_new))                                 # Condition
table(q9_new)                                      # Total, n
round(prop.table(table(q9_new))*100, 1)            # Total, %
table(q9_new, year8)                               # Per year, n
round(prop.table(table(q9_new, year8), 2)*100, 1)  # per year, %
# Logistic regression
q9_reg <- glm(ifelse(q9_new == "Yes", 1, 0) ~ year8, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q9_reg))[2], 2); round(exp(confint(q9_reg))[2,], 2)


## Reporting the Table of Characteristics ----
# [10] A Table of Characteristics (ToC) is provided in the publication
q10 <- factor(dataset[, 78], levels = c("Yes", "No"))

# Exclude the systematic reviews without access to their supplementary material
q10_new <- subset(q10, dataset[, 80] != "No access")
year10 <- subset(year, dataset[, 80] != "No access")

table(q10_new)                                      # Total, n
round(prop.table(table(q10_new))*100, 1)            # Total, %
table(q10_new, year10)                              # Per year, n
round(prop.table(table(q10_new, year10), 2)*100, 1) # per year, %
# Logistic regression
q10_reg <- glm(ifelse(q10_new == "Yes", 1, 0) ~ year10, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q10_reg))[2], 2); round(exp(confint(q10_reg))[2,], 2)

# [11] If [10] is 'Yes', the Table presents the information at [ONE choice possible]
q11 <- ifelse(subset(dataset[, 88:94],
                     q10 == "Yes" & dataset[, 80] != "No access") == "Yes", 1, 0)

# Distinguish between reviews with 'Proper' and 'Improper' ToC for transitivity evaluation
q11_new <- factor(ifelse(rowSums(q11[, 1:5]) == 1 & rowSums(q11[, 6:7]) == 0, "Proper", "Improper"), 
                 levels = c("Proper", "Improper"))
year11 <- subset(year, q10 == "Yes" & dataset[, 80] != "No access")

sum(table(q11_new))                                 # Condition
table(q11_new)                                      # Total, n
round(prop.table(table(q11_new))*100, 1)            # Total, %
table(q11_new, year11)                              # Per year, n
round(prop.table(table(q11_new, year11), 2)*100, 1) # per year, %
# Logistic regression
q11_reg <- glm(ifelse(q11_new == "Proper", 1, 0) ~ year11, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q11_reg))[2], 2); round(exp(confint(q11_reg))[2,], 2)

#* [12] Among the reviews with a proper table structure, there is at least one missing 
#* characteristic across the trials or comparisons in the ToC
q12_cond <- ifelse(dataset[, 88:92] == "Yes", 1, 0)

# Keep only the reviews with proper ToC and access to supplementary material 
q12 <- factor(subset(dataset[, 95], q10 == "Yes" & dataset[, 80] != "No access" & rowSums(q12_cond) == 1),
              levels = c("Yes", "No"))
year12 <- subset(year, q10 == "Yes" & dataset[, 80] != "No access" & rowSums(q12_cond) == 1) 

sum(table(q12))                                 # Condition
table(q12)                                      # Total, n
round(prop.table(table(q12))*100, 1)            # Total, %
table(q12, year12)                              # Per year, n
round(prop.table(table(q12, year12), 2)*100, 1) # per year, %
# Logistic regression
q12_reg <- glm(ifelse(q12 == "Yes", 1, 0) ~ year12, family = binomial)
# Odds ratio & 95% CI
round(exp(coef(q12_reg))[2], 2); round(exp(confint(q12_reg))[2,], 2)
