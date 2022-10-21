#*******************************************************************************
#*
#*
#*                               Create Figure 3                                                          
#*        (Barplot on the quality of transitivity evaluation over time)                                                                          
#*                                                                 
#* Author: Loukia M. Spineli 
#* Date: October 2022
#*******************************************************************************

## Install the development version of rnmamod R-package ----
install.packages("devtools")
devtools::install_github("LoukiaSpin/rnmamod")

## Load libraries ----
list.of.packages <- c("ggplot2", "dplyr", "rnmamod")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)

## Load data ----
load("./data/dataset.RData")

#' Create new variables from the extraction items to judge the quality of transitivity evaluation.
#' We used our rule (section 2.4 - Quality of the reported transitivity evaluation) to create the new variables

## Planned in the protocol - Protocol level (Both/Only direct methods/Only indirect methods/No/No protocol) ----
# Rename the levels of the item "Planned trans evaluation: No".
q3_0 <- ifelse(dataset[, 10] == "Not applicable", "No protocol", dataset[, 10])

#' Count the 'Yes' across the items that refer to direct evaluations of the transitivity assumption: 
#' "Planned trans evaluation: Interpretation 1 Salanti", "Planned trans evaluation: Interpretations 2&3 Salanti", 
#' "Planned trans evaluation: Interpretation 4 Salanti", and "Planned trans evaluation: Interpretation 5 Salanti". 
dir_meth <- apply(dataset[, c(11, 13, 15, 17)], 1, function(x) {length(x[x == "Yes"])})

#' Count the 'Yes' across the items that refer to indirect evaluation of the transitivity assumption: 
#' "Planned trans evaluation: Sensitivity analysis", "Planned trans evaluation: Subgroup analysis",
#' "Planned trans evaluation: Meta-regression", "Planned trans evaluation: Statistical or qualitative consistency evaluation".
indir_meth <- apply(dataset[, 19:22], 1, function(x) {length(x[x == "Yes"])})

#' Characterise the review as having considered "Only direct methods", "Only indirect methods", or "Both",
#' as well as having provided "No" (plan of transitivity evaluation) or "No protocol".
both0 <- ifelse(dir_meth > 0 & indir_meth > 0, "Both", 
                ifelse(dir_meth > 0 & indir_meth == 0, "Only direct methods", 
                       ifelse(dir_meth == 0 & indir_meth > 0, "Only indirect methods", "No")))
both <- ifelse(both0 == "No" & q3_0 == "No protocol", "No protocol", 
               ifelse(both0 == "No" & q3_0 == "Yes", "No", both0))

## Planned & reported methods - Review level (Only direct/Only indirect/Both/No) ---- 
#' Rename the levels of the item "If [6] is 'Yes', the authors reported the transitivity evaluation 
#' in the Results or Discussion section".
q7 <- ifelse(dataset[, 27] == "Not applicable", "No", dataset[, 27])

#' Count the 'Yes' across the items that refer to direct evaluations of the transitivity assumption: 
#' "Trans evaluated: Interpretation 1 Salanti", "Trans evaluated: Interpretations 2&3 Salanti", 
#' "Trans evaluated: Interpretation 4 Salanti", and "Trans evaluated: Interpretation 5 Salanti". 
dir_meth1 <- apply(dataset[, c(30, 32, 34, 38)], 1, function(x) {length(x[x == "Yes"])})

#' Count the 'Yes' across the items that refer to indirect evaluation of the transitivity assumption: 
#' "Trans evaluated: Sensitivity analysis", "Trans evaluated: Subgroup analysis",
#' "Trans evaluated: Meta-regression", "Trans evaluated: Statistical or qualitative consistency evaluation".
indir_meth1 <- apply(dataset[, 40:43], 1, function(x) {length(x[x == "Yes"])})

#' Characterise the review as having considered "Only direct methods", "Only indirect methods", or "Both",
#' as well as having provided "No" (methods of transitivity evaluation).
both1 <- ifelse(dir_meth1 > 0 & indir_meth1 > 0, "Both", 
                ifelse(dir_meth1 > 0 & indir_meth1 == 0, "Only direct methods", 
                       ifelse(dir_meth1 == 0 & indir_meth1 > 0, "Only indirect methods", "No")))

## Did they abstain from conducting NMA? ----
q_nma <- ifelse(is.element(dataset[, 68], c("No, they performed NMA", "Not applicable")), "No", "Υes")

## Discussed judgment based on NMA parameter (Only treatment effect/Other NMA parameter/Both/NMA not conducted/No) ----
#' Rename the levels of the item "Parameter discussed: Not mentioned".
q14_2 <- ifelse(dataset[, 63] != "No", "Yes", dataset[, 63])  

#' Rename the levels of the item "Parameter discussed: Treatment effects".
treat_effect <- ifelse(dataset[, 64] == "Yes", "Yes", "No") 

#' Count the 'Yes' across the items that refer to other NMA parameters:
#' "Parameter discussed: Intervention ranking", "Parameter discussed: Statistical heterogeneity",
#' and "Parameter discussed: Evidence (in)consistency"
other_par <- apply(dataset[, 65:67], 1, function(x) {length(x[x == "Yes"])})

#' Characterise the review as having discussed "Only treatment effects", "Other parameters", or "Both",
#' as well as having provided "No" (discussion), or reported that "NMA not conducted".
both_2 <- ifelse(treat_effect == "Yes" & other_par > 0, "Both",
                 ifelse(treat_effect == "Yes" & other_par == 0, "Only treatment effects",
                        ifelse(treat_effect == "No" & other_par > 0, "Other parameter", "No"))) 
both2 <- ifelse(q14_2 == "Yes" & both_2 != "No", "No", both_2)
both2[q_nma != "No"] <- "NMA not conducted"

## Proper ToC structure (Yes/No/No table) ----
#' Count the 'Yes' across the items that refer to a proper structure of the Table of Characteristics:
#' "Trial-level without arm-level", "Trial-level with arm-level", "Comparison-level with arm-level",
#' "Comparison-level without arm-level", and "Comparison-level characteristic summarised across trials".
proper <- apply(dataset[, c(80:84)], 1, function(x) {length(x[x == "Yes"])})

#' Count the 'Yes' across the items that refer to an improper structure of the Table of Characteristics:
#' "Intervention level", and "Descriptive statistics per characteristic" .
improper <- apply(dataset[, 85:86], 1, function(x) {length(x[x == "Yes"])})

#' Characterise the review as having "Proper", "Improper", or "No table",
proper_toc <- ifelse(proper == 1 & improper == 0, "Yes", 
                     ifelse(proper == 0 & improper == 1, "No", "No table"))

## Prepare dataset for the function 'trans_quality' ----
item <- c("Planned in protocol",
          "Described in Methods",
          "Reported in Results",
          "Discussed transitivity",
          "Provided proper Table")
data_function <- data.frame(both, 
                            dataset[, 26],
                            both1,
                            both2,
                            proper_toc)
colnames(data_function) <- item



## Save as .RData to use for R-script 'Create Figure 4' ----
save(data_function, file = "./data/judge_trans_quality.RData")

## Obtain transitivity quality evaluation for each review ----
judge_quality <- rep(NA, dim(dataset)[1])
for (i in 1:dim(dataset)[1]) {
  judge_quality[i] <- trans_quality(plan_protocol = data_function[i, 1], 
                                    plan_methods = data_function[i, 2],
                                    report_results = data_function[i, 3],
                                    discuss_trans = data_function[i, 4],
                                    proper_table = data_function[i, 5])
}


## Number of reviews with 'High', 'Low' or 'Unclear' transitivity evaluation ----
table(judge_quality)

## Stacked barplot on transitivity evaluation quality ----
# Prepare the dataset for ggplot2
dataset[judge_quality == "Low", 3] # dataset[, 3] refers to the 'Year'
year_x <- c(dataset[judge_quality == "Low", 3], 
            dataset[judge_quality == "Unclear", 3], 
            dataset[judge_quality == "High", 3])
quality <- c(rep("Low", length(dataset[judge_quality == "Low", 3])), 
             rep("Unclear", length(dataset[judge_quality == "Unclear", 3])), 
             rep("High", length(dataset[judge_quality == "High", 3])))
data_quality <- data.frame(year_x, quality) 

# Calculate % for each stacked bar
proportion <- data_quality %>%
  group_by(year_x, quality) %>%
  tally() %>%
  group_by(year_x) %>%
  mutate(pct = (n / sum(n))*100)

# Get the precious barplot
ggplot(proportion, 
       aes(factor(year_x), 
           pct, 
           fill = factor(quality, levels = c("Low", "Unclear", "High")))) + 
  geom_bar(stat = "identity", 
           color = "grey40") +
  scale_fill_manual(breaks = c("Low", "Unclear", "High"), 
                    values = c("#D55E00", "orange", "#009E73")) +
  theme_classic() +
  geom_text(aes(label = paste0(round(pct,0),"%"), 
                y = pct), 
            position = position_stack(vjust = 0.5), 
            size = 3.7)+
  labs(y = "Proportion within category", 
       x = "", 
       fill = "Transitivity evaluation quality") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11), 
        legend.position = "bottom",
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"))
