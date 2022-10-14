#*******************************************************************************
#*
#*
#*               Quality of the reported transitivity evaluation                             
#*
#*
#*******************************************************************************



## Load libraries ----
list.of.packages <- c("readxl", "ggplot2", "dplyr", "writexl")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load function ----
source("./31_Analysis - Descriptives/Quality transitivity_function.R")



## Load data ----
path <- "./30_Analysis - Extraction/Loukia Extraction Package/Extracted reviews_Transitivity_Loukia_Version4.xlsm"
dataset0 <- as.data.frame(read_excel(path, 
                                     sheet = "Original", 
                                     na = "NA"))[2:358, ]
dataset <- subset(dataset0, PMID != 24756870) # Remove SR from Pollock (not an NMA!)



## Obtain frequencies ----
# Planned in the protocol - Protocol level (Yes/No/No protocol) - OK!
q3_0 <- ifelse(dataset[, 11] == "Not applicable", "No protocol", dataset[, 11])
dir_meth <- apply(dataset[, c(12, 14, 16, 18)], 1, function(x) {length(x[x == "Yes"])})
indir_meth <- apply(dataset[, 20:23], 1, function(x) {length(x[x == "Yes"])})
both0 <- ifelse(dir_meth > 0 & indir_meth > 0, "Both", 
                ifelse(dir_meth > 0 & indir_meth == 0, "Only direct methods", 
                       ifelse(dir_meth == 0 & indir_meth > 0, "Only indirect methods", "No")))
both <- ifelse(both0 == "No" & q3_0 == "No protocol", "No protocol", 
               ifelse(both0 == "No" & q3_0 == "Yes", "No", both0))

# Planned & reported methods (Only direct/Only indirect/Both/No) - OK!
q7 <- ifelse(dataset[, 28] == "Not applicable", "No", dataset[, 28])
dir_meth1 <- apply(dataset[, c(31, 33, 35, 39)], 1, function(x) {length(x[x == "Yes"])})
indir_meth1 <- apply(dataset[, 41:44], 1, function(x) {length(x[x == "Yes"])})
both_1 <- ifelse(dir_meth1 > 0 & indir_meth1 > 0, "Both", 
                 ifelse(dir_meth1 > 0 & indir_meth1 == 0, "Only direct methods", 
                        ifelse(dir_meth1 == 0 & indir_meth1 > 0, "Only indirect methods", "No")))
both1 <- both_1

# Compared with Petropoulou - Planned & reported methods (Only direct/Only indirect/Both/No) - OK!
#dataset0  <- subset(dataset, Year < 2016)
#dir_meth01 <- apply(dataset0[, c(31, 33, 35, 39)], 1, function(x) {length(x[x == "Yes"])})
#table(dir_meth01)

# Did they abstained from conducting NMA?
q_nma <- ifelse(is.element(dataset[, 69], c("No, they performed NMA", "Not applicable")), "No", "Υes")

# Discussed judgment based on NMA parameter (Only treatment effect/Other NMA parameter/Both/No)
q14_2 <- ifelse(dataset[, 64] != "No", "Yes", dataset[, 64])  # BL: Not mentioned
treat_effect <- ifelse(dataset[, 65] == "Yes", "Yes", "No") 
other_par <- apply(dataset[, 66:68], 1, function(x) {length(x[x == "Yes"])})
both_2 <- ifelse(treat_effect == "Yes" & other_par > 0, "Both",
                 ifelse(treat_effect == "Yes" & other_par == 0, "Only treatment effects",
                        ifelse(treat_effect == "No" & other_par > 0, "Other parameter", "No"))) 
both2 <- ifelse(q14_2 == "Yes" & both_2 != "No", "No", both_2)
both2[q_nma != "No"] <- "NMA not conducted"


# Proper ToC structure (Yes/No/No table)
proper <- apply(dataset[, c(85:87, 90, 93)], 1, function(x) {length(x[x == "Yes"])})
improper <- apply(dataset[, 94:95], 1, function(x) {length(x[x == "Yes"])})
proper_toc <- ifelse(proper == 1 & improper == 0, "Yes", 
                     ifelse(proper == 0 & improper == 1, "No", "No table"))



## Prepare dataset ----
item <- c("Planned in protocol",
          "Described in Methods",
          "Reported in Results",
          "Discussed transitivity",
          "Provided proper Table")
data_function <- data.frame(both, 
                            dataset[, 27],
                            both1,
                            both2,
                            proper_toc)
colnames(data_function) <- item



## Obtain transitivity quality evaluation ----
judge_quality <- rep(NA, 356)
for (i in 1:356) {
  judge_quality[i] <- trans_quality(plan_protocol = data_function[i, 1], 
                                    plan_methods = data_function[i, 2],
                                    report_results = data_function[i, 3],
                                    discuss_trans = data_function[i, 4],
                                    proper_table = data_function[i, 5])
}
table(judge_quality)



## Save as excel all SRs with their quality level ----
dataset_to_save <- data.frame(dataset[, 2:6], data_function, judge_quality)
colnames(dataset_to_save)[11] <- "Transitivity.evaluation.quality"
write_xlsx(dataset_to_save, path = "./40_Manuscript preparation/Quality transitivity.xlsx") 



## Characteristics of SRs with unclear quality and protocol plan ----
unclear_with_protocol <- subset(data_function, 
                                judge_quality == "Unclear" & !is.element(data_function[, 1], c("No", "No protocol")))
table_res <- list()
for (i in 1:dim(data_function)[2]) {
  table_res[[i]] <- table(unclear_with_protocol[, i])
}
names(table_res) <- colnames(unclear_with_protocol)



## Barplot on transitivity evaluation quality ----
dataset[judge_quality == "Low", 4]
year_x <- c(dataset[judge_quality == "Low", 4], 
            dataset[judge_quality == "Unclear", 4], 
            dataset[judge_quality == "High", 4])
quality <- c(rep("Low", length(dataset[judge_quality == "Low", 4])), 
             rep("Unclear", length(dataset[judge_quality == "Unclear", 4])), 
             rep("High", length(dataset[judge_quality == "High", 4])))
data_quality <- data.frame(year_x, quality) 

proportion <- data_quality %>%
  group_by(year_x, quality) %>%
  tally() %>%
  group_by(year_x) %>%
  mutate(pct = (n / sum(n))*100)


tiff("./31_Analysis - Descriptives/Figure 3.tiff", height = 20, width = 37, units = "cm", compression = "lzw", res = 300)
ggplot(proportion, 
       aes(factor(year_x), 
           pct, 
           fill = factor(quality, levels = c("Low", "Unclear", "High")))) + 
  geom_bar(stat = "identity", color = "grey40") +
  scale_fill_manual(breaks = c("Low", "Unclear", "High"), values = c("#D55E00", "orange", "#009E73")) +
  theme_classic() +
  geom_text(aes(label = paste0(round(pct,0),"%"), 
                y = pct), 
            position = position_stack(vjust = 0.5), 
            size = 3.7)+
  labs(y = "Proportion within category", x = "", fill = "Transitivity evaluation quality") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11), 
        legend.position = "bottom",
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"))
dev.off()