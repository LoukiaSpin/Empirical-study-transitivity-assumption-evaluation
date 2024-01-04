#*******************************************************************************
#*
#*            
#*                         Create Table 3 (Main article)                                                                                                                                                                                                                                       
#*                                                                 
#* Author: Loukia M. Spineli 
#* Date: January 2024
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("dplyr", "reshape2", "ggplot2", "stringr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load data ----
load("./data/Analysis dataset.RData")


## Restrict to 2016 and 2021
dataset_new <- subset(dataset, is.element(Year, c("2016", "2021")))


## Define 'Year' as binary
year <- factor(ifelse(dataset_new[, "Year"] == 2016, "During 2016", "During 2021"),
               levels = c("During 2016", "During 2021"))


## Protocol level ----
#' % SRs with available protocol (item 1, Table 2)
q1 <- factor(dataset_new[, 6], levels = c("Available", "Not available"))
q1_prop <- round(prop.table(table(q1, year), 2)*100, 1)  
with_prot_b <- q1_prop[1, 1]  # During 2016
with_prot_a <- q1_prop[1, 2]  # During 2021

#' % SRs defined transitivity  (item 2, Table 2)
q2 <- factor(subset(dataset_new[, 7], q1 == "Available"), levels = c("Yes", "No"))
year2 <- subset(year, q1 == "Available")
q2_prop <- round(prop.table(table(q2, year2), 2)*100, 1) 
def_trans_prot_b <- q2_prop[1, 1]  # During 2016
def_trans_prot_a <- q2_prop[1, 2]  # During 2021

#' Restrict to SRs that provided a protocol & consider only methods for transitivity *exclusively*
q3_restrict_trans_prot <- ifelse(subset(dataset_new[, c(11, 13, 19, 18, 22, 24, 26, 27)], dataset_new[, 6] == "Available") == "Yes", 1, 0)
year3 <- subset(year, dataset_new[, 6] == "Available")
q3_before_prot <- subset(q3_restrict_trans_prot, year3 == "During 2016")
q3_after_prot <- subset(q3_restrict_trans_prot, year3 == "During 2021")

#' % SRs planned 'direct methods' (out of those with protocol)
plan_dir_prot_b <- round((sum(ifelse(rowSums(q3_before_prot[, 1:4]) > 0, 1, 0)) / dim(q3_before_prot)[1]) * 100, 1)  # During 2016
plan_dir_prot_a <- round((sum(ifelse(rowSums(q3_after_prot[, 1:4]) > 0, 1, 0)) / dim(q3_after_prot)[1]) * 100, 1)    # During 2021

#' % SRs planned 'indirect methods' (out of those with protocol)
plan_ind_prot_b <- round((sum(ifelse(rowSums(q3_before_prot[, 5:8]) > 0, 1, 0)) / dim(q3_before_prot)[1]) * 100, 1)  # During 2016
plan_ind_prot_a <- round((sum(ifelse(rowSums(q3_after_prot[, 5:8]) > 0, 1, 0)) / dim(q3_after_prot)[1]) * 100, 1)    # During 2021

#' Bring protocol results together
data_gap_prot <- 
  data.frame(item = rep(c("Protocol available", "Transitivity defined", "Direct methods", "Indirect methods"), each = 2),
             timeframe = rep(c("During 2016", "During 2021"), 4),
             value = c(with_prot_b, with_prot_a, 
                       def_trans_prot_b, def_trans_prot_a, 
                       plan_dir_prot_b, plan_dir_prot_a, 
                       plan_ind_prot_b, plan_ind_prot_a)) # Numbers in %

# Sort the categories to the desired order
data_gap_prot$item <- factor(data_gap_prot$item,
                             levels = c("Protocol available", "Transitivity defined", "Planned & reported", "Direct methods", "Indirect methods"))
data_gap_prot$timeframe <- factor(data_gap_prot$timeframe,
                                  levels = c("During 2016", "During 2021"))


## Review level ----
#' % SRs defined transitivity (item 4, Table 2)
q4 <- factor(dataset_new[, 28], levels = c("Yes", "No"))
q4_prop <- round(prop.table(table(q4, year), 2)*100, 1) 
def_trans_rev_b <- q4_prop[1, 1]  # During 2016
def_trans_rev_a <- q4_prop[1, 2]  # During 2021

#' Restrict to SRs that planned at least one methods for transitivity *exclusively* 
q3_restrict_trans_rev <- ifelse(subset(dataset_new[, c(35, 37, 43, 42, 46, 48, 50, 51)]) == "Yes", 1, 0)
q3_before_rev <- subset(q3_restrict_trans_rev, year == "During 2016")
q3_after_rev <- subset(q3_restrict_trans_rev, year == "During 2021")

#' % SRs planned 'direct methods' 
plan_dir_rev_b <- round((sum(ifelse(rowSums(q3_before_rev[, 1:4]) > 0, 1, 0)) / dim(q3_before_rev)[1]) * 100, 1)  # During 2016
plan_dir_rev_a <- round((sum(ifelse(rowSums(q3_after_rev[, 1:4]) > 0, 1, 0)) / dim(q3_after_rev)[1]) * 100, 1)    # During 2021

#' % SRs planned 'indirect methods' 
plan_ind_rev_b <- round((sum(ifelse(rowSums(q3_before_rev[, 5:8]) > 0, 1, 0)) / dim(q3_before_rev)[1]) * 100, 1)  # During 2016
plan_ind_rev_a <- round((sum(ifelse(rowSums(q3_after_rev[, 5:8]) > 0, 1, 0)) / dim(q3_after_rev)[1]) * 100, 1)    # During 2021

#' % SRs planned and reported transitivity evaluation (item 5, Table 2)
q_cond <- ifelse(dataset_new[, c(35, 37, 43, 42, 46, 48, 50, 51)] == "Yes", 1, 0)

#' Reviews that used at least one method for transitivity ('Yes')
#' Note that some of the methods that used consistency may have also used the remaining indirect methods
#' but for statistical heterogeneity source.
q_cond_new <- ifelse(rowSums(q_cond) > 0, "Trans+", "Other") 
q_cond_fin <- ifelse(dataset_new[, 34] == "Yes", "Yes", q_cond_new)

# Correcting dataset[, 32] for indirect methods that were explicitly used for trans!
q5 <- factor(ifelse(dataset_new[, 32] == "Yes" & q_cond_fin == "Trans+", "Yes", "No"),
             levels = c("Yes", "No"))
q5_prop <- round(prop.table(table(q5, year), 2)*100, 1)  
report_b <- q5_prop[1, 1]  # During 2016
report_a <- q5_prop[1, 2]  # During 2021

#' % SRs drew conclusions about transitivity (item 7, Table 2)
q7 <- factor(ifelse(is.element(dataset_new[, 53], c("Not applicable", "Nothing stated")) | 
                      q_cond_fin != "Trans+", "No", "Yes"), levels = c("Yes", "No"))
q7_prop <- round(prop.table(table(q7, year), 2)*100, 1) 
conclude_b <- q7_prop[1, 1]  # During 2016
conclude_a <- q7_prop[1, 2]  # During 2021

#' % SRs discussed NMA parameter (item 9, Table 2)
q9 <- ifelse(subset(dataset_new[, 72:75], q7 == "Yes") == "Yes", 1, 0)
year9 <- subset(year, q7 == "Yes")

# Reviews that used at least one NMA parameter
q9_new <- factor(ifelse(rowSums(q9) > 0, "Yes", "No"), levels = c("Yes", "No"))
q9_prop <- round(prop.table(table(q9_new, year9), 2)*100, 1) 
discuss_b <- q9_prop[1, 1]  # During 2016
discuss_a <- q9_prop[1, 2]  # During 2021

data_gap_rev <- 
  data.frame(item = rep(c("Transitivity defined", "Planned & reported", "Direct methods", "Indirect methods", "Transitivity conclusion", "Discussed parameter"), each = 2),
             timeframe = rep(c("During 2016", "During 2021"), 6),
             value = c(def_trans_rev_b, def_trans_rev_a, 
                       report_b, report_a, 
                       plan_dir_rev_b, plan_dir_rev_a, 
                       plan_ind_rev_b, plan_ind_rev_a, 
                       conclude_b, conclude_a, 
                       discuss_b, discuss_a)) # Numbers in %

# Sort the categories to the desired order
data_gap_rev$item <- factor(data_gap_rev$item,
                            levels = c("Transitivity defined", "Planned & reported", "Direct methods", "Indirect methods", "Transitivity conclusion", "Discussed parameter"))
data_gap_rev$timeframe <- factor(data_gap_rev$timeframe,
                                 levels = c("During 2016", "During 2021"))

# Bring together
data_gap <- rbind(data_gap_prot, data_gap_rev)
data_gap$location <- rep(c("Protocol level", "Systematic review level"), 
                         c(dim(data_gap_prot)[1], dim(data_gap_rev)[1]))
data_gap$level <- ifelse(data_gap$value < 25, "very low", 
                         ifelse(data_gap$value >= 25 & data_gap$value < 50, "low",
                                ifelse(data_gap$value >= 50 & data_gap$value < 75, "moderate", "high")))
data_gap$order <- factor(rep(1:10, each = 2))

# Get Figure 6
ggplot(data_gap, 
       aes(x = item, 
           y = value,
           group = timeframe)) +
  geom_linerange(aes(ymin = 0, 
                     ymax = value), 
                 position = position_dodge(width = 0.6),
                 linewidth = 1.5) +
  geom_point(aes(colour = level,
                 fill = timeframe),
             position = position_dodge(width = 0.6),
             size = 8, 
             shape = 21, 
             stroke = 4) + 
  geom_text(aes(label = paste0(value, "%")), 
            position = position_dodge(0.6),
            vjust = -1.7,
            size = 4.5, 
            color = "black",
            fontface = "bold") +
  facet_grid(~ location, scales = "free_x") +
  scale_fill_manual(name = "Timeframe",
                    breaks = c("During 2016", "During 2021"),
                    values = c("white", "#0099FF")) +
  scale_colour_manual(name = "Reporting frequency",
                      breaks = c("very low", "low", "moderate", "high"),
                      values = c("red", "orange", "green4", "blue"),
                      limits = c("very low", "low", "moderate", "high"),
                      drop = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) + 
  labs(x = "",
       y = "Percentage (%)",
       fill = "") +
  ylim(0, 100) +
  theme_classic() +
  guides(fill = guide_legend(override.aes = list(size = 4, stroke = 1.8), order = 1),
         colour = guide_legend(override.aes = list(size = 4, stroke = 1.8))) + 
  theme(title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))
