#*******************************************************************************
#*
#*            
#*                              Create Main Figures                                                                                                                   
#*                                                                 
#* Authors: Loukia M. Spineli & Katerina Papadimitropoulou
#* Date: July 2023
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("reshape2", "ggplot2", "plyr", "dplyr", "ggpubr", "stringr", "ggh4x")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load data ----
load("./data/Analysis dataset.RData")


## General information ----
dataset$Year <- factor(ifelse(dataset$Year <= 2015, "Before PRISMA-NMA", "After PRISMA-NMA"),
                       levels = c("Before PRISMA-NMA", "After PRISMA-NMA"))


## Total reviews per PRISMA-NMA status
before <- length(dataset$Year[dataset$Year == "Before PRISMA-NMA"])
after <- length(dataset$Year[dataset$Year == "After PRISMA-NMA"])


## Protocol level ----
#* Among the reviews that planned to evaluate transitivity in the Protocol,
#* what methods did the authors planned to employ
# Restrict to reviews that did plan at least one method
q3 <- ifelse(subset(dataset[, c(11, 13, 19, 18, 21:27)], dataset[, 10] == "No") == "Yes", 1, 0)
colnames(q3) <- c("A",  # Intervention similarity
                  "B",  # Missing-at-random interventions
                  "C",  # Jointly randomisable participants
                  "D",  # Comparison similarity
                  "E1", # Sensitivity analysis
                  "E2", # Sensitivity analysis trans
                  "F1", # Subgroup analysis
                  "F2", # Subgroup analysis trans
                  "G1", # Meta-regression analysis
                  "G2", # Meta-regression analysis trans
                  "H")  # Consistency evaluation
year3 <- subset(dataset$Year, dataset[, 10] == "No")

# Number of reviews that made a protocol available per timeframe
before_prot <- length(dataset$Year[dataset$Year == "Before PRISMA-NMA" & dataset[, 6] == "Available"])
after_prot <- length(dataset$Year[dataset$Year == "After PRISMA-NMA" & dataset[, 6] == "Available"])

# Out of those with protocol, how many reported at least one method per timeframe
vv1 <- table(year3); vv2 <- round(table(year3)/c(before_prot, after_prot) * 100, 0)  

# Tabulate: percentage of 'Yes' per method and timeframe
table_protocol <- data.frame(year3, q3) %>% 
  group_by(year3) %>% 
  summarise(across(A:H, list(n = ~sum(.x == 1)))) %>%
  rowwise() %>%
  mutate(across(A_n:H_n, list(prop = ~.x / 
                                sum(c(A_n, B_n, C_n, D_n, E1_n, E2_n, F1_n, F2_n, G1_n, G2_n, H_n)))))

# Prepare the dataset for ggplot2 
data_protocol_prop <- melt(table_protocol[, -c(2:12)])
data_protocol_count <- melt(table_protocol[, c(2:12)])
data_protocol <- cbind(data_protocol_prop, data_protocol_count[, -1])
data_protocol$indirect <- rep(c("Direct methods", "Indirect methods"), c(2*4, 2*7))
data_protocol$ind <- c(rep(1, 8), rep(rep(0:1, each = 2), 3), 1, 1)
colnames(data_protocol) <- c("timeframe", "method", "prop", "count", "indirect", "trans_only")
data_protocol$prop <- round(data_protocol$prop * 100, 1)
data_protocol$timeframe <- revalue(data_protocol$timeframe, 
                                   c("Before PRISMA-NMA" = paste0("Before PRISMA-NMA:", " ", vv1[1], " ", "(", vv2[1], "%) out of", " ", before_prot),
                                     "After PRISMA-NMA" = paste0("After PRISMA-NMA:", " ", vv1[2], " ", "(", vv2[2], "%) out of", " ", after_prot)))
data_protocol$method <- revalue(data_protocol$method, 
                                c("A_n_prop" = "A", 
                                  "B_n_prop" = "B", 
                                  "C_n_prop" = "C",
                                  "D_n_prop" = "D",
                                  "E1_n_prop" = "E",
                                  "E2_n_prop" = "E", 
                                  "F1_n_prop" = "F",
                                  "F2_n_prop" = "F",
                                  "G1_n_prop" = "G",
                                  "G2_n_prop" = "G",
                                  "H_n_prop" = "H"))

# Get Figure 1
ggplot(data_protocol,
       aes(x = method,
           y = prop,
           fill = as.factor(trans_only))) +
  geom_bar(stat = "identity", 
           position = "stack",
           linewidth = 0.6) +
  geom_text(aes(label = paste0(prop, "%", " ", "(", count, ")")),
            hjust = 0.5, 
            vjust = -0.2, 
            size = 5,
            colour = "black",
            position = "stack") +
  scale_fill_manual(breaks = c("0", "1"),
                    values = c("#66CCFF", "#0099FF"), 
                    labels = c("For heterogeneity assessment", "For transitivity assessment")) +
  facet_nested( ~ timeframe + indirect, scales = "free_x") + 
  labs(x = "",
       y = "Percentage (%)",
       fill = "") +
  ggtitle("Protocol level") +
  ylim(0, 100) +
  theme_classic() +
  guides(colour = "none") + 
  theme(title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))



## Review level (Evaluation methods) ----
#* Among the reviews that evaluated transitivity, what methods did the authors 
#* employed to evaluate the transitivity assumption in the review
# Restrict to reviews that did employ at least one method
q3_review <- ifelse(subset(dataset[, c(35, 37, 43, 42, 45:51)], dataset[, 34] == "No") == "Yes", 1, 0)
year3_review <- subset(dataset$Year, dataset[, 34] == "No"); table(year3_review)/c(361, 360)
colnames(q3_review) <- colnames(q3)

# Out of total reviews, how many employed at least one method per timeframe
ww1 <- table(year3_review); ww2 <- round(table(year3_review)/c(before, after) * 100, 0)

# Percentage of 'Yes' per method and timeframe
table_review <- data.frame(year3_review, q3_review) %>% 
  group_by(year3_review) %>% 
  summarise(across(A:H, list(n = ~sum(.x == 1)))) %>%
  rowwise() %>%
  mutate(across(A_n:H_n, list(prop = ~.x / 
                                sum(c(A_n, B_n, C_n, D_n, E1_n, E2_n, F1_n, F2_n, G1_n, G2_n, H_n)))))

# Prepare the dataset for ggplot2 
data_review_prop <- melt(table_review[, -c(2:12)])
data_review_count <- melt(table_review[, c(2:12)])
data_review <- cbind(data_review_prop, data_review_count[, -1])
data_review$indirect <- rep(c("Direct methods", "Indirect methods"), c(2*4, 2*7))
data_review$ind <- c(rep(1, 8), rep(rep(0:1, each = 2), 3), 1, 1)
colnames(data_review) <- colnames(data_protocol)
data_review$prop <- round(data_review$prop * 100, 1)
data_review$timeframe <- revalue(data_review$timeframe, 
                                 c("Before PRISMA-NMA" = paste0("Before PRISMA-NMA:", " ", ww1[1], " ", "(", ww2[1], "%) out of", " ", before),
                                   "After PRISMA-NMA" = paste0("After PRISMA-NMA:", " ", ww1[2], " ", "(", ww2[2], "%) out of", " ", after)))
data_review$method <- revalue(data_review$method, 
                              c("A_n_prop" = "A", 
                                "B_n_prop" = "B", 
                                "C_n_prop" = "C",
                                "D_n_prop" = "D",
                                "E1_n_prop" = "E",
                                "E2_n_prop" = "E", 
                                "F1_n_prop" = "F",
                                "F2_n_prop" = "F",
                                "G1_n_prop" = "G",
                                "G2_n_prop" = "G",
                                "H_n_prop" = "H"))

# Get Figure 2
ggplot(data_review,
       aes(x = method,
           y = prop,
           fill = as.factor(trans_only))) +
  geom_bar(stat = "identity", 
           position = "stack",
           linewidth = 0.6) +
  geom_text(aes(label = paste0(prop, "%", " ", "(", count, ")")),
            hjust = 0.5, 
            vjust = -0.2, 
            size = 5,
            colour = "black",
            position = "stack") +
  scale_fill_manual(breaks = c("0", "1"),
                    values = c("#66CCFF", "#0099FF"), 
                    labels = c("For heterogeneity assessment", "For transitivity assessment")) +
  facet_nested( ~ timeframe + indirect, scales = "free_x") + 
  ggtitle("Systematic review level") +
  labs(x = "",
       y = "",
       fill = "") +
  ylim(0, 100) +
  theme_classic() +
  guides(colour = "none") +
  theme(title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))


## Review level (Acknowledging) ----
# The authors conclude or imply the (im)plausibility of transitivity (as binary)
q9_bin <- factor(ifelse(is.element(dataset[, 53], c("Not applicable", "Nothing stated")), "No", "Yes"), 
             levels = c("Yes", "No"))

#' First, keep the review that explicitly stated in the methods that they planned to 
#' *evaluate transitivity* and reported the evaluation results 
q_cond <- ifelse(dataset[, c(35, 37, 43, 42, 46, 48, 50, 51)] == "Yes", 1, 0)

#' Then, separate between reviews that employed at least one method for transitivity ("Trans+")
#' from those that used only indirect method(s) for heterogeneity sources or made no evaluation ("Other").
q_cond_new <- ifelse(rowSums(q_cond) > 0, "Trans+", "Other") 
q_cond_fin <- ifelse(dataset[, 34] == "Yes", "Yes", q_cond_new)

# Consider the actual levels after restricting to q9_bin != "No" and q_cond_fin == "Trans+"
q9 <- factor(subset(dataset[, 53], q9_bin != "No" & q_cond_fin == "Trans+"),  
             levels = c("Transitivity may be plausible", 
                        "Transitivity may be questionable",
                        "Difficult to judge due to limited data"))


#* Among the reviews with a conclusion about transitivity, which method did the 
#* authors use to justify their conclusion
q3_acknow <- ifelse(subset(dataset[, c(56:58, 60, 59, 61:64)], 
                           q9_bin != "No" & q_cond_fin == "Trans+") == "Yes", 1, 0) 
colnames(q3_acknow) <- c("A", # Limited data
                         "B", # Intervention similarity
                         "C", # Missing-at-random interventions
                         "D", # Jointly randomisable participants
                         "E", # Comparison similarity
                         "F", # Sensitivity analysis
                         "G", # Subgroup analysis
                         "H", # Meta-regression analysis
                         "I") # Consistency evaluation
year10 <- subset(dataset$Year, q9_bin == "Yes" & q_cond_fin == "Trans+")  
aa1 <- table(year10); aa2 <- round(table(year10)/c(before, after) * 100, 0)

# Percentage of 'Yes' per method, timeframe and conclusion
table_acknow <- data.frame(year10, q9, q3_acknow) %>% 
  group_by(year10, q9) %>% 
  summarise(across(A:I, list(n = ~sum(.x == 1)))) %>%
  rowwise() %>%
  mutate(across(A_n:I_n, list(prop = ~.x / sum(c(A_n, B_n, C_n, D_n, E_n, F_n, G_n, H_n, I_n)))))

# Percentage of 'Yes' per method and timeframe (total)
table_acknow_total <- data.frame(year10, q3_acknow) %>% 
  group_by(year10) %>% 
  summarise(across(A:I, list(n = ~sum(.x == 1)))) %>%
  rowwise() %>%
  mutate(across(A_n:I_n, list(prop = ~.x / sum(c(A_n, B_n, C_n, D_n, E_n, F_n, G_n, H_n, I_n)))))

# Prepare the dataset for ggplot2 
# Considering the conclusions
data_acknow <- melt(table_acknow[, -c(3:11)])
# Without the conclusions
data_acknow_total0 <- melt(table_acknow_total[, -c(2:10)])
data_acknow_total <- data.frame(year10 = data_acknow_total0[, 1], 
                                q9 = rep("Total", dim(data_acknow_total0)[1]),
                                variable = data_acknow_total0[, 2],
                                value = data_acknow_total0[, 3])

# Bring together
data_acknow_both <- rbind(data_acknow, data_acknow_total)
data_acknow_both$ind <- c(rep(c("Limited data", "(In)direct methods"), c(1*2*3, 8*2*3)), 
                          rep(c("Limited data", "(In)direct methods"), c(2, 8*2)))
data_acknow_both$indirect <- c(rep(c("Direct methods", "Indirect methods"), c(5*2*3, 4*2*3)), 
                               rep(c("Direct methods", "Indirect methods"), c(5*2, 4*2)))
colnames(data_acknow_both) <- c("timeframe", "conclusion", "method", "prop", "limited", "indirect")
data_acknow_both$prop <- round(data_acknow_both$prop * 100, 1)
data_acknow_both$timeframe <- revalue(data_acknow_both$timeframe, 
                                      c("Before PRISMA-NMA" = paste0("Before PRISMA-NMA:", " ", aa1[1], " ", "(", aa2[1], "%) out of", " ", before),
                                        "After PRISMA-NMA" = paste0("After PRISMA-NMA:", " ", aa1[2], " ", "(", aa2[2], "%) out of", " ", after)))
data_acknow_both$conclusion <- factor(revalue(data_acknow_both$conclusion, 
                                              c("Transitivity may be plausible" = "Plausible",
                                                "Transitivity may be questionable" = "Questionable",
                                                "Difficult to judge due to limited data" = "Difficult to judge")),
                                      levels = c("Total", "Difficult to judge", "Questionable", "Plausible"))
data_acknow_both$method <- revalue(data_acknow_both$method, 
                                   c("A_n_prop" = "A", 
                                     "B_n_prop" = "B", 
                                     "C_n_prop" = "C",
                                     "D_n_prop" = "D",
                                     "E_n_prop" = "E",
                                     "F_n_prop" = "F",
                                     "G_n_prop" = "G",
                                     "H_n_prop" = "H",
                                     "I_n_prop" = "I"))
data_acknow_both$radius <- sqrt(data_acknow_both$prop/pi)

# Get Figure 3
ggplot(data_acknow_both, 
       aes(x = method, 
           y = conclusion,
           fill = limited)) +
  geom_point(aes(size = radius*7.4), #7.8, 10
             shape = 21, 
             stroke = 1.2,
             alpha = 0.7) +
  geom_text(aes(label = prop), 
            size = 4.5, 
            color="black",
            fontface = "bold") +
  scale_fill_manual(name = "Conclusion was based on",
                    breaks = c("Limited data", "(In)direct methods"),
                    values = c("#CCCCCC", "#0099FF")) +
  facet_nested( ~ timeframe + indirect, scales = "free_x") + 
  labs(x = "",
       y ="") + 
  scale_size_identity() +
  theme_classic() +
  ggtitle("Systematic review level") +
  guides(colour = "none", 
         fill = guide_legend(override.aes = list(size = 6))) + 
  theme(title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))


## Reporting the Table of Characteristics (Characteristic types) ----
# A Table of Characteristics (ToC) is provided in the publication
q12 <- factor(dataset[, 78], levels = c("Yes", "No"))

# Type of characteristics (Quantitative, Qualitative, Textual)
q19 <- subset(dataset[, 81:83], q12 == "Yes" & dataset[, 80] != "No access")
year19 <- subset(dataset$Year, q12 == "Yes" & dataset[, 80] != "No access"); table(year19) / c(359, 360)

# Dataframe with columns 'Year', 'Characteristic type', and 'Frequency'
level19 <- c("Quantitative", "Qualitative", "Combination")
q19_new <- as.numeric(unlist(q19))
q19_names <- rep(level19, each = length(q19[, 1]))
q19_names_new <- factor(q19_names, levels = level19)
year19_new <- as.factor(rep(year19, length(level19)))

# Conditioning on publication timeframe
data19 <- data.frame(value = q19_new, 
                     levels = q19_names_new, 
                     year = year19_new)

# Summary descriptive statistics (total and by timeframe)
summary(data19$value) # median: 2, IQR: 0 to 5, range: 0 to 35
data19 %>%
  group_by(year) %>%
  summarise(median = median(value),
            q25 = quantile(value, 0.25),
            q75 = quantile(value, 0.75),
            minimum = min(value),
            maximum = max(value))

# Without conditioning on publication timeframe (Total)
data19_new <- data.frame(value = q19_new, 
                         levels = q19_names_new, 
                         year = rep("Total", dim(data19)[1]))

# Prepare dataset for ggplot2 (bring together)
data19_fin <- rbind(data19_new, data19)
data19_fin$year <- factor(data19_fin$year, 
                          levels = c("Total", "Before PRISMA-NMA", "After PRISMA-NMA"))

# Create boxplot with jitter points
fig19 <- ggplot(data19_fin, 
                aes(x = year, 
                    y = value, 
                    color = levels)) +
  geom_boxplot(size = 1, 
               outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.height = 0.5),
             size = 1.3,
             alpha = .4) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(y = "Number of characteristics", 
       x = "") +
  ylim(-0.5, 40) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 15), 
        legend.title = element_blank())


## Reporting the Table of Characteristics (Characteristic regarding PICO) ----
# PICO-related characteristics (Participants, Intervention, Outcome, Design)
q20 <- subset(dataset[, 84:87], q12 == "Yes" & dataset[, 80] != "No access")

# Dataframe with columns 'Year', 'Characteristic type', and 'Frequency'
level20 <- c("Participants", "Interventions", "Outcomes", "Design")
q20_new <- as.numeric(unlist(q20))
q20_names <- rep(level20, each = length(q20[, 1]))
q20_names_new <- factor(q20_names, levels = level20)
year20_new <- as.factor(rep(year19, length(level20)))

# Conditioning on publication timeframe
data20 <- data.frame(value = q20_new, 
                     levels = q20_names_new, 
                     year = year20_new)

# Without conditioning on publication timeframe (Total)
data20_new <- data.frame(value = q20_new, 
                         levels = q20_names_new, 
                         year = rep("Total", dim(data20)[1]))

# Prepare dataset for ggplot2 (bring together)
data20_fin <- rbind(data20_new, data20)
data20_fin$year <- factor(data20_fin$year, 
                          levels = c("Total", "Before PRISMA-NMA", "After PRISMA-NMA"))

# Create boxplot with jitter points
fig20 <- ggplot(data20_fin, 
                aes(x = year, 
                    y = value, 
                    color = levels)) +
  geom_boxplot(size = 1, 
               outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.height = 0.5),
             size = 1.3,
             alpha = .4) + 
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  labs(y = "", 
       x = "") +
  ylim(-0.5, 40) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 15), 
        legend.title = element_blank())

# Get Figure 4
ggarrange(fig19, fig20, labels = c("(a)", "(b)"))


## Finding reporting gaps (Results have been gathered from Table 2 and Main Figures)
# At protocol level
data_gap_prot <- 
  data.frame(item = rep(c("Protocol available", "Transitivity defined", "Direct methods", "Indirect methods"), each = 2),
             timeframe = rep(c("Before PRISMA-NMA", "After PRISMA-NMA"), 4),
             value = c(16, 43, 16, 14, 10, 10, 16, 33)) # Numbers in %

# Sort the categories to the desired order
data_gap_prot$item <- factor(data_gap_prot$item,
                             levels = c("Protocol available", "Transitivity defined", "Direct methods", "Indirect methods"))
data_gap_prot$timeframe <- factor(data_gap_prot$timeframe,
                                  levels = c("Before PRISMA-NMA", "After PRISMA-NMA"))

# At review level
data_gap_rev <- 
  data.frame(item = rep(c("Transitivity defined", "Planned & reported", "Direct methods", "Indirect methods", "Transitivity conclusion", "Discussed parameter"), each = 2),
             timeframe = rep(c("Before PRISMA-NMA", "After PRISMA-NMA"), 6),
             value = c(36, 24, 53, 70, 12, 11, 40, 54, 35, 38, 82, 69)) # Numbers in %

# Sort the categories to the desired order
data_gap_rev$item <- factor(data_gap_rev$item,
                            levels = c("Transitivity defined", "Planned & reported", "Direct methods", "Indirect methods", "Transitivity conclusion", "Discussed parameter"))
data_gap_rev$timeframe <- factor(data_gap_rev$timeframe,
                                 levels = c("Before PRISMA-NMA", "After PRISMA-NMA"))

# Bring together
data_gap <- rbind(data_gap_prot, data_gap_rev)
data_gap$location <- rep(c("Protocol level", "Systematic review level"), 
                         c(dim(data_gap_prot)[1], dim(data_gap_rev)[1]))
data_gap$level <- ifelse(data_gap$value < 25, "very low", 
                         ifelse(data_gap$value >= 25 & data_gap$value < 50, "low",
                                ifelse(data_gap$value >= 50 & data_gap$value < 75, "moderate", "high")))
data_gap$order <- factor(rep(1:10, each = 2))

# Get Figure 5
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
  scale_fill_manual(name = "Timeframe",
                    breaks = c("Before PRISMA-NMA", "After PRISMA-NMA"),
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
  facet_grid(~ location, scales = "free_x") +
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
