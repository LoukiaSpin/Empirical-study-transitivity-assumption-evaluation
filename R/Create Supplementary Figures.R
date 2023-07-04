#*******************************************************************************
#*
#*            
#*                         Create Supplementary Figures                                                                                                                                                                                                                      
#*                                                                 
#* Author: Loukia M. Spineli 
#* Date: July 2023
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("reshape2", "ggplot2", "plyr", "dplyr", "ggpubr", "stringr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load data ----
load("./data/Analysis dataset.RData")


## General information ----
dataset$Year <- factor(ifelse(dataset$Year <= 2015, "Before PRISMA-NMA", "After PRISMA-NMA"),
                       levels = c("Before PRISMA-NMA", "After PRISMA-NMA"))


## Total reviews per PRISMA-NMA status
before <- length(dataset$Year[dataset$Year == "Before PRISMA-NMA"])
after <- length(dataset$Year[dataset$Year == "After PRISMA-NMA"])


## Bubble plot: Year by healthcare field ----
# Healthcare frequency per year (conditional)
length(table(dataset[, "Health-related field"]))
table_bubble <- data.frame(dataset$Year, dataset[, "Health-related field"]) %>% 
  count(dataset$Year, dataset[, "Health-related field"]) 
colnames(table_bubble) <- c("Year", "Health", "Freq")

# Healthcare frequency regardless of year (unconditional)
table_total <- data.frame(dataset$Year, dataset[, "Health-related field"]) %>% 
  count(dataset[, "Health-related field"])

# Prepare dataset for ggplot2 (bring togethet)
data_bubble <- rbind(data.frame(Year = rep("Total", dim(table_total)[1]), 
                                Health = table_total[, 1],
                                Freq = table_total[, 2]),
                     table_bubble)
data_bubble$Year <- factor(data_bubble$Year, 
                           levels = c("Total", "Before PRISMA-NMA", "After PRISMA-NMA"))

# Add radius as new variable to 'data_bubble'
data_bubble$radius <- sqrt(data_bubble$Freq/pi)

# Get Figure S1
ggplot(data_bubble, 
       aes(x = Year, 
           y = Health)) + 
  geom_point(aes(size = radius*4.7), 
             shape = 21, 
             fill = "white", 
             color = "black",
             stroke = 1.2,
             alpha = 0.5) +
  geom_text(aes(label = Freq), 
            size = 3.8, 
            color="black",
            fontface = "bold") +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 18)) + 
  labs(x = "Publication timeframe",
       y ="") + 
  scale_size_identity() +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = 2, color = "grey"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none")


## Review level (definition) ----
# The authors defined transitivity assumption in the Review
q5 <- factor(dataset[, 28], levels = c("Yes", "No"))

#* Among the reviews that defined transitivity, where in the Review did the 
#* authors report the transitivity definition
q5_defin <- factor(subset(dataset[, 29], q5 == "Yes"), 
                   levels = c("Abstract", "Introduction", "Methods", "Results", "Discussion", "Supplementary"))
year5_defin <- subset(dataset$Year, q5 == "Yes")
aa1 <- table(year5_defin); aa2 <- round(table(year5_defin)/c(before, after) * 100, 0)

# Percentage of 'Yes' per method and timeframe
table_defin <- data.frame(year5_defin, q5_defin) %>% 
  count(year5_defin, q5_defin) %>% 
  group_by(year5_defin) %>% 
  mutate(prop = n / sum(n))

# Prepare the dataset for ggplot2 
data_defin <- table_defin
colnames(data_defin) <- c("timeframe", "method", "count", "prop")
data_defin$prop <- round(data_defin$prop * 100, 1)
data_defin$timeframe <- revalue(data_defin$timeframe, 
                                c("Before PRISMA-NMA" = paste0("Before PRISMA-NMA:", " ", aa1[1], " ", "(", aa2[1], "%) out of", " ", before),
                                  "After PRISMA-NMA" = paste0("After PRISMA-NMA:", " ", aa1[2], " ", "(", aa2[2], "%) out of", " ", after)))
data_defin$method <- revalue(data_defin$method, 
                             c("Introduction" = "A", 
                               "Methods" = "B", 
                               "Results" = "C", 
                               "Discussion" = "D",
                               "Supplementary" = "E"))

# Get Figure S2 (a)
fig1 <- ggplot(data_defin,
               aes(x = method,
                   y = prop)) +
  geom_bar(stat = "identity", 
           position = "stack",
           fill = "#0099FF") +
  geom_text(aes(label = paste0(prop, "%", " ", "(", count, ")")),
            hjust = 0.5, 
            vjust = -0.2,
            size = 5,
            colour = "black",
            position = "stack") +
  facet_grid(~ timeframe) +
  labs(x = "",
       y = "Percentage (%)",
       fill = "") +
  ylim(0, 100) +
  theme_classic() +
  ggtitle("Transitivity definition found in") +
  theme(title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))



## Review level (Acknowledging reported) ----
# The authors conclude or imply the (im)plausibility of transitivity (as binary)
q9_bin <- factor(ifelse(is.element(dataset[, 53], c("Not applicable", "Nothing stated")), "No", "Yes"), 
                 levels = c("Yes", "No"))

#' First, keep the review that explicitly stated in the methods that they planned to 
#' *evaluate transitivity* and reported the evaluation results 
q_cond <- ifelse(dataset[, c(35, 37, 43, 42, 46, 48, 50, 51)] == "Yes", 1, 0)

#' Then, separate between reviews that employed at least one method for transitivity ("Trans+")
#' from those that used only indirect method(s) for heterogeneity sources ("Hetero").
q_cond_new <- ifelse(rowSums(q_cond) > 0, "Trans+", "Hetero") 
q_cond_fin <- ifelse(dataset[, 34] == "Yes", "Yes", q_cond_new)

# Among the reviews with a conclusion about transitivity, this information was found in the 
q11_where <- ifelse(subset(dataset[, 66:69], q9_bin != "No" & q_cond_fin == "Trans+") == "Yes", 1, 0)
colnames(q11_where) <- c("Abstract", "Results", "Discussion", "Conclusions") 
year11_where <- subset(dataset$Year, q9_bin != "No" & q_cond_fin == "Trans+"); 
bb1 <- table(year11_where); bb2 <- round(table(year11_where)/c(before, after) * 100, 0)

# Percentage of 'Yes' per method and timeframe
table_where <- data.frame(year11_where, q11_where) %>% 
  group_by(year11_where) %>% 
  summarise(across(Abstract:Conclusions, list(n = ~sum(.x == 1)))) %>%
  rowwise() %>%
  mutate(across(Abstract_n:Conclusions_n, list(prop = ~.x / 
                                       sum(c(Abstract_n, Results_n, Discussion_n, Conclusions_n)))))

# Prepare the dataset for ggplot2 
data_where_prop <- melt(table_where[, c(1, 6:9)])
data_where_count <- melt(table_where[, 2:5])
data_where <- cbind(data_where_prop, data_where_count[, -1]) 
colnames(data_where) <- c("timeframe", "method", "prop", "count")
data_where$prop <- round(data_where$prop * 100, 1)
data_where$timeframe <- revalue(data_where$timeframe, 
                                c("Before PRISMA-NMA" = paste0("Before PRISMA-NMA:", " ", bb1[1], " ", "(", bb2[1], "%) out of", " ", before),
                                  "After PRISMA-NMA" = paste0("After PRISMA-NMA:", " ", bb1[2], " ", "(", bb2[2], "%) out of", " ", after)))
data_where$method <- revalue(data_where$method, 
                             c("Abstract_n_prop" = "Abstract", 
                               "Results_n_prop" = "Results", 
                               "Discussion_n_prop" = "Discussion",
                               "Conclusions_n_prop" = "Conclusions"))

# Get Figure S2 (b)
fig2 <- ggplot(data_where,
               aes(x = method,
                   y = prop)) +
  geom_bar(stat = "identity", 
           position = "stack",
           fill = "#0099FF") +
  geom_text(aes(label = paste0(prop, "%", " ", "(", count, ")")),
            hjust = 0.5, 
            vjust = -0.2,
            size = 5,
            colour = "black",
            position = "stack") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  facet_grid(~ timeframe) +
  labs(x = "",
       y = "",
       fill = "") +
  ylim(0, 100) +
  theme_classic() +
  ggtitle("Transitivity conclusions found in") +
  theme(title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))

# Bring together into Figure S2
ggarrange(fig1, 
          fig2, 
          ncol = 2, 
          labels = c("(a)", "(b)"),
          common.legend = TRUE, 
          legend = "bottom")


## Review level (Performed as planned?) ----
#* Among the reviews that explicitly planned transitivity evaluation, did they 
#* authors perform the evaluation as planned?
# Correcting dataset[, 32] for explicit evaluation of trans!
q6 <- revalue(subset(dataset[, 52], dataset[, 31] == "Yes" & q_cond_fin != "Hetero"),
              c("They performed the evaluation exactly as planned" = "Performed as planned",
                "They performed some of the planned methods (in [9]) due to limited data" = "Performed partly due to limited data",
                "They did not report in the Results or Discussion section any transitivity evaluation" = "Evaluation not reported",
                "They could not perform the evaluation due to limited data" = "Not performed due to limited data"))
q6_new <- factor(q6,
                 levels = c("Performed as planned", 
                            "Performed partly due to limited data", 
                            "Not performed due to limited data", 
                            "Evaluation not reported"))
year6 <- subset(dataset$Year, dataset[, 31] == "Yes" & q_cond_fin != "Hetero")
cc1 <- table(year6); cc2 <- round(table(year6)/c(before, after) * 100, 0)

# Percentage of 'Yes' per method and timeframe
table_perform <- data.frame(year6, q6_new) %>% 
  count(year6, q6_new) %>% 
  group_by(year6) %>% 
  mutate(prop = n / sum(n))

# Prepare the dataset for ggplot2 
data_perform <- table_perform
colnames(data_perform) <- c("timeframe", "method", "count", "prop")
data_perform$prop <- round(data_perform$prop * 100, 1)
data_perform$timeframe <- revalue(data_perform$timeframe, 
                                  c("Before PRISMA-NMA" = paste0("Before PRISMA-NMA:", " ", cc1[1], " ", "(", cc2[1], "%) out of", " ", before),
                                    "After PRISMA-NMA" = paste0("After PRISMA-NMA:", " ", cc1[2], " ", "(", cc2[2], "%) out of", " ", after)))

# Get Figure S3
ggplot(data_perform,
       aes(x = method,
           y = prop)) +
  geom_bar(stat = "identity", 
           position = "stack",
           fill = "#0099FF") +
  geom_text(aes(label = paste0(prop, "%", " ", "(", count, ")")),
            hjust = 0.5, 
            vjust = -0.2,
            size = 5,
            colour = "black",
            position = "stack") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) + 
  facet_grid(~ timeframe) +
  labs(x = "",
       y = "Percentage (%)",
       fill = "") +
  ylim(0, 100) +
  theme_classic() +
  ggtitle("Transitivity evaluation performed as planned?") +
  theme(title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))


## Review level (Acknowledging) ----
# The authors conclude or imply the (im)plausibility of transitivity (as binary)
q9_bin <- factor(ifelse(is.element(dataset[, 53], c("Not applicable", "Nothing stated")), "No", "Yes"), 
                 levels = c("Yes", "No"))
# Consider the actual levels after restricting to q9_bin != "No" and the reviews that assessed only heterogeneity
q9 <- factor(subset(dataset[, 53], q9_bin != "No" & q_cond_fin == "Trans+"),  
             levels = c("Transitivity may be plausible", 
                        "Transitivity may be questionable",
                        "Difficult to judge due to limited data"))
year9 <- subset(dataset$Year, q9_bin != "No" & q_cond_fin == "Trans+")
kk1 <- table(year9); kk2 <- round(table(year9)/c(before, after) * 100, 0)

# Rename the levels
q9_new <- revalue(q9, c("Transitivity may be plausible" = "Plausible", 
                        "Transitivity may be questionable" = "Questionable",
                        "Difficult to judge due to limited data" = "Difficult to judge"))

# Percentage of 'Yes' per method and timeframe
table_disc <- data.frame(year9, q9_new) %>% 
  count(year9, q9_new) %>% 
  group_by(year9) %>% 
  mutate(prop = n / sum(n))

# Prepare the dataset for ggplot2 
data_disc <- table_disc
colnames(data_disc) <- c("timeframe", "method", "count", "prop")
data_disc$prop <- round(data_disc$prop * 100, 1)
data_disc$timeframe <- revalue(data_disc$timeframe, 
                               c("Before PRISMA-NMA" = paste0("Before PRISMA-NMA:", " ", kk1[1], " ", "(", kk2[1], "%) out of", " ", before),
                                 "After PRISMA-NMA" = paste0("After PRISMA-NMA:", " ", kk1[2], " ", "(", kk2[2], "%) out of", " ", after)))

# Get Figure S4
ggplot(data_disc,
       aes(x = method,
           y = prop)) +
  geom_bar(stat = "identity", 
           position = "stack",
           fill = "#0099FF") +
  geom_text(aes(label = paste0(prop, "%", " ", "(", count, ")")),
            hjust = 0.5, 
            vjust = -0.2,
            size = 5,
            colour = "black",
            position = "stack") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) + 
  facet_grid(~ timeframe) +
  labs(x = "",
       y = "Percentage (%)",
       fill = "") +
  ylim(0, 100) +
  theme_classic() +
  ggtitle("Transitivity was concluded to be") +
  theme(title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))


## Review level (Acknowledging NMA parameters) ----
#* Among the reviews that discussed transitivity in the context of a NMA parameter, 
#* which NMA parameters where considered
q11 <- ifelse(subset(dataset[, 72:75], dataset[, 71] == "No" & q_cond_fin == "Trans+") == "Yes", 1, 0)
colnames(q11) <- c("A", # Treatment effects
                   "B", # Intervention ranking
                   "C", # Statistical heterogeneity
                   "D") # Evidence (in)consistency
year11 <- subset(dataset$Year, dataset[, 71] == "No" & q_cond_fin == "Trans+")
dd1 <- table(year11); dd2 <- round(table(year11)/c(before, after) * 100, 0)

# Percentage of 'Yes' per method and timeframe
table_param <- data.frame(year11, q11) %>% 
  group_by(year11) %>% 
  summarise(across(A:D, list(n = ~sum(.x == 1)))) %>%
  rowwise() %>%
  mutate(across(A_n:D_n, list(prop = ~.x / sum(c(A_n, B_n, C_n, D_n)))))

# Prepare the dataset for ggplot2 
data_param_prop <- melt(table_param[, c(1, 6:9)])
data_param_count <- melt(table_param[, 2:5])
data_param <- cbind(data_param_prop, data_param_count[, -1])
colnames(data_param) <- c("timeframe", "method", "prop", "count")
data_param$prop <- round(data_param$prop * 100, 1)
data_param$timeframe <- revalue(data_param$timeframe, 
                                c("Before PRISMA-NMA" = paste0("Before PRISMA-NMA:", " ", dd1[1], " ", "(", dd2[1], "%) out of", " ", before),
                                  "After PRISMA-NMA" = paste0("After PRISMA-NMA:", " ", dd1[2], " ", "(", dd2[2], "%) out of", " ", after)))
data_param$method <- revalue(data_param$method, 
                             c("A_n_prop" = "Treatment effects", 
                               "B_n_prop" = "Intervention ranking", 
                               "C_n_prop" = "Statistical heterogeneity",
                               "D_n_prop" = "Evidence (in)consistency"))

# Figure S5
ggplot(data_param,
       aes(x = method,
           y = prop)) +
  geom_bar(stat = "identity", 
           position = "stack",
           fill = "#0099FF") +
  geom_text(aes(label = paste0(prop, "%", " ", "(", count, ")")),
            hjust = 0.5, 
            vjust = -0.2,
            size = 5,
            colour = "black",
            position = "stack") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  facet_grid(~ timeframe) +
  labs(x = "",
       y = "Percentage (%)",
       fill = "") +
  ylim(0, 100) +
  theme_classic() +
  ggtitle("Network meta-analysis parameters discussed") +
  theme(title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))


## Reporting the Table of Characteristics (Structure) ----
# A Table of Characteristics (ToC) is provided in the publication
q12 <- factor(dataset[, 78], levels = c("Yes", "No"))

#* If a ToC is provided in the publication (and after excluding the systematic reviews without access 
#* to their supplementary material), the Table presents the information at 
q13 <- ifelse(subset(dataset[, 88:94], q12 == "Yes" & dataset[, 80] != "No access") == "Yes", 1, 0) 
year13 <- subset(dataset$Year, q12 == "Yes" & dataset[, 80] != "No access") 
ee1 <- table(year13); ee2 <- round(table(year13)/c(before, after) * 100, 0)

#* Merge 'Trial-level without arm-level' with 'Trial-level with arm-level' and 
#* 'Comparison-level with arm-level' with 'Comparison-level without arm-level'
q13_new <- data.frame(apply(q13[, 1:2], 1, sum),  
                      apply(q13[, 3:4], 1, sum),
                      q13[, 5:7])
colnames(q13_new) <- c("A", # Trial-level 
                       "B", # Comparison-clustered trial-level 
                       "C", # Comparison-level 
                       "D", # Intervention-level 
                       "E") # Characteristic-level

# Percentage of 'Yes' per method and timeframe
table_toc <- data.frame(year13, q13_new) %>% 
  group_by(year13) %>% 
  summarise(across(A:E, list(n = ~sum(.x == 1)))) %>%
  rowwise() %>%
  mutate(across(A_n:E_n, list(prop = ~.x / sum(c(A_n, B_n, C_n, D_n, E_n)))))

# Prepare the dataset for ggplot2 
data_toc_prop <- melt(table_toc[, c(1, 7:11)])
data_toc_count <- melt(table_toc[, 2:6])
data_toc <- cbind(data_toc_prop, data_toc_count[, -1]) 
colnames(data_toc) <- c("timeframe", "method", "prop", "count")
data_toc$structure <- rep(1:0, c(3*2, 2*2))
data_toc$prop <- round(data_toc$prop * 100, 1)
data_toc$timeframe <- revalue(data_toc$timeframe, 
                              c("Before PRISMA-NMA" = paste0("Before PRISMA-NMA:", " ", ee1[1], " ", "(", ee2[1], "%) out of", " ", before),
                                "After PRISMA-NMA" = paste0("After PRISMA-NMA:", " ", ee1[2], " ", "(", ee2[2], "%) out of", " ", after)))
data_toc$method <- revalue(data_toc$method, 
                             c("A_n_prop" = "A", 
                               "B_n_prop" = "B", 
                               "C_n_prop" = "C",
                               "D_n_prop" = "D",
                               "E_n_prop" = "E"))

# Create Figure S6 (a)
fig3 <- ggplot(data_toc, 
               aes(x = method,
                   y = prop,
                   fill = as.factor(structure))) +
  geom_bar(stat = "identity", 
           position = "stack") +
  geom_text(aes(label = paste0(prop, "%", " ", "(", count, ")")),
            hjust = 0.5, 
            vjust = -0.2,
            size = 5,
            colour = "black",
            position = "stack") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  scale_fill_manual(breaks = c("1", "0"),
                    values = c("#0099FF", "#66CCFF"), 
                    labels = c("Facilitates transitivity evaluation", "Hinders transitivity evaluation")) +
  facet_grid(~ timeframe) +
  labs(x = "",
       y = "Percentage (%)",
       fill = "") +
  ylim(0, 100) +
  theme_classic() +
  ggtitle("Table of characteristics structure") +
  theme(title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))


## Reporting the Table of Characteristics (Found in) ----
# If a table of characteristics is provided, it can be found in the 
q13_found <- factor(subset(dataset[, 79], q12 == "Yes" & dataset[, 80] != "No access"),
                    levels = c("Main article", "Supplementary", "Both"))

# Percentage of 'Yes' per method and timeframe
table_found <- data.frame(year13, q13_found) %>% 
  count(year13, q13_found) %>% 
  group_by(year13) %>% 
  mutate(prop = n / sum(n))

# Prepare the dataset for ggplot2 
data_found <- table_found
colnames(data_found) <- c("timeframe", "method", "count", "prop")
data_found$prop <- round(data_found$prop * 100, 1)
data_found$timeframe <- revalue(data_found$timeframe, 
                                c("Before PRISMA-NMA" = paste0("Before PRISMA-NMA:", " ", ee1[1], " ", "(", ee2[1], "%) out of", " ", before),
                                  "After PRISMA-NMA" = paste0("After PRISMA-NMA:", " ", ee1[2], " ", "(", ee2[2], "%) out of", " ", after)))
data_found$method <- revalue(data_found$method, c("Main article" = "Article"))

# Create Figure S6 (b)
fig4 <- ggplot(data_found,
               aes(x = method,
                   y = prop,
                   colour = method)) +
  geom_bar(stat = "identity", 
           position = "stack",
           fill = "#CCCCCC") +
  geom_text(aes(label = paste0(prop, "%", " ", "(", count, ")")),
            hjust = 0.5, 
            vjust = -0.2,
            size = 5,
            colour = "black",
            position = "stack") +
  scale_colour_manual(breaks = c("Article", "Supplementary", "Both"), 
                    values = c("white", "white", "white")) + 
  facet_grid(~ timeframe) +
  labs(x = "",
       y = "Percentage (%)",
       colour = "") +
  ylim(0, 100) +
  theme_classic() +
  ggtitle("Table of characteristics is found in") +
  theme(title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14, colour = "white"),
        legend.justification = c(2, 0),
        strip.text = element_text(size = 14, face = "bold"))

# Get Figure S6
ggarrange(fig3, 
          fig4, 
          ncol = 2, 
          labels = c("(a)", "(b)"),
          common.legend = FALSE, 
          legend = "bottom")