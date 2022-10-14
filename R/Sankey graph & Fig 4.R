#*******************************************************************************
#*
#*
#*               SANKEY GRAPH FOR TRANSITIVITY EVALUATION QUALITY                              
#*                         (All 356 Systematic Reviews)                                                                                                      
#*
#*
#*******************************************************************************



## Load libraries ----
devtools::install_github("davidsjoberg/ggsankey")
list.of.packages <- c("readxl", "dplyr", "ggsankey", "stringr", "plotly")
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
#both1 <- ifelse(q7 == "No" & both_1 != "No", "No", both_1)
both1 <- both_1

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



## Prepare dataset for Sankey plot (ggplot2) ----
item <- c("Planned in protocol",
          "Described in Methods",
          "Reported in Results",
          "Discussed transitivity",
          "Provided proper Table",
          "Transitivity evaluation quality")
data_sankey0 <- data.frame(both, 
                           dataset[, 27],
                           both1,
                           both2,
                           proper_toc,
                           judge_quality)
colnames(data_sankey0) <- item

# Turn into wide
data_sankey <- data_sankey0 %>% 
  make_long(colnames(data_sankey0)) 



## Draw sankey plot (ggplot2) ----
tiff("./31_Analysis - Descriptives/Figure 4.tiff", height = 20, width = 37, units = "cm", compression = "lzw", res = 300)
ggplot(data_sankey, 
       aes(x = x, 
           next_x = next_x , 
           node = node, 
           next_node = next_node, 
           label = node)) +
  geom_sankey(flow.color = "grey60",
              flow.fill = "grey90",
              flow.alpha = 0.75,
              node.color = "#009E73", 
              node.fill = "#009E73") +
  geom_sankey_label() +
  theme_classic() +
  labs(x = " ") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 33)) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
dev.off()

  

## Prepare dataset for Sankey plot (plotly) ----
q6_q3 <- as.data.frame(table(unclear_qual[, 27], q3_0)) 
q9_q6 <- as.data.frame(table(both,  unclear_qual[, 27]))
q11_q9 <- as.data.frame(table(q11_0, both))
q12_q11 <- as.data.frame(table(both1, q11_0))
q14_q12 <- as.data.frame(table(both2, both1))
q16_q14 <- as.data.frame(table(unclear_qual[, 71], both2))
q19_q16 <- as.data.frame(table(q19_0, unclear_qual[, 71]))
q21_q19 <- as.data.frame(table(q21_0, q19_0))
colnames(q6_q3) <- c("target", "source", "value")
colnames(q9_q6) <- colnames(q11_q9) <- colnames(q12_q11) <- colnames(q6_q3) 
colnames(q14_q12) <- colnames(q16_q14) <- colnames(q19_q16) <- colnames(q21_q19) <- colnames(q6_q3)



## Draw Sankey plot (plotly) ----
fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c(unique(q6_q3$source), 
              unique(q9_q6$source), 
              unique(q11_q9$source),
              unique(q12_q11$source), 
              unique(q14_q12$source), 
              unique(q16_q14$source),
              unique(q19_q16$source),
              unique(q21_q19$source)),
    #color = c("red", "blue", "blue", "blue", "blue", "green", "green", "green", "black", "orange", "grey"),
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = c(rep(0:2, each = 2), # q6_q3 (AB) 
               rep(3:4, each = 4), # q9_q6 (BC)
               rep(5:8, each = 2), # q11_q9 (CD)
               9, 9, 10, 10, 10, 10, # q12_q11 (DE)
               rep(11:14, each = 4), # q14_q12 (EF)
               rep(15:18, each = 2), # q16_q14 (FG)
               rep(19:20, each = 2), # q19_q16 (GH)
               21, 21, 21, 22, 22), # q21_q19 (HI)
    target = c(rep(3:4, 3), # q6_q3 (AB)
               rep(5:8, 2), # q9_q6 (BC)
               rep(9:10, 4), # q11_q9 (CD)
               12:13, 11:14, # q12_q11 (DE)
               rep(15:18, 4), # q14_q12 (EF)
               rep(19:20, 4), # q16_q14 (FG)
               rep(21:22, 2), # q19_q16 (GH)
               23:25, 24:25), # q21_q19 (HI)
    value =  c(q6_q3$value, 
               q9_q6$value, 
               q11_q9$value,
               q12_q11$value[c(-1, -4)], 
               q14_q12$value, 
               q16_q14$value,
               q19_q16$value,
               q21_q19$value[-4])
  )
) %>% add_annotations(
  x = c(1,0),
  y = 0.5,
  xshift = c(25, -25),
  text = item,
  font = list(
    size = 14
  ),
  showarrow = FALSE,
  textangle = c(90, -90)
)
fig <- fig %>% layout(
  title = "Basic Sankey Diagram",
  font = list(
    size = 10
  ),
  xaxis = list(showgrid = F, zeroline = F)
)
fig
