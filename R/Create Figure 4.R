#*******************************************************************************
#*
#*
#*                               Create Figure 4                                                          
#*           (Sankey graph for the quality of transitivity evaluation)                                                                                    
#*                                                                 
#* Author: Loukia M. Spineli 
#* Date: October 2022
#*******************************************************************************

## Install the development version of ggsankey R-package ----
install.packages("devtools")
devtools::install_github("davidsjoberg/ggsankey")

## Load libraries ----
list.of.packages <- c("dplyr", "ggsankey", "rnmamod", "stringr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)

## Load data ----
load("./data/judge_trans_quality.RData")  # Saved from R-script 'Create Figure 3'

## Obtain transitivity quality evaluation ----
judge_quality <- rep(NA, dim(data_function)[1])
for (i in 1:dim(data_function)[1]) {
  judge_quality[i] <- trans_quality(plan_protocol = data_function[i, 1], 
                                    plan_methods = data_function[i, 2],
                                    report_results = data_function[i, 3],
                                    discuss_trans = data_function[i, 4],
                                    proper_table = data_function[i, 5])
}

## Prepare dataset for Sankey plot (ggplot2) ----
data_sankey0 <- data.frame(data_function,
                           judge_quality)
colnames(data_sankey0) <-  c("Planned in protocol",
                             "Described in Methods",
                             "Reported in Results",
                             "Discussed transitivity",
                             "Provided proper Table",
                             "Transitivity evaluation quality")


## Turn into wide to be yield the Sankey plot
data_sankey <- data_sankey0 %>% 
  make_long(colnames(data_sankey0)) 

## Draw the Sankey plot (magnificent :-)) ----
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
