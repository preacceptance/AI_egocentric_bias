
## ================================================================================================================
##                                DATA ANALYSIS | AV SCENARIOS | BETWEEN SUBJECTS               
## ================================================================================================================

## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',         # plotting
               'ggsignif',        # plotting significance bars
               'ggpubr',          # arrange plots in a grid
               'lme4',            # functions for fitting linear regression models
               'rstatix',         # summary statistics and data visualization tools
               'dplyr'            # data manipulation
)

library("lmerTest")
library(dplyr)

mediation <- FALSE

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

# import data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d_raw <- read.csv('AI_Curse_Disclaimer.csv') 

## number of participants BEFORE exclusions: 
n_original <- dim(d_raw)[1] # extracting number of rows only, not columns
n_original 

## perform attention exclusions: 
d_raw <- subset(d_raw, ( d_raw$attn_1 == 1 & d_raw$attn_2 == 2))
n_original <- dim(d_raw)[1]

## perform comprehension exclusions
d_raw <- subset(d_raw, (d_raw$comp_images == 1))
n_final <- dim(d_raw)[1]; n_final



## define new data frame to extract pre-processed data into:
d <- array(dim=c(n_final, 4))
colnames(d) <- c('cond', 'capability', 'trust', 'why')
d <- as.data.frame(d, stringsAsFactors=FALSE) 

## extract good data from the middle part of raw data in AV:
for(i in 1:dim(d_raw)[1]) {
  curr <- d_raw[i,14:22][!is.na(d_raw[i,14:22])] # for a given row, get only the non-NA values
  d[i,2:4] <- curr[curr!= ""][1:3] # and only the non-empty values
  
  cond_ns <- names(d_raw[i,14:22])[which(d_raw[i,14:22] != "")]
  d[i,1] <- strsplit(cond_ns[[1]], "_")[[1]][1]
}

d <- cbind(d, d_raw[,24:31])

sum(d$cond == 'Present')
sum(d$cond == 'Absent')
sum(d$cond == 'Disclaimer')

#convert to numeric
d$AI_Familiarity_1 <- as.numeric(d$AI_Familiarity_1)
d$Visual_Familiarity_1 <- as.numeric(d$Visual_Familiarity_1)
d$capability <- as.numeric(d$capability)
d$trust <- as.numeric(d$trust)

d$age <- as.numeric(as.character(d$age)); mean(d$age, na.rm = TRUE)
count_gender_1 <- sum(d$gender == 1, na.rm = TRUE); (count_gender_1/n_final)*100

## ================================================================================================================
##                                              ANALYSES                
## ================================================================================================================

### Capability ----

# Fit the ANOVA model
anova_model <- aov(capability ~ cond, data = d)
summary(anova_model)
eta_squared(anova_model)

tapply(d$capability, d$cond, mean)
tapply(d$capability, d$cond, sd)

# Absent v Annotated 
absent_v_present <- t.test(d$capability[d$cond=='Absent'], d$capability[d$cond=='Present']); absent_v_present
cohen.d(d$capability[d$cond=='Absent'], d$capability[d$cond=='Present'])

# Disclaimer v Annotated
present_v_disclaimer <- t.test(d$capability[d$cond=='Present'], d$capability[d$cond=='Disclaimer']); present_v_disclaimer
cohen.d(d$capability[d$cond=='Present'], d$capability[d$cond=='Disclaimer'])

# Disclaimer v Absent
absent_v_disclaimer <- t.test(d$capability[d$cond=='Absent'], d$capability[d$cond=='Disclaimer']); absent_v_disclaimer
cohen.d(d$capability[d$cond=='Absent'], d$capability[d$cond=='Disclaimer'])

# Compare to midpoint
t.test(d$capability[d$cond=='Absent'], mu=50)
t.test(d$capability[d$cond=='Disclaimer'], mu=50)
t.test(d$capability[d$cond=='Present'], mu=50)


### Trust ----

# Fit the ANOVA model
anova_model <- aov(trust ~ cond, data = d)
summary(anova_model)
eta_squared(anova_model)

tapply(d$trust, d$cond, mean)
tapply(d$trust, d$cond, sd)

# Absent v Annotated 
absent_v_present_t <- t.test(d$trust[d$cond=='Absent'], d$trust[d$cond=='Present']); absent_v_present_t
cohen.d(d$trust[d$cond=='Absent'], d$trust[d$cond=='Present'])

# Disclaimer v Annotated
present_v_disclaimer_t <- t.test(d$trust[d$cond=='Present'], d$trust[d$cond=='Disclaimer']); present_v_disclaimer_t
cohen.d(d$trust[d$cond=='Present'], d$trust[d$cond=='Disclaimer'])

# Disclaimer v Absent
absent_v_disclaimer_t <- t.test(d$trust[d$cond=='Absent'], d$trust[d$cond=='Disclaimer']); absent_v_disclaimer_t
cohen.d(d$trust[d$cond=='Absent'], d$trust[d$cond=='Disclaimer'])


# Compare to midpoint
t.test(d$trust[d$cond=='Absent'], mu=50)
t.test(d$trust[d$cond=='Disclaimer'], mu=50)
t.test(d$trust[d$cond=='Present'], mu=50)

## ================================================================================================================
##                                              MEDIATION                
## ================================================================================================================

cor.test(d$capability, d$trust)

# Subset data for Mediation Analysis  (Present and Absent conditions)
d$cond_n <- ifelse(d$cond == 'Present', 1,
            ifelse(d$cond == 'Disclaimer', 2,
            ifelse(d$cond == 'Absent', 0, NA)))

if(mediation) {
  
  # Mediation Analysis
  source("../common_scripts/process.R")

    process(data = d, y = "trust", x = "cond_n",
            m = "capability", model = 4, effsize = 1, mcx = 1, total = 1, stand = 1,
            contrast = 1, boot = 10000, modelbt = 1, seed = 654321)
    
    process(data = d, y = "trust", x = "Absent0",
            m =c("capability"), w =c("AI_Familiarity_1"), model = 7, effsize = 1, total = 1, stand = 1, 
            contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
    
    process(data = d, y = "trust", x = "cond_n",
            m =c("capability"), w =c("Visual_Familiarity_1"), model = 7, effsize = 1, total = 1, stand = 1, 
            contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
}
    
## ================================================================================================================
##                                              PLOT                
## ================================================================================================================

# Define the variable names and titles
t_names <- c("annotations-only","verbal-cue","image-annotated")
title_size <- 16
axis_size <- 10

plotter <- function(data, value_column, input_title) {
  p <- ggplot(data, aes_string(x = "cond", y = value_column, fill = "cond")) +
    geom_bar(stat = "summary", fun = mean, position = position_dodge(width = 0.9), color = "black", size = 0.4) +
    geom_errorbar(stat = "summary", fun.data = "mean_cl_boot", width = 0.2, position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = c("darkgrey", "lightgrey", "white")) +
    labs(title = input_title, x = "", y = "") +
    scale_x_discrete(labels = t_names) +  # Add custom x-tick labels
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
    geom_hline(yintercept = 50, linetype = "dotted") +
    theme_bw() +
    theme(text = element_text(size = title_size),
          axis.text.x = element_text(size = axis_size, angle = 45, hjust = 1),
          axis.text.y = element_text(size = axis_size),
          plot.title = element_text(size = title_size, hjust = 0.5),
          legend.position = "none")
  
  return(p)
}

# Create the plots using the original data
p1 <- plotter(d, "trust", 'Trust')
p2 <- plotter(d, "capability", 'Capability')


# Create the figure with two plots
figure <- ggarrange(p2, p1, nrow = 1, ncol = 2, common.legend = FALSE, vjust = 1.0, hjust = 1.1)

# Add annotations to the figure
figure <- ggarrange(p2, p1, nrow = 1, ncol = 2, common.legend = FALSE, vjust = 1.0, hjust = 1.1)
annotated_figure <- annotate_figure(figure, left = text_grob("Mean Rating", color = "black", face = "plain", size = 26, rot = 90),
                                    bottom = text_grob("Image Annotation Condition", color = "black", face = "plain", size = 26, vjust = -.1))
ggsave("figure3.pdf", annotated_figure, width = 7, height = 5, dpi = 300)

# Create the 'figures' directory if it doesn't exist
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Move the PDF to the 'figures' subfolder
file.rename("figure3.pdf", "figures/figure3.pdf")

## ================================================================================================================
##                                              END                
## ================================================================================================================