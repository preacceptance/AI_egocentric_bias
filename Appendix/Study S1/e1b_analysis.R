## Harvard Business School, Ethical Intelligence Lab
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
               'lme4',            # linear regression models
               'rstatix',         # summary statistics and data visualization tools
               'dplyr',           # data manipulation
               'rlang'            # programming tools for tidy evaluation
)

mediation <- FALSE

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data: 
# if importing from Qualtrics: (i) export data as numeric values, and (ii) delete rows 2 and 3 of the .csv file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d_raw <- read.csv('AI_Curse_v4.csv') 

## number of participants BEFORE exclusions: 
n_original <- dim(d_raw)[1] # extracting number of rows only, not columns
n_original 

## perform attention exclusions: 
d_raw <- subset(d_raw, (d_raw$attn_1 == 1 & d_raw$attn_2 == 2))
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
  curr <- d_raw[i,14:19][!is.na(d_raw[i,14:19])] # for a given row, get only the non-NA values
  d[i,2:4] <- curr[curr!= ""][1:3] # and only the non-empty values
  
  cond_ns <- names(d_raw[i,14:19])[which(d_raw[i,14:19] != "")]
  d[i,1] <- strsplit(cond_ns[[1]], "_")[[1]][1]
}

d <- cbind(d, d_raw[,21:28])

sum(d$cond == 'Absent')
sum(d$cond == 'Present')

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
tapply(d$capability, d$cond, mean)
tapply(d$capability, d$cond, sd)

# Annotated v Absent 
present_v_absent <- t.test(d$capability[d$cond=='Present'], d$capability[d$cond=='Absent']); present_v_absent
cohen.d(d$capability[d$cond=='Present'], d$capability[d$cond=='Absent'])

# Compare to midpoint
t.test(d$capability[d$cond=='Present'], mu=50)
t.test(d$capability[d$cond=='Absent'], mu=50)

### Trust ----
tapply(d$trust, d$cond, mean)
tapply(d$trust, d$cond, sd)

# Present v Absent 
present_v_absent_t <- t.test(d$trust[d$cond=='Present'], d$trust[d$cond=='Absent']); present_v_absent_t
cohen.d(d$trust[d$cond=='Present'], d$trust[d$cond=='Absent'])

#Midpoint test
t.test(d$trust[d$cond=='Present'], mu=50)
t.test(d$trust[d$cond=='Absent'], mu=50)

## ================================================================================================================
##                                              MEDIATION                
## ================================================================================================================

cor.test(d$capability, d$trust)

# Subset data for Mediation Analysis  (Present and Absent conditions)
d$cond_n <- ifelse(d$cond=='Absent', 0,
            ifelse(d$cond=='Present', 1, NA))

if(mediation){

    # Mediation Analysis
    source("../common_scripts/process.R")
    
    process(data = d, y = "trust", x = "cond_n",
            m = "capability", model = 4, effsize = 1, total = 1, stand = 1,
            contrast = 1, boot = 10000, modelbt = 1, seed = 654321)
    
    process(data = d, y = "trust", x = "cond_n",
            m =c("capability"), w =c("AI_Familiarity_1"), model = 7, effsize = 1, total = 1, stand = 1, 
            contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
    
    process(data = d, y = "trust", x = "cond_n",
            m =c("capability"), w =c("Visual_Familiarity_1"), model = 7, effsize = 1, total = 1, stand = 1, 
            contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
}

## ================================================================================================================
##                                              END                
## ================================================================================================================