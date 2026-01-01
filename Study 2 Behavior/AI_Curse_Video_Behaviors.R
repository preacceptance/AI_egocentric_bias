## Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV SCENARIOS | WITHIN SUBJECTS               
## ================================================================================================================

# Clear workspace and set options
rm(list = ls()) 
options(download.file.method="libcurl")

# Install and load necessary packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(
  'ggplot2',      # For plotting
  'dplyr',        # For data manipulation
  'lme4',         # For mixed-effects models
  'ggpubr',       # For arranging ggplot figures
  'ltm',          # For calculating Cronbach's alpha
  'tidyr',        # For data tidying
  'effsize',      # For effect size calculations
  'pwr',          # For power analysis
  'Hmisc',        # For miscellaneous functions like Cronbach's alpha
  'reshape2',     # For reshaping data
  'rstatix',      # For statistical tests
  'scales',       # For scaling data in plots
  'mediation',    # For mediation analysis
  'lmerTest'      # For linear mixed-effects models with p-values
)

library("lmerTest")
library(dplyr)

# Import data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
d_raw <- read.csv('AI_Curse_Video_Behavior.csv') 

# Number of participants before exclusions
n_original <- dim(d_raw)[1]; n_original 

# Perform attention exclusions
d_raw <- subset(d_raw, ( d_raw$att_1 == 2 & d_raw$att_2 == 2 ))
n_final <- dim(d_raw)[1]; n_final

# Initialize new data frame
d <- array(dim=c(n_final, 5))
colnames(d) <- c('cond', 'auto', 'handsOff', 'watch', 'nap')
d <- as.data.frame(d, stringsAsFactors=FALSE) 

## extract good data from the middle part of raw data in AV:
for(i in 1:dim(d_raw)[1]) {
    curr <- d_raw[i,14:21][!is.na(d_raw[i,14:21])] # for a given row, get only the non-NA values
    d[i,2:5] <- curr[curr!= ""][1:4] # and only the non-empty values
    
    cond_ns <- names(d_raw[i,14:21])[which(d_raw[i,14:21] != "")]
    d[i,1] <- strsplit(cond_ns[[1]], "_")[[1]][1]
}

d <- cbind(d, d_raw[,22:35])

#convert to numeric
d$AI_Familiarity_1 <- as.numeric(d$AI_Familiarity_1)
d$Visual_Familiarity_1 <- as.numeric(d$Visual_Familiarity_1)
d$auto <- as.numeric(d$auto)
d$handsOff <- as.numeric(d$handsOff)
d$watch <- as.numeric(d$watch)
d$nap <- as.numeric(d$nap)
d$Present_TimeVehicleStopped <- as.numeric(d$Present_TimeVehicleStopped)
names(d)[names(d) == "Present_TimeVehicleStopped"] <- "rt"

# Calculate Cronbach's alpha
cron <- cronbach.alpha(d[,c("auto", "handsOff", "watch","nap")]); cron
d$distract <- rowMeans(d[,c("auto", "handsOff", "watch","nap")], na.rm=TRUE)

# Remove rows with non-numeric TimeVehicleStopped values
sum(is.na(d$rt))
sum(is.na(d$rt))/n_final
d$rt <- replace(d$rt, is.na(d$rt), 19.36) 
#d <- d[!is.na(d$rt), ]
n_final <- dim(d)[1]; n_final

# Sum conditions
sum(d$cond == 'Present')
sum(d$cond == 'Faded')

# Check column names
colnames(d)

d$age <- as.numeric(as.character(d$age)); mean(d$age, na.rm = TRUE)
count_gender_1 <- sum(d$gender == 1, na.rm = TRUE); (count_gender_1/n_final)*100

## ================================================================================================================
##                                              ANALYSES                
## ================================================================================================================

# Midpoint
t.test(d$distract[d$cond=='Present'], mu=50)
t.test(d$distract[d$cond=='Faded'], mu=50)

# Distraction ----
tapply(d$distract, d$cond, mean)
tapply(d$distract, d$cond, sd)

# Present v Faded
present_v_faded_d <- t.test(d$distract[d$cond=='Present'], d$distract[d$cond=='Faded']); present_v_faded_d
cohen.d(d$distract[d$cond=='Present'], d$distract[d$cond=='Faded'])



### TimeVehicleStopped ----
tapply(d$rt, d$cond, mean)
tapply(d$rt, d$cond, sd)

# Present v Faded
present_v_faded <- t.test(d$rt[d$cond=='Present'], d$rt[d$cond=='Faded']); present_v_faded
cohen.d(d$rt[d$cond=='Present'], d$rt[d$cond=='Faded'])

## Distribution of Time to Take Control
## Kolmogorov-Smirnov 
ks.test(d[d$cond == "Present",]$rt, d[d$cond == "Faded",]$rt)


## ================================================================================================================
##                                              PLOT                
## ================================================================================================================

# Reaction Time PLot
t_names <- c("Faded", "Annotated")
title_size <- 16
axis_size <- 10

plotter <- function(data, value_column, input_title) {
  p <- ggplot(data, aes_string(x = "cond", y = value_column, fill = "cond")) +
    geom_bar(stat = "summary", fun = mean, position = position_dodge(width = 0.9), color = "black", size = 0.4) +
    geom_errorbar(stat = "summary", fun.data = "mean_cl_boot", width = 0.2, position = position_dodge(width = 0.9)) +
    
    ## --- significance marker ---
    annotate("segment", x = 1, xend = 2, y = 18, yend = 18,   # the line
             linewidth = 0.7) +
    annotate("text", x = 1.5, y = 18, label = "***",  # the stars
             size = 6, vjust = 0) +
    
    scale_fill_manual(values = c("#e2eefd", "#e2eefd")) + 
    labs(title = input_title, x = "", y = "") +
    scale_x_discrete(labels = t_names) +  # Add custom x-tick labels
    theme_bw() +
    theme(panel.grid = element_blank())+
    theme(text = element_text(size = title_size),
          axis.text.x = element_text(size = axis_size, angle = 45, hjust = 1),
          axis.text.y = element_text(size = axis_size),
          plot.title = element_text(size = title_size, hjust = 0.5),
          legend.position = "none") +
    geom_hline(yintercept = 50, linetype = "dotted") + # Add dotted line at y = 50
    scale_y_continuous(limits = c(0, 22)) # Set y-axis to go from 0 to 100
  
  return(p)
}

p1 <- plotter(d, "rt", 'Reaction Time')
p1

#time density plot

names(d)[names(d) == "cond"] <- "Scene Annotation Condition"

p2 <- ggplot(data = d, aes(color =`Scene Annotation Condition`, x=rt )) +
  stat_density(geom="line", position="identity", alpha=.75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=10), legend.position = 'top' ) + 
  scale_color_manual(values = c("#8f2d14", "#4294f7")) +
  guides(colour = guide_legend(override.aes = list(),
                               title = "Scene Annotation Condition",
                               label = c("faded", "annotated")))+
  ylab("Density") +
  xlab("Time to Take Control (s)") + 
  ggplot2::annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax = .2,alpha = .1) +
  ggplot2::annotate("text", x = 7.5, y = .08, label = "Vehicle approaches intersection", size = 2) +
  ggplot2::annotate("rect", xmin = 15, xmax = 20, ymin = 0, ymax = .2,alpha = .1) +
  ggplot2::annotate("text", x = 17.5, y = .19, label = "Vehicle approaches jaywalkers", size = 2) +
  theme(legend.key = element_rect(fill = NA))

figure <- ggarrange(p1, p2, nrow = 1, ncol = 2, widths = c(1, 2),  vjust = 1.0, hjust = 0.5)
ggsave(paste0('e5', ".png"), last_plot(), dpi = 300, width = 12, height = 6)

# Create the 'figures' directory if it doesn't exist
if (!dir.exists("figures")) {
  dir.create("figures")
}



# Move the PDF to the 'figures' subfolder
file.rename("e5.png", "figures/e5.png")



# Define the variable names and titles
t_names <- c("Present", "Faded")
title_size <- 16
axis_size <- 10

# Correcting the Distraction data frame
Distraction <- d %>%
  filter(cond_n %in% c("Present", "Faded")) %>%
  select(cond_n, Distraction) %>%
  rename(Value = Distraction) %>%
  mutate(Condition = cond_n)

# Correcting the VehicleTimeStopped data frame
TimeVehicleStopped <- d %>%
  filter(cond_n %in% c("Present", "Faded")) %>%
  select(cond_n, TimeVehicleStopped) %>%
  rename(Value = TimeVehicleStopped) %>%
  mutate(Condition = cond_n)

# Define the plotting function
plotter <- function(data, input_title, stars, y_limit) {
  p <- ggplot(data, aes(x = Condition, y = Value, fill = Condition)) +
    geom_bar(stat = "summary", fun = mean, position = position_dodge(width = 0.9), color = "black", size = 0.4) +
    geom_errorbar(stat = "summary", fun.data = mean_cl_boot, width = 0.2, position = position_dodge(width = 0.9)) +
    theme_bw() +
    theme(text = element_text(size = 16),
          panel.grid.major = element_line(color = "grey", size = 0.2),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 16, hjust = 0.5),
          legend.position = "none") +
    scale_fill_manual(values = c("darkgrey", "lightgrey")) +
    labs(title = input_title, x = "", y = "Value") +
    annotate("text", x = 1.5, y = y_limit * 0.95, label = stars, size = 5) + # Annotate with stars
    ylim(0, y_limit) + # Set y-axis limits
    geom_hline(yintercept = y_limit / 2, linetype = "dotted", color = "black", size = 0.5) # Add dotted line at y midpoint
  
  return(p)
}

# Ensure 'stars' variable is correctly defined
stars_distraction = "***" 
stars_time = "***"

# Create the plots
p1 <- plotter(Distraction, "Distraction", stars_distraction, 100)
p2 <- plotter(TimeVehicleStopped, "Time to Stop the Vehicle", stars_time, 30)

# Combine the plots into a single figure
figure <- ggarrange(p1, p2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "top")

# Print the figure
print(figure)


## ================================================================================================================
##                                              END                
## ================================================================================================================