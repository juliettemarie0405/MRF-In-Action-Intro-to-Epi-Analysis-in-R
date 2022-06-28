################################################################################

###############    MRF In Action: Intro to epi analysis in R     ###############

################################################################################

# Questions ----------------------------------------------------------


### Q1: how to add legend labels

## load packages
library(readxl)
library(tidyverse)
library(skimr)

## set working directory
setwd("C:/Users/jr3639/OneDrive - Drexel University/Documents/students/RWorkshop2022") #insert your filepath here

## import and prep dataset
d1 <- read_excel("Rworkshop_dataset.xlsx")

d1$asd=factor(d1$asd)
d1$prenatal_vitamin=factor(d1$prenatal_vitamin)
d1$income=factor(d1$income)
d1$smoking=factor(d1$smoking)

#scatterplot
ggplot(data = d1) + 
  geom_point(mapping=aes(x=fruit, y=srs, color=income)) + 
  geom_smooth(method="lm",mapping=aes(x=fruit, y=srs))+
  ylab("SRS Score") + #add y axis title
  xlab("Fruit Intake (SPD)") + #add x axis title
  scale_y_continuous(breaks = c(0,30,60,90)) + #customize y axis scale
  ggtitle("SRS Score by Fruit Intake in EARLI") + #add plot title
  theme_bw()+ #removes gray background
  theme(plot.title = element_text(hjust = 0.5)) + #centers title
  scale_color_manual(labels = c("<50k", "50k-100k", ">100k"), values = c("red", "green", "blue")) #change legend labels




### Q2: How to get the 95% CI for regressions?

# crude - only one independent variable (our primary exposure)
lin_c <- lm(srs ~ fruit, data = d1)

summary(lin_c)

#find confidence intervals
confint(lin_c)

#If the confidence interval for ?? contains 0, it can be concluded there is no significant
  #evidence of a linear relationship between IV and DV




### Q3: how to get the OR for the logistic regression? 

## crude
d1$asd <- relevel(d1$asd, ref = "0") #setting my reference to 0 just in case
#this isn't the default

log_c <- glm(asd ~ fruit, family="binomial", data=d1)

summary(log_c)
#The logistic regression coefficients give the change in the log odds of the
  #outcome for a one unit increase in the predictor variable

## odds ratios only
exp(coef(log_c)) #saying you want to exponentiate (exp), and that the object you 
  #want to exponentiate is called coefficients and it is part of log_c

## odds ratios and 95% CI
exp(cbind(OR = coef(log_c), confint(log_c)))

#UCLA resource: https://stats.oarc.ucla.edu/r/dae/logit-regression/




### Q4: exporting the entire console in R

#Resource: https://statisticsglobe.com/r-save-all-console-input-output-to-file

#put this at beginning of your R script
RWorkshop_log <- file("RWorkshop_log.txt") # File name of output log

sink(RWorkshop_log, append = TRUE, type = "output") # Writing console output to log file
sink(RWorkshop_log, append = TRUE, type = "message")

cat(readChar(rstudioapi::getSourceEditorContext()$path, # Writing currently opened R script to file
             file.info(rstudioapi::getSourceEditorContext()$path)$size))


#put this at end of your R script
closeAllConnections() # Close connection to log file

#Now txt log file should be in your working directory
#The first part of the log file contains the R script itself (i.e. the console 
  #input) and the second part of the log file contains the RStudio console output.
#this does not include plots!





# More Resources ----------------------------------------------------------

# software carpentry: https://software-carpentry.org/

# Code Academy: https://www.codecademy.com/catalog/language/r

# Drexel courses: https://catalog.drexel.edu/coursedescriptions/quarter/undergrad/info/ "INFO 332 Exploratory Data Analytics"

# R support slack: coming soon!