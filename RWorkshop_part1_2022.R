################################################################################

###############    MRF In Action: Intro to epi analysis in R     ###############

################################################################################


# The Basics --------------------------------------------------------------

## part 1: install packages (only need to run once)

install.packages("readxl") #for importing excel files

install.packages("tidyverse") #collection of R packages including ggplot2 (visualizations),
  #dplyr (data manipulation), tidyr (tidying data)

install.packages("skimr") #for descriptive stats


## part 2: load packages
library(readxl)
library(tidyverse)
library(skimr)


## part 3: set working directory

setwd("C:/Users/jr3639/OneDrive - Drexel University/Documents/students/RWorkshop2022") #insert your filepath here


## part 4: import dataset

d1 <- read_excel("Rworkshop_dataset.xlsx")


## sidenote: here's how you would export an excel file

install.packages("writexl")
library("writexl")

write_xlsx(d1,"dataexample.xlsx")



# Data Exploration and Visualization ----------------------------------------------------

View(d1)

dim(d1) #gives dimensions of the table


## part 1: summary stats

summary(d1) #shows datatype and other attributes; for numeric variables it also
  #displays min, 1st quartile, median, mean, 3rd quartile and max values

#something funny is going on...
class(d1$asd) #"numeric"
d1$asd=factor(d1$asd)

class(d1$asd) #"factor"

#now do this for the other categorical variables too
d1$prenatal_vitamin=factor(d1$prenatal_vitamin)
d1$income=factor(d1$income)
d1$smoking=factor(d1$smoking)

summary(d1) #looks better!

skim(d1) #displays most of the numerical attributes from summary, and also missing
  #values, more quantile information and an inline histogram for each variable


## part 2: basic distributions of variables

# make a plot of the distribution of the fruit variable
ggplot(data = d1) +
  geom_histogram(aes(x = fruit))

# so what you're doing here is saying:
# 1) I want to make a plot and I want it to use the d1 dataset
# 2) I want to display a histogram of the fruit variable

#notice the warning message that pops up below - this means the data is being 
  #divided into 30 'bins' to display in the chart and R is prompting you that you
  #probably need more

# let's try using 60 bins to display the data instead of the default 30
ggplot(data = d1) +
  geom_histogram(aes(x = fruit), bins = 60)
#it looks like a pretty normal distribution

ggplot(data = d1) +
  geom_histogram(aes(x = srs), bins = 60)
#close to normally distributed


## part 3: more visualizations

## Barchart - one discrete/categorical variable
ggplot(data=d1) + 
  geom_bar(mapping=aes(x=asd))

# now let's add color
ggplot(data=d1) + 
  geom_bar(mapping=aes(x=asd, fill=asd))

#anything in your aes() part of your command is something that is defined by the data
  #so obviously your x variables is in your data
#above, I colored the bars by ASD status, so goes inside aes()
#but I wanted to just make the bars blue, that would not go inside aes()
  #because it isn't determined by the data

ggplot(data=d1) + 
  geom_bar(mapping=aes(x=asd), fill="blue")


## Boxplot - one continuous and one categorical variable
ggplot(data=d1) + 
  geom_boxplot(mapping=aes(x=income,y=srs))
#this plot shows the "minimum" (Q1-1.5*IQR), "maximum" (Q3+1.5*IQR), median, first
  #quartile and third quartile
#it would show outliers if we had them!

# let's make it fancy
ggplot(data=d1) + 
  geom_boxplot(mapping=aes(x=income,y=srs, fill=income), outlier.colour="black", outlier.shape=16, #changing appearance of outliers (which we don't have)
               outlier.size=2, notch=TRUE)
#notch = TRUE - notch displays a confidence interval around the median which is 
  #normally based on the median +/- 1.58*IQR/sqrt(n). Notches are used to compare 
  #groups; if the notches of two boxes do not overlap, this is a strong evidence 
  #that the medians differ


## Scatterplot - two continuous variables

ggplot(data=d1) +
  geom_point(mapping = aes(x=fruit, y=srs))

# adding color
ggplot(data=d1) +
  geom_point(mapping = aes(x=fruit, y=srs, color=income))

## Different Facets 
# adding a facet_wrap() lets you break apart the charts by another variable in the dataset
# you can specify nrow or ncol to tell it how many rows or columns to arrange the charts in
# When you call facet_wrap(), you have to add a ~ in front of the variable you're faceting on

ggplot(data=d1) +
  geom_point(mapping = aes(x=fruit, y=srs, color=income))+
  facet_wrap(~ income, nrow = 2) 

## Multiple geoms, one plot 
# here's an example of how things can stack on top of each other
ggplot(data = d1) + 
  geom_point(mapping=aes(x=fruit, y=srs, color=income)) + 
  geom_smooth(method="lm",mapping=aes(x=fruit, y=srs)) #adding a regression line

#lets make it look better!
ggplot(data = d1) + 
  geom_point(mapping=aes(x=fruit, y=srs, color=income)) + 
  geom_smooth(method="lm",mapping=aes(x=fruit, y=srs))+
  ylab("SRS Score") + #add y axis title
  xlab("Fruit Intake (SPD)") + #add x axis title
  scale_y_continuous(breaks = c(0,30,60,90)) + #customize y axis scale
  ggtitle("SRS Score by Fruit Intake in EARLI") + #add plot title
  theme_bw()+ #removes gray background
  theme(plot.title = element_text(hjust = 0.5)) #centers title


# there are a lot of fun things you can do with themes if you want to do some googling
# for example, there's a theme you can use that makes your charts look like charts in The Economist
install.packages("ggthemes")
library(ggthemes)

ggplot(data = d1) + 
  geom_point(mapping=aes(x=fruit, y=srs, color=income)) + 
  geom_smooth(method="lm",mapping=aes(x=fruit, y=srs))+
  ylab("SRS Score") +
  xlab("Fruit Intake (SPD)") +
  scale_y_continuous(breaks = c(0,30,60,90)) +
  ggtitle("SRS Score by Fruit Intake in EARLI") +
  theme_economist()


## Exporting plots
#you can always click "export" in the output window to save a plot, or you can
  #automate it with the following code

#first save your plot as an object
p1 <- ggplot(data=d1) +
  geom_point(mapping = aes(x=fruit, y=srs))+
  geom_smooth(method="lm",mapping=aes(x=fruit, y=srs))

#then export as a jpeg - it will save to your working directory folder
jpeg(filename="srs_fruit_scatter.jpg")
p1
dev.off()



# Data Analysis ----------------------------------------------------

## Part 1: linear regression
#LR is a model that uses a straight line to describe the relationship between 
  #variables.

# crude - only one independent variable (our primary exposure)
lin_c <- lm(srs ~ fruit, data = d1)

summary(lin_c)

# adjusted - multiple indpendent variables (primary exposure and covariates)
lin_a <- lm(srs ~ fruit + prepreg_bmi + maternal_age, data = d1)

summary(lin_a)

#now adding a categorical covariate to the adjusted model
lin_a2 <- lm(srs ~ fruit + prepreg_bmi + maternal_age + income, data = d1)

summary(lin_a2)
#notice that income is a categorical variable so R created dummy variables for me
#however... R is assuming the the ref for income is 1 but actually most people in
  #the sample are in income level 3 (60/154)

d1$income <- relevel(d1$income, ref = "3")
#this tells R that I want level 3 (income 100k or more) to be the referent

lin_a2 <- lm(srs ~ fruit + prepreg_bmi + maternal_age + income, data = d1)

summary(lin_a2)
#it worked!




# Part 2: logistic regression

## crude
d1$asd <- relevel(d1$asd, ref = "0") #setting my reference to 0 just in case
  #this isn't the default

log_c <- glm(asd ~ fruit, family="binomial", data=d1)

summary(log_c)

#The logistic regression coefficients give the change in the log odds of the
  #outcome for a one unit increase in the predictor variable

## adjusted

log_a <- glm(asd ~ fruit + prepreg_bmi + maternal_age, family="binomial", data=d1)

summary(log_a)

#adding a categorical covariate
log_a2 <- glm(asd ~ fruit + prepreg_bmi + maternal_age + income, family="binomial", data=d1)

summary(log_a2)


## export regression results table to .txt

install.packages("stargazer")
library(stargazer)

x<-stargazer(log_a2, type = "text")
write.table(x, file = "logreg_adjusted.txt", sep = ",", quote = FALSE, row.names = F)

# this creates a .txt file but you can import that into word by:
  # 1. Copy and paste the content of the .txt file into Word
  # 2. In Word, select the text you just pasted from the .txt file
  # 3. Go to Table ??? Convert ??? Convert Text to Table.
  # 3. make sure "Commas" is selected under "Separate text at", click OK
