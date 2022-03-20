#### Preamble ####
# Purpose: Clean the survey data downloaded from Canadian General Social Survey.
# Author: Yilin Lan, Xuetong Tang, Lida Liu
# Data: 19 March 2021
# Contact: eileen.lan@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)
# Read the download data. 
data <- read.csv("AAG02j38.csv")
# Remove some leverage points
filter(agec!=96|97|98|99|80,asepma0c>25&asepma0c<60,agema0c>15&agema0c<60,
       ma0_133==1|2,ma0_220==2|1,totchdc!=96|97|98|99,ma0_220!=7)%>%
# Create some new variables
mutate(current_marriage_time = asepma0c-agema0c, 
       age_difference=abs(aprma0c-agema0c),
       ma0_220=as.factor(ma0_220),ma0_133=as.factor(ma0_133))%>%
# Keep some variables that we are interested in.
select(age_respondent,current_marriage_time,age_difference,
       ageofmarriage_respondent,children_born,is_first_marriage)
         

# Create a histogram showing the distribution of the marriage time
clean_data %>% ggplot(aes(x=current_marriage_time))
+geom_histogram(fill="blue",color="white")+theme_classic()
+labs(x="Marriage time",title="Histogram of Marriage time")
+theme(plot.title = element_text(size=10))

# Create a summary table
summary_table<-clean_data%>%summarise(
  median=median(current_marriage_time),
  mean=mean(current_marriage_time),
  sd=sd(current_marriage_time),
  IQR=quantile(current_marriage_time,0.75)-quantile(current_marriage_time,0.25),
  max=max(current_marriage_time),
  min=min(current_marriage_time))

# Create A histogram showing the age difference between respondent
# and whose spouse
clean_data %>% ggplot(aes(x=age_difference))
+geom_histogram(fill="blue",color="white")
+theme_classic()+labs(x="Age difference",
                      title="Histogram of age difference between 
                      respondent and whose spouse")
+theme(plot.title = element_text(size=10))

# Scatter plots showing the relationship between marriage time and
# different age components
graph2<-clean_data %>% ggplot(aes(x=ageofmarriage_respondent,
                                  y=current_marriage_time))+geom_point()
+theme_classic()+labs(x="Age at the start of marriage",y="Marriage time",
                      title = "Marriage time vs. Age at start of 
                      first marriage")+theme(plot.title = element_text(size=9))

graph3<-clean_data %>% ggplot(aes(x=age_respondent,y=current_marriage_time))
+geom_point()+theme_classic()+labs(x="Age of respondent",y="Marriage time",
                                   title = "Marriage time vs. Age of 
                                   respondent")
+theme(plot.title = element_text(size=9))

(graph2+graph3)



         