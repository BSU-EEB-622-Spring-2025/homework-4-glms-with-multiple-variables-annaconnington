## Homework 4 Submission ## 

library(AICcmodavg)
library(agricolae)
library(ape) 
library(broom)
library(car)
library(corrplot)
library(dplyr)
library(gapminder)
library(ggeffects)
library(gmodels)
library(ggplot2)
library(gt)
library(ggtext)
library(kableExtra)
library(marginaleffects)
library(MASS)
library(MLmetrics)
library(modelr)
library(MuMIn)
library(performance)
library(pixiedust)
library(plyr)
library(pROC)
library(tidyverse)

##I'm still messing with the code here. I am struggling a bit getting it to where I want it to be and feeling confident in interpretations. I plan to attend office hours when we come back from break. I appreciate your understanding in offering me the 48 hr extension, and I apologize that this work isn't complete in the intended way before the extended deadline.

## Question 1:
mistletoe <- read.csv("mistletoes.csv")
head(mistletoe)
mistletoe$Year <- as.character(mistletoe$Year) #this will change the variable "Year" from an int to chr, to tell R that the year is a category and not a continuous number
mistletoe$Treatment <- factor(mistletoe$Treatment, levels = c("unparasitized", "parasitized")) # I did this to make "unparasitized" the highest level in the Treatments to make it the baseline. This helps me to read the summary table output with a bit more ease. 
str(mistletoe)

## 1a)


m_mod <- glm.nb(Seedlings~Treatment, #seedlings is the response variable which I have set against a potential predictor variable, which is the treatment of parasitized or unparasitized
                data=mistletoe) #this is my data
##I am using a negative binomial as the response variable is discrete without the possibility of entering negative numbers and will also account for overdispersed data. By using nbinom it is able to best analyze my variables under those conditions. 
summary(m_mod) #int:2.5733, slope:-3.1575 

performance::mae(m_mod) #the returned value of MAE is 145.841 which seems quite high which is not ideal

## 1b)
plot_predictions(m_mod, condition="Treatment")
predictions(m_mod, new=data.frame(Treatment=c("parasitized", "unparasitized"))) #The number of seedlings predicted for a parasitized tree is 308.2 and for an unparasitized tree is 13.1.
marginaleffects::comparisons(m_mod, condition="Treatment", 
                             newdata=data.frame(Treatment=c("parasitized"))) #produces the difference between predicted values between treatment groups

## There is a signficant difference in the number of seedlings under each tree between the treatment groups ("parasitized" vs "Unparasitized").For a tree not parasitized by misletoe, it has a predicted number of seedlings of 2.5. When trees are parasitized that prediction increases with a slope of 3.15 seedlings.

## 1c) 
my_mod <- glm.nb(Seedlings~Treatment*Year, 
                 data=mistletoe)
summary(my_mod)
#The number of seedlings predicted for a parasitized tree is 308.2 and for an unparasitized tree is 13.1.

plot_predictions(my_mod, condition=c("Treatment", "Year"))
##There is a significant difference in the number of mistletoe seedlings between years as well as varying effect sizes between treatment groups in each year.

#I'm struggling to figure out to code to get the predictions specifically, even though the graph is showing a visible difference


## Question 2:
treemortality <- read.csv("treemortality.csv")
head(treemortality)
str(treemortality)

## 2a)
mod.agg<- glm(mortality~thinning, 
              data=treemortality, family="binomial")


plot_predictions(mod.agg, condition="thinning") + 
  ylab("Probability of tree mortality") + # Add label axes
  xlab("Thinning Treatment") + # Add label axes
  theme_bw()

coef(mod.agg)
summary(mod.agg)
confint(mod.agg)
## Without the effect of thinning, the likelihood of a tree surviving is at 99.33%. However, when thinning is performed, the probability of a tree surviving decrease at a rate of -1.855.

## 2b) It might be useful to consider how the size of the trees themselves might impact tree mortality. However, for the purpose of this system and research question, randomizing the sizing of the trees in the sample would be sufficient to  try and remove it as a factor in the model.



## 2c) 
mod.agg2<- glm(mortality~thinning*slope + roaddist, 
               data=treemortality, family="binomial")

##I'm having trouble with the predictions for the code after added more x's

plot_predictions(mod.agg2, condition=c("thinning", "slope", "roaddist") + 
                   ylab("Probability of tree mortality") + # Add label axes
                   xlab("Thinning Treatment") + # Add label axes
                   theme_bw()
                 
                 coef(mod.agg2)
                 summary(mod.agg2)
                 confint(mod.agg2)