#Load libraries
library(tidyverse)
library(gglorenz)


#Load data
dat <- read.csv(file ="seg_input_data_swd.csv")


#All variable view and summary
View(dat)
str(dat)
summary(dat)
glimpse(dat)


# Motivation: Lorenz curves of cost/usage
dat %>% 
    filter(util.Spend.Total > 0) %>% 
    ggplot(aes(util.Spend.Total)) + stat_lorenz(desc = TRUE) +
    geom_abline(linetype = "dashed")


#Labelling and accessing column headers
str(dat)
dat %>% select(starts_with("util.Activity")) %>%  head()
dat %>% select(ends_with("Community")) %>%  head()
dat %>% select(contains("Spend")) %>%  head()


#Some initial insights
hist(dat$att.Age)
hist(dat$att.Long_term_conditions_count, breaks = 0:10)
hist(dat$util.Spend.Total, breaks = 100)

quantile(dat$util.Spend.Total, probs = seq(0,1,0.1))

dat %>% 
    select(contains("Spend")) %>%  
    summarise_all(mean)


dat %>%
    filter(att.Long_term_conditions_count < 2) %>% 
    select(contains("Spend")) %>% 
    summarise_all(mean)

dat %>%
    filter(att.Age > 65) %>% 
    select(contains("Spend")) %>% 
    summarise_all(mean)


#LTC/age spend box plots
dat %>% 
    ggplot(aes(x = att.Long_term_conditions_count, 
               y = util.Spend.Total, 
               group = att.Long_term_conditions_count)) + 
    geom_boxplot(outlier.shape = NA) + ylim(c(0, 15000))


dat %>% 
    mutate(age_group = cut(dat$att.Age, seq(0,100,5))) %>% 
    ggplot(aes(x = age_group, 
               y = util.Spend.Total, 
               group = age_group)) +
    geom_boxplot(outlier.shape = NA) + ylim(c(0, 10000))







