########################
# WORKSHOP 3 (Age seg)
########################

#Pre-amble
library(tidyverse)
library(treemap)
library(rpart)
library(rpart.plot)

#Load data
dat <- read.csv(file ="seg_input_data_swd.csv")



# Format for segmentation methods 
# [] 0. Do the segmentation
# [] 1. View
# [] 2. Reduction in var
# [] 3. Attributes of group




# [Age] 0. Do the segmentation
dat <- dat %>% 
    mutate(segment.age = case_when(att.Age <= 17 ~ "0-17yr",
                                   att.Age >= 65 ~ "65+yr",
                                   TRUE ~ "18-64yr"))


# [Age] 1. View 
dat %>% 
    group_by(segment.age) %>% 
    summarise(n = n(),
              spend = mean(util.Spend.Total))



# [Age] 2. Reduction in variance
reduction_in_var <- function(df, segment_field, target){
    
    df$target <- df[[target]]
    var_pop <- var(df$target)
    
    var_group <-  df %>% 
        group_by_at(segment_field) %>%  
        summarise(var = var(target), n = n())
    
    var_within <- sum(var_group$var * var_group$n) / nrow(df)
    
    return(100 * (var_pop - var_within) / var_pop)
}

reduction_in_var(dat, "segment.age", "util.Spend.Total")



# [Age] 3. Attributes of group
dat %>% 
    group_by(segment.age) %>% 
    summarise(spend = mean(util.Spend.Total),
              pop = n(),
              avg_age = mean(att.Age),
              pc_male = sum(att.Sex == "male") / n(),
              avg_imd = mean(att.IMD_level, na.rm = TRUE),
              avg_ltc = mean(att.Long_term_conditions_count)) 





########################
# WORKSHOP 4 (Age + LTC)
########################

# [Age + LTC] 0. Do the segmentation
dat <- dat %>% 
    mutate(segment.ltc = case_when(att.Long_term_conditions_count == 0 ~ "0LTC",
                                   att.Long_term_conditions_count >= 3 ~ "3+LTC",
                                   TRUE ~ "1-2LTC"))
dat <- dat %>% 
    unite("segment.ageltc", segment.age, segment.ltc, sep = ',', remove = FALSE)



# [Age + LTC] 1. View 
dat %>% 
    group_by(segment.ageltc, segment.age, segment.ltc) %>% 
    summarise(n = n(),
              spend = mean(util.Spend.Total)) %>% 
    filter(n > 20) %>% 
    ggplot(aes(segment.age,segment.ltc)) +
    geom_tile(aes(fill = spend), colour = "white")
  


# [Age + LTC] 2. Reduction in variance
reduction_in_var(dat, "segment.ageltc", "util.Spend.Total")



# [Age + LTC] 3. Attributes of group
dat %>% 
    group_by(segment.ageltc) %>% 
    summarise(spend = mean(util.Spend.Total),
              pop = n(),
              avg_age = mean(att.Age),
              pc_male = sum(att.Sex == "male") / n(),
              avg_imd = mean(att.IMD_level, na.rm = TRUE),
              avg_ltc = mean(att.Long_term_conditions_count)) 





########################
# WORKSHOP 5 (DT)
########################

# [RT] 0. Do the segmentation
model_dt_example <- rpart(util.Spend.Total ~ att.Age + att.Long_term_conditions_count,
                   data = dat,
                   control = list(minbucket = 50, maxdepth = 3, cp = 0.001))



atts <- dat %>%  select(starts_with("att")) %>% colnames()
model_dt <- rpart(as.formula(paste("util.Spend.Total ~", paste(atts, collapse = "+"))),
                   data = dat,
                   control = list(minbucket = 50, maxdepth = 3, cp = 0.001))



# [RT] 1. View 
rpart.plot(model_dt_example)
rpart.plot(model_dt)



# [RT] 2. Reduction in variance
dat$segment.dt <- model_dt$where 
reduction_in_var(dat, "segment.dt", "util.Spend.Total")



# [RT] 3. Attributes of group
dat %>% 
  group_by(segment.dt) %>% 
  summarise(spend = mean(util.Spend.Total),
            pop = n(),
            avg_age = mean(att.Age),
            pc_male = sum(att.Sex == "male") / n(),
            avg_imd = mean(att.IMD_level, na.rm = TRUE),
            avg_ltc = mean(att.Long_term_conditions_count)) 





########################
# WORKSHOP 6 (BTH)
########################

# [BTH] 0. Do the segmentation
dat <- dat %>% 
    mutate(segment.bth = case_when(att.Dementia == 1 | att.EFI_classification %in% c("Moderate","Severe") ~ "Frailty",
                                   att.Heart_failure == 1 | att.CHD == 1 | att.CKD == 1 ~ "Limited reserve",
                                   att.Cancer == 1 ~ "Cancer",
                                   att.Learning_disability == 1 | att.Blindness == 1 |  att.Deafness == 1 ~ "Disability",
                                   att.Long_term_conditions_count > 1 ~ "Long-term conditions",
                                   att.Sex == "female" & util.Activity.Maternity > 0 ~ "Maternity",
                                   util.Activity.NEL_admits > 0 ~ "Acutely ill",
                                   TRUE ~ "Healthy"))


# [BTH] 1. View 
dat %>% 
    group_by(segment.bth) %>% 
    summarise(n = n()) %>% 
    treemap(index = "segment.bth", vSize = "n")

dat %>% 
    group_by(segment.bth) %>% 
    summarise(n = n(), cost = mean(util.Spend.Total)) 
    


# [BTH] 2. Reduction in variance
reduction_in_var(dat, "segment.bth", "util.Spend.Total")



# [BTH] 3. Attributes of group
dat %>% 
    group_by(segment.bth) %>% 
    summarise(spend = mean(util.Spend.Total),
              pop = n(),
              avg_age = mean(att.Age),
              pc_male = sum(att.Sex == "male") / n(),
              avg_imd = mean(att.IMD_level, na.rm = TRUE),
              avg_ltc = mean(att.Long_term_conditions_count)) 