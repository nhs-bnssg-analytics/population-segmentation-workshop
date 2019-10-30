rm(list=ls())


### USER INPUTS ###

file <- "seg_input_data_swd.csv"

seg_by_1 <- 'Total'
seg_by_2 <- 'Spend'
pres_author <- 'Random CCG'



### Required Packages ###

require(tidyverse)
require(knitr) 
require(tidyquant)
require(treemap)
require(RColorBrewer)
require(rpart)
require(rpart.plot)
require(scales)

pres_date <- format(Sys.Date(), "%d %B %Y")
pres_title <- paste0("Population Segmentation by ",seg_by_1," ",seg_by_2)

rmarkdown::render(input = 'popseg_mdcode.Rmd',
                  output_file = paste0(pres_title," for ",pres_author," ",Sys.Date(),'.docx'))





