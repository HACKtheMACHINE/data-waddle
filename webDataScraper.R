#### Data Scraper Template #####################################################

### Libraries ##################################################################
install.packages("magrittr")
install.packages("tidyverse")
install.packages("rvest")
library(magrittr) # Pipe operator
library(tidyverse) # data tidying package
library(rvest) # html scraping package

### Scrape Data ################################################################
data.scraper <- rep("a", 400)
for(i in 1:400) {
  data.scraper[i] <- 
    read_html(paste0()) %>% # enter URL to be scraped
    html_node() %>% # CSS tag (Use chrome inspector)
    html_text
}

### Export Data ################################################################
write.csv(data.scraper, "scrapeddata.csv") # Exports file to .csv
data.scraper <- read.csv("scrapeddata.csv") # imports data back as dataframe
