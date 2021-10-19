# CRYPTO PUNKS NFT Project -- 
# Scraper for all descriptive and transactional data of the first NFT project in the crypto space 

# 1.0 Load libraries

library(fs)
library(tidyverse)
library(stringr)
library(rvest)
library(glue)
library(splitstackshape)

# git
library(usethis)
use_git_config(user.name = 'Albert Campillo', user.email ='albertcampillo@gmail.com')


## 1.1 Directory setup ----

fs::dir_create('00_data/data_raw')



## 2.0 'PUNKS' data set ----
## Obtain the main features of all 9,999 punks within the NFT project (id, owner and punk features, mainly)  


## 2.1. Scraper setup ----

# 2.1.1. Empty data frame 
punks <- 
    setNames(
        data.frame(matrix(ncol = 5, nrow = 0)),
        paste0(c("punk_id", "punk_img", "punk_owner", "punk_type", "punk_attr" ))
        )



# 2.1.2. Populate data frame for all 9,999 punks

for (i in 1:9999){
    url <- glue('https://www.larvalabs.com/cryptopunks/details/{i}') %>% read_html()
    
    punk_id <- html_nodes(url,'.active') %>% html_text()
    
    img <- sprintf("%04d", i)
    punk_img <-glue("https://www.larvalabs.com/public/images/cryptopunks/punk{img}.png")
    
    punk_owner <- html_nodes(url,xpath = '//*[@id="punkDetails"]/div[3]/div/div[1]/a') %>% html_attr("href")
    
    punk_type <-html_nodes(url, xpath = '//*[@id="punkDetails"]/div[1]/div[2]/h4/a') %>% html_text()
    
    punk_attr <- html_nodes(url,'.col-md-4 a') %>%
        html_text() %>%
        .[-(0:4)] %>%
        paste(collapse = ",") %>%
        head()
    
    # create structured list
    punk_temp <- data.frame(punk_id   = punk_id,
                            punk_img  = punk_img,
                            punk_owner = 
                                if(identical(punk_owner, character(0))== TRUE)
                                {"NA"} else {print(gsub(".*\\=", "",  punk_owner))}, # keep owner_id only 
                            punk_type = punk_type, 
                            punk_attr = punk_attr)
    
    # update data frame
    punks <- rbind(punk_temp, punks)
}


# 2.1.3. Save as rds file

punks %>% write_rds(path = '00_data/data_raw/punks.rds')


# Clear remain objects
rm(i, img, url, punk_id, punk_img, punk_owner, punk_type, punk_attr, punk_temp)





# 3.0. 'TRANSACTION HISTORY' data set ----
## Get all transactional data for Crypto Punks since first mint date. This includes Assign, Bids, Bids Withdraws
## and Actual sales between individuals


## 3.1. Scraper setup ----

# 3.1.1. Empty data frame 
punks_trans <- 
    setNames(
        data.frame(matrix(ncol = 7, nrow = 0)),
        paste0(c("Type", "amount_eth", "amount_usd", "Txn", "punk_id", "From", "To"))
    )



# 3.1.2. Populate data frame for all 9,999 punk-related transaction

for (i in 1:9999){


    url <- glue('https://www.larvalabs.com/cryptopunks/details/{i}') %>% read_html()

    # scrap main table
    table <- html_nodes(url, xpath = '//*[@id="punkHistory"]/div/table') %>% 
        html_table() %>%
        as.data.frame() %>%
        # Remove 'From' & 'To' columns 
        .[ !(names(.) %in% c("From", "To"))]
    
    # set row element
    trows <- html_nodes(url, xpath = '//*[@id="punkHistory"]/div/table') %>% html_nodes("table, tr")
    
    # scrap the owner_id for the 'From' column
    links_from <-trows %>% html_node("td:nth-child(2) a") %>% 
        html_attr("href") %>%
        as.data.frame() %>%
        .[-1,] %>%
        as.data.frame() %>%
        rename(., From = .) %>%
        mutate(From = (gsub(".*\\=", "",  From)))
    
    # scrap the owner_id for the 'To' column    
    links_to <-trows %>% html_node("td:nth-child(3) a") %>% 
        html_attr("href") %>%
        as.data.frame() %>%
        .[-1,] %>%
        as.data.frame() %>%
        rename(., To = .) %>%
        mutate(To = (gsub(".*\\=", "",  To)))
    
    punks_trans_temp <- 
        cbind(table, From = links_from, To = links_to) %>%
        separate(Amount, sep = " ", into =c("amount_eth", "amount_usd")) %>%
        mutate(amount_usd = gsub("[()]", "", amount_usd),
               amount_eth = gsub('.{1}$', '', amount_eth),
               punk_id = i
        )
    
    # update data frame
    punks_trans <- rbind(punks_trans_temp, punks_trans)

}

# 2.1.3. Save as rds file

punks_trans %>% write_rds(path = '00_data/data_raw/punks_trans.rds')


# Clear remain objects
rm(table, url, i,trows, amount_usd, amount_eth, punk_id, trans_temp, trans_temp, links_from, links_to)














