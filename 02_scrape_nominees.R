library(tidyverse)
library(httr)  
library(rvest) 
library(janitor)
library(writexl)

### STEP BY STEP #### ---------------------------------------------------------

#set url for nominees list page
url <- "https://buildbackbetter.gov/the-administration/nominees-and-appointees/"

#perform the GET call
website1 <- GET(url)

#let's see what we have in content
print(content(website1))


#grab the names
names <- html_nodes(content(website1), "h3")
#show them all
print(html_text(names, trim = TRUE))
#show just one
name1 <- print(html_text(names, trim = TRUE)[[1]])
name1


#grab the titles
titles <- html_nodes(content(website1), "h4")
#show them all
print(html_text(titles, trim = TRUE))
#show just one
title1 <- print(html_text(titles, trim = TRUE)[[1]])
title1


#grab the links
links <- html_nodes(content(website1), "a.full-link")
#show them all
html_attr(links, 'href')
#show just one
link1 <- html_attr(links, 'href')[[1]]
link1


df <- data.frame("name" = name1, "title" = title1, "link" = link1)
df



### FUNCTION FOR SCRAPING NOMINEES #### 

#set url for transition list page
scrape_names <- function(namenum) {
  #set url for nominees list page
  url <- "https://buildbackbetter.gov/the-administration/nominees-and-appointees/"
  #perform the GET call
  website1 <- GET(url)
  #grab the names
  names <- html_nodes(content(website1), "h3")
  #isolate just one
  name1 <- print(html_text(names, trim = TRUE)[[namenum]])
  #grab the titles
  titles <- html_nodes(content(website1), "h4")
  #show just one
  title1 <- print(html_text(titles, trim = TRUE)[[namenum]])
  #grab the links
  links <- html_nodes(content(website1), "a.full-link")
  #show just one
  link1 <- html_attr(links, 'href')[[namenum]]
  #combine into dataframe
  df <- data.frame("name" = name1, "title" = title1, "link" = link1)
  
  return(df)

}

#run function once
scrape_names(1)



#### LOOP THROUGH ALL NAMES ####

#we'll use the map function to loop through all the names at once and combine

#get length of how many names on the page
myurl <- "https://buildbackbetter.gov/the-administration/nominees-and-appointees/"
website1 <- GET(myurl) 
names <- html_nodes(content(website1), "h3")
num_names_nominees <- length(html_text(names, trim = TRUE))
#we'll use this length to create sequential numbers vector to match
num_names_nominees_sequence <- seq(1, num_names_nominees)
num_names_nominees_sequence

#now we'll feed the sequence of numbers into the function
nominees_data_scraped <- map_df(num_names_nominees_sequence, scrape_names)

#add a unique ID field string
nominees_data_scraped <- nominees_data_scraped %>% 
  mutate(
    idstring = str_trim(paste0(name, title))
  ) %>% 
  as_tibble()

nominees_data_scraped


# SAVE RESULTS ####
saveRDS(nominees_data_scraped, "processed_data/nominees_data_scraped.rds")
write_xlsx(nominees_data_scraped, "processed_data/nominees_data_scraped.xlsx")

#save archived copy to use for identifying changes later on
filestring <- paste0("archived_data/nominees_data_archived_", Sys.time())
filestring <- str_replace_all(filestring, "-", "_")
filestring <- str_replace_all(filestring, ":", "_")
filestring <- str_replace(filestring, " ", "t")
#remove seconds for clarity
filestring <- str_sub(filestring, 1, -4L)
#add file extension
filestring <- paste0(filestring, ".rds")
#run the string through and save the file
saveRDS(nominees_data_scraped, filestring)




#### SCRAPING WH STAFF LIST ###### ------------------------------------

#Because the staff page uses the same structure, we can repurpose our 
# existing function from above to capture this page's names as well


#set url for transition list page
scrape_names_staff <- function(namenum) {
  #set url for nominees list page
  url <- "https://buildbackbetter.gov/the-administration/white-house-senior-staff/"
  #perform the GET call
  website1 <- GET(url)
  #grab the names
  names <- html_nodes(content(website1), "h3")
  #isolate just one
  name1 <- print(html_text(names, trim = TRUE)[[namenum]])
  #grab the titles
  titles <- html_nodes(content(website1), "h4")
  #show just one
  title1 <- print(html_text(titles, trim = TRUE)[[namenum]])
  #grab the links
  links <- html_nodes(content(website1), "a.full-link")
  #show just one
  link1 <- html_attr(links, 'href')[[namenum]]
  #combine into dataframe
  df <- data.frame("name" = name1, "title" = title1, "link" = link1)
  
  return(df)
  
}

#run function once
scrape_names_staff(1)



#### LOOP THROUGH ALL NAMES ####

#get length of how many names on the page
myurl2 <- "https://buildbackbetter.gov/the-administration/white-house-senior-staff/"
website2 <- GET(myurl2) 
names2 <- html_nodes(content(website2), "h3")
num_names_staff <- length(html_text(names2, trim = TRUE))
#we'll use this length to create sequential numbers vector to match
num_names_staff <- seq(1, num_names_staff)
num_names_staff

#now we'll feed the sequence of numbers into the function
staff_data_scraped <- map_df(num_names_staff, scrape_names_staff)

#add a unique ID field string
staff_data_scraped <- staff_data_scraped %>% 
  mutate(
    idstring = str_trim(paste0(name, title))
  ) %>% 
  as_tibble()

staff_data_scraped


# SAVE RESULTS ####
saveRDS(staff_data_scraped, "processed_data/staff_data_scraped.rds")
write_xlsx(staff_data_scraped, "processed_data/staff_data_scraped.xlsx")

#save archived copy to use for identifying changes later on
filestring <- paste0("archived_data/staff_data_archived_", Sys.time())
filestring <- str_replace_all(filestring, "-", "_")
filestring <- str_replace_all(filestring, ":", "_")
filestring <- str_replace(filestring, " ", "t")
#remove seconds for clarity
filestring <- str_sub(filestring, 1, -4L)
#add file extension
filestring <- paste0(filestring, ".rds")
#run the string through and save the file
saveRDS(staff_data_scraped, filestring)

