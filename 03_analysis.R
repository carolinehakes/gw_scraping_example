library(tidyverse)
library(janitor)
library(writexl)

#to run new fresh scrape(s), uncomment out one or both of these lines
#otherwise can leave commented out and use existing saved datasets

# source("01_scrape_agencyteams.R")
# source("02_scrape_nominees.R")



#### AGENCY TEAMS ##### --------------------------------------------------------


### COMPARE agency team members with previous archived version ######

#load current data
transition_data_current <- readRDS("processed_data/transition_data_scraped.rds")
transition_data_current

# load archived data to compare against
transition_data_previous <- readRDS("archived_data/transition_data_archived_2020_11_23t16_13.rds")
# transition_data_previous <- readRDS("archived_data/transition_data_archived_2020_11_23t16_13_07.rds")
transition_data_previous

#find new records of names added since previous woth anti-join
newnames <- anti_join(transition_data_current, transition_data_previous, by = "idstring")

#let's see what we've got!
newnames 



# TOTALS 
# Compare totals by department #######

agencycount_current <- transition_data_current %>% 
  count(agency, name = "current_count")

agencycount_current

agencycount_previous <- transition_data_previous %>% 
  count(agency, name = "previous_count")

agencycount_previous

#join
agencycount_compare <- left_join(agencycount_current, agencycount_previous, by = "agency")
agencycount_compare

#add change columns
agencycount_compare <- agencycount_compare %>% 
  mutate(
    change = current_count - previous_count
  )




#### Analysis of current agency team members ############

#we'll create a newly named object to use from here on out 
agencyteams <- transition_data_current


#quick counts
agencyteams %>% 
  count(agency, sort = TRUE)

agencyteams %>% 
  count(most_recent_employment, sort = TRUE)

agencyteams %>% 
  count(source_of_funding, sort = TRUE)


employers_count <- agencyteams %>% 
  count(most_recent_employment, sort = TRUE)
employers_count
#note: the employment field likely needs to be standardized...Deliotte, etc

agencyteams %>% 
  filter(most_recent_employment == "Georgetown University") 



### EXPORTS FOR SHARING #### ----------------

#names of new agency review team members
newnames %>% 
  select(-idstring) %>% 
  write_xlsx("output/newnames.xlsx")

#aggregate county of agency totals compared
write_xlsx(agencycount_compare, "output/agencycount_compare.xlsx")

#entire combined agency teams file
agencyteams %>% 
  select(-idstring) %>% 
  write_xlsx("output/agencyreviewteams.xlsx")

