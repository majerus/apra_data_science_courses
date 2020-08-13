# APRA - Data Science Courses
# This script creates the data sets that will be used across all courses. 
# This includes three data tables: 
# 1. bio
# 2. giving
# 3. engagement  
# Each data table will be available as a .csv, Excel, and a table in a database. 


# to do
# make address data match for donors that share a household
# build address info at household id level
# correlate giving to capacity


# setup----

# load libraries
library(tidyverse)
library(randomNames)
library(lubridate)

# helper functions
# make p (10%) of a vector missing
insert_NAs <- function(x, p = .1) {
  l <- length(x)
  n <- p*l
  i <- sample(1:l, n)
  x[i] <- NA 
  x
}




# define parameters for data set----

# bio_records is the number of unique ids and subsequently the number of rows in the bio table
bio_records <- 100000

# pct_shared_household defines the percetnage defines the percentage of individuals who will share a household id with another individual 
pct_shared_household <- .4

# start and end define the time period for giving and activities covered by the data
start_date  <- Sys.Date() - 5*365
end_date    <- Sys.Date()


# Biographic Table (individual level) ----
# Included variables: 
# Id
# Name
# Household id 
# City
# State
# Country
# Zip
# Deceased
# Birthday 
# Capacity rating(s)

# Data science questions/activities to be built in
# Cleaning zipcodes (+plus, international)
# zipcode and state mismatches
# Learning to exclude deceased individuals from queries, etc. 
# Geocoding addresses based on zipcode
# Handling individual vs. household level data
# Calculate age from birthday (build in wrong dates 1900)
# Bin dates - what is the distribution, how to make this information useful in modeling
# Imputing age if missing 


# id
id <- sample(1000000:9999999, bio_records, replace=F) 

# household_id
# household records is the number of households represented in the data 
# pct_shared_household defines the percetnage defines the percentage of individuals who will share a household id with another individual 
household_records <- bio_records * (1-pct_shared_household)
household_id <- sample(1000000:9999999, household_records, replace=F) 

# expand vector of household ids to be same length as id vector, by duplicing the necessary number of household ids (i.e., placing individuals in the same household)
household_id <- c(household_id, sample(household_id, bio_records - length(household_id), replace=F)) 

# country
# top ten most populated countires are represented in example data
country_data <- tibble(
  country = c("China", "India", "United States", "Indonesia", "Pakistan",
              "Brazil", "Nigeria", "Bangladesh", "Russia", "Mexico"),
  country_proportion = c(.05, .008, .9, rep(.006, 7)),
  country_records = country_proportion * bio_records
)

# top three most populated cities within international countries are reprsented in the sample data
country_cities <- tibble(
  country = sort(rep(
    c("Bangladesh", "Brazil", "China", "India", "Indonesia",  
      "Mexico", "Nigeria", "Pakistan", "Russia", "United States"), 3)),
  city = c("Dhaka", "Chittagong", "Gazipur",
           "São Paulo", "Rio de Janeiro", "Brasília",
           "Shanghai", "Beijing", "Shenzhen",
           "Mumbai", "Kolkata", "Bengaluru",
           "Jakarta", "Surabaya", "Bandung",
           "Mexico City", "Ecatepec", "Guadalajara",
           "Lagos", "Onitsha", "Kano", 
           "Karachi", "Lahore", "Faisalabad",
           "Moscow", "Saint Petersburg", "Novosibirsk",
           NA, NA, NA)
)

country_cities_expanded <- tibble()

for(i in unique(country_cities$country)){
  
  country_cities_expanded <- bind_rows(country_cities_expanded,     
                                       sample_n(country_cities[country_cities$country == i,], 
                                                country_data$country_records[country_data$country == i], 
                                                replace = TRUE)
  )
  
}


# zip, city, state for domestic

# load zipcode data
# zip_data <- read_csv("http://federalgovernmentzipcodes.us/free-zipcode-database-Primary.csv")
# write_csv(zip_data, "free-zipcode-database-Primary.csv")
zip_data <- read_csv("free-zipcode-database-Primary.csv")


zip_data <- 
  zip_data %>% 
  filter(ZipCodeType == "STANDARD",
         Decommisioned == FALSE,
         !is.na(TaxReturnsFiled)) 

zips <- sample(rep(zip_data$Zipcode, zip_data$TaxReturnsFiled/100), 
               country_data$country_records[country_data$country == "United States"], replace = TRUE)

zip_data <- 
  zip_data %>% 
  select(Zipcode, City, State, Lat, Long) %>% 
  rename(zip = Zipcode,
         city = City, 
         state = State, 
         lat = Lat, 
         lon = Long) %>% 
  mutate(city = paste0(toupper(substr(city, 1, 1)), tolower(substr(city, 2, nchar(city)))))

# birthdays
# correlate birthdays with prob of deceased
probs <- 
  tibble(prob_day =  c(rep(.01, 3000), 
                       rep(.05, 3000), 
                       rep(.05, 3000), 
                       rep(.10, 3000), 
                       rep(.20, 3000), 
                       rep(.20, 3000), 
                       rep(.15, 3000), 
                       rep(.15, 3000), 
                       rep(.10, 3000), 
                       # limit last category to match number of days in range of birthdays
                       rep(.10, abs(as.numeric((Sys.Date() - 102*365) - (Sys.Date() - 20*365) + 9*3000))+1)),         
         days = seq(Sys.Date() - 102*365, Sys.Date() - 20*365, by = "day"),
         prob_deceased =  c(rep(.95, 3000), 
                            rep(.85, 3000), 
                            rep(.50, 3000), 
                            rep(.40, 3000), 
                            rep(.20, 3000), 
                            rep(.10, 3000), 
                            rep(.05, 3000), 
                            rep(.05, 3000), 
                            rep(.025, 3000), 
                            # limit last category to match number of days in range of birthdays
                            rep(.025, abs(as.numeric((Sys.Date() - 102*365) - (Sys.Date() - 20*365) + 9*3000))+1))
  )


birthday <- tibble(birthday = sort(sample(seq(Sys.Date() - 102*365, Sys.Date() - 20*365, by = "day"), 
                                          prob = probs$prob_day, 
                                          size = bio_records, 
                                          replace = TRUE)))

birthday <- 
  birthday %>% 
  left_join(probs, by = c("birthday" = "days")) %>% 
  mutate(decade = round(year(birthday), -1))

birthday$deceased <-  
  birthday$prob_deceased %>%
  map_chr(~ sample(c("Y", "N"), 
                   size = 1, 
                   prob = c(.x, 1-.x),
                   replace = TRUE))

# make birthdays random within decade before joining on household id so that households are not compromised of adjacent birthdays
birthday <-
  birthday %>% 
  group_by(decade) %>% 
  sample_frac(size = 1, replace = FALSE)



# create bio table
bio_table <- tibble(
  id = id,
  name = randomNames(bio_records),
  household_id = household_id,
  country = country_cities_expanded$country,
  city = country_cities_expanded$city
)

# add birthdays
bio_table <- 
  bio_table %>% 
  arrange(household_id) %>% 
  bind_cols(birthday = birthday$birthday,
            deceased = birthday$deceased)

# add zips for domestic addresses
bio_table$zip <- ifelse(bio_table$country == "United States", zips, NA)

# join on city, state based on zip
bio_table <- 
  bio_table %>% 
  left_join(zip_data, by ="zip") %>% 
  mutate(city.x = ifelse(is.na(city.x), city.y, city.x)) %>% 
  rename(city = city.x) %>% 
  select(-city.y)

# capacity
capacity_ratings <- tibble(
  rating = c("$100M+",
             "$50M - $100M",
             "$25M - $50M",
             "$10M - $25M",
             "$5M - $10M",
             "2.5M - $5M",
             "$1M - $2.5M",
             "$750k - $1M",
             "$500k - $750k",
             "$250k - $500k",
             "$100k - $250k",
             "$75k - $100k",
             "$50k - $75K",
             "$25k - $50k",
             "$10k - $25k",
             "$5k - $10k",
             "$2.5k - $5k",
             "$1k - $2.5k",
             ">$1k"),
  probability = c(.000001,
                  .000001,
                  .00001,
                  .0001,
                  .0001,
                  .0001,
                  .001,
                  .025,
                  .05,
                  .05,
                  .05,
                  .1,
                  .1,
                  .15,
                  .15,
                  .1,
                  .1,
                  .06,
                  .06))

bio_table$capacity <- sample(x = capacity_ratings$rating, 
                             size = bio_records, 
                             prob = capacity_ratings$probability, 
                             replace = TRUE)

bio_table$capacity_source <- sample(x = c("institutional", "screening"), 
                                    size = bio_records, 
                                    prob = c(.4, .6), 
                                    replace = TRUE)

# race 
# race probabilities based on 2019 US Census Estimates of American population (https://www.census.gov/quickfacts/fact/table/US/PST045219)
bio_table$race <- 
  sample(x = c("Non-Hispanic white",
               "Hispanic or Latino",
               "Black or African American",
               "Asian",
               "Native Americans or Alska Natives",
               "Native Hawaiians and Other Pacific Islanders",
               "Two or more races"),
         size = bio_records,
         prob = c(.6,
                  .19,
                  .13,
                  .06,
                  .013,
                  .002,
                  .03),
         replace = TRUE)

# inject missing data 
bio_table$birthday[bio_table$id %in% sample(bio_table$id, bio_records*.01)] <- as.Date("1/1/1900", "%m/%d/%Y")

bio_table <- 
  bio_table %>% 
  # add NAs
  mutate_at(.vars = vars(deceased, capacity, capacity_source, birthday),
            .funs = insert_NAs) 

write_csv(bio_table, "bio_data_table.csv")


# Giving Table (gift level) ----
# Included variables: 
# Giftid 
# Id
# Credit-type (soft-credit, hard-credit) 
# Gift type (cash, pledge, pledge-payments) <- need to add this yet
# Gift date

# Fund id 
# Fund type (endowment, current use, restricted vs. unrestricted, etc.) 
# Fund name
# Fund description

# Data science questions/activities to be built in
# Not all people in bio table are in gift table
# Who are non-donors? 
#   0’s vs NAs
# Difference between cash, pledges, pledge payments
# What is the first gift date for the individual, household, group? With/without soft-credit?
#   First gift, last gift, largest gift 
# Splits and designations
# Calendar year giving vs. fiscal year giving


# giving data for households with two people
tmp <- filter(bio_table, duplicated(household_id))

households_w_two_people <- 
  bio_table %>% 
  filter(household_id %in% tmp$household_id) %>% 
  select(household_id, id)

giving_table <- tibble(household_id = sample(households_w_two_people$household_id, 
                                             size = nrow(households_w_two_people)*3, 
                                             replace = TRUE),
                       gift_id = sample(1000000:9999999, nrow(households_w_two_people)*3, replace=F)) 

households_w_two_people <- 
  households_w_two_people %>% 
  left_join(giving_table) 

households_w_two_people$credit_type <- ifelse(duplicated(households_w_two_people$gift_id), "Soft-Credit", "Hard-Credit") 


# small gifts - 75% of gifts
amt <- sample(1:2500, 
              size = .75*nrow(households_w_two_people)/2, # divide by two because all gifts will be dupliced for hard and soft credit
              replace = TRUE, 
              prob = 2500:1)

# mid size gifts - 15% of gifts
amt <- c(amt, 
         sample(2500:50000, 
                size = .15*nrow(households_w_two_people)/2, 
                replace = TRUE, 
                prob = 50000:2500))

# leadership size gifts - 7% of gifts
amt <- c(amt, 
         sample(50000:100000, 
                size = .07*nrow(households_w_two_people)/2, 
                replace = TRUE, 
                prob = 100000:50000))

# major gifts 2.95% of gifts 
amt <- c(amt,
         sample(100000:1000000, 
                size = .0295*nrow(households_w_two_people)/2, 
                replace = TRUE, 
                prob = 1000000:100000))

# major gifts .05% of gifts 
amt <- c(amt,
         sample(1000000:10000000, 
                size = .0005*nrow(households_w_two_people)/2, 
                replace = TRUE, 
                prob = 10000000:1000000))

# put amounts in random order
amt <- sample(amt, size = length(amt), replace = FALSE)


gifts <- tibble(gift_amt = amt,
                gift_date = sample(seq(start_date, end_date, by = "day"),length(amt), replace = TRUE))

gifts <- bind_rows(gifts, gifts)

# make number of rows in giving table which length of amt vector by adding necessary number of $100 gifts to amount
if(nrow(households_w_two_people) > nrow(gifts)){
  gifts <- bind_rows(gifts, 
                     tibble(
                       gift_amt = rep(100, nrow(households_w_two_people) - nrow(gifts)),
                       gift_date = end_date))
}

households_w_two_people <- 
  households_w_two_people %>% 
  arrange(gift_id) %>% 
  bind_cols(gifts)


# giving data for households with one person
households_w_one_person <- 
  bio_table %>% 
  filter(!household_id %in% tmp$household_id) %>% 
  select(household_id, id)

giving_table <- tibble(household_id = sample(households_w_one_person$household_id, 
                                             size = nrow(households_w_one_person)*3, 
                                             replace = TRUE),
                       gift_id = sample(1000000:9999999, nrow(households_w_one_person)*3, replace=F)) 

households_w_one_person <- 
  households_w_one_person %>% 
  left_join(giving_table)

households_w_one_person$credit_type <- "Hard-Credit"


# small gifts - 75% of gifts
amt <- sample(1:2500, 
              size = .75*nrow(households_w_one_person), # divide by two because all gifts will be dupliced for hard and soft credit
              replace = TRUE, 
              prob = 2500:1)

# mid size gifts - 15% of gifts
amt <- c(amt, 
         sample(2500:50000, 
                size = .15*nrow(households_w_one_person), 
                replace = TRUE, 
                prob = 50000:2500))

# leadership size gifts - 7% of gifts
amt <- c(amt, 
         sample(50000:100000, 
                size = .07*nrow(households_w_one_person), 
                replace = TRUE, 
                prob = 100000:50000))

# major gifts 2.95% of gifts 
amt <- c(amt,
         sample(100000:1000000, 
                size = .0295*nrow(households_w_one_person), 
                replace = TRUE, 
                prob = 1000000:100000))

# major gifts .05% of gifts 
amt <- c(amt,
         sample(1000000:10000000, 
                size = .0005*nrow(households_w_one_person), 
                replace = TRUE, 
                prob = 10000000:1000000))

# put amounts in random order
amt <- sample(amt, size = length(amt), replace = FALSE)

gifts <- tibble(gift_amt = amt,
                gift_date = sample(seq(start_date, end_date, by = "day"), length(amt), replace = TRUE))

# make number of rows in giving table which length of amt vector by adding necessary number of $100 gifts to amount
if(nrow(households_w_one_person) > nrow(gifts)){
  gifts <- bind_rows(gifts, 
                     tibble(
                       gift_amt = rep(100, nrow(households_w_one_person) - nrow(gifts)),
                       gift_date = end_date))
}

households_w_one_person <- 
  households_w_one_person %>% 
  arrange(gift_id) %>% 
  bind_cols(gifts)

giving_table <- bind_rows(households_w_one_person, households_w_two_people)

giving_table <- filter(giving_table, !is.na(gift_id))

# make names different to require changing
#colnames(giving_table) <- c("household ID", "ID", "gift id", "credit Type", "gift amt", "gift date")


# reduce number of gifts to create more never donors
giving_table <- 
  giving_table %>% 
  slice(round(.3*nrow(giving_table)):nrow(giving_table))


write_csv(giving_table, "giving_data_table.csv")



# Engagement/Prospect Table (individual level) ----
# Included variables
# Id
# Last contact date 
# Number of personal contacts - correlate with total giving
# Assigned - 
# Event attendance
# Volunteer activities
# Count of events attended in last five years
# Digital readership/engagement metric 

# Data science questions/activities to be built in
# Prospect and Engagement Table
# Prospects assigned to former employees
# Errors in date entry (event attended dates in future)
# Prospects assigned to multiple gift officers
# Misspelled volunteer activities that throw off counts, etc. 
# Prospects who are assigned but have not been contacted in over a year
# Scoring / MG Model -> create personas/profiles 
# Engagement score calculation
# Identification new location to host an event


# split out giving data by total giving to create correlations between giving and engagement data
tmp <- 
  giving_table %>% 
  group_by(id) %>% 
  summarise(total_giving = sum(gift_amt)) %>% 
  ungroup() 

never_givers <- 
  bio_table %>% 
  select(id) %>% 
  filter(!id %in% tmp$id) %>% 
  mutate(total_giving = 0)

annual_givers <- 
  tmp %>% 
  filter(total_giving < 10000)

leadership_givers <- 
  tmp %>% 
  filter(total_giving >= 10000 &
           total_giving <  50000) 

mid_level_givers <- 
  tmp %>% 
  filter(total_giving >= 50000 &
           total_giving <  100000)

major_givers <- 
  tmp %>% 
  filter(total_giving >= 100000 &
           total_giving < 1000000)

principal_givers <- 
  tmp %>% 
  filter(total_giving >= 1000000)


# great vector of 20 gift officers
officers <- randomNames(n = 20)

# create vector of potential interests
# interests from https://www.businessinsider.com/billionaire-hobbies-of-richest-people-in-the-world-2016-8
interests <- c("travel", "art", "fashion", "politics", "wine", 
               "boating/sailing", "health/exercise", "cars", "sports", "reading",
               "golf", "food/dining/cooking", "hunting/fishing", "skiing")

# combine giving dataframes into a list
# this list of data frames will be used to create all variables to ensure relationships between giving and engagement variables
dfList <- list(never_givers, annual_givers, leadership_givers, mid_level_givers, major_givers, principal_givers)

# set counter for lapply call 
# this tracks the runs through the lapply call
# used to distinguish between never givers and annual givers and major givers, etc. 
# probably a better way to do this 
counter <- (as.numeric(end_date - start_date)/365) - length(dfList) 

# create engagement data based on giving levels
engagement_table <- 
  bind_rows(
    lapply(dfList, function(x) {
      
      if(counter >= 0){
        
        x$last_contact <- sample(seq(start_date + counter*365, end_date, by = "day"),
                                 size = nrow(x), 
                                 prob = c((as.numeric(end_date-start_date) - counter*365): 0), 
                                 replace = TRUE)
        
        x$numer_of_contacts <- sample(1:50,
                                      size = nrow(x), 
                                      prob = 50:1, 
                                      replace = TRUE)
        
        x$gift_officer <- sample(officers,
                                 size = nrow(x), 
                                 replace = TRUE)
        
        x$event <- sample(c("Y", "N"),
                          size = nrow(x),
                          prob = c(.6, 4), 
                          replace = TRUE)
        
        x$volunteer <- sample(c(1, 0),
                              size = nrow(x),
                              prob = c(.6, 4), 
                              replace = TRUE)
        
        x$interests <- 
          as.character(
            lapply(x$id, function(y) paste(sample(interests, sample(1:5), replace = FALSE), collapse = ",")))
        
        
        x$time_on_site <- sample(1:1000,
                                 size = nrow(x), 
                                 prob = 1000:1, 
                                 replace = TRUE)
        
        x$last_contact <- insert_NAs(x$last_contact, p =.1)
        x$numer_of_contacts <- insert_NAs(x$numer_of_contacts, p =.1)
        x$gift_officer <- insert_NAs(x$gift_officer, p =.96)   
        x$event <- insert_NAs(x$event, p =.1)
        x$volunteer <- insert_NAs(x$volunteer, p =.1)
        x$interests <- insert_NAs(x$interests, p =.3)
        x$time_on_site <- insert_NAs(x$time_on_site, p =.8)    
        
      }else{
        
        x$last_contact <- sample(seq(start_date, end_date, by = "day"),
                                 size = nrow(x), 
                                 prob = c((as.numeric(end_date-start_date): 0)), 
                                 replace = TRUE) 
        
        x$numer_of_contacts <- sample(0:25,
                                      size = nrow(x), 
                                      prob = c(100, rep(1, 25)), 
                                      replace = TRUE)
        
        x$gift_officer <- sample(officers,
                                 size = nrow(x), 
                                 replace = TRUE)
        
        x$event <- sample(c("Y", "N"),
                          size = nrow(x), 
                          prob = c(.3, .7), 
                          replace = TRUE)
        
        x$volunteer <- sample(c(0, 1),
                              size = nrow(x),
                              prob = c(.6, 4), 
                              replace = TRUE)
        
        x$time_on_site <- sample(1:1000,
                                 size = nrow(x), 
                                 prob = 1000:1, 
                                 replace = TRUE)
        
        x$interests <-           
          as.character(
            lapply(x$id, function(y) paste(sample(interests, sample(1:5), replace = FALSE), collapse = ",")))
        
        
        x$last_contact <- insert_NAs(x$last_contact, p =.4)
        x$numer_of_contacts <- insert_NAs(x$numer_of_contacts, p =.4)
        x$gift_officer <- insert_NAs(x$gift_officer, p =.98)  
        x$event <- insert_NAs(x$event, p =.1)
        x$volunteer <- insert_NAs(x$volunteer, p =.1)
        x$interests <- insert_NAs(x$interests, p =.6)
        x$time_on_site <- insert_NAs(x$time_on_site, p =.8)    
        
      }
      
      counter <<- counter + 1
      
      x} )
    
  )


write_csv(engagement_table, "engagement_data_table.csv")


