# APRA - Data Science Courses
# This script creates the data sets that will be used across all courses. 
# This includes three data tables: 
# 1. bio
# 2. giving
# 3. prospect/engagement.  
# Each data table will be available as a .csv, Excel, and a table in a database. 

library(tidyverse)
library(randomNames)
library(zipcode)


# define parameters for data set----

# bio records is the number of unique ids and subsequently the number of rows in the bio table
bio_records <- 100000
pct_shared_household <- .4
prob_deceased <- .1
start_date  <- Sys.Date() - 5*365
end_date    <- Sys.Date()


# helper functions ----
# make ~10% of data missing
insert_NAs <- function(x) {
  len <- length(x)
  n <- sample(1:floor(0.1*len), 1)
  i <- sample(1:len, n)
  x[i] <- NA 
  x
}



# Biographic Table (individual level) ----
# Included variables: 
# Id
# Name
# Household id or status
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
birthdays <- sort(
  sample(
    seq(Sys.Date() - 102*365, Sys.Date() - 20*365+69, by = "day"), 
    prob = c(rep(.01, 3000), rep(.05, 3000), rep(.05, 3000), rep(.10, 3000), rep(.20, 3000), 
             rep(.20, 3000), rep(.15, 3000), rep(.15, 3000), rep(.10, 3000), rep(.10, 3000)),
    size = bio_records, 
    replace = TRUE))


# create bio table
bio_table <- tibble(
  id = id,
  name = randomNames(bio_records),
  household_id = household_id,
  deceased = sample(c("Y", "N"), size = bio_records, prob = c(prob_deceased, 1-prob_deceased), replace = TRUE),
  country = country_cities_expanded$country,
  city = country_cities_expanded$city
)

# add birthdays
bio_table <- 
  bio_table %>% 
  arrange(household_id) %>% 
  bind_cols(birthday = birthdays)

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
  
  # Appeal type (direct mail, phonathon, etc.)
  

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

giving_table <- 
households_w_two_people %>% 
  left_join(giving_table) 

giving_table$credit_type <- ifelse(duplicated(giving_table$gift_id), "Soft-Credit", "Hard-Credit") 


# small gifts - 80% of gifts
summary(round((rexp(nrow(giving_table)/2*.8, rate = .05)+1)*10, 0))
amt <- round((rexp(nrow(giving_table)/2*.8, rate = .05)+1)*10, 0)

# mid size gifts - 20% of gifts
summary(rnorm(nrow(giving_table)/2*.15, mean = 50000, sd = 10000))
amt <- c(amt, round((rexp(nrow(giving_table)/2*.15, rate = .05)+1)*10, 0))

# major gifts 4.5% of gifts 
summary(round((rexp(nrow(giving_table)/2*.045, rate = .1)+10)*10000, 0))
amt <- c(amt, round((rexp(nrow(giving_table)/2*.045, rate = .1)+10)*10000, 0))

# major gifts .5% of gifts 
summary(round((rexp(nrow(giving_table)/2*.005, rate = .1)+10)*10000, 0))
amt <- c(amt, round((rexp(nrow(giving_table)/2*.005, rate = .1)+10)*10000, 0))

# put amounts in random order
amt <- sample(amt, size = length(amt), replace = FALSE)

# double amounts for shared households
amt <- c(amt,amt)

if(nrow(giving_table) > length(amt)) amt <- c(amt, rep(100, nrow(giving_table) - length(amt)))

amt <- sort(amt)

giving_table <- 
giving_table %>% 
  arrange(gift_id) %>% 
  mutate(gift_amt = amt)

# gift date
dates <- sample(seq(start_date, end_date, by = "day"), nrow(giving_table)/2, replace = TRUE)
dates <- c(dates, dates)
dates <- sort(dates)

giving_table <- 
  giving_table %>% 
  arrange(gift_id) %>% 
  mutate(gift_date = dates)





# giving data for households with one person
households_w_one_person <- 
  bio_table %>% 
  filter(!household_id %in% tmp$household_id) %>% 
  select(household_id, id)

tmp <- tibble(household_id = sample(households_w_one_person$household_id, 
                                             size = nrow(households_w_one_person)*3, 
                                             replace = TRUE),
                       gift_id = sample(1000000:9999999, nrow(households_w_one_person)*3, replace=F)) 

tmp$credit_type <- "Hard-Credit"

# small gifts - 80% of gifts
summary(round((rexp(nrow(tmp)/2*.8, rate = .05)+1)*10, 0))
amt <- round((rexp(nrow(tmp)/2*.8, rate = .05)+1)*10, 0)

# mid size gifts - 20% of gifts
summary(rnorm(nrow(tmp)/2*.15, mean = 50000, sd = 10000))
amt <- c(amt, round((rexp(nrow(tmp)/2*.15, rate = .05)+1)*10, 0))

# major gifts 4.5% of gifts 
summary(round((rexp(nrow(tmp)/2*.045, rate = .1)+10)*10000, 0))
amt <- c(amt, round((rexp(nrow(tmp)/2*.045, rate = .1)+10)*10000, 0))

# principal gifts .5% of gifts 
summary(round((rexp(nrow(tmp)/2*.005, rate = .1)+10)*10000, 0))
amt <- c(amt, round((rexp(nrow(tmp)/2*.005, rate = .1)+10)*10000, 0))

# put amounts in random order
amt <- sample(amt, size = length(amt), replace = TRUE)

# double amounts for shared households
amt <- c(amt,amt)

if(nrow(tmp) > length(amt)) amt <- c(amt, rep(100, nrow(tmp) - length(amt)))

amt <- sort(amt)

tmp <- 
  tmp %>% 
  arrange(gift_id) %>% 
  mutate(gift_amt = amt) %>% 
  mutate(gift_date = sample(seq(start_date, end_date, by = "day"), nrow(tmp), replace = TRUE)) %>% 
  left_join(households_w_one_person) 
  

giving_table <- bind_rows(giving_table, tmp)

giving_table <- filter(giving_table, !is.na(gift_id))

# make names different to require changing
colnames(giving_table) <- c("household ID", "ID", "gift id", "credit Type", "gift amt", "gift date")

write_csv(giving_table, "giving_data_table.csv")





  # Data science questions/activities to be built in
  # Not all people in bio table are in gift table
  # Who are non-donors? 
  #   0’s vs NAs
  # Difference between cash, pledges, pledge payments
  # What is the first gift date for the individual, household, group? With/without soft-credit?
  #   First gift, last gift, largest gift 
  # Splits and designations
  # Calendar year giving vs. fiscal year giving
  
  # Engagement/Prospect Table (individual level) ----
  # Included variables
  # Id
  # Last contact date 
  # Number of personal contacts
  # Assigned
  # Solicitor
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
  
  
  
  