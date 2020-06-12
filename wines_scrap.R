# This script is used to webscrap the "Sparkling wines" reviews of winemag.com; the execution should take between 8 and 12 hours
# As an alternative, use the loading functions of the clean dataset directly in the wine_modelling script

# Loading the required libraries

if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(pander)) install.packages("pander", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

# Defining the universe of web scraping: Reviews of Sparkling wines (without geographic specification, so as to include all types:
# non-Champagne sparkling wines, Champagnes and "methode champenoise" Champagne knock-offs made out of France)

site = read_html("https://www.winemag.com/varietals/sparkling/?drink_type=wine&search_type=reviews&page=1")

# Identifying the number of pages of reviews

nb_pages <- html_nodes(site,".pagination li a") %>%
  html_text() %>% tail(.,1) %>% as.numeric

# The following function collects various information from the individual wine pages:
# 1. 'review' will scrap the review of the wine (including the first & last names of the reviewer)
# 2. 'wine_type' will scrap the domain, specific desgination and geographic origin of the wine
# 3. 'bottle_info' will scrap the alcohol content and the size of the bottle
# 4. 'badge' will scrap the potential badge (Editor's Choice...) associated to the wine.
# we do not scrap the variety, as this will always be a blend (the identification of grapes can often be mined from the review text)
# we do not scrap the site visitor average rating, as a first scraping suggests this field is never populated
# We do not scrap the review date, as a first scraping suggests it is inconcistently encoded depending on the type of wine
# (and wihout a unique identifier) as it appears after the 'importer' field, which may or may not be part of a review

info_extract <- function(x){
  page <- read_html(x)
  review <- page %>% html_nodes("p.description") %>%
    html_text
  wine_type <- page %>% html_nodes(".info.medium-9.columns span") %>%
    html_text
  bottle_info <- page %>% html_nodes(".info.small-9.columns span span") %>%
    html_text()
  badge <- page %>% html_nodes(".badge span") %>%
    html_text()
  if(length(badge) == 0){
    badge <- NA
  }
  cbind(domain = wine_type[7], designation = wine_type[3], origin = wine_type[6], review, badge, alcohol = gsub("\\%","",bottle_info[1]), bottle_size = gsub(" ml","",bottle_info[2]))
}

# Initializing a list to retrieve the info gathered on the successive wines

wine_list <- list()

# Main function, lengthy execution time: extracting information from the individual pages

for(i in 1:nb_pages)
{
  
  # Message indicating the state of the process
  cat("Sparkling wine reviews, page: ", i, " of ", nb_pages, "\n")
  
  # Referring to each successive page of summary of the reviews
  site = read_html(paste0("https://www.winemag.com/varietals/sparkling/?drink_type=wine&search_type=reviews&page=",i))
  
  # Scraping title of the review
  wine_title <- html_nodes(site,".review-listing .title") %>%
    html_text()
  
  # Extracting the vintage year (if any) from this title
  wine_year <- as.numeric(str_extract(wine_title, "\\d{4}"))
  
  # Extracting the professional grading and keeping only the numeric value
  points <- html_nodes(site,".info .rating") %>%
    html_text() %>% gsub(" Points","",.) %>% as.numeric(.)
  
  # Extracting the price and keeping only the numeric value
  price <- html_nodes(site,"span.price") %>%
    html_text() %>% gsub("\\$","",.) %>% as.numeric(.)
  
  # Extracting the URLs of individual reviews
  wine_href <- html_nodes(site,".review-listing") %>% html_attr("href")
  
  # Extracting the information defined in the info_extract function for each individual review
  reviews <- sapply(wine_href, function(x) info_extract(x))
  
  # Organizing the output
  reviews <- t(reviews)
  row.names(reviews) <- NULL
  colnames(reviews) <- c("domain", "designation", "origin", "review", "badge", "alcohol", "bottle_size")
  
  # Concatenating the information obtained from the individual pages and from the title in a data frame
  wine_list[[i]] <- data.frame(reviews, wine_year, points, price, stringsAsFactors = F)
  
  # Saving the list after each summary page
  saveRDS(wine_list,"wine_list.RDS")
  
  # Putting a random-duration sleep in the requests so as not to flood the server and risk being banned
  Sys.sleep(runif(1,0,3))
}

wines <- bind_rows(wine_list)

# Removing potential duplicates

wines <- distinct(wines)

# First exploratory approach of the data

pander(head(wines))
pander(summary(wines))

# We need to correct the absence of domain, badge or vintage to "None" instead of NAs, for the modelling part

wines$domain[is.na(wines$domain)] <- "None"
wines$badge[is.na(wines$badge)] <- "None"
wines$wine_year[is.na(wines$wine_year)] <- "None"

# About 2050 wines do not have a price; we replace these NAs with the median of the price column

sum(is.na(wines$price))
wines$price[is.na(wines$price)] <- median(wines$price, na.rm = T)

# We need to change the type of variables alchohol and bottle_size;
# a warning message will appear regarding the introduction of NAs, due to some non-populated fields in the original reviews

wines$alcohol <- as.numeric(wines$alcohol)
wines$bottle_size <- as.numeric(wines$bottle_size)

# We also need to correct some typos regarding alcohol content, first assessing how many outliers there are

head(sort(-wines$alcohol), n = 15)
head(sort(wines$alcohol), n = 15)

# Low-end figures are credible; figures above 22 are reset at the median of non-NAs alcohol values.

wines <- wines %>% filter(., !is.na(alcohol)) %>%
  mutate(alcohol = ifelse(alcohol > 22, median(wines$alcohol, na.rm = T), alcohol))

head(sort(-wines$alcohol), n = 15)
head(sort(wines$alcohol), n = 15)

# Finally, we replace the coerced NAs in 'alcohol' and 'bottle_size' respectively by the median of the non-NAs alcohol value,
# and 750 (standard bottle size; we do not use the median to avoid introducing non-realistic bottle size values)

wines$alcohol[is.na(wines$alcohol)] <- median(wines$alcohol, na.rm = T)
wines$bottle_size[is.na(wines$bottle_size)] <- 750

# We extract the appellation from the origin text (it appears before the first comma)
# We extract the production country from the origin text (as the location of this information relative to commasis not always the same
# from one review to another, we have to extract from a list of countries having more than 25 of their wines reviewed on the site)
# We can afterwards drop the origin column for better legibility and easier data manipulation

unique(wines$origin)

wines$appellation <- str_extract(wines$origin, ".+?(?=,)")

wines$country <- str_extract(wines$origin, '[,][^,]+$') %>% sub(", ","",.)

wines <- select(wines, -origin)

# We replace the NAs in the new 'appellation' and 'country' variables by "None", for modelling purposes

wines$appellation[is.na(wines$appellation)] <- "None"
wines$country[is.na(wines$country)] <- "None"

# We check that no wine labelled "Champagne" comes from another country than France

wines %>%
  filter(country != "France") %>%
  filter(appellation == "Champagne") %>%
  summarise(n())

# We add a variable regarding the length of the review, as longer reviews may be related to higher enthusiasm for the wine, and hence higer ratings
# Before computing the length, We drop the reviewer first and last names (appearing after the last period in the review field), as it would be cheating in our text mining algorithm
# (which aims at identifying Champagnes based solely on the review text, given that reviewer Roger Voss and the Champagne category are in bijection)

wines$review <- sub('[.][^.]+$', '', wines$review)

wines$rev_length <- str_length(wines$review)

# Finally, we add a unique index for each Wine.

wines <- tibble::rowid_to_column(wines, "wineId")

# Checking the absence of NAs

sum(is.na(wines))

# Saving the resulting file as the database for the main script

saveRDS(wines, "wines.RDS")

# END