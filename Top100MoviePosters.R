#Loading the rvest package
library(rvest)
library(tidyverse)
library(writexl)

#Specifying the url for desired website to be scraped
url <- 'https://www.moviepostershop.com/feature/mini/100-best-selling-movie-posters/'

# Scraping the Title 1----
#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
title_data_html <- html_nodes(webpage,'.item_title')

#Converting the ranking data to text
title_data <- html_text(title_data_html)

#Let's have a look at the rankings
head(title_data)


title_1 <- as.data.frame(title_data) %>%
    mutate(rank = cbind(c(1:20)))

#Specifying the url for desired website to be scraped
url <- 'https://www.moviepostershop.com/feature/mini/100-best-selling-movie-posters/default.asp?page=2#results'

# Scraping the Title 2----
#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
title_data_html <- html_nodes(webpage,'.item_title')

#Converting the ranking data to text
title_data <- html_text(title_data_html)

#Let's have a look at the rankings
head(title_data)
title_2 <- as.data.frame(title_data) %>%
    mutate(rank = cbind(c(21:40)))

#Specifying the url for desired website to be scraped
url <- 'https://www.moviepostershop.com/feature/mini/100-best-selling-movie-posters/default.asp?page=3#results'

# Scraping the Title 3 ----
#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
title_data_html <- html_nodes(webpage,'.item_title')

#Converting the ranking data to text
title_data <- html_text(title_data_html)

#Let's have a look at the rankings
head(title_data)
title_3 <- as.data.frame(title_data) %>%
    mutate(rank = cbind(c(41:60)))


#Specifying the url for desired website to be scraped
url <- 'https://www.moviepostershop.com/feature/mini/100-best-selling-movie-posters/default.asp?page=4#results'

# Scraping the Title 4  ----
#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
title_data_html <- html_nodes(webpage,'.item_title')

#Converting the ranking data to text
title_data <- html_text(title_data_html)

#Let's have a look at the rankings
head(title_data)
title_4 <- as.data.frame(title_data) %>%
    mutate(rank = cbind(c(61:80)))

#Specifying the url for desired website to be scraped
url <- 'https://www.moviepostershop.com/feature/mini/100-best-selling-movie-posters/default.asp?page=5#results'

# Scraping the Title 5  ----
#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
title_data_html <- html_nodes(webpage,'.item_title')

#Converting the ranking data to text
title_data <- html_text(title_data_html)

#Let's have a look at the rankings
head(title_data)
title_5 <- as.data.frame(title_data) %>%
    mutate(rank = cbind(c(81:100)))

movie_posters_1 <- union(title_1, title_2)
movie_posters_2 <- union(title_3,title_4)
movie_poster_set <- union(movie_posters_1, movie_posters_2)
# Top 100 Movie Posters ----
movie_poster <- union(movie_poster_set, title_5)

movie_poster <- movie_poster %>%
    mutate(originalTitle = title_data)

