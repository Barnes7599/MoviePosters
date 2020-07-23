library(tidyverse)
library(ggplot2)
library(plotly)
library(extrafont)
library(scales)
library(tidytext)
library(tidyquant)
library(gt)
library(kableExtra)
library(formattable)
library(reactable)
library(DT)
library(ggridges)
library(gridExtra)
library(flexdashboard)
library(highcharter)
font_import()

retrieved_date <- Sys.Date()

fonts()
# Import Rating Data ----
df_rating <- read_tsv('https://datasets.imdbws.com/title.ratings.tsv.gz', na = "\\N", quote = '')

# Import Basic Data ----
df_basics <- read_tsv('https://datasets.imdbws.com/title.basics.tsv.gz', na = "\\N", quote = '')

#Import Actors Data ----
df_actors <- read_tsv('https://datasets.imdbws.com/name.basics.tsv.gz', na = "\\N", quote = '') %>%
    filter(str_detect(primaryProfession, "actor|actress"))  %>%
    select(nconst, primaryName, birthYear)

df_rating_basics <- 
    left_join(df_basics, df_rating)


#Import Principals Data ----
df_principals <- read_tsv('https://datasets.imdbws.com/title.principals.tsv.gz', na = "\\N", quote = '') %>%
    filter(str_detect(category, "actor|actress")) %>%
    select(tconst, ordering, nconst, category) %>%
    group_by(tconst) %>%
    filter(ordering == min(ordering))

# Data Clean up
# Searching by Original Title 
df_rating_basic_2 <- df_basics %>% 
    mutate(star = df_basics$primaryTitle %>% str_detect(pattern = "Lawrence")) %>%
    filter(star == TRUE & titleType == 'movie')

# Finding Ratings with tconst key   
df_rating %>%
    filter(tconst == "tt0121164")

#Joining cleaned table with principals and actors/acctresses
top_100 <- read_csv('poster_movies_top_100.csv')

top_100_and_principles <- left_join(top_100, df_principals)

top_100 <- left_join(top_100_and_principles, df_actors)

#Actor / Actress Age
top_100 <- top_100 %>%
    mutate(Lead_age = startYear - birthYear) %>%
    mutate(decade = fct_rev(factor(((floor(startYear - startYear %% 10))))))

#Tidy up dataset
top_100_tidy <-  top_100 %>%
    ungroup() %>%
    select(rank, primaryTitle, averageRating, numVotes, startYear, runtimeMinutes, genres, primaryName) %>%
    arrange(rank)

#Create a Table
top_100_tidy %>%
    gt() %>%
    tab_header(
        title = md("**Top 100 Rated Movie Posters**"),
        subtitle = md(glue::glue("Data *retrieved* on {retrieved_date}"))
    ) %>%
    tab_source_note(
        source_note = "Source: IMDb Datasets (https://www.imdb.com/interfaces/)"
    ) %>%
    tab_source_note(
        source_note = "Source: Movie Poster Shop (https://www.moviepostershop.com/feature/mini/100-best-selling-movie-posters/default.asp?page=1#results)"
    ) %>%
    cols_label(
        rank = "Rank",
        primaryTitle = "Title",
        averageRating = "Rating",
        numVotes = "Votes",
        startYear = " Release Year",
        runtimeMinutes = "Run Time",
        genres = "Genres",
        primaryName = "Lead Actor/Actress"
    )

#Movies by Genre
df_rating_basics %>%
    filter(runtimeMinutes < 210, titleType == "movie", numVotes >= 10) %>%
    select(runtimeMinutes, averageRating, genres) %>%
    unnest_tokens(genre, genres, token = str_split, pattern = ",") %>%
    group_by(genre) %>%
    summarise(avgRating = mean(averageRating), avgtime = mean(runtimeMinutes), sum(n())) %>%
    ungroup() %>%
    ggplot(aes(x = avgRating, y = reorder(genre, avgRating))) +
    geom_col(fill = "#2c3e50") +
    geom_text(aes(label = round(avgRating,2)), hjust = -.2) +
    labs(
        title = "Rating by Genre",
        subtitle = "What genre has the average highest rating?",
        y = "",
        x = "Average Rating",
        caption = "Source: IMDb Dataset"
    ) +
    theme(#axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())

# GT table movies by Genre
df_unnest %>%
    gt() %>%
    tab_header(
        title = md("**Movie Poster Genre Ratings**"),
        subtitle = md(glue::glue("Data *retrieved* on {retrieved_date}"))
    ) %>%
    tab_source_note(
        source_note = "Source: IMDb Datasets (https://www.imdb.com/interfaces/)"
    ) %>%
    tab_source_note(
        source_note = "Source: Movie Poster Shop (https://www.moviepostershop.com/feature/mini/100-best-selling-movie-posters/default.asp?page=1#results)"
    ) %>%
    cols_label(
        genre = "Genre",
        avgRating = "Average Rating",
        avgtime = "Average runtime (min)",
    )

# Data clean up posters ----
df_unnest_posters <- top_100 %>%
    select(runtimeMinutes, averageRating, genres) %>%
    unnest_tokens(genre, genres, token = str_split, pattern = ',') %>%
    mutate(genre = case_when(
        genre == "frama" ~ "drama",
        TRUE ~ genre
    ))

# Highest rated Movie posters by Genre
df_unnest_posters %>%
    group_by(genre) %>%
    summarise(avgRating = mean(averageRating), avgtime = mean(runtimeMinutes), sum(n())) %>%
    ungroup() %>%
    ggplot(aes(x = avgRating, y = reorder(genre, avgRating))) +
    geom_col(fill = "#2c3e50") +
    geom_text(aes(label = round(avgRating,2)), hjust = -.2) +
    labs(
        title = "Top 100 Posters",
        subtitle = "What genre has the average highest rating?",
        y = "",
        x = "Average Rating",
        caption = "Source: IMDb Dataset"
        ) +
    theme(#axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank()) 

#movie poster counts by Genre
df_unnest_counts <- df_unnest_posters %>% 
    group_by(genre) %>%
    summarise(avgRating = round(mean(averageRating),2), avgtime = round(mean(runtimeMinutes),2)) %>%
    ungroup()

#GT datatable Movie Posters
df_unnest_counts %>% 
    gt() %>%
    tab_header(
    title = md("**Movie Poster Genre Ratings**"),
    subtitle = md(glue::glue("Data *retrieved* on {retrieved_date}"))
) %>%
    tab_source_note(
        source_note = "Source: IMDb Datasets (https://www.imdb.com/interfaces/)"
    ) %>%
    tab_source_note(
        source_note = "Source: Movie Poster Shop (https://www.moviepostershop.com/feature/mini/100-best-selling-movie-posters/default.asp?page=1#results)"
    ) %>%
    cols_label(
        genre = "Genre",
        avgRating = "Average Rating",
        avgtime = "Average runtime (min)",
        )

# DT Datatable Movie Posters
df_unnest_counts %>%
    datatable(class = 'cell-border stripe', 
              rownames = F, 
              colnames = c('Genre', 'Avg Rating', 'Avg Time (min)'),
              caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; Table 2:',
              htmltools::em('Movie Posters by Genre')),
              extensions = 'Scroller', options = list(
                  deferRender = TRUE,
                  scrollY = 200,
                  scroller = TRUE
              ))

#Facet Wrap Genre - All movies
df_rating_basics %>%
ggplot(aes(x = runtimeMinutes, y = averageRating)) +
    geom_bin2d() +
    #scale_x_continuous(breaks = seq(0, 180, 60), labels = 0:3) +
    #scale_y_continuous(breaks = 1:10) +
    #Color options magma, inferno, plasma, viridis
    scale_fill_viridis_c(option = "viridis", labels = comma) +
    theme_minimal(base_family = ".SF Compact Rounded", base_size = 11) +
    labs(title = "Relationship between Movie Runtime and Average Rating",
         subtitle = glue::glue("Data from IMDb retrieved {retrieved_date}"),
         x = "Runtime (Hours)",
         y = "Average User Rating",
         caption = "Source: IMDb Dataset",
         fill = "# Movies") +
    theme_minimal() +
    facet_wrap(~ genres)

# Filter Genre

df_ratings_unnest <- df_rating_basics %>%
    filter(runtimeMinutes < 180, titleType=="movie", numVotes >= 10) %>%
    select(runtimeMinutes, averageRating, genres) %>%
    unnest_tokens(genre, genres, token = str_split, pattern = ",")

df_temp <- df_ratings_unnest %>%
    filter(!(genre %in% c("game-show", "reality-tv", "short", "talk-show", NA))) %>%
    group_by(genre) %>%
    mutate(prop = 1/n())

df_temp_posters <- df_unnest_posters %>%
    filter(!(genre %in% c("game-show", "reality-tv", "short", "talk-show", NA))) %>%
    group_by(genre) %>%
    mutate(prop = 1/n())

#Facet Wrap proportion based on each Genre ----
ggplot(df_temp_posters, aes(x = runtimeMinutes, y = averageRating)) +
    #stat_summary_2d(fun=sum) +
    geom_point() +
    geom_smooth(method = lm, se = F, color = "#2c3e50") +
    #scale_x_continuous(breaks = seq(0, 180, 60), labels = 0:3) +
    #scale_y_continuous(breaks = 1:10) +
    #scale_fill_viridis_c(option = "magama", labels = comma, limits=c(0, 0.02), oob=squish, guide=F) +
    theme_tq(base_family = ".SF Compact Display", base_size = 11) +
    labs(title="Relationship between Movie Runtime and Average Movie Rating",
         subtitle="Data from IMDb retrieved July 4th, 2018",
         x="Runtime (Minutes)",
         y="Average User Rating",
         caption = "Max Woolf — minimaxir.com") +
    facet_wrap(~ genre, scales = 'free_x')

#Saving Plots
#ggsave("imdb-3b.png", plot, width = 6, height = 6)


#Rating vs Movie Year by Top 100 posters----

year_vs_rating <- top_100 %>%
    mutate(maxrating = ifelse(averageRating == max(averageRating), primaryTitle, NA)) %>% 
    mutate(minrating = ifelse(averageRating == min(averageRating), primaryTitle, NA)) %>%
ggplot(aes(x = startYear, y = averageRating, label = minrating)) +
    geom_point(fill = "#2c3e50") +
    geom_smooth(method = lm, se = FALSE) +
    #geom_text(nudge_y = .1, nudge_x = 8) +
    ggrepel::geom_text_repel(nudge_y = .2, vjust = 2) +
    scale_x_continuous() +
    scale_y_continuous(breaks = 1:10) +
    labs(title = "Relationship between Movie Release Year and Average Rating",
         subtitle = "Line represents average user rating",
         x = "Year Movie was Released",
         y = "Average User Rating For Movie",
         caption = glue::glue("Data from IMDb retrieved {retrieved_date}"),
         fill = "# Movies") +
    theme_() +
    theme(
        panel.background = element_blank(),
        plot.background = element_blank()
    )

#gridExtra::grid.arrange(plot, year_vs_rating, ncol = 2)

#ggsave("imdb-4.png", year_vs_rating, width = 5, height = 3)

#Density Ridge Plots----
density_plot <- ggplot(top_100_tidy %>% filter(startYear >= 1933) %>% mutate(startYear = factor(startYear)), aes(x = averageRating, y = startYear, fill = startYear)) +
    geom_density_ridges2() +
    scale_fill_hue(guide = F) +
    scale_x_continuous(breaks = 1:10) +
    #theme_tq(base_family = ".SF Compact Rounded", base_size = 9) +
    theme(plot.background = element_blank(),
          panel.background = element_blank()) + 
    theme_tq()
#ggsave("imdb-5.png", plot, width = 4, height = 3)

# Bucket by Decade
Top_100_decades <- top_100_tidy %>%
    filter(startYear >= 1950) %>%
    mutate(decade = fct_rev(factor(((floor(startYear - startYear %% 10))))))

avg_rating_by_decade <- Top_100_decades %>%
    select(averageRating, numVotes, decade) %>%
    mutate(decade = as_factor(decade)) %>%
    group_by(decade) %>%
    summarise(avg = mean(averageRating)) %>%
    ungroup() %>%
    fct_reorder(decade) %>%
    ggplot(aes(x = decade, y = avg)) +
    coord_flip() +
    geom_col(fill = "#2c3e50") +
    #theme_tq() +
    theme(panel.background = element_blank(),
          plot.background = element_blank()) +
    labs(
        title = "Average Rating by Decade",
        x = "Decade of Release"
    )

#Experimental Visuals ----
df_unnest_posters %>% 
    ggplot(aes(x = averageRating, y = runtimeMinutes)) +
    geom_boxplot() +
    geom_point() +
    #facet_wrap(~ genre) +
    theme_tq() +
    #geom_smooth(method = loess, se = FALSE, color = "black", size = .5, span = .1) +
    #scale_fill_brewer(palette = 'Blues', direction = -1) +
    labs(
        title = 'Does movie runtime effect ratings?',
        subtitle = 'How much time do you have?',
        caption = glue::glue("Data from IMDb retrieved {retrieved_date}"),
        y = 'Genres',
        x = 'Average User Rating'
        #fill = 'Genre'
    ) +
    theme(
        # will change all text at once
        title = element_text(
            color = '#2c3e50',
            face = 'bold',
            family = ".New York",
            size = 13
        ),
        axis.title.y = element_text(
            color = '#2c3e50',
            face = 'bold',
            family = '.SF Compact Display'
        ),
        axis.ticks = element_blank(),
        plot.caption = element_text(
            hjust = 0,
            family = '.SF Compact Display'),
        plot.subtitle = element_text(
            family = '.SF Compact Display',
            size = 11
        ),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = .2, color = 'black'),
        legend.text = element_text(
            family = '.SF Compact Display',
            color = '#2c3e50'
        ),
        # Adjust legend title
        legend.title = element_text(
            family = '.SF Compact Display',
            size = 10
        ),
        # Adjust axis colors and fonts and size
        axis.text = element_text(
            family = '.SF Compact Display',
            color = '#2c3e50'
        )
    )


#Lead actor vs actress
ggplot(top_100, aes(x = decade, fill = category )) +
    geom_bar(position = "fill", width = 1 ) +
    theme_minimal(base_family = "SF Compact Rounded", base_size = 9) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1")

#Avg Votes vs Rating
ggplot(df_rating, aes(x = numVotes, y = averageRating)) +
    geom_bin2d() +
    scale_x_log10(labels = scales::comma) +
    scale_y_continuous(breaks = 1:10) +
    scale_fill_viridis_c(labels = scales::comma)

#Top 100 Posters (Movie Release Year and Average Ratings)
ggplot(top_100, aes(x = startYear, y = averageRating)) +
    geom_point() +
    geom_smooth(color = "black", se = FALSE, span = .5) +
    labs(
        title = "Top 100 Posters as it relates to average rating and year released",
        subtitle = "For 191,926 Movies/Ratings. Data from IMDB retrived 7/6/2020",
        x = "Year Movie was Released",
        y = "Average User Rating For Movie"
        ) +
    scale_x_continuous() +
    scale_y_continuous(breaks = 1:10) +
    theme_minimal(base_family = "Tahoma", base_size = 11) +
    theme(
        plot.title = element_text(face = 'bold')
    )
    
# Changes in Ages of Leads over time for Top 100 Posters
ggplot(top_100, aes(x = startYear, y = Lead_age)) +
    geom_point() +
    #geom_ribbon(aes(ymin = low_age, ymax = high_age), alpha = 0.2) +
    geom_smooth(se = FALSE) +
    theme_minimal(base_family = "Tahoma", base_size = 11) +
    labs(title = "Change in Ages of Movie Lead Actors/Actress Over Time",
         subtitle = "Line represents median age",
         caption = glue::glue("Data from IMDB retrived {retrieved_date}"),
         y = "Age of Lead Actor/Actress",
         x = "Year Movie was Released"
    ) +
    scale_x_continuous(limits = c(1920,2025)) +
    theme(plot.title = element_text(face = "bold"))

df_actor_ages_lead <- df_ratings_movies %>%
    group_by(startYear, category) %>%
    summarize(low_age = quantile(age_lead, 0.25, na.rm = T),
              med_age = quantile(age_lead, 0.50, na.rm = T),
              high_age = quantile(age_lead, 0.75, na.rm = T))


plot <- ggplot(df_actor_ages_lead %>% filter(startYear >= 1920), aes(x = startYear, fill = category)) +
    geom_ribbon(aes(ymin = low_age, ymax = high_age), alpha = 0.2) +
    geom_line(aes(y = med_age, color = category)) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    labs(
        title = "Change in Ages of Movies Lead Actors/Actress Over Time",
        subtitle = "For 114,973 Actors. Line represents median age.
Ribbon bounds represent 25th - 75th Percentiles. Data from IMDB retrived 7/6/2020",
        y = "Age of Lead Actor/Actress",
        x = "Year Movie was Released"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(color = "darkgrey"),
          axis.text.y = element_text(color = "darkgrey"),
         plot.subtitle = element_text(color = "darkgrey"),
         legend.title = element_blank())

plot

df_ratings_movies_nth <- df_ratings_movies %>%
    group_by(nconst) %>%
    arrange(startYear) %>%
    mutate(nth_lead = row_number())

#Movie Poster Rank vs Ratings
ggplot(top_100, aes(averageRating, rank)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    scale_y_reverse() + 
    labs(
        title = "Top 100 Posters vs IMDb Rating",
        subtitle = "Do ratings influence top selling poster rank?"
    ) +
    theme_tq()

df_ratings_movies_nth %>%
    select(primaryTitle, primaryName, nth_lead)


Top_10_votes <- ggplot(top_100 %>% filter(rank <= 10), aes(numVotes, reorder(primaryTitle, -rank))) +
    geom_col(fill = "#2c3e50") + 
    theme_tq() + 
    labs(
        title = "Top 10 selling posters and IMDb votes",
        x = "Number of Votes (IMDb)",
        y = ""
    )

top_10_ratings <- ggplot(top_100 %>% filter(rank <= 10), aes(averageRating, reorder(primaryTitle, -rank))) +
    geom_col(fill = "#2c3e50") +
    theme_tq() +
    labs(
        title = "Top 10 selling posters and IMDb average rating",
        x = "Average Rating (IMDb)",
        y = ""
    )

gridExtra::grid.arrange(Top_10_votes, top_10_ratings, ncol = 2)
top_10_ratings %>% hc_add_theme(hc_theme_handdrawn())
    
top_100 %>%
    ggplot(aes(x = rank, y = averageRating)) +
    geom_point() +
    theme_tq() +
    labs(
        title = "Top 100 Selling Movie Posters",
        subtitle = "Does rating effect Poster rank"
    )

df_rating_basics %>%
    ggplot(aes(x = numVotes, y = averageRating)) +
    geom_line() +
    theme_tq()

df <- df_rating_basics %>%
    filter(titleType == "movie", numVotes > 10) 

mean(df$averageRating)
