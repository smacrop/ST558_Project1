ST558 Project1
================
Stephen Macropoulos
2023-06-24

# Intro

This vignette is a demonstration of pulling data from an API and
performing some basic exploratory data analysis using RMarkdown. We will
be using the Movie API found [here](http://omdbapi.com%5D).

# REQUIRED PACKAGES

In order to grab the data from the API and perform EDA we will need the
following packages installed:

\-`httr`  
-`jasonlite`  
-`readr`  
-`tidyverse`  
-`hrbrthemes`  
-`viridis`  
-`forcats`  
-`ggplot2`  
-`tidyr`  
-`rmarkdown`

So let’s install and load them!

``` r
library(httr)
library(jsonlite)
library(readr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(ggplot2)
library(tidyr)
library(rmarkdown)
```

# Writing a Function to Query the API

We write a function called `getmovies` to query the movie data from the
API. Our function has four arguments:

\-`titles`: character vector of desired movie titles from the database  
-`plot`: “short” by default, can be changed to “full” if desired  
-`dats`: character vector of desired variables in dataframe  
-`key`: string of API key found using the link above

The movie titles can be given to the function in upper or lower case,
but the desired variables must be specified as uppercase.

The function uses the `GET` and `fromJSON` functions from the `httr` and
`jsonlite` packages respectively.

Our function returns a dataframe object for the chosen movies with the
desired variables and “plot” type.

``` r
getmovies <- function(titles, plot="short", dats, key=""){
  res <- parsed <- list()
  url <- character()
  df <- data.frame(matrix(ncol = 24, nrow = length(titles)))
  arch <- "http://www.omdbapi.com/?t="
  for (i in 1:length(titles)) {
      if (plot=="full") {
      url[i] <- paste0(arch, gsub(" ", "+", tolower(titles[i])), "&plot=full")
    } else {
      url[i] <- paste0(arch, gsub(" ", "+", tolower(titles[i])))
    }
    url[i] <- paste0(url[i], "&apikey=", key)
    res[[i]] <- GET(url[i])
    parsed[[i]] <- fromJSON(rawToChar(res[[i]]$content))
    parsed[[i]] <- parsed[[i]][-15]
    df[i,] <- as.data.frame(parsed[[i]])
  }
  colnames(df) <- names(parsed[[1]])
  df2 <- select(df,dats)
  return(df2)
}
```

Let’s see an example of the full dataframe output with all the
variables!

``` r
movies <- getmovies(titles=c("Due date", "Red", "the fault in our stars",
                      "bruce almighty", "the green hornet", 
                      "nacho libre", "la dolce vita"), 
                      dats = c("Title", "Year", "Rated", "Released",
                               "Runtime", "Genre", "Director", "Writer",
                               "Actors", "Plot", "Language", "Country",
                               "Awards", "Poster", "Metascore",
                               "imdbRating", "imdbVotes", "imdbID",
                               "Type", "DVD", "BoxOffice", "Production",
                               "Website", "Response"),
                      key = "563d1f82")
movies
```

    ##                    Title Year     Rated    Released Runtime
    ## 1               Due Date 2010         R 05 Nov 2010  95 min
    ## 2                    RED 2010     PG-13 15 Oct 2010 111 min
    ## 3 The Fault in Our Stars 2014     PG-13 06 Jun 2014 126 min
    ## 4         Bruce Almighty 2003     PG-13 23 May 2003 101 min
    ## 5       The Green Hornet 2011     PG-13 14 Jan 2011 119 min
    ## 6            Nacho Libre 2006        PG 16 Jun 2006  92 min
    ## 7          La Dolce Vita 1960 Not Rated 19 Apr 1961 174 min
    ##                   Genre         Director
    ## 1         Comedy, Drama    Todd Phillips
    ## 2 Action, Comedy, Crime Robert Schwentke
    ## 3        Drama, Romance       Josh Boone
    ## 4       Comedy, Fantasy      Tom Shadyac
    ## 5 Action, Comedy, Crime    Michel Gondry
    ## 6 Comedy, Family, Sport       Jared Hess
    ## 7         Comedy, Drama Federico Fellini
    ##                                            Writer
    ## 1    Alan R. Cohen, Alan Freedland, Adam Sztykiel
    ## 2          Jon Hoeber, Erich Hoeber, Warren Ellis
    ## 3  Scott Neustadter, Michael H. Weber, John Green
    ## 4       Steve Koren, Mark O'Keefe, Steve Oedekerk
    ## 5    Seth Rogen, Evan Goldberg, George W. Trendle
    ## 6            Jared Hess, Jerusha Hess, Mike White
    ## 7 Federico Fellini, Ennio Flaiano, Tullio Pinelli
    ##                                                    Actors
    ## 1 Robert Downey Jr., Zach Galifianakis, Michelle Monaghan
    ## 2              Bruce Willis, Helen Mirren, Morgan Freeman
    ## 3               Shailene Woodley, Ansel Elgort, Nat Wolff
    ## 4            Jim Carrey, Jennifer Aniston, Morgan Freeman
    ## 5                   Seth Rogen, Jay Chou, Christoph Waltz
    ## 6         Jack Black, Ana de la Reguera, HÃ©ctor JimÃ©nez
    ## 7        Marcello Mastroianni, Anita Ekberg, Anouk AimÃ©e
    ##                                                                                                                                                                                      Plot
    ## 1                    High-strung father-to-be Peter Highman is forced to hitch a ride with aspiring actor Ethan Tremblay on a road trip in order to make it to his child's birth on time.
    ## 2 When his peaceful life is threatened by a high-tech assassin, former black-ops agent Frank Moses reassembles his old team in a last-ditch effort to survive and uncover his assailants.
    ## 3                                                                                    Two teenage cancer patients begin a life-affirming journey to visit a reclusive author in Amsterdam.
    ## 4                                                                                                                     A whiny news reporter is given the chance to step into God's shoes.
    ## 5                  Following the death of his father, Britt Reid, heir to his father's large company, teams up with his late dad's assistant Kato to become a masked crime fighting team.
    ## 6                                                       Berated all his life by those around him, a monk follows his dream and dons a mask to moonlight as a Luchador (Mexican wrestler).
    ## 7                                                                                   A series of stories following a week in the life of a philandering tabloid journalist living in Rome.
    ##                           Language              Country
    ## 1                 English, Spanish        United States
    ## 2                 English, Russian United States, China
    ## 3                          English        United States
    ## 4                 English, Spanish        United States
    ## 5                English, Mandarin        United States
    ## 6                 English, Spanish        United States
    ## 7 Italian, English, French, German        Italy, France
    ##                                        Awards
    ## 1                               7 nominations
    ## 2                     4 wins & 19 nominations
    ## 3                    23 wins & 18 nominations
    ## 4                      7 wins & 9 nominations
    ## 5                      4 wins & 7 nominations
    ## 6                      1 win & 11 nominations
    ## 7 Won 1 Oscar. 11 wins & 12 nominations total
    ##                                                                                                                                               Poster
    ## 1                                                 https://m.media-amazon.com/images/M/MV5BMTU5MTgxODM3Nl5BMl5BanBnXkFtZTcwMjMxNDEwNA@@._V1_SX300.jpg
    ## 2                                                 https://m.media-amazon.com/images/M/MV5BMzg2Mjg1OTk0NF5BMl5BanBnXkFtZTcwMjQ4MTA3Mw@@._V1_SX300.jpg
    ## 3                 https://m.media-amazon.com/images/M/MV5BNTVkMTFiZWItOTFkOC00YTc3LWFhYzQtZTg3NzAxZjJlNTAyXkEyXkFqcGdeQXVyODE5NzE3OTE@._V1_SX300.jpg
    ## 4                 https://m.media-amazon.com/images/M/MV5BNzMyZDhiZDUtYWUyMi00ZDQxLWE4NDQtMWFlMjI1YjVjMjZiXkEyXkFqcGdeQXVyNjU0OTQ0OTY@._V1_SX300.jpg
    ## 5                                                 https://m.media-amazon.com/images/M/MV5BMTcwOTMwMDYyMl5BMl5BanBnXkFtZTcwMzAxMjMyNA@@._V1_SX300.jpg
    ## 6                 https://m.media-amazon.com/images/M/MV5BN2ZkNDgxMjMtZmRiYS00MzFkLTk5ZjgtZDJkZWMzYmUxYjg4XkEyXkFqcGdeQXVyNTIzOTk5ODM@._V1_SX300.jpg
    ## 7 https://m.media-amazon.com/images/M/MV5BODQ0NzY5NGEtYTc5NC00Yjg4LTg4Y2QtZjE2MTkyYTNmNmU2L2ltYWdlL2ltYWdlXkEyXkFqcGdeQXVyNjc1NTYyMjg@._V1_SX300.jpg
    ##   Metascore imdbRating imdbVotes    imdbID  Type         DVD    BoxOffice
    ## 1        51        6.5   350,379 tt1231583 movie 22 Feb 2011 $100,539,043
    ## 2        60        7.0   315,498 tt1245526 movie 25 Jan 2011  $90,380,162
    ## 3        69        7.7   387,436 tt2582846 movie 16 Sep 2014 $124,872,350
    ## 4        46        6.8   416,002 tt0315327 movie 25 Nov 2003 $242,829,261
    ## 5        39        5.8   162,557 tt0990407 movie 03 May 2011  $98,780,042
    ## 6        52        5.8    90,522 tt0457510 movie 24 Oct 2006  $80,197,993
    ## 7        95        8.0    75,524 tt0053779 movie 21 Sep 2004          N/A
    ##   Production Website Response
    ## 1        N/A     N/A     True
    ## 2        N/A     N/A     True
    ## 3        N/A     N/A     True
    ## 4        N/A     N/A     True
    ## 5        N/A     N/A     True
    ## 6        N/A     N/A     True
    ## 7        N/A     N/A     True

# EDA for Best and Worst Movies of All Time

Let’s explore some movies! We found the top 96 and worst 100 movies of
all time according to some famous people, and we will take a look at
some patterns using `ggplot`.

First let’s store the top 96 movie titles in a character vector called
`top96`.

``` r
top96 <- c("a space odyssey", "the godfather", "citizen kane", 
            "raiders of the lost ark", "la dolce vita", "seven samurai",
            "in the mood for love", "there will be blood", "singin' in the rain",
            "goodfellas", "north by northwest", "mulholland drive", "bicycle thieves",
            "the dark knight", "city lights", "grand illusion", "his girl friday",
            "the red shoes", "vertigo", "beau travail", "the searchers", "persona",
            "do the right thing", "rashomon", "the rules of the game", "jaws", 
            "double indemnity", "the 400 blows", "star wars", "the passion of joan of arc",
            "once upon a time in the west", "alien", "tokyo story", "pulp fiction",
            "the truman show", "lawrence of arabia", "psycho", "sansho the bailiff",
            "andrei rublev", "the umbrellas of cherbourg", "chinatown", "the seventh seal",
            "lost in translation", "taxi driver", "spirited away", "night of the living dead",
            "battleship potemkin", "modern times", "breathless", "m", "blade runner",
            "the bitter tears of petra von kant", "rome, open city", "nosferatu", 
            "airplane!", "under the skin", "mad max: fury road", "apocalypse now",
            "brokeback mountain", "duck soup", "the blair witch project", "all the president's men",
            "the general", "eternal sunshine of the spotless mind", "the texas chainsaw massacre",
            "come and see", "heat", "the shining", "toy story", "killer of sheep", 
            "a woman under the influence", "annie hall", "some like it hot", "metropolis",
            "the maltese falcon", "this is spinal tap", "it happened one night", "die hard",
            "the conformist", "the thing", "daughters of the dust", "barry lyndon", "raging bull",
            "seven", "aguirre, the wrath of god", "the battle of algiers", 
            "women on the verge of a nervous breakdown", "boyhood",
            "paths of glory", "secrets & lies", "sweet smell of success", "the cabinet of dr. caligari",
            "nashville", "don't look now", "bonnie and clyde", "get out")
```

And we do the same for the worst 100 in a vector called `bottom100`.

``` r
bottom100 <- c("disaster movie", "Manos: The Hands of Fate", "Birdemic: Shock and Terror",
               "Superbabies: Baby Geniuses 2", "The Hottie & the Nottie", "Kirk Cameron's Saving Christmas",
               "House of the Dead", "Son of the Mask", "Epic Movie", "Radhe", "Battlefield Earth",
               "Pledge This!", "Alone in the Dark", "Dragonball Evolution", "From Justin to Kelly", 
               "Race 3", "Going Overboard", "Turks in Space", "Foodfight!", "Meet the Spartans",
               "Gigli", "Date Movie", "cats", "Daniel the Wizard", "Baby Geniuses", "Glitter", 
               "Jaws: The Revenge", "The Human Centipede III (Final Sequence)", "Cosmic Sin",
               "Troll 2", "BloodRayne", "365 Days: This Day", "Hobgoblins", "Left Behind",
               "365 Days", "The Cost of Deception", "Jeepers Creepers: Reborn", "Who's Your Caddy?",
               "Jack and Jill", "Rollerball", "Enes Batur", "Santa Claus Conquers the Martians",
               "Catwoman", "Slender Man", "kazaam", "Vampires Suck", "The Emoji Movie", "steel",
               "The Starving Games", "The Open House", "Student of the Year 2", "laxmii",
               "Tees Maar Khan", "Scary Movie V", "Breach", "Texas Chainsaw Massacre: The Next Generation",
               "Dumb and Dumberer: When Harry Met Lloyd", "the room", "gunday", "The Master of Disguise",
               "Police Academy: Mission to Moscow", "bratz", "Daddy Day Camp", "Hercules in New York",
               "far cry", "barb wire", "Mortal Kombat: Annihilation", "feardotcom", "Winnie the Pooh: Blood and Honey",
               "3 Ninjas: High Noon at Mega Mountain", "Batman & Robin", "smolensk", "Fifty Shades of Black",
               "crossroads", "Baaghi 3", "Captain America", "Spice World", "Spy Kids 4: All the Time in the World",
               "jaws 3-D", "The NeverEnding Story III", "Superman IV: The Quest for Peace",
               "Dungeons & Dragons", "The Wicker Man", "the fog", "Dragon Wars: D-War", "black christmas",
               "mac and me", "Bucky Larson: Born to Be a Star", "The Adventures of Sharkboy and Lavagirl 3-D",
               "Piranha 3DD", "I Know Who Killed Me", "The Love Guru", "In the Name of the King: A Dungeon Siege Tale",
               "The Hungover Games", "The Avengers", "Adipurush", "The Flintstones in Viva Rock Vegas",
               "Street Fighter: The Legend of Chun-Li", "The Human Centipede 2 (Full Sequence)",
               "Stan Helsing")
```

We now create a character vector of movie variables called `columns` to
be used in the `dats` argument of `getmovies`. We do not want all the
variables. These are the most interesting according to the author of
this vignette.

``` r
columns <- c("Year","Rated","Runtime","Language","Country","Metascore",
             "imdbRating","imdbVotes","BoxOffice")
```

We now store those dataframes into R objects called `top96movies` and
`bottom100movies` using the API key we found using the link above. We
combine the two dataframes into one called `moviedata` using `rbind`.

``` r
top96movies <- getmovies(titles=top96, dats=columns, key = "563d1f82")

bottom100movies <- getmovies(titles=bottom100, dats=columns, key = "563d1f82")

moviedata <- rbind(top96movies, bottom100movies)
```

We create a new variable called `Score` to get an idea of the success of
the movies. `Score` will average the `Metascore` and `imdbRating`
variables. We add the `Score` column to `moviedata`. We also make the
`Runtime` variable numeric in order to use it for quantitative
summaries.

``` r
Score <- 0.5*(as.numeric(moviedata$Metascore)/10 + as.numeric(moviedata$imdbRating))

moviedata <- cbind(moviedata, Score)

moviedata$Runtime <- as.numeric(strtrim(moviedata$Runtime, 3))
```

Now we categorize each movie by *Top* or *Bottom* by adding a factor
column to `moviedata`.

``` r
TopBottom <- rep(0,196)
TopBottom[1:96] <- "Top"
TopBottom[97:196] <- "Bottom"
TopBottom <- as.factor(TopBottom)
moviedata <- cbind(moviedata, TopBottom)
```

Finally, we will change the format of the `BoxOffice` variable to be
numeric. We then divide by 10 million which will facilitate plot
readability below.

``` r
moviedata$BoxOffice <- parse_number(moviedata$BoxOffice)
moviedata$BoxOffice <- moviedata$BoxOffice/1e7 # get in 10millions made
```

Let’s have a look at our clean dataframe now!

``` r
head(moviedata)
```

    ##   Year     Rated Runtime                                         Language
    ## 1 1968         G     149                         English, Russian, French
    ## 2 1972         R     175                          English, Italian, Latin
    ## 3 1941        PG     119                                 English, Italian
    ## 4 1981        PG     115 English, German, Hebrew, Spanish, Arabic, Nepali
    ## 5 1960 Not Rated     174                 Italian, English, French, German
    ## 6 1954 Not Rated     207                                         Japanese
    ##                         Country Metascore imdbRating imdbVotes  BoxOffice Score
    ## 1 United Kingdom, United States        84        8.3   685,693  6.0481243  8.35
    ## 2                 United States       100        9.2 1,910,007 13.6381073  9.60
    ## 3                 United States       100        8.3   450,876  0.1627530  9.15
    ## 4                 United States        85        8.4   986,622 24.8159971  8.45
    ## 5                 Italy, France        95        8.0    75,524         NA  8.75
    ## 6                         Japan        98        8.6   352,938  0.0318649  9.20
    ##   TopBottom
    ## 1       Top
    ## 2       Top
    ## 3       Top
    ## 4       Top
    ## 5       Top
    ## 6       Top

## Contingency Tables

Now we are ready to see our movie data. Let’s start with some
contingency tables.

For the `Rated` variable:

``` r
table(moviedata$Rated)
```

    ## 
    ##       16+  Approved         G       N/A Not Rated    Passed        PG     PG-13 
    ##         1         4         7         5        40         9        35        33 
    ##         R     TV-MA     TV-PG   Unrated 
    ##        55         5         1         1

It seems that most of our movies are either rated PG, PG-13, R, or not
rated at all.

Now for the `Country` variable:

``` r
table(moviedata$Country)
```

    ## 
    ##                                                          Australia 
    ##                                                                  1 
    ##                                                             Canada 
    ##                                                                  1 
    ##                                     Canada, Germany, United States 
    ##                                                                  1 
    ##               Canada, India, United States, Japan, Thailand, China 
    ##                                                                  1 
    ##                                              Canada, United States 
    ##                                                                  2 
    ##                                                             France 
    ##                                                                  6 
    ##                                              France, United States 
    ##                                                                  1 
    ##                                               France, West Germany 
    ##                                                                  1 
    ##                                                            Germany 
    ##                                                                  5 
    ##                                                    Germany, Canada 
    ##                                                                  1 
    ##                                     Germany, Canada, United States 
    ##                                                                  2 
    ##                             Germany, United Kingdom, United States 
    ##                                                                  1 
    ##                                             Germany, United States 
    ##                                                                  2 
    ##                                                  Hong Kong, France 
    ##                                                                  1 
    ##                                                            Hungary 
    ##                                                                  1 
    ##                                                              India 
    ##                                                                  8 
    ##                                                              Italy 
    ##                                                                  3 
    ##                                                     Italy, Algeria 
    ##                                                                  1 
    ##                                                      Italy, France 
    ##                                                                  1 
    ##                                        Italy, France, West Germany 
    ##                                                                  1 
    ##                                               Italy, United States 
    ##                                                                  1 
    ##                                                              Japan 
    ##                                                                  6 
    ##                                                             Poland 
    ##                                                                  3 
    ##                                                        South Korea 
    ##                                                                  1 
    ##                                                       Soviet Union 
    ##                                                                  3 
    ##                                                              Spain 
    ##                                                                  1 
    ##                                                             Sweden 
    ##                                                                  2 
    ##                                                             Turkey 
    ##                                                                  2 
    ##                                                     United Kingdom 
    ##                                                                  5 
    ##         United Kingdom, Germany, Luxembourg, United States, Canada 
    ##                                                                  1 
    ##                             United Kingdom, Germany, United States 
    ##                                                                  1 
    ##                                              United Kingdom, Italy 
    ##                                                                  1 
    ##                   United Kingdom, Mexico, Hong Kong, United States 
    ##                                                                  1 
    ##                                        United Kingdom, Switzerland 
    ##                                                                  1 
    ##                                      United Kingdom, United States 
    ##                                                                  6 
    ##     United Kingdom, United States, Canada, Australia, Japan, China 
    ##                                                                  1 
    ##                                                      United States 
    ##                                                                 99 
    ##                                              United States, Canada 
    ##                                                                  3 
    ## United States, Canada, United Kingdom, Iceland, Ireland, Australia 
    ##                                                                  1 
    ##                                             United States, Germany 
    ##                                                                  2 
    ##                              United States, Germany, Japan, Canada 
    ##                                                                  1 
    ##                                               United States, Japan 
    ##                                                                  3 
    ##                                              United States, Mexico 
    ##                                                                  1 
    ##                                         United States, Netherlands 
    ##                                                                  2 
    ##                                         United States, Switzerland 
    ##                                                                  1 
    ##                                      United States, United Kingdom 
    ##                                                                  2 
    ##                             United States, United Kingdom, Finland 
    ##                                                                  1 
    ##                                 United States, Yugoslavia, Croatia 
    ##                                                                  1 
    ##                                                       West Germany 
    ##                                                                  1 
    ##                                               West Germany, Mexico 
    ##                                                                  1

It seems that most of our movies were shown in more than one country
while the most popular country was the United States.

Now for the `Language` variable:

``` r
table(moviedata$Language)
```

    ## 
    ##                                         Arabic, French, English 
    ##                                                               1 
    ##                                     Belarusian, Russian, German 
    ##                                                               1 
    ##                        Cantonese, Shanghainese, French, Spanish 
    ##                                                               1 
    ##                                                         English 
    ##                                                              95 
    ##                                         English, American Sign  
    ##                                                               1 
    ##                                        English, Arabic, Turkish 
    ##                                                               1 
    ##                                     English, Cantonese, Spanish 
    ##                                                               1 
    ##                                                 English, French 
    ##                                                               3 
    ##                                         English, French, German 
    ##                                                               1 
    ##                                        English, French, Russian 
    ##                                                               1 
    ##                                        English, French, Spanish 
    ##                                                               1 
    ##                                     English, French, Vietnamese 
    ##                                                               1 
    ##                                                 English, German 
    ##                                                               3 
    ## English, German, Cantonese, Japanese, Hungarian, Arabic, Korean 
    ##                                                               1 
    ##                                         English, German, French 
    ##                                                               1 
    ##                English, German, Hebrew, Spanish, Arabic, Nepali 
    ##                                                               1 
    ##                              English, German, Italian, Japanese 
    ##                                                               1 
    ##                                          English, German, Latin 
    ##                                                               1 
    ##                                                English, Italian 
    ##                                                               4 
    ##                                         English, Italian, Latin 
    ##                                                               1 
    ##                                       English, Italian, Spanish 
    ##                                                               1 
    ##                               English, Italian, Spanish, Korean 
    ##                                                               1 
    ##                               English, Japanese, German, French 
    ##                                                               1 
    ##                                        English, Japanese, Hindi 
    ##                                                               1 
    ##                                                 English, Korean 
    ##                                                               1 
    ##                                               English, Mandarin 
    ##                                                               1 
    ##                                        English, Navajo, Spanish 
    ##                                                               1 
    ##                                              English, Norwegian 
    ##                                                               1 
    ##                                                English, Russian 
    ##                                                               3 
    ##                                 English, Russian, Arabic, Latin 
    ##                                                               1 
    ##                                        English, Russian, French 
    ##                                                               1 
    ##      English, Russian, French, Arabic, Korean, German, Japanese 
    ##                                                               1 
    ##                               English, Russian, French, Italian 
    ##                                                               1 
    ##                                                English, Spanish 
    ##                                                               8 
    ##                                        English, Spanish, French 
    ##                                                               2 
    ##                                         English, Spanish, Latin 
    ##                                                               1 
    ##                             English, Spanish, Ukrainian, Arabic 
    ##                                                               1 
    ##                                                English, Swahili 
    ##                                                               1 
    ##                                                 French, English 
    ##                                                               3 
    ##                                         French, German, English 
    ##                                                               1 
    ##                                French, German, English, Russian 
    ##                                                               1 
    ##                                        French, Italian, Russian 
    ##                                                               1 
    ##                                                          German 
    ##                                                               4 
    ##                                                 German, English 
    ##                                                               2 
    ##                                        German, Quechua, Spanish 
    ##                                                               1 
    ##                                                           Hindi 
    ##                                                               5 
    ##                                                  Hindi, Bengali 
    ##                                                               1 
    ##                                                  Hindi, English 
    ##                                                               1 
    ##                        Hindi, Telugu, Tamil, Malayalam, Kannada 
    ##                                                               1 
    ##                                                       Hungarian 
    ##                                                               1 
    ##                                Italian, English, French, German 
    ##                                                               1 
    ##                                        Italian, English, German 
    ##                                                               1 
    ##                                       Italian, English, Spanish 
    ##                                                               1 
    ##                                 Italian, French, Latin, Chinese 
    ##                                                               1 
    ##                                          Italian, German, Latin 
    ##                                                               1 
    ##                                                        Japanese 
    ##                                                               5 
    ##                                               Japanese, English 
    ##                                                               1 
    ##                                                            None 
    ##                                                               1 
    ##                                                   None, English 
    ##                                                               1 
    ##                                                    None, French 
    ##                                                               1 
    ##                                          None, Russian, English 
    ##                                                               1 
    ##                                                 Polish, English 
    ##                                                               1 
    ##                                        Polish, English, Italian 
    ##                                                               1 
    ##                                        Polish, Italian, English 
    ##                                                               1 
    ##                                         Russian, Italian, Tatar 
    ##                                                               1 
    ##                                                         Spanish 
    ##                                                               1 
    ##                                                Swedish, English 
    ##                                                               1 
    ##                                                  Swedish, Latin 
    ##                                                               1 
    ##                         Thai, Chinese, English, Hindi, Japanese 
    ##                                                               1 
    ##                                                         Turkish 
    ##                                                               2

Naturally, it looks like most of our movies were shown in mutiple
languages. This is not surprising based on the previous table!

Now for the `Year` variable:

``` r
table(moviedata$Year)
```

    ## 
    ## 1920 1922 1925 1926 1927 1928 1931 1933 1934 1936 1937 1939 1940 1941 1944 1945 
    ##    1    1    1    1    1    1    2    1    1    1    1    1    1    2    1    1 
    ## 1948 1950 1952 1953 1954 1956 1957 1958 1959 1960 1962 1964 1966 1967 1968 1970 
    ##    2    1    1    1    3    1    3    1    3    3    1    2    4    1    3    2 
    ## 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1987 1988 
    ##    3    2    3    3    2    2    1    2    4    1    2    1    1    1    2    4 
    ## 1989 1990 1991 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 
    ##    3    3    1    3    3    2    3    2    3    3    3    4    7    4    5    3 
    ## 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 
    ##    9    6    3    3    4    3    3    5    2    2    2    4    2    3    3    3 
    ## 2023 
    ##    3

It looks like almost every year since 1920 there has been a “best”
and/or “worst” movie of all time!

## Histograms

Now let’s look at some numerical summaries. First, I want to see the
distribution of `Score` for each level of the `Rated` variable.

``` r
moviedata %>%
  ggplot( aes(x=Score, color=Rated, fill=Rated)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Score") +
  ylab("Frequency") +
  facet_wrap(~Rated)
```

![](./images/unnamed-chunk-18-1.png)<!-- -->

It seems that the rated R movies had the best scores overall. The PG-13
movies had generally lower scores, PG was fairly uniform, and the Not
Rated movies performed decently well.

Now I want to see the distribution of `BoxOffice` for each level of the
`Rated`.

``` r
moviedata %>%
  ggplot( aes(x=BoxOffice, color=Rated, fill=Rated)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("BoxOffice") +
  ylab("Frequency") +
  facet_wrap(~Rated)
```

![](./images/unnamed-chunk-19-1.png)<!-- -->

Almost all the movies made less than 300 million dollars and most of
them made less than 100 million. The TV-PG, G, and R movies are heavily
right-skewed.

## Plots

Okay now let’s make some plots!

First I want to see scatterplots for `Score` vs `RunTime` for the best
and worst movies together.

``` r
g1 <- ggplot(moviedata, aes(x = Runtime, y = Score))
g1 + labs(title = "RunTime vs Score") +
  geom_point(aes(col=TopBottom), alpha = 0.6, size = 2, 
             position = "jitter") 
```

![](./images/unnamed-chunk-20-1.png)<!-- -->

It appears that the length of the movie has almost no relationship with
the score! I suppose that makes sense…

Now I want to see `Score` vs `BoxOffice`.

``` r
g2 <- ggplot(moviedata, aes(x = Score, y = BoxOffice))
g2 + labs(title = "Score vs BoxOffice") +
  geom_point(aes(col=TopBottom), alpha = 0.6, size = 2, 
             position = "jitter") 
```

![](./images/unnamed-chunk-21-1.png)<!-- -->

There seems to be just a slight positive relationship between these two
variables. I would expect the relationship to be much stronger, but
perhaps people want to see bad movies too!

Okay now let’s look at a barplot for the `Rated` variable.

``` r
g3 <- ggplot(data = moviedata, aes(x = Rated))
g3 + labs(title = "Barplot of Rated") + 
  geom_bar(aes(fill = Rated),position = "dodge") +
  labs(x = "Rated") 
```

![](./images/unnamed-chunk-22-1.png)<!-- -->

Nothing new here… This is just the first contingency table above
reformatted. I’m not surprised that the most extremely rated movies are
rated R.

Now I want to see some boxplots of `Score` for each level of `Rated`.
I’ll do this for the best and worst movies separately.

``` r
g4 <- ggplot(moviedata[1:96,], aes(x = Rated, y = Score))
g4 + geom_boxplot() +
  labs(x = "Rated", title = "BoxPlots of Score and Rated for Top 96") +
  geom_point(position = "jitter", aes(colour = Rated))  
```

![](./images/unnamed-chunk-23-1.png)<!-- -->

``` r
g5 <- ggplot(moviedata[97:196,], aes(x = Rated, y = Score))
g5 + geom_boxplot() +
  labs(x = "Rated", title = "BoxPlots of Score and Rated for Bottom 100") + geom_point(position = "jitter", aes(colour = Rated))  
```

![](./images/unnamed-chunk-23-2.png)<!-- -->

The worst movies were almost exclusively rated PG, PG-13, and R. Some of
the worst PG-13 and R movies have very high scores while some of the
best R movies have mediocre scores.

And finally, let’s see `BoxOffice` vs `Year`.

``` r
g6 <- ggplot(moviedata, aes(x = Year, y = BoxOffice))
g6 + labs(title = "BoxOffice vs Year") +
  geom_point(aes(col=TopBottom),alpha = 0.6, size = 2, 
             position = "jitter") 
```

![](./images/unnamed-chunk-24-1.png)<!-- -->

It looks like most of the best movies of all time happened many years
ago while most of the worst movies of all time were more recent. There
also appears to be an upward trend in the amount of money made as time
went on.
