# see working directory
getwd()

# change your working directory
setwd()

# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
list.files(path)

# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# copy file from dslabs package to your working directory
file.copy(fullpath, getwd())

# check if the file exists
file.exists(filename)

# readr and readxl
library(dslabs)
library(tidyverse)    # includes readr
library(readxl)

# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)

# read file in CSV format
dat <- read_csv(filename)

#read using full path
dat <- read_csv(fullpath)
head(dat)

#Ex：
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(path, filename))
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))

#check class
# filename is defined in the previous video
# read.csv converts strings to factors
dat2 <- read.csv(fullpath)
class(dat2$abb)
class(dat2$region)

#download from internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

#assessment 2
library(tidyverse)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
test <- read_csv(url, col_names = FALSE)

#section 2
library(tidyverse)
library(dslabs)
data(gapminder)

# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

# using gather and spread functions to convert to tidy format and back
# original wide data
library(tidyverse) 
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# tidy data from dslabs
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)

# gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

# gather all columns except country
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)

# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

# convert gathered column names to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

# ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

# separate, unite
# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))

# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 

# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")

# full code for tidying data
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)


stats <- read_table("stats")
tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge") %>% 
  spread(key = variable_name, value = value)

tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name1", "variable_name2"), sep = "_", fill = "right") %>% 
  unite(col = variable_name, variable_name1, variable_name2, sep = "_") %>% 
  spread(key = variable_name, value = value)

tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_") %>% 
  spread(key = variable_name, value = value)

# part 2 assessment
library(tidyverse)
library(dslabs)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- gather(co2_wide,month,co2,-year)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()


library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat_tidy <- spread(dat, gender, admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2 <- unite(tmp, column_name, c(key, gender))

#joins
# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

# experiment with different joins
left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)

#bind
bind_cols(a = 1:3, b = 4:6)

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

#sets
# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

# set difference of vectors or data frames
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)

#assessment
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  rbind(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

atleast_one_award <- AwardsPlayers %>% filter(yearID == 2016) %>% select(playerID)
  test0 <- intersect(atleast_one_award[,1], top_names$playerID)
  
won_award_not_top10 <- AwardsPlayers %>% filter(yearID == 2016) %>% select(playerID)
 test <- setdiff(won_award_not_top10[,1], top_names$playerID)
  
# web scraping
 # import a webpage into R
 library(rvest)
 url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
 h <- read_html(url)
 class(h)
 h
 
 tab <- h %>% html_nodes("table")
 tab <- tab[[2]]
 
 tab <- tab %>% html_table
 class(tab)
 
 tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
 head(tab)

 # css
 h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
 recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
 prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
 ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

 get_recipe <- function(url){
   h <- read_html(url)
   recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
   prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
   ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
   return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
 } 
 
 get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")
 
 #assessment
 library(rvest)
 url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
 h <- read_html(url)
 
 nodes <- html_nodes(h, "table")
 
 html_text(nodes[[8]])
 
 html_table(nodes[[8]])

 tab <- h %>% html_nodes("table")
 tab1 <- tab[[1]]
 tab2 <- tab[[2]]
 tab3 <- tab[[3]]
 tab4 <- tab[[4]]
 
 tab <- tab4 %>% html_table
 
 html_table(nodes[[21]])
 html_table(nodes[[20]])
 html_table(nodes[[19]])
 
 library(tidyverse)
 
 tab_1 <- html_table(nodes[[10]])
 tab_1 <- tab_1 %>% filter(X1 != "No.") %>%
   select(X2:X4) %>%
   setNames(c("Team", "Payroll", "Average"))
 names(tab_1)
 
 tab_2 <- html_table(nodes[[19]])
 tab_2 <- tab_2 %>% filter(X1 != "Team") %>% 
   setNames(c("Team", "Payroll", "Average"))
 tab_3 <- full_join(tab_1, tab_2, by = "Team")

 library(rvest)
 library(tidyverse)
 url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
 h <- read_html(url)
 tab <- h %>% html_nodes("table")
 tab_test <- html_table(tab[[5]], fill=TRUE)

 #string operations
 # murders_raw was defined in the web scraping section
 
 # detect whether there are commas
 commas <- function(x) any(str_detect(x, ","))
 murders_raw %>% summarize_all(funs(commas))
 
 # replace commas with the empty string and convert to numeric
 test_1 <- str_replace_all(murders_raw$population, ",", "")
 test_1 <- as.numeric(test_1)
 
 # parse_number also removes commas and converts to numeric
 test_2 <- parse_number(murders_raw$population)
 identical(test_1, test_2)
 
 murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
 murders_new %>% head
 
 # string operations 2
 
 # load raw heights data and inspect
 library(dslabs)
 data(reported_heights)
 class(reported_heights$height)
 
 # convert to numeric, inspect, count NAs
 x <- as.numeric(reported_heights$height)
 head(x)
 sum(is.na(x))
 
 # keep only entries that result in NAs
 reported_heights %>% mutate(new_height = as.numeric(height)) %>%
   filter(is.na(new_height)) %>% 
   head(n=10)
 
 # calculate cutoffs that cover 99.999% of human population
 alpha <- 1/10^6
 qnorm(1-alpha/2, 69.1, 2.9)
 qnorm(alpha/2, 63.7, 2.7)
 
 # keep only entries that either result in NAs or are outside the plausible range of heights
 not_inches <- function(x, smallest = 50, tallest = 84){
   inches <- suppressWarnings(as.numeric(x))
   ind <- is.na(inches) | inches < smallest | inches > tallest
   ind
 }
 
 # number of problematic entries
 problems <- reported_heights %>% 
   filter(not_inches(height)) %>%
   .$height
 length(problems)
 
 # 10 examples of x'y or x'y" or x'y\"
 pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
 str_subset(problems, pattern) %>% head(n=10) %>% cat
 
 # 10 examples of x.y or x,y
 pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
 str_subset(problems, pattern) %>% head(n=10) %>% cat
 
 # 10 examples of entries in cm rather than inches
 ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
 ind <- ind[!is.na(ind)]
 problems[ind] %>% head(n=10) %>% cat
 
 # regex
 # load stringr through tidyverse
 library(tidyverse)
 library(dslabs)
 # detect whether a comma is present
 pattern <- ","
 str_detect(murders_raw$total, pattern) 
 
 # show the subset of strings including "cm"
 str_subset(reported_heights$height, "cm")
 
 # use the "or" symbol inside a regex (|)
 yes <- c("180 cm", "70 inches")
 no <- c("180", "70''")
 s <- c(yes, no)
 str_detect(s, "cm") | str_detect(s, "inches")
 str_detect(s, "cm|inches")
 
 # highlight the first occurrence of a pattern
 str_view(s, pattern)
 
 # highlight all instances of a pattern
 str_view_all(s, pattern)
 
 # s was defined in the previous video
 yes <- c("5", "6", "5'10", "5 feet", "4'11")
 no <- c("", ".", "Five", "six")
 s <- c(yes, no)
 pattern <- "\\d"
 
 # [56] means 5 or 6
 str_view(s, "[56]")
 
 # [4-7] means 4, 5, 6 or 7
 yes <- as.character(4:7)
 no <- as.character(1:3)
 s <- c(yes, no)
 str_detect(s, "[4-7]")
 
 # ^ means start of string, $ means end of string
 pattern <- "^\\d$"
 yes <- c("1", "5", "9")
 no <- c("12", "123", " 1", "a4", "b")
 s <- c(yes, no)
 str_view(s, pattern)
 
 # curly braces define quantifiers: 1 or 2 digits 
 pattern <- "^\\d{1,2}$"
 yes <- c("1", "5", "9", "12")
 no <- c("123", "a4", "b")
 str_view(c(yes, no), pattern)
 
 # combining character class, anchors and quantifier
 pattern <- "^[4-7]'\\d{1,2}\"$"
 yes <- c("5'7\"", "6'2\"",  "5'12\"")
 no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
 str_detect(yes, pattern)
 str_detect(no, pattern)

 
 # number of entries matching our desired pattern
 pattern <- "^[4-7]'\\d{1,2}\"$"
 sum(str_detect(problems, pattern))
 
 # inspect examples of entries with problems
 problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
 str_subset(problems, "inches")
 str_subset(problems, "''")
 
 # replace or remove feet/inches words before matching
 pattern <- "^[4-7]'\\d{1,2}$"
 problems %>% 
   str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
   str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
   str_detect(pattern) %>% 
   sum()
 
 # R does not ignore whitespace
 identical("Hi", "Hi ")
 
 # \\s represents whitespace
 pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
 str_subset(problems, pattern_2)
 
 # * means 0 or more instances of a character
 yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
 no <- c("A2B", "A21B")
 str_detect(yes, "A1*B")
 str_detect(no, "A1*B")
 
 # test how *, ? and + differ
 data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
            none_or_more = str_detect(yes, "A1*B"),
            nore_or_once = str_detect(yes, "A1?B"),
            once_or_more = str_detect(yes, "A1+B"))
 
 # update pattern by adding optional spaces before and after feet symbol
 pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
 problems %>% 
   str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
   str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
   str_detect(pattern) %>% 
   sum() 

 
 # define regex with and without groups
 pattern_without_groups <- "^[4-7],\\d*$"
 pattern_with_groups <-  "^([4-7]),(\\d*)$"
 
 # create examples
 yes <- c("5,9", "5,11", "6,", "6,1")
 no <- c("5'9", ",", "2,8", "6.1.1")
 s <- c(yes, no)
 
 # demonstrate the effect of groups
 str_detect(s, pattern_without_groups)
 str_detect(s, pattern_with_groups)
 
 # demonstrate difference between str_match and str_extract
 str_match(s, pattern_with_groups)
 str_extract(s, pattern_with_groups)
 
 # improve the pattern to recognize more events
 pattern_with_groups <-  "^([4-7]),(\\d*)$"
 yes <- c("5,9", "5,11", "6,", "6,1")
 no <- c("5'9", ",", "2,8", "6.1.1")
 s <- c(yes, no)
 str_replace(s, pattern_with_groups, "\\1'\\2")
 
 # final pattern
 pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
 
 # combine stringr commands with the pipe
 str_subset(problems, pattern_with_groups) %>% head
 str_subset(problems, pattern_with_groups) %>% 
   str_replace(pattern_with_groups, "\\1'\\2") %>% head

 
 
 # function to detect entries with problems
 not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
   inches <- suppressWarnings(as.numeric(x))
   ind <- !is.na(inches) &
     ((inches >= smallest & inches <= tallest) |
        (inches/2.54 >= smallest & inches/2.54 <= tallest))
   !ind
 }
 
 # identify entries with problems
 problems <- reported_heights %>% 
   filter(not_inches_or_cm(height)) %>%
   .$height
 length(problems)
 
 converted <- problems %>% 
   str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
   str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
   str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format
 
 # find proportion of entries that fit the pattern after reformatting
 pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
 index <- str_detect(converted, pattern)
 mean(index)
 
 converted[!index]    # show problems  
sum(index) 

# assessment
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")


converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
converted[!index]

# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)

# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")

str_replace(s, "^([56])'?$", "\\1'0")

pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")


#problematic functions
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

#conversion
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

new_heights %>% arrange(height) %>% head(n=7)

# str_split example
# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",") 
x %>% head()
col_names <- x[[1]]
x <- x[-1]

# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head()
map(x, 1) %>% head()

# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)

dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

library(dslabs)
data("research_funding_rates")
research_funding_rates

library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

raw_data_research_funding_rates <- txt[2]
data("raw_data_research_funding_rates")

raw_data_research_funding_rates %>% head
tab <- str_split(raw_data_research_funding_rates, "\n")

tab <- tab[[1]]
tab %>% head

the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

identical(research_funding_rates, new_research_funding_rates)

# life expectancy time series for Caribbean countries
library(dslabs)
data("gapminder")
gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# display long country names
gapminder %>% 
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 

# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

df <- tibble(
  a = list(c("a", "b"), "c"),
  b = list(1:2, 3),
  c = c(11, 22)
)
df %>% unnest(c(a, b))

#assessment
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

sum(str_detect(polls$Remain, "%", negate=TRUE))

#dates

# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate: the tidyverse date package
library(lubridate)

# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE)    # extract month label

# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

# tweets sentiment analysis
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST"))

library(dslabs)
data("trump_tweets")

head(trump_tweets)

names(trump_tweets)

trump_tweets %>% select(text) %>% head

trump_tweets %>% count(source) %>% arrange(desc(n))

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

library(tidytext)
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

stop_words

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

sentiments
get_sentiments("bing")
get_sentiments("afinn")

get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

tweet_words %>% group_by(source) %>% summarize(n = n())

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))


android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#assessment
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits
data(brexit_polls)

brexit_polls[month(brexit_polls$startdate)=="04"]

df <- data.frame(day=weekdays(brexit_polls$enddate))
df %>% group_by(day) %>% count()

data(movielens)
df <- data.frame(date_gen=as_datetime(movielens$timestamp))
df %>% mutate(year=year(date_gen)) %>% group_by(year) %>% count() %>% arrange(desc(n))
df %>% mutate(hour=hour(date_gen)) %>% group_by(hour) %>% count() %>% arrange(desc(n))

#assignment 2
ibrary(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata

hit <- data.frame(match_=str_detect(gutenberg_metadata$title, "Pride and Prejudice"), gutenberg_metadata[,])
hit %>% filter(match_==TRUE)

gutenberg_works(title == "Pride and Prejudice", languages = "en",
                only_text = TRUE, distinct = TRUE)

book <- gutenberg_download(1342)
words <- book %>% unnest_tokens(word, text)

test <- words %>% filter(!word %in% stop_words$word &
                           !str_detect(word, "\\d+")) %>%
  group_by(word) %>% count() %>% arrange(desc(n)) %>%
  filter(n>100)

words2 <- words %>% filter(!word %in% stop_words$word &
                           !str_detect(word, "\\d+"))
afinn <- get_sentiments("afinn")

afinn_sentiments <- words2 %>% inner_join(afinn, by = "word") %>% 
  mutate(sentiment=value) %>%
  select(word, sentiment)

mean(afinn_sentiments$sentiment > 0)

test2 <- afinn_sentiments %>% filter(sentiment==4) 

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts
