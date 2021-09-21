packages <- c("tidyverse", "stringr")

lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }  
})

the_data <- read_csv("dataset/movies.csv")

colSums(is.na(the_data))

the_data <- the_data[complete.cases(the_data), ]

str(the_data)

## convert release date to date and country, since it is a 
## combination of both

## first split the character into a list of two
the_data <- the_data %>% mutate(temp = strsplit(released, split = "\\("))
## Now spread into two different columns
the_data <- cbind(the_data, t(data.frame(the_data$temp)))
## rename columns
names(the_data)[names(the_data) == "1"] <- "release_date"
names(the_data)[names(the_data) == "2"] <- "release_country"
## rename row names
rownames(the_data) <- c(1:nrow(the_data))
## remove the temporary column that held the list that
## stored both values
the_data <- the_data[, !names(the_data) == "temp"]
## remove the comma in the date to make conversion to 
## date object easier
the_data$release_date <- gsub(",", "", the_data$release_date)
## convert character to date
the_data$release_date <- as.Date(the_data$release_date, "%B %d %Y")
## remove the last character in release country which happens
## to be a ")"
the_data$release_country <- str_sub(the_data$release_country, 1, nchar(the_data$release_country)-1) 

## Visualise

## histogram of amount grossed by each
ggplot(the_data) + 
  geom_histogram(aes(gross), fill = "blue", color = "white") + 
  scale_x_log10()

## histogram of budget of each
ggplot(the_data) + 
  geom_histogram(aes(budget), fill = "blue", color = "white") + 
  scale_x_log10()

## Is there a relationship between budget and gross?
ggplot(the_data, aes(budget, gross)) + 
  geom_point(alpha = 0.4) + geom_smooth() + scale_x_log10() + 
  scale_y_log10()
## The higher the budget, the higher the amounted grossed

## get the most frequent companies
the_data %>% group_by(company) %>% summarise(n = n()) %>% 
  slice_max(n, n = 10) %>% ggplot() + 
  geom_bar(aes(reorder(company, n), n), stat = "identity", 
           fill = "blue", color = "white") + coord_flip() + 
  xlab("") + ylab("frequency")

## get the most frequent stars and the genres that they are in
top_stars_count <- the_data %>% group_by(star) %>% summarise(n = n()) %>% 
  slice_max(n, n = 10) %>% pull(star)

the_data %>% filter(star %in% top_stars_count) %>% ggplot() + 
  geom_bar(aes(star, fill = genre)) + 
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.text = element_text(size = 6))

## highest grossing stars
the_data %>% group_by(star) %>% 
  summarise(money = mean(gross, na.rm = TRUE)) %>% 
  slice_max(money, n = 10) %>% ggplot() + 
  geom_bar(aes(reorder(star, money), money), 
           stat = "identity", fill = "blue", color = "white") + 
  coord_flip() + xlab("") + ylab("Average gross")


 
  