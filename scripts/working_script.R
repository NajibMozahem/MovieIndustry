packages <- c("tidyverse", "stringr", "cluster", "factoextra",
              "Rtsne", "lares", "ggridges")

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

## look at rating values
unique(the_data$rating)
## we notice some unwanted values like "not rated" and 
## "unrated". Treat these like missing values and remove them
the_data <- the_data[!the_data$rating %in% c("Not Rated", "Unrated"), ]

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
## Note that five na values are generated. Let us look at them:
the_data %>% filter(is.na(release_date)) %>% select(released)
## we see that there are some records where we only have the
## release year. Let us drop these missing records
the_data <- the_data[complete.cases(the_data), ]
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
## The higher the budget, the higher the amount grossed

## Is there a relationship between amount grossed and run time?
ggplot(the_data, aes(gross, runtime)) + 
  geom_point(alpha = 0.4) + geom_smooth() + scale_x_log10()
## the relationship seems weak

## Do longer movies cost more?
ggplot(the_data, aes(budget, runtime)) + 
  geom_point(alpha = 0.4) + geom_smooth() + scale_x_log10()
## Again, seems like a weak relationship

## Let us look at the highest grossing movies
the_data %>% slice_max(gross, n = 10) %>% 
  ggplot() + geom_bar(aes(reorder(substr(name, 1, 20), gross), gross), 
                      stat = "identity", fill = "blue") + 
  xlab("") + coord_flip()

## Look at countries. Look at only top 10 because there are
## too many countries
the_data %>% group_by(country) %>% 
  summarise(n = n()) %>% slice_max(n ,n = 10) %>% 
  ggplot() + 
  geom_bar(aes(country, n), stat = "identity", fill = "blue") + 
   ylab("Frequency") + coord_flip()
## We see that the USA far exceeds the rest. It would therefore
## make sense to create a dummy variable that has two values,
## either USA or foreign. We will do that later when we create
## other variables

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
  theme(axis.text.x = element_text(angle = 90, size = 6, vjust = 0.5),
        legend.text = element_text(size = 6))

## highest grossing stars
the_data %>% group_by(star) %>% 
  summarise(money = mean(gross, na.rm = TRUE)) %>% 
  slice_max(money, n = 10) %>% ggplot() + 
  geom_bar(aes(reorder(star, money), money), 
           stat = "identity", fill = "blue", color = "white") + 
  coord_flip() + xlab("") + ylab("Average gross")

## look at the budget and gross in each year

hold <- the_data %>% group_by(year) %>% 
  summarise(avg_budget = mean(budget, na.rm = TRUE),
            avg_gross = mean(gross, na.rm = TRUE)) %>%
  pivot_longer(avg_budget:avg_gross, names_to = "type") 

hold$type2 <- factor(hold$type, levels = c("avg_gross", "avg_budget"))

hold %>% ggplot() + 
  geom_area(aes(year, value, fill = type2)) + 
  scale_fill_discrete(name = "", labels = c("Gross", "Budget"))

rm(hold)
## we see that both have been increasing over time, but gross
## is increasing more

## check the release time
the_data %>% mutate(release_month = factor(months(release_date),
                                           levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>% 
  group_by(release_month) %>% summarise(n = n()) %>% 
  ggplot() + 
  geom_bar(aes(release_month, n), stat = "identity", fill = "blue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  xlab("Release month") + ylab("Frequency")
## we see that most movies are release in August and October.
## I think that this month variable will be useful. Let us
## create a permanent column for it
the_data <- the_data %>% mutate(release_month = factor(months(release_date),
                                           levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))

## Let us now see if amount grossed and budget differ by month
the_data %>% group_by(release_month) %>% 
  summarise(avg_budget = mean(budget, na.rm = TRUE),
            avg_gross = mean(gross, na.rm = TRUE)) %>% 
  pivot_longer(avg_budget:avg_gross, names_to = "type") %>% 
  ggplot() + 
  geom_bar(aes(release_month, value, fill = type),
           color = "black",
           stat = "identity",
           position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  xlab("Release month") + ylab("Money") + 
  scale_fill_discrete(name = "", labels = c("budget", "gross"))
## We can see that the budget and gross are highest for
## movies released in May and June, followed by November and
## December

## Is the budget related to the score?
ggplot(the_data, aes(budget, score)) + 
  geom_point(alpha = 0.4) + geom_smooth() + scale_x_log10() 
#No. What about amount grossed?
ggplot(the_data, aes(gross, score)) + 
  geom_point(alpha = 0.4) + geom_smooth() + scale_x_log10()
## there seems to be a relationship

## CLUSTER ANALYSIS

## create some useful variables. First, create a dummy 
## variable that indicates whether the star of the movie
## was one of the top stars. There are a total of 1843 unique
## stars. Take the top 10%
top_stars_gross <- the_data %>% group_by(star) %>% 
  summarise(total_gross = sum(gross, na.rm = TRUE)) %>% 
  slice_max(total_gross, n = 185) %>% pull(star)

the_data <- the_data %>% 
  mutate(top_star = ifelse(star %in% top_stars_gross, 
                            TRUE, FALSE))

## Now do the same for top companies. There are 1472 unique
## companies. Again take the top 10%
top_companies_gross <- the_data %>% group_by(company) %>% 
  summarise(total_gross = sum(gross, na.rm = TRUE)) %>% 
  slice_max(total_gross, n = 148) %>% pull(company)

the_data <- the_data %>% 
  mutate(top_company = ifelse(company %in% top_companies_gross, 
                            TRUE, FALSE))

## Now create a variable to record whether the movie was released
## during the high months (May, June, November, December)

the_data <- the_data %>% mutate(release_vacation = ifelse(
  release_month %in% c("May", "June", "November", "September"),
  TRUE,
  FALSE
))

## Now take the log of gross and budget because as we saw
## above, they are highly skewed

the_data <- the_data %>% mutate(gross = log(gross),
                                          budget = log(budget))

## Now create the dummy variable for countries
the_data <- the_data %>% 
  mutate(USA = ifelse(country == "United States", TRUE, FALSE))


## next we keep the variables that we want
data_analysis <- the_data %>% select(rating, score, 
                                      budget, gross, genre,
                                      runtime, top_star, 
                                      top_company, release_vacation,
                                      USA)

## we need to convert character columns into factors
data_analysis <- data_analysis %>% mutate(across(where(is.character), as.factor))

gower_df <- daisy(data_analysis, metric = "gower")
summary(gower_df)
cluster_number <- 2:20
sil_values <- map_dbl(cluster_number, function(x){
  pam_clusters = pam(as.matrix(gower_df), diss = TRUE, k = x)
  pam_clusters$silinfo$avg.width
})
cluster_data <- data.frame(clusters = cluster_number, silhouette_width = sil_values)
ggplot(cluster_data, aes(clusters, silhouette_width)) + 
  geom_point() + geom_line() + scale_x_continuous(breaks = c(1:20))
## based on the graph, 12 clusters seems to be the optimal number

## Run the cluster analysis again specifying the optimal
## number of clusters
pam_data = pam(gower_df, diss = TRUE, k = 12)

## Now visualise
tsne_object <- Rtsne(gower_df, is_distance = TRUE)
tsne_df <- tsne_object$Y %>% data.frame() %>% setNames(c("X", "Y")) %>% 
  mutate(cluster = factor(pam_data$clustering))
ggplot(tsne_df, aes(X, Y)) + geom_point(aes(color = cluster))

## We see that the movies seem to be distributed more or less
## evenly among the clusters.

## Let us add the clustering variable to the original data set
data_analysis <- data_analysis %>% mutate(cluster = 
                                            pam_data$clustering)
#the_data <- the_data %>% mutate(cluster = pam_data$clustering)

## Check the number of movies in each cluster
ggplot(data_analysis) + geom_bar(aes(cluster), fill = "blue") + 
  scale_x_continuous(breaks = c(1:12))
## We see that there isnt a dominant cluster that contains
## most movies.

## Look at differences in amount grossed
ggplot(data_analysis) + 
  geom_density_ridges(aes(x = gross, y = factor(cluster)), 
                      fill = "blue") + ylab("cluster")

ggplot(data_analysis) + 
  geom_density_ridges(aes(x = budget, y = factor(cluster)),
                      fill = "blue") + ylab("cluster")
## Look at cluster formation in terms of whether the movie
## had a top star or not
ggplot(data_analysis) + 
  geom_bar(aes(cluster, fill = top_star), position = "fill") + 
  scale_x_continuous(breaks = c(1:12))

## Look at cluster formation in terms of whether the movie
## was produced by a top company or not
ggplot(data_analysis) + 
  geom_bar(aes(cluster, fill = top_company), position = "fill") + 
  scale_x_continuous(breaks = c(1:12))

## Look at cluster formation in terms of whether the movie
## was produced by an American company or not
ggplot(data_analysis) + 
  geom_bar(aes(cluster, fill = USA), position = "fill") + 
  scale_x_continuous(breaks = c(1:12))

## Try to see whether there are differences in genres among
## the clusters
data_analysis %>% group_by(cluster, genre) %>% 
  summarise(n = n()) %>% ggplot() + 
geom_raster(aes(cluster, genre, fill = n)) + 
  scale_x_continuous(breaks = c(1:12))
## We see that most genres are spread throughout the cluster.
## There are some exceptions, such as family being grouped
## in three clusters.
