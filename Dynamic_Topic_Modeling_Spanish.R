## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tm)
library(topicmodels)
library(ldatuning)
library(LDAvis)
library(tidytext)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(data.table)
library(servr)
library(stringr)  # For str_pad
library(ggthemes)



## ----data-preparation-----------------------------------------------------------------------------------------------------------
# Load CSV data
file_path <- "./ALL_articles.csv"  # Update with your CSV file path
data <- read.csv(file_path, colClasses = "character")

head(data)

topic_yearly_distribution <- read.csv("./tables/topic_yearly_distribution.csv")

topic_yearly_distribution5 <- read.csv("./tables/migration_topic_yearly_distribution.csv")

migration_keywords <- read.csv("./tables/migration_keywords.csv")
migration_titles <- read.csv("./tables/migration_top_titles.csv")
titles <- read.csv("./tables/top_titles.csv")
keywords <- read.csv("./tables/topic_keywords.csv")

# gamma_values <- read.csv("./tables/gamma_values.csv")
# 
# thresholds <- read.csv("./tables/thresholds.csv")

keywords <- keywords %>% 
  mutate(`Topic Name` = topic_name, Term = term) %>% 
  select(`Topic Name`, beta, Term) %>% 
  group_by(`Topic Name`) %>% 
  slice_max(beta, n = 5 )

titles <- titles %>% 
  mutate(`Topic Name` = topic_name, Title = title) %>% 
  select(`Topic Name`, max_gamma, Title) %>% 
  group_by(`Topic Name`) %>% 
  slice_max(max_gamma, n = 5 )

migration_keywords <- migration_keywords %>% 
  mutate(`Topic Name` = topic_name, Term = term) %>% 
  select(`Topic Name`, beta, Term) %>% 
  group_by(`Topic Name`) %>% 
  slice_max(beta, n = 5 )

migration_titles <- migration_titles %>% 
  mutate(`Topic Name` = topic_name, Title = title) %>% 
  select(`Topic Name`, max_gamma, Title) %>% 
  group_by(`Topic Name`) %>% 
  slice_max(max_gamma, n = 5 )


## model 1 ----------------------------------------------------------------------------------------------------------------------

## Text Preprocessing

# Create a corpus
corpus <- Corpus(VectorSource(data$processed_text))

## Tokenization and Term-Document Matrix

# Create a Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms to optimize computation
dtm <- removeSparseTerms(dtm, 0.99)

# Summary of the DTM
dim(dtm)

# 
# ## -------------------------------------------------------------------------------------------------------------------------------
topic_dist <- ggplot(topic_yearly_distribution, aes(x = year, y = num_docs, fill = as.factor(topic_name))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # "dodge" separates the bars for each topic
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a")) +
  labs(
    title = "Topic Distribution per Year (Count of Papers)",
    subtitle = "Number of papers assigned to each topic per year",
    x = "Year",
    y = "Number of Papers",
    fill = "Topic"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90", size = 0.5),  # Light grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white")
  )


# 
# ## -------------------------------------------------------------------------------------------------------------------------------
migration_topic_dist <- ggplot(topic_yearly_distribution5, aes(x = year, y = num_docs, fill = as.factor(topic_name))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # "dodge" separates the bars for each topic
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a")) +
  labs(
    title = "Topic Distribution per Year (Count of Papers)",
    subtitle = "Number of papers assigned to each topic per year",
    x = "Year",
    y = "Number of Papers",
    fill = "Topic"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90", size = 0.5),  # Light grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white")
  )



# ## -------------------------------------------------------------------------------------------------------------------------------
# # Make sure the gamma_values is a data frame and the gamma values are numeric
# gamma_values <- data.frame(x = as.numeric(gamma_values$x))
# 
# # Overall average threshold for visualization
# thresholds <- as.numeric(thresholds)  # Assuming thresholds is a numeric vector
# average_gamma <- mean(thresholds)
# 
# # NYT-inspired theme customization
# nyt_theme <- theme(
#   panel.background = element_rect(fill = "white", color = "white"),
#   panel.grid.major = element_line(color = "grey90"),
#   panel.grid.minor = element_blank(),
#   axis.title = element_text(size = 14, face = "bold"),
#   axis.text = element_text(size = 12, color = "black"),
#   plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#   plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
#   plot.caption = element_text(size = 10, hjust = 1, color = "grey40"),
#   legend.position = "none"
# )


