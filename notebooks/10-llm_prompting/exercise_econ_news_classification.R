# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Exercise on LLM prompting for text classification
#' @course VU 402150 "Intro to Computational Text Analysis with R"
#' @author Hauke Licht
#' @date   2026-01-20
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

## libraries ----
library(readr)
library(dplyr)
library(ellmer)
library(yardstick)

## data ----

# NOTE: assuming you run this file in th context of our courses R project
fp <- file.path("data", "labeled", "barbera_automated_2021", "barbera_automated_2021-econ_topic.csv")

# load the CSV file
df <- read_csv(fp)

# NOTE: the label categories are "yes" (about economy) and "no" (not about economy)
df$label <- factor(df$label, c("yes", "no"), c("economy", "other"))


## create data splits ----

# NOTE: set random number generation seed for reproducibility
set.seed(1234) 

# randomly assign 20% of documents to test split
df$metadata__split <- sample(
  size = nrow(df), 
  x = c("train", "test"), 
  prob = c(0.8, 0.2),
  replace = TRUE 
)

df |> 
  with(table( metadata__split, label)) |> 
  prop.table(1) |>
  round(3)

# TODO ----

#' 1. Define task instructions for the classification of news headlines into 
#'    _economic news_ and other topics
#'    hint: apply the best practices described in the course slides
#'
#' 2. Define an appropriate response format.
#' 
#' 3. Setup your LLM using the Hugging Face API token and the model  
#' 
#' 4. Test your prompt on a few examples from the training set
#' 
#' 5. Refine your task instructions, if needed
#' 
#' 6. Apply your LLM prompt to the test set and collect predictions
#' 
#' 7. Evaluate the classification performance using appropriate metrics
#'    hint: refer to the materials for our session on supervised classification
#'    on how to evaluate classifiers (especially: precision, recall, F1)
