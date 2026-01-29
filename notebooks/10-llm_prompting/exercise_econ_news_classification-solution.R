# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Solution for exercise on LLM prompting for text classification
#' @course VU 402150 "Intro to Computational Text Analysis with R"
#' @author Hauke Licht
#' @date   2026-01-29
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

## libraries ----
library(readr)
library(dplyr)
library(ellmer)
library(yardstick)

classification_report <- metric_set(
  precision, 
  recall,
  f_meas, 
  bal_accuracy, 
  accuracy, 
  mcc
)

## data ----

# NOTE: assuming you run this file in th context of our courses R project
fp <- file.path("data", "labeled", "barbera_automated_2021", "barbera_automated_2021-econ_topic.csv")

# load the CSV file
df <- read_csv(fp)

df$label <- factor(df$label, levels = c("yes", "no"))


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

# TODOs ----

#' STEP 1. Define task instructions for the classification of news headlines into 
#'    _economic news_ and other topics
#'    hint: apply the best practices described in the course slides
instructions <- '
You will be presented with the text of a m newspaper articles.

Your task is to classify whether or not the article provides information about how the US economy is doing.

Process
1. Read the article
2. 

For example, an article with this headline probably gives you an indication of
how the U.S. economy is performing: “Unemployment just went up by 2%.”
Note that you are only being asked whether the article tells you something
about how the United States economy is doing – so articles that only contain
information about the economy in other countries are not relevant.

You must classify the articel into one of the following categories:

- "yes": the article gives an indication of how the United States economy is performing.
- "no": otherwise

## Details

For example, an article with the headline “Unemployment just went up by 2%” probably gives you an indication of how the U.S. economy is performing.

Important: You are only being asked whether the article tells you something about how the United States economy is doing. Articles that only contain information about the economy in other countries are not relevant.

## Step-by-step instructions

1. Carefully read the text of the sentence, paying close attention to details.
2. Reason about whether the sentence provides information about the state of the US economy.
3. Classify the sentence with the category it belongs to.

## Response format

Return your response as a JSON dictionary with the following fields:

- "reasoning": your reasoning of what category should be assigned to the text
- "category": the category you assigned to the sentence, either "yes" or "no"
'
# NOTE: the instructions end here

# remove leading and trailing white spaces
instructions <- trimws(instructions)


#' STEP 2. Define an appropriate response format.
#' 
response_format <- type_object(
  reasoning = type_string(
    description = "Your reasoning of whether or not the article gives an indication of how the United States economy is performing"
  ),
  category = type_enum(
    c("yes", "no"), 
    description = "Your classification decision"
  ),
  .description = "Response format for news article classification task"
)


#' STEP 3. Setup your LLM using the Hugging Face API token and the model  

stopifnot("HUGGINGFACE_API_KEY env variable not see" = !is.na(Sys.getenv("HUGGINGFACE_API_KEY", unset = NA)))

model_id <- "Qwen/Qwen3-Next-80B-A3B-Instruct:together"

model <- chat_huggingface(
  system_prompt = instructions,
  model = model_id,
  params = params(seed = 42),
  echo = NULL,
)


#' STEP 4. Test your prompt on a few examples from the training set
set.seed(1234)
train_df <- df |> 
  filter(metadata__split == "train") |> 
  group_by(label) |>
  # generate a random number to allow drawing random samples
  mutate(r_ = dense_rank(runif(n()))) |>
  ungroup() |> 
  arrange(r_)

# draw first ten examples (five per label class)
expls <- subset(train_df, r_ >= 1 & r_ <= 5, c(text, label))

annotations <- parallel_chat_structured(
  model,
  as.list(expls$text),
  type = response_format,
)
  
annotations <- bind_cols(expls, annotations)

# evaluate
classification_report(
  data = annotations,
  truth = label,
  estimate = category
)
#' @findings  precision low and recall perfect because LLM 
#'  "overshoots" (i.e., assigns "yes" too often)


#' STEP 5. Refine your task instructions, if needed
#' 
# inspect cases where classification ≠ label
annotations |> 
  filter(label != category) |>
  with(paste(category, "(pred) vs", label, "(true):\n", reasoning)) |> 
  cat(sep = "\n------------\n")


#' @note
#'  we may adapt the prompt based on these insights, e.g., by specifying rules like
#'  
#'  - only classify as "yes" if the state of the economic is in the focus of 
#'    the article, not if it is peripheral

#' STEP 6. Apply your LLM prompt to the test set and collect predictions
 
# NOTE: we subsample the test data because it is large

set.seed(1234)
test_df <- df |> 
  filter(metadata__split == "test") |> 
  group_by(label) |>
  sample_n(50) |> 
  ungroup() |> 
  select(text, label)

annotations <- parallel_chat_structured(
  model,
  as.list(test_df$text),
  type = response_format,
)

#' STEP 7. Evaluate the classification performance using appropriate metrics
#'    hint: refer to the materials for our session on supervised classification
#'    on how to evaluate classifiers (especially: precision, recall, F1)

annotations <- bind_cols(test_df, annotations)

# evaluate
classification_report(
  data = annotations,
  truth = label,
  estimate = category
)

#' @findings The issue of overshooting persists:
#'    - good recall = 0.98 
#'    - but precision = 0.653 (~1 in 3 predicted "yes" is actually "no")
#'  but comparing to the Barbera et al. results (Figure 3), whose 
#'   _supervised_ text classifier
#'    - accuracy = 0.71 (vs. our 0.73)
#'    - precision = 0.713
#'  the LLM is not that bad at all ;)
