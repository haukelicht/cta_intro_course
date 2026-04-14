# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Code from slides on dictionary analysis (session 2) 
#' @author Hauke Licht
#' @date   2026-03-17
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

library(readr)
library(dplyr)
library(quanteda)
library(quanteda.sentiment)
library(quanteda.corpora)
library(lexicon) # for lemmatization
library(ggplot2)
source(file.path("R", "token_distinctiveness.R"))

# fightin words ----

## create corpus ----

# step 1: load corpus and prepare DTM 
data("data_corpus_ukmanifestos", package = "quanteda.corpora")
toks <- tokens(
	data_corpus_ukmanifestos, 
	remove_punct=TRUE, 
	remove_symbols=TRUE,
	remove_numbers=TRUE
)
toks <- tokens_remove(toks, stopwords("en"))
dtm <- dfm(toks, tolower=TRUE)

# step 2: subset to selected parties' manifestos
dtm <- dfm_subset(x = dtm, subset = docvars(dtm, "Party") %in% c("Lab", "Con")
)

# step 3: aggregate documents at by grouping variable
grouped_corp <- dfm_group(x = dtm, groups = docvars(dtm, "Party"))

grouped_corp <- grouped_corp[match(c("Lab", "Con"), docnames(grouped_corp)), ]  # ensure row order: Labour, Conservative

## compute log odds ----

# step 1: get tokens' counts by group
tok_counts <- as.matrix(grouped_corp)

# step 2: compute proportions per group
tok_props <- tok_counts/rowSums(tok_counts)

# step 3: smooth proportions by 
#  1. adding a small constant (to avoid zero proportions)
#  2. "clipping" numbers (to avoid 0 or 1)
eps <- 1e-12 
tok_props_clipped <- pmin(pmax(tok_props, eps), 1 - eps)

# step 4: compute odds
odds <- tok_props_clipped / (1 - tok_props_clipped)
odds_ratio <- odds["Lab", ] / odds["Con", ]
log_odds <- log(odds_ratio)
log_odds[1:4]

## compute fighting words ---

# NOTE: textstat_fighting_words() provided in R/token_distinctiveness.R
fw_stats <- textstat_fighting_words(
  x = grouped_corp, 
  group.var = "Party", 
  .pairs = "permutations"
)

str(fw_stats, 1)

fw_stats[["Lab-Con"]] |> 
  select(feature, z_score, Party) |> 
  head(6)

top_zscores <- fw_stats[["Lab-Con"]] |> 
  # for each party group
  group_by(Party) |>
  # sort from highest to lowest
  arrange(desc(abs(z_score))) |> 
  # get top 20
  slice_head(n = 20) |> 
  ungroup()

plot_scores(top_zscores, value.var = "z_score", group.var = "Party")
