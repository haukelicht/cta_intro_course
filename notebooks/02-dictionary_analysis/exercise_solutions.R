# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Solutions for exercises in session 02 on dictionary analysis
#' @author Hauke Licht
#' @date   2026-04-04
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

library(quanteda)
library(lexicon) # TODO: install if needed `renv::install("lexicon")`
library(quanteda.corpora)
library(quanteda.sentiment)
library(dplyr)
library(ggplot2)



# Exercise 1 ----

data("data_corpus_immigrationnews", package = "quanteda.corpora")

## task 1 ----


# let's first create a grid of all combinations of the four options
options_grid <- expand.grid(
  lowercase = c(TRUE, FALSE),
  remove_punct = c(TRUE, FALSE),
  remove_stopwords = c(TRUE, FALSE),
  stem = c(TRUE, FALSE)
)

# now we can iterate over the rows of this grid, creating a DFM for each combination of options
options_grid$sparsity <- NA_real_

for (i in seq_len(nrow(options_grid))) {
  opts <- options_grid[i, ]

  toks <- tokens(data_corpus_immigrationnews, remove_punct = opts$remove_punct)
  toks <- as.tokens_xptr(toks)

  if (opts$lowercase) {
    toks <- tokens_tolower(toks)
  }

  # NOTE: stop words in `stopwords("en")` are all lower case already
  if (opts$remove_stopwords) {
    toks <- tokens_remove(toks, pattern = stopwords("en"))
  }

  # NOTE: stem after stopword removal
  if (opts$stem) {
    toks <- tokens_wordstem(toks, language = "en")
  }

  # NOTE: turn off lowercasing in dfm() since we already did it above if needed
  dtm <- dfm(toks, tolower = FALSE)

  options_grid$sparsity[i] <- sparsity(dtm)
}

options_grid[which.min(options_grid$sparsity), ]

# NOTE: no need to use `dfm_trim()` here;
#       we can just rely on output of `textstat_frequency`
#       created previously as `tok_freqs`

## tasks 2.1 ----

# tokens that occur in more than 90% of documents
threshold_ <- ndoc(dtm)*0.90
tok_freqs[tok_freqs$docfreq > threshold_, ]

## task 2.2 ----

# tokens that occur less than 5 times in the corpus
tok_freqs[tok_freqs$frequency < 5, ]

# Exercise 2 ----

data("data_corpus_ukmanifestos", package = "quanteda.corpora")

# create document-term matrix
dtm <- data_corpus_ukmanifestos |> 
  tokens(remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE) |> 
  tokens_tolower() |> 
  tokens_replace(
    pattern = lexicon::hash_lemmas$token, 
    replacement = lexicon::hash_lemmas$lemma
  ) |> 
  dfm()

## apply dictionary -----

# # NOTE: in the slides, I used the LSD dictionary
# data("data_dictionary_LSD2015", package="quanteda.sentiment")

# check what other dictionaries are in the `quanteda.sentiment` package 
data(package="quanteda.sentiment")

# let's try a few

# Nielsen's (2011) 'new ANEW' valenced word list
data("data_dictionary_AFINN", package="quanteda.sentiment")
print(data_dictionary_AFINN)
# NOTE: this dictionary doesn't have the positive and negative categories we require
#  the same applies to 
# - `data_dictionary_vader` (Affective Norms for English Words, ANEW): also has only one category with scores
# - `data_dictionary_ANEW` (Affective Norms for English Words, ANEW): has only categories "pleasure", "arousal", "dominance

# here is one that works: Positive and negative words from Hu and Liu (2004)
data("data_dictionary_HuLiu", package="quanteda.sentiment")
names(data_dictionary_HuLiu)
# others that'd work:
#  - `data_dictionary_NRC` (NRC Word-Emotion Association Lexicon): many categories, including "positive" and "negative"
#  - `data_dictionary_LoughranMcDonald` (Loughran and McDonald Sentiment Word Lists): many categories, including "POSITIVE" and "NEGATIVE"



dtm_scored <- dfm_lookup(
  dtm, 
  dictionary = data_dictionary_HuLiu, 
  valuetype = "fixed" # beacuse the dictionary uses globs (wildcards)
)

# result just contains columns ("features") recording
#  count of terms matching a given dictionary category
dtm_scored

# get document variables
doc_metadata <- docvars(dtm_scored)
doc_metadata$doc_id <- docnames(dtm_scored)

scores_df <- dtm_scored |> 
  convert("data.frame", docid_field = "doc_id") |> 
  left_join(doc_metadata, by = "doc_id") |> 
  mutate(
    # NOTE: we need to change the next two lines compared to code from slides
    total_pos = positive, 
    total_neg = negative,
    # see formula (1) in Proksch et al. (2019)
    sentiment = log( (total_pos+0.5) / (total_neg+0.5) )
  )

# NOTE: this is just copied from code from slides
scores_df <- scores_df |> 
  # subset
  filter(
    Country=="UK",
    Type=="natl",
    Language=="en",
    Party %in% c("Con", "Lab", "Lib", "LibSDP", "LD", "DUP", "SNP")
  ) |> 
  transmute(
    # recode the LibDems party label
    party = ifelse(
      Party %in% c("Lib", "LibSDP", "LD"), 
      "Lib", 
      Party
    ),
    year = as.integer(Year),
    sentiment
  ) 

party_colors_map <- c(
  "Con" = "#0087DC",
  "Lab" = "#E4003B",
  "Lib" = "#FAA61A",
  "DUP" = "#D46A4C",
  "SNP" = "#FFFF00"
)

scores_df |> 
  ggplot(aes(x=party, y=sentiment, fill=party)) +
  geom_boxplot(alpha = 0.8, color = "grey", outliers = FALSE) + 
  scale_fill_manual(breaks = names(party_colors_map), values=party_colors_map) +
  labs(x=NULL) + 
  theme(legend.position="none")

# asses gov't opposition differences
sitting_governments_at_elections <- tribble(
  ~year, ~sitting_government,
  1945, "Lab",
  1950, "Lab",
  1951, "Lab", # Con won  election
  1955, "Con",
  1959, "Con",
  1964, "Con", # Lab won election
  1966, "Lab",
  1970, "Lab", # Con won election
  1974, "Con", # Lab won election
  1979, "Lab", # Con won election
  1983, "Con",
  1987, "Con",
  # 1990, "Con",
  1992, "Con",
  1997, "Con", # Lab won election
  2001, "Lab",
  # 2003, "Lab",
  2005, "Lab",
)

scores_df |> 
  filter(party %in% c("Con", "Lab")) |> 
  left_join(sitting_governments_at_elections, by="year") |>
  mutate(status = ifelse(party == sitting_government, "Government", "Opposition")) |> 
  ggplot(aes(x=status, y=sentiment, fill=party)) +
  geom_boxplot(alpha = 0.8, color = "grey", outliers = FALSE) + 
  scale_fill_manual(breaks = names(party_colors_map), values=party_colors_map) +
  labs(x=NULL) +
  facet_wrap(~party) + 
  theme(legend.position = "none")

# Exercise 3 ----

# NOTE: no solution provided yet since we did not yet cover the relevant material during session 2

# Exercise 4 ----

# NOTE: no solution provided yet since we did not yet cover the relevant material during session 2

