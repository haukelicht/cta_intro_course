# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Code from slides on dictionary analysis (session 2) 
#' @author Hauke Licht
#' @date   2026-03-17
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

library(quanteda)
library(lexicon) # TODO: install if needed `renv::install("lexicon")`
library(quanteda.corpora)
library(quanteda.sentiment)
library(dplyr)
library(ggplot2)


data("data_corpus_ukmanifestos", package = "quanteda.corpora")

# pre-processing ----

# create tokens from the corpus
toks <- tokens(data_corpus_ukmanifestos)

# print tokens for first three documents
print(toks[1])

# create a document-feature matrix from the tokens
dtm <- dfm(toks)

# print(dtm)

print(dtm, max_ndoc=3, max_nfeat=4)

cat("Number of documents in the DFM: ", ndoc(dtm), "\n")
# NOTE: = number of rows of `dtm`

cat("Number of features in the DFM: ", nfeat(dtm), "\n")
# NOTE: = number of columns of `dtm`

# compute sparsity of the DFM
spar <- sparsity(dtm)
spar

word_freqs <- sort(table(unlist(toks)), decreasing = TRUE)
pdat <- as.data.frame.table(word_freqs)
pdat$rank <- 1:nrow(pdat)
ggplot(pdat, aes(x = rank, y = Freq)) +
  geom_line() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Rank (log scale)", y = "Frequency (log scale)")

spar_upper  <- sparsity(dfm(toks, tolower=FALSE))
spar_default <- sparsity(dfm(toks, tolower=TRUE))
spar_upper; spar_default

spar_stemmed <- sparsity(dfm_wordstem(dtm))
spar_stemmed; spar_default

tokens_example <- tokens(c("running", "better", "dogs", "is"))
tokens_replace(
  tokens_example, 
  pattern = lexicon::hash_lemmas$token, 
  replacement = lexicon::hash_lemmas$lemma
)

dtm_trimmed <- dfm_trim(
  dtm, 
  # discard terms that ooccur < 5 times
  min_termfreq = 5,
  # discard terms that occur in >80% of docs
  max_docfreq = 0.8, docfreq_type = "prop"
)

spar_trimmed <- sparsity(dtm_trimmed)
spar_default; spar_trimmed

# corp <- data_corpus_ukmanifestos
# dtm_w_punct  <- dfm(tokens(corp, remove_punct=FALSE))
# dtm_wo_punct <- dfm(tokens(corp, remove_punct=TRUE))
# sparsity(dtm_w_punct)
# sparsity(dtm_wo_punct)

corp <- data_corpus_ukmanifestos
spar_with_punct    <- sparsity(dfm(tokens(corp, remove_punct=FALSE)))
spar_without_punct <- sparsity(dfm(tokens(corp, remove_punct=TRUE)))
spar_with_punct; spar_without_punct

dtm_no_sws <- dfm_remove(dtm, pattern = stopwords("en"))
spar_no_sws <- sparsity(dtm_no_sws)
spar_default; spar_no_sws


# dictionary analysis ----

## create DFM ----


dtm <- data_corpus_ukmanifestos |> 
  tokens(remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE) |> 
  as.tokens_xptr() |>
  tokens_tolower() |> 
  # lemmatize (based on https://stackoverflow.com/a/62330539)
  tokens_replace(
    pattern = lexicon::hash_lemmas$token, 
    replacement = lexicon::hash_lemmas$lemma
  ) |> 
  dfm()

## apply dictionary -----

data("data_dictionary_LSD2015", package="quanteda.sentiment")

class(data_dictionary_LSD2015)

names(data_dictionary_LSD2015)

dtm_scored <- dfm_lookup(
  dtm, 
  dictionary = data_dictionary_LSD2015, 
  valuetype = "glob" # beacuse the dictionary uses globs (wildcards)
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
    total_pos = positive+neg_negative,
    total_neg = negative+neg_positive,
    # see formula (1) in Proksch et al. (2019)
    sentiment = log( (total_pos+0.5) / (total_neg+0.5) )
  )

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

fp <- file.path("data", "dictionaries", "anew2010", "ANEW2010All.txt")
anew_scores <- readr::read_tsv(fp, col_select = c("Word", "ValMn"))
colnames(anew_scores) <- c("term", "valence")
# NOTE: valence = sentiment

scored <- anew_scores |> 
  corpus(text_field = "term") |> 
  tokens() |> 
  dfm() |> 
  dfm_lookup(data_dictionary_LSD2015, valuetype = "glob") |> 
  convert(to = "data.frame")

bind_cols(scored, anew_scores) |> 
  filter(negative + positive > 0) |> 
  mutate(
    category = ifelse(negative == 1, "negative", "positive")
  ) |> 
  ggplot(aes(x = category, y = valence, fill = category)) +
    geom_boxplot(alpha = 0.8, color = "grey", outliers = FALSE) +
    labs(
      x = "category in LSD dictionary",
      y = "ANEW valence score"
    ) +
    ylim(0, 10) +
    theme(legend.position = "none")

