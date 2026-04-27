# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Code from slides on topic modeling
#' @author Hauke Licht
#' @date   2026-04-28
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup 
library(quanteda)
library(quanteda.corpora)
library(topicmodels)
source(file.path("R", "tm_plotting.R"))

# prepare the data ----

# load corpus 
data("data_corpus_ungd2017", package = "quanteda.corpora")


# segment into speech paragraphs
speech_paragraphs <- corpus_segment(
  data_corpus_ungd2017, 
  pattern = "\n+", 
  valuetype = "regex"
)


# create the DTM
dtm <- speech_paragraphs |> 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, xptr = TRUE) |> 
  tokens_tolower() |> 
  tokens_remove(pattern = stopwords("en")) |> 
  tokens_wordstem() |> 
  # NOTE: create n-grams (unigrams, bigrams, trigrams)
  tokens_ngrams(1:3) |> 
  dfm() |> 
  dfm_trim(
    min_termfreq = 5, termfreq_type = "count",
    max_docfreq = 0.85, docfreq_type = "prop"
  )

# remove empty documents
idxs <- which(rowSums(dtm)==0)
dtm <- dtm[-idxs, ]

# fit the LDA topic model ----

# convert to `topicmodels` package data format 
tm_dtm <- convert(dtm, to = "topicmodels")

# fit the LDA model
set.seed(1234)
tm <- LDA(
  tm_dtm,
  method  = "Gibbs",
  k       = 30,
  control = list(seed = 1234, burnin = 1000, iter = 2000, thin = 100)
)
# NOTE: this is going to run for a few seconds/minutes

# NOTE: alternatively, load the pre-fitted model from file
# load(file.path("models", "lda_ungd2017_30topics.RData"))

# extract the model parameters 
tm_estimates <- posterior(tm)
str(tm_estimates, 1)

# inspect topic words ----

# inspect top 10 terms per topic
top_terms <- terms(tm, 8)
top_terms[, "Topic 1"]
top_terms[, "Topic 2"]
top_terms[, "Topic 3"]


# look at the topics x words matrix
dim(tm_estimates$terms)


# plot example worsd for selected topics
important_toks <- c()
selected_topics <- c(2, 5, 10, 14, 25, 27)
for (t in selected_topics)
  important_toks <- c(important_toks, top_terms[1:5, paste("Topic", t)])

phis <- t(tm_estimates$terms[, important_toks])
plot_heatmap(phis)


# show top words for topic 2 
plot_topic_words(tm, topic.nr = 2, n.words = 20)


# show top words for topic 5 
plot_topic_words(tm, topic.nr = 5, n.words = 20)


# get the parameters of documents x topics matrix
thetas <- tm_estimates$topics  
dim(thetas)


# get representative documents ----

# set number of top documents to retrieve per topic
top_n <- 3

# retrieve top `top_n` documents per topic
top_docs_per_topic <- lapply(
  seq_len(ncol(thetas)), 
  function(k) {
    # sort document parameters for topic k in decreasing order
    ord <- order(thetas[, k], decreasing = TRUE)
    # get names of `top_n` documents with highest topic proportions 
    docnames(dtm)[ord[1:top_n]]
  }
)


# show examples for topic 5
topic_nr <- 5
doc_ids <- top_docs_per_topic[[topic_nr]]
doc_ids

# get distinctive terms of topic
terms(tm, 20)[, topic_nr]

# first repr. doc 
cat(stringr::str_wrap(as.character(speech_paragraphs[doc_ids[1]]), width = 60, exdent = 1))

# second repr. doc
cat(stringr::str_wrap(as.character(speech_paragraphs[doc_ids[2]]), width = 60, exdent = 1))


# inspect topic for selected country ----

# get paragraphs from speech by Austrian representative
idxs <- grep("Austria", rownames(thetas))
plot_heatmap(thetas[idxs, ], what = "topic")
# NOTE: topics 4, 6, 26, and 27 seem to be important here

# inspect content  of topic 4
plot_topic_words(tm, topic.nr = 4, n.words = 30)

# inspect content  of topic 6
plot_topic_words(tm, topic.nr = 6, n.words = 30)

# inspect content  of topic 26 
plot_topic_words(tm, topic.nr = 26, n.words = 30)

# inspect content  of topic 27
plot_topic_words(tm, topic.nr = 27, n.words = 30)

# aggregate topic proportions at country level ----

# determine country of speech paragraph
cidxs <- sub("\\.\\d+$", "", rownames(thetas))
# aggregate topic proportions at country level
thetas_country <- aggregate(
  thetas, 
  by = list(country = cidxs), 
  FUN = mean
)
rownames(thetas_country) <- thetas_country$country
thetas_country <- as.matrix(thetas_country[, -1])


# get mapping of continents to country names
country_names <- with(
  docvars(data_corpus_ungd2017), 
  split(country, continent)
)

# show country-level topic focus for African countries
plot_heatmap(thetas_country[country_names$Africa, ], what = "topic")

# show country-level topic focus for Asian countries
plot_heatmap(thetas_country[country_names$Asia, ], what = "topic")

# show country-level topic focus for European countries
plot_heatmap(thetas_country[country_names$Europe, ], what = "topic")


