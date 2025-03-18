# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi
#
#
# Script for creating smart word clouds; Illustrate differences in vocabularies
# Inspired by Johannes Gruber: https://www.johannesbgruber.eu/post/smarter-wordclouds/
#
#
# PREPARATIONS #################################################################

# Clean the environment
rm(list = ls())

# Install and load packages
if(!require("pacman")) {
  install.packages("pacman")
  library(pacman) }

# Load packages
pacman::p_load(dplyr,      # For data manipulation
               tidyr,      # For data manipulation
               purrr,      # For data manipulation
               readr,      # For data manipulation
               quanteda,
               spacyr,
               textstem,   # TEMP?
               quanteda.textstats,  # textstat_frequency()
               ggplot2,
               ggwordcloud)

# TEMP
sessionInfo()
getwd()
setwd(r"(Z:\tkjheikk_research\)")

# Load data
labeled_tweets <- read_csv("1 TWEETS (stance) - codes_stance.csv") # FIXME REPLACE
labeled_replies <- read_csv("2 REPLIES (social correction) - codes_correction.csv") # FIXME REPLACE

# TEXT PREPROCESS ##############################################################

# Define a function to tokenize and lemmatize tweet corpus; additional, optional
# functionalities for removing stopwords, trimming corpus, and collocating
# compound words

text_cleanup <- function(data,  # Exported and annotated df with text and doc id
                         remove_stopwords = FALSE,
                         stop_words = stopwords::stopwords("en"),
                         word_collocations = FALSE,
                         collocations_min_count = 3,
                         collocations_p = 0.001,
                         dfm_trim = FALSE) {

  # Reduce and filter data frame
  keep_cols <- c("id", "text", "label", "code")

  data <- data %>%
    filter(check == "labeled") %>%
    select(all_of(keep_cols))

  # Clean, tokenize and pre-process
  tokens <- data %>%

    # Clean Twitter specific expressions and badly encoded characters
    mutate(text = gsub("[AT MULTIPLE USERS]", "", text, fixed = TRUE)) %>%
    mutate(text = gsub("[LINK]", "", text, fixed = TRUE)) %>%
    mutate(text = gsub("#", "", text)) %>%
    mutate(text = gsub("@.+?\\b", "", text)) %>%
    mutate(text = gsub("&lt;", ">", text)) %>%   # Less than
    mutate(text = gsub("&gt;", "<", text)) %>%   # Greater than
    mutate(text = gsub("&gt;", "<", text)) %>%   # Greater than
    mutate(text = gsub("’", "'", text)) %>%   # Greater than

    # Separate words inside hashtags ("#IvermectinSavesLives")
    mutate(text = gsub("((?<=[a-z]|[1-9])[A-Z])", " \\1", text, perl = TRUE)) %>%

    # Create corpus
    corpus(docid_field = "id", text_field = "text") %>%

    # Remove punctuation, symbols, and numbers; retain padding for later
    quanteda::tokens(what = "word",
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     padding = word_collocations) %>%

    # Lowercase
    tokens_tolower()

  # Optional: Remove stop words; reserve padding for later
  if(remove_stopwords) {

    tokens <- tokens %>%
      tokens_remove(stop_words, padding = word_collocations)

    }

  # Optional: Collocate compound words that appear together often with
  # statistically significant regularity (Note: may cause trouble when stop words
  # are not removed)

  if(word_collocations) {

  collocations <-
    textstat_collocations(tokens,
                          min_count = collocations_min_count, # abs. threshold
                          size = 2) %>%       # bigrams only

    # Keep collocations above critical value for 99.9% confidence level
    filter(z > qnorm(p = collocations_p / 2, lower.tail = FALSE) |
           z < qnorm(p = collocations_p / 2)) %>%

    # Arrange by descending word frequency
    arrange(desc(count))

  # Replace multi-token sequences with newly created compound tokens
  tokens <- tokens_compound(tokens,
                            pattern = collocations,
                            concatenator = "·",     # Join with interpunct
                            keep_unigrams = FALSE)

  }

  # Create a Document-Feature Matrix
  dfm <-
    tokens %>%

    # Create equivalency classes: lemmatize tokens
    tokens_replace(pattern = lexicon::hash_lemmas$token,
                   replacement = lexicon::hash_lemmas$lemma) %>%

    # TEMP
    # Custom collocations: same root, e.g., peer review and peer reviewed
    tokens_replace(pattern = "peer·reviewed", replacement = "peer·review") %>%
    tokens_replace(pattern = "clinical·trials", replacement = "clinical·trial") %>%
    tokens_replace(pattern = "meta-analysis", replacement = "meta·analysis") %>%

    # Remove padding
    dfm(remove_padding = TRUE)

  # Optional: Trim terms appreaing in less than 1% or more than 99% of documents
  if(dfm_trim) {

    dfm <- dfm_trim(dfm,  min_termfreq = 0.01, max_docfreq = 0.99,
                  docfreq_type = "prop", verbose = TRUE)
    }

  return(dfm)

}

# Combine multiple stop words dictionaries
my_stopwords <-
  stopwords::stopwords("en", source = "snowball") %>%      # Append two
  append(stopwords::stopwords("en", source = "nltk")) %>%  # stopword libraries

  append(c("one", "two", "three", "four", "five",          # and numbers 1–10
           "six", "seven", "eight", "nine", "ten")) %>%

  append(c("…", "”", "“", "‘", "•")) %>% # Append exotic punctuation

  unique() %>%
  sort()

# Keep stopwords that potentially encode meaning
# (e.g. "why was it pulled?", "no benefit", "most doctors disagree")
keep_stopwords = c("no", "not", "more", "most", "less", "why")

my_stopwords <- my_stopwords[!my_stopwords %in% keep_stopwords]

# Use functions
dfm_tweets <- labeled_tweets %>%
  text_cleanup(remove_stopwords = TRUE,
               stop_words = my_stopwords,
               word_collocations = TRUE,
               dfm_trim = TRUE)

dfm_replies <- labeled_replies %>%
  text_cleanup(remove_stopwords = TRUE,
               stop_words = my_stopwords,
               word_collocations = TRUE,
               dfm_trim = TRUE)

# Inspect DFMs
dfm_tweets
topfeatures(dfm_tweets, n = 100)
textstat_frequency(dfm_tweets, n = 50, groups = code, ties_method = "max")

dfm_replies
topfeatures(dfm_replies, n = 100)
textstat_frequency(dfm_replies, n = 50, groups = code, ties_method = "max")

# WORDCLOUD ####################################################################

# Define function for calculating text keyness
calculate_keyness <- function(dfm,
                              target_code,
                              subset_by = vector(),
                              textstat_frequency = 100) {

  if(length(subset_by) > 0) { dfm <- dfm_subset(dfm, ! code %in% subset_by) }

  keyness <- dfm %>%
    textstat_keyness(
      target = which(docvars(., "code") == target_code),
      measure = "chi2")

  # Maybe do slicing here: select 50/75/100 top features/group (some mutual)
  keyness <- dfm %>%
    textstat_frequency(n = 100, groups = code) %>%
    select(feature) %>%
    left_join(keyness, by = "feature") %>%
    distinct(feature, .keep_all = TRUE) %>%
    arrange(desc(n_target))

  # Encode "overrepresentation" as relative term frequencies in target divided
  # over reference
  keyness_over <- keyness %>%
    mutate(
      frequency = (n_target + n_reference),
      relfreq_target =  (n_target + 0.1) / (frequency + 0.1), # xlims: -2, 2 (log10)
      relfreq_reference = (n_reference + 0.1) / (frequency + 0.1), # xlims: -2, 2 (log10)
      overrepresentation = log10((relfreq_target) / (relfreq_reference)))

  # Add a new column for words pasted with significance level indicators
  plot_data <- keyness_over %>%
    # slice_max(frequency, n = 150, with_ties = FALSE) %>% # Top words up to N # Redundant
    # Encode significance level to the feature displayed
    mutate(feature_sig = case_when(
      p < 0.001 ~ paste0(feature, "***"),
      p < 0.01 ~ paste0(feature, "**"),
      p < 0.05 ~ paste0(feature, "*"),
      .default = feature), .after = feature) %>%

    # Add boolean variable to indicate significance (to highlight w/ disc. alpha)
    mutate(significant = ifelse(p < 0.05, TRUE, FALSE))

  return(plot_data)

}

keyness_tweets <- dfm_tweets %>%
  calculate_keyness(target_code = "P",
                    subset_by = "U",
                    textstat_frequency = 100)

head(keyness_tweets, n = 10)

keyness_replies <- dfm_replies %>%
  calculate_keyness(target_code = "1",
                    textstat_frequency = 100)

head(keyness_replies, n = 10)

target_code <- "P" # Positive stance as target group
target_code <- "1" # Corrections as target group

calculate_keyness()
nrow(keyness)
head(keyness, n = 20)

wordcloud_plot <- function(plot_data) {

  # Determine size breaks for text size legend
  min_size <- ceiling(min(plot_data$frequency/10))*10
  max_size <- ceiling(max(plot_data$frequency/10))*10
  size_breaks <- c(min_size, seq(50, max_size, 50))

# Note to self:
# Keyness chi-square: signed positively if the observed value in the target set exceeds its expected value

  # Plot
  wordplot <- plot_data %>%
    ggplot(aes(
      x = overrepresentation,
      y = chi2,
      label = feature_sig,
      size = frequency,
      colour = overrepresentation)) +

    # geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.5) +

    # Ei näytä kovin hyvältä
    # annotate("rect", xmin = -2, xmax = 0, ymin = -20, ymax = 0,
    #              alpha = .1) +

    # TURHA?
    # annotate("segment", x = 0.035, xend = 1, y = -20, colour = "grey20",
    #          lineend = "butt", linejoin = "round", linewidth = 0.3,
    #          arrow = arrow(length = unit(5, "pt"))) +

    # Plot word cloud
    geom_text_wordcloud(aes(alpha = significant),
                        grid_margin = 0.75, # default 1
                        seed = 1234,
                        shape = "diamond", # default "circle"
                        show.legend = TRUE) +

    # "Highlight" statistically significant values of chi2 (alpha = 0.05)
    scale_alpha_discrete(range = c(0.3, 1)) +

    scale_size_area(
      # max_size = 8,
      max_size = 10,
      breaks = size_breaks) +

    scale_colour_gradient(low =  "red", high = "blue") +

    # geom_segment(x = 0.025, y = -20, xend = 0.5, colour = "black",
    #              lineend = "butt", linejoin = "round", linewidth = 0.3,
    #              arrow = arrow(length = unit(5, "pt"))) +

    # Group 1: right-pointing arrow and label
    annotate("text", x = 0.55, y = -25, size = 3, hjust = "left",
             label = "Features that are relatively\nmore frequent in target set") +

    annotate("segment", x = 0.035, xend = 0.5, y = -25, colour = "black",
             lineend = "butt", linejoin = "round", linewidth = 0.5,
             arrow = arrow(length = unit(4, "pt"), type = "closed")) +

    # Group 2: left-pointing arrow and label
    annotate("text", x = -0.55, y = -25, size = 3, hjust = "right",
             label = "Features that are relatively\nmore frequent in reference set") +

    annotate("segment", x = -0.035, xend = -0.5, y = -25, colour = "black",
             lineend = "butt", linejoin = "round", linewidth = 0.5,
             arrow = arrow(length = unit(4, "pt"), type = "closed")) +

    # Group 3: example observation ("retract"***)
    # annotate(geom = "curve", x = 1.23, y = 22, xend = 0.9, yend = 15,
    #   curvature = 0.3, arrow = arrow(length = unit(4, "pt"))) +

    # Some area would be nice also... quadrant (0..-2, 0..-20)

    theme_minimal() +

    labs(y = expression("Keyness (" ~ chi^{2} * ")"),
         x = expression("Over-representation (" * log[10] * ")")) +

    guides(colour = "none", size = "legend", Overrepresentation = "none",
           alpha = "none")

  return(wordplot)

}

# Plot data
wordcloud_plot(keyness_tweets)

# Mahdollinen väripaletti
# display.brewer.all(colorblindFriendly = TRUE)
# RdYlBu

plot_data %>%
  # select(feature, frequency, n_target, n_reference, chi2, Overrepresentation) %>%
  filter(feature %in% c("garbage", "know", "doctor", "please", "likely"))
