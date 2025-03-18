# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi
#
# Script for creating smart wordclouds to illustrate differences in vocabularies
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
               textstem,
               quanteda.textstats,  # textstat_frequency()
               ggplot2,
               ggwordcloud)

sessionInfo()
getwd()
setwd(r"(Z:\tkjheikk_research\)")

# Load data
labeled_tweets <- read_csv("1 TWEETS (stance) - codes_stance.csv")
labeled_replies <- read_csv("2 REPLIES (social correction) - codes_correction.csv")

# Approach inspired by Johannes Gruber
# https://www.johannesbgruber.eu/post/smarter-wordclouds/

text_cleanup <- function(data,
                         remove_stopwords = FALSE,
                         stop_words = stopwords::stopwords("en"),
                         word_collocations = FALSE,
                         collocations_min_count = 3,
                         collocations_p = 0.001,
                         dfm_trim = FALSE) {

  keep_cols <- c("id", "text", "label", "code")

  data <- data %>%
    filter(check == "labeled") %>%
    select(all_of(keep_cols))


  # Tokenize and preprocess
  tokens <- data %>%

    # Preprocess Twitter specific expressions and badly encoded characters
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

    # Remove punctuation, symbols, and numbers
    quanteda::tokens(what = "word", remove_punct = TRUE, remove_symbols = TRUE,
                     remove_numbers = TRUE, split_hyphens = FALSE, # TEMP SPLIT
                     split_tags = TRUE, padding = TRUE) %>%

    # Lowercase
    tokens_tolower(keep_acronyms = FALSE) # FDA yes, but BUMBASS, no

  # Remove stopwords (optional)
  if(remove_stopwords) {

    tokens <- tokens %>%  # Remove stopwords (Snowball, NLTK and word numbers)
      tokens_remove(stop_words, padding = TRUE) # padding: preserve word order

  }

  tokens

  if(word_collocations) {

  # Beside unigram tokens, collocate compound words or 2-grams that appear together often
  collocations <-
    textstat_collocations(tokens,
                          min_count = collocations_min_count,
                          tolower = TRUE, size = 2) %>%

    # Keep collocations above critical value for 99.9% confidence level
    filter(z > qnorm(p = collocations_p/2, lower.tail = FALSE) |
             z < qnorm(p = collocations_p/2)) %>%

    arrange(desc(count))

  # Replace multi-token sequences with a multi-word, or "compound" token
  tokens <- tokens_compound(
    tokens,
    pattern = collocations,
    concatenator = "·",      # Join compounds with interpunct character
    keep_unigrams = FALSE)

  }

  # Create a Document-Feature Matrix
  dfm <-
    tokens %>%

    # Create equivalency classes: lemmatize tokens
    tokens_replace(pattern = lexicon::hash_lemmas$token,
                   replacement = lexicon::hash_lemmas$lemma) %>%

    # Custom collocations: same root, e.g., peer review and peer reviewed
    tokens_replace(pattern = "peer·reviewed", replacement = "peer·review") %>%
    tokens_replace(pattern = "clinical·trials", replacement = "clinical·trial") %>%
    tokens_replace(pattern = "meta-analysis", replacement = "meta·analysis") %>%

    dfm(remove_padding = TRUE)

  # Inspect DFM
  # dfm

  # Print most frequent terms
  # topfeatures(dfm, n = 50)

  # Trim terms that appear in less than 1% or more than 99% of documents
  if(dfm_trim) {

    dfm <- dfm_trim(dfm,  min_termfreq = 0.01, max_docfreq = 0.99,
                  docfreq_type = "prop", verbose = TRUE)
    }

  # topfeatures(dfm, n = 50)

  return(dfm)

}

# Combine multiple stopwords dictionaries
my_stopwords <-
  stopwords::stopwords("en", source = "snowball") %>%      # Append two
  append(stopwords::stopwords("en", source = "nltk")) %>%  # stopword libraries
  append(c("one", "two", "three", "four", "five",          # and numbers from
           "six", "seven", "eight", "nine", "ten")) %>%      # 1 to 10
  append(c("…", "”", "“", "‘", "•")) %>% # Append exotic punctuation
  unique() %>%
  sort()

# Keep stopwords that potentially encode meaning
# (e.g. "why was it pulled?", "no benefit", "most doctors disagree")
keep_stopwords = c("no", "not", "more", "most", "less", "why")

my_stopwords <- my_stopwords[!my_stopwords %in% keep_stopwords]

# Use function
dfm <- text_cleanup(labeled_replies, remove_stopwords = TRUE,
                    stop_words = my_stopwords,
                    word_collocations = TRUE,
                    dfm_trim = TRUE,
                    )

# Inspect DTM
dfm
topfeatures(dfm, n = 100)
textstat_frequency(dfm, n = 50, groups = code, ties_method = "max")

# WORDCLOUD ####################################################################

target_code <- "P" # Positive stance as target group
target_code <- "1" # Corrections as target group

keyness <-
  dfm %>%
  # dfm_subset(dfm, code != "U") %>% # HOX! MAYBE
  textstat_keyness(
    target = which(docvars(., "code") == target_code),
    measure = "chi2")

# Maybe do slicing here: select 50/75/100 top features/group (some mutual)
keyness <- dfm %>%
  textstat_frequency(n = 100, groups = code) %>%
  select(feature) %>%
  left_join(keyness, by = "feature") %>%
  distinct(feature, .keep_all = TRUE) %>%
  arrange(p)

nrow(keyness)

head(keyness, n = 20)

# Overrepresentation FIXME

keyness_over <- keyness %>%
  mutate(frequency = (n_target + n_reference),
         # Introduce dampening factor (??) to deal with comparing zero
         # Depends on what limits you would want to have for x axis (log10)
         relfreq_target =  (n_target + 0.1) / (frequency + 0.1), # xlims: -2, 2 (log10)
         relfreq_reference = (n_reference + 0.1) / (frequency + 0.1), # xlims: -2, 2 (log10)
         overrepresentation = log10((relfreq_target) / (relfreq_reference)))

# Add a new column for words pasted with significance level indicators
plot_data <- keyness_over %>%
  # slice_max(frequency, n = 150, with_ties = FALSE) %>% # Top words up to N # Redundant
  # Encode significance level to the feature displayed
  mutate(feature_stat = case_when(
    p < 0.001 ~ paste0(feature, "***"),
    p < 0.01 ~ paste0(feature, "**"),
    p < 0.05 ~ paste0(feature, "*"),
    .default = feature), .after = feature) %>%
  # Add boolean variable to indicate significance (to highlight w/ disc. alpha)
  mutate(significant = ifelse(p < 0.05, TRUE, FALSE))

# Try out: guides for wordcloud text size label breaks
# (from min to max in step of 50)
min_size <- ceiling(min(plot_data$frequency/10))*10
max_size <- ceiling(max(plot_data$frequency/10))*10
size_breaks <- c(min_size, seq(50, max_size, 50))

plot_data %>% arrange(desc(frequency)) %>% select(feature, frequency)

# Note to self:
# Keyness chi-square: signed positively if the observed value in the target set exceeds its expected value

# Plot
wordplot <- plot_data %>%
  ggplot(aes(
    x = overrepresentation, y = chi2,
    label = feature_stat, size = frequency, colour = overrepresentation)) +

  # geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.5) +

  # Ei näytä kovin hyvältä
  # annotate("rect", xmin = -2, xmax = 0, ymin = -20, ymax = 0,
  #              alpha = .1) +

  # TURHA?
  # annotate("segment", x = 0.035, xend = 1, y = -20, colour = "grey20",
  #          lineend = "butt", linejoin = "round", linewidth = 0.3,
  #          arrow = arrow(length = unit(5, "pt"))) +

  # "Highlight" statistically significant values of chi2 (alpha = 0.05)
  geom_text_wordcloud(aes(alpha = significant),
                       grid_margin = 0.75, # default 1
                       seed = 1234,
                       shape = "diamond", # default "circle"
                       show.legend = TRUE) +

  scale_alpha_discrete(range = c(0.3, 1)) +

  scale_size_area(
    # max_size = 8,
    max_size = 10,
    # breaks = c(10, 50, 100, 150, 200, 500, 1000)
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

wordplot

# Mahdollinen väripaletti
# display.brewer.all(colorblindFriendly = TRUE)
# RdYlBu

plot_data %>%
  # select(feature, frequency, n_target, n_reference, chi2, Overrepresentation) %>%
  filter(feature %in% c("garbage", "know", "doctor", "please", "likely"))
