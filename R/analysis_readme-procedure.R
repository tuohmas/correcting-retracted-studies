# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi

# Script for running Automated Nonparametric Content Analysis with Readme2

# PREPARATIONS #################################################################

# Clean the environment
rm(list = ls())

# Suppress scientific notation
options(scipen = 999)

# Install packages
if(!require("tensorflow")) {
  remotes::install_github("rstudio/tensorflow", upgrade = "never") }

if(!require("readme")) {
remotes::install_github("iqss-research/readme-software/readme",
                        upgrade = "never") }

if(!require("pacman")) {
  install.packages("pacman") }

# Load other packages
pacman::p_load(dplyr,
               tidyr,
               reticulate,
               tensorflow,
               text,
               readme)

# Unset Environment variable RETICULATE_PYTHON, if one exists
if(nchar(Sys.getenv("RETICULATE_PYTHON")) > 0) {
  Sys.unsetenv("RETICULATE_PYTHON") }

# Install python, if necessary
version <- "3.10.11"
envname <- "r-readme"

if(py_discover_config()$version != "3.10") {
  install_python(version = version) }

packages <- c("tensorflow",
              "keras",
              "torch==2.0.0",
              "transformers>=4.19.2",
              "numpy",
              "pandas",
              "nltk")

if(!virtualenv_exists(envname)) {
  virtualenv_create(envname, version, packages) }

use_virtualenv(envname)
virtualenv_install(envname, packages)

# Testing whether TensorFlow is working
tensorflow::tf$constant("Hello world")

# Initialize text package
textrpp_initialize(virtualenv = py_config()$pythonhome, refresh_settings = TRUE,
                   save_profile = FALSE, textEmbed_test=TRUE, check_env=TRUE)

## WORD VECTOR SUMMARIES #######################################################

# Proof of concept with Clinton data
data(clinton, package = "readme")

my_data <- clinton %>% as_tibble() %>% slice(1:500)

glimpse(my_data)

# Models
models <- c("bert" = "google-bert/bert-large-cased-whole-word-masking",
            "roberta" = "openai-community/roberta-large-openai-detector",
            "gpt2" = "openai-community/gpt2-xl",
            "nvidia-llama3" = "nvidia/Llama3-ChatQA-1.5-8B",
            "pegasus" = "google/pegasus-xsum")

set.seed(987654321)

# Generate a word vector summary for first 500 documents
my_dfm <- undergrad(
  documentText = tolower(my_data$TEXT),
  numericization_method = "transformer_based",
  textEmbed_control = list(model = models["roberta"],
                           layers = -1L,
                           tokenizer_parallelism = TRUE,
                           device = "cpu")
)



my_dfm[1:10,1:10]

saveRDS(my_dfm, "my_dfm.rds")

## PROPORTION ESTIMATION #######################################################

nProj <- 50L

# Perform estimation
readme_results <- readme(
  dfm = my_dfm,                       # Result from undergrad()
  labeledIndicator = my_data$TRAININGSET,
  categoryVec = my_data$TRUTH,
  nBoot = 100,
  numProjections = 50,
  sgdIters = 750,
  nCores = 15,
  nCores_OnJob = 15,
  verbose = T)

saveRDS(readme_results, "readme_results.rds")

res <- readme_results$point_readme

data.frame(categories = names(res), estimates = res) %>%
  write.table("readme_results.txt", sep = "\t", row.names = FALSE)
