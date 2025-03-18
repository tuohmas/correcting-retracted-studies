# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi

# Script for running Automated Nonparametric Content Analysis with Readme2

## Login overriding the default MAC algorithm
# ssh -m hmac-sha2-512 tuomheik@puhti.csc.fi

## Create a folder for your R packages in /projappl
# cd /projappl/project_2010556
# mkdir project_rpackages

## Launch interactive mode with default memory and local scratch space. for example:
# sinteractive --account project_2010556 --cores 2 --time 02:00:00 --mem 8000 --tmp 100

## Go to projappl and activate r-readme
# cd /projappl/project_2010556
# module load python-data
# source /projappl/project_2010556/r-readme/bin/activate

# Start R
# module load r-env
# start-r

# PREPARATIONS #################################################################

# Clean the environment
rm(list = ls())

# Suppress scientific notation
options(scipen = 999)

# In R, add the folder you created above to the list of directories where R will look for packages:
.libPaths(c("/projappl/project_2010556/project_rpackages", .libPaths()))

# Assign libpath
libpath <- .libPaths()[1]

# Install packages
if(!require("tensorflow")) {
  remotes::install_github("rstudio/tensorflow", dependencies = TRUE,
                          upgrade = "always", lib = libpath) }

if(!require("readme")) {
  remotes::install_github("iqss-research/readme-software/readme",
                          dependencies = TRUE,
                          upgrade = "always", lib = libpath) }

if(!require("pacman")) {
  install.packages("pacman",dependencies = TRUE, lib = libpath) }

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

envname <- "./r-reticulate"

# Create new virtual environment to home path
if(!virtualenv_exists(envname)) { break

  # # Create virtual env
  # virtualenv_create(envname = envname, version = "3.10.11",
  #                   packages = NULL, module = "venv",
  #                   system_site_packages = FALSE)
  #
  # # Install tensorflow and dependencies to virtual env
  # tensorflow::install_tensorflow(method = "virtualenv",
  #                                envname = envname,
  #                                extra_packages = "keras",
  #                                restart_session = FALSE,
  #                                python_version = "3.10.11")
  #
  # # Install tensorflow and dependencies to virtual env
  # textrpp_install_virtualenv(envname = envname, prompt = FALSE)

}

# Activate virtual env
use_virtualenv(envname)

# If anything else needs to be installed
# virtualenv_install(envname, py_pkgs, ignore_installed = TRUE,
#                    python_version = "3.10.11")

# Sys.setenv("RETICULATE_PYTHON_ENV" = "/projappl/project_2010556/r-reticulate")

# Testing whether TensorFlow is working
tensorflow::tf$constant("Hello world")

# Initialize and test text package
textrpp_initialize(virtualenv = envname, refresh_settings = TRUE,
                   save_profile = FALSE, textEmbed_test = FALSE, check_env = FALSE)

# TRY OUT TEXT PACKAGE

# Models
# models <- c("bert" = "google-bert/bert-large-cased-whole-word-masking",
#             "roberta" = "openai-community/roberta-large-openai-detector",
#             "gpt2" = "openai-community/gpt2-xl",
#             "nvidia-llama3" = "nvidia/Llama3-ChatQA-1.5-8B",
#             "pegasus" = "google/pegasus-xsum")

# Generate text from the prompt
embeddings <- texts <- c("I feel great!",
                         "Are you serious?",
                         "I am really sad today")

set.seed(987654321)

embeddings <- textEmbed(texts,
                        model = "bert-base-uncased",
                        layers = -2L,
                        tokenizer_parallelism = TRUE)

saveRDS(embeddings, "/scratch/project_2010556/readme/outputs/trash/test_embeddings1.rds")
