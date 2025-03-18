# Clean the environment
rm(list = ls())

out_path <- "/scratch/project_2010556/"

generated_text <- "abc"

generated_text

saveRDS(generated_text, paste0(out_path, "abc.rds"))
