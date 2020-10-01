library(magrittr)
library(miRetrieveShiny)

# Path to abstracts of malignant melanoma as of October 1st, 2020
pubmed_file <- "/Users/Julian/Documents/Jupyter/miRetrieve Shiny pkg/MM_mirna.txt"

# Create miRetrieve table with extracted miRNAs
# Threshold is 2
# Topic is "Melanoma"
# Only abstracts of research articles
# No letters are extracted
preloaded_pubmed <- miRetrieveShiny::upload_abstracts_shiny(pubmed_file,
                                                            threshold = 2,
                                                            topic = "Melanoma",
                                                            subset_research = TRUE,
                                                            extract_letters = FALSE)

# Save preloaded table as a data file
usethis::use_data(preloaded_pubmed, overwrite = TRUE)
