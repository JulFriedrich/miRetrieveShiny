library(magrittr)

# Path to adjusted miRTarBase 8.0
mirtarbase_file <- "/Users/Julian/Documents/Jupyter/miRetrieve Shiny pkg/miRetrieveShiny/data-raw/miRTarBase_shiny.xlsx"

# Stem for miRNAs
# Used to extract only miRNA stem from miRTarBase (e.g. "miR-200" instead
# of "hsa-miR-200b-5p")
pattern_stem <- "[mM][iI][cC]?[rR][oO]?[rR]?[nN]?[aA]?[-]?\\d+"

# Load miRTarBase
# Extract miRNA stem
# Convert targets to upper case only
mirtarbase <- readxl::read_excel(mirtarbase_file) %>%
    dplyr::mutate(miRNA_tarbase = stringr::str_extract(miRNA_tarbase,
                                               pattern = pattern_stem),
                  Target = stringr::str_to_upper(Target))

# Save mirtarbase as a data file
usethis::use_data(mirtarbase, overwrite = TRUE)
