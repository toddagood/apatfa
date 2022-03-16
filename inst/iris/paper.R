#######################################################################
# Load libraries.
#######################################################################
library(apatfa)
library(flextable)
library(tidyverse)
library(utils)

#######################################################################
# Define styles.
#######################################################################
styles <- get_styles()
set_apa_defaults()

#######################################################################
# Initialize the list of table, figure, and appendix content.
#######################################################################
init_tfas()

#######################################################################
# Add an appendix containing this R code.
#######################################################################
add_appendix("aCode", function(x, brief, ...) {
  file_name <- getSrcFilename(function(){})
  x %>%
    add_md("The R code below was used in RStudio to ",
                  "automate the method of the research.") %>%
    add_code_file(file_name, head = brief)
})

#######################################################################
# Add styling for your own data frame.
#######################################################################
gsub("[.]", "", names(iris)) -> names(iris)
styles %>% add_styling(iris) -> styles

#######################################################################
title <- title_n("Statistics for iris$PetalWidth by Species", iris)
#######################################################################
note_that("Group sizes were balanced.") %>%
  note_normal("Coloring (blue=Yes, red=No, grey=NA)") %>%
  note_intro() -> notes
col_keys <- c("Species", "n", "Mean", "SD", "Skewness", "Kurtosis")
iris %>%
  group_by(Species) %>%
  summarize(dstats = dstats(PetalWidth),
            Normal = is_normal(PetalWidth)) %>%
  unnest(dstats) %>%
  flextable(col_keys = col_keys) %>%
  colformat_double() %>%
  bg(j = c("Skewness", "Kurtosis"),
     source = "Normal",
     bg = function(Normal) styles$colors.yes_no_na[[Normal]]) %>%
  styler(styles) %>%
  add_table(bookmark = "tStatsPetalWidthBySpecies",
            title = title,
            styles = styles,
            notes = notes, wide = FALSE)

#######################################################################
title <- title_n("Boxplot of iris$PetalWidth by Species", iris)
#######################################################################
iris %>%
  group_by(Species) %>%
  mutate(Normal = is_normal(PetalWidth)) %>%
  ungroup() %>%
  ggplot(aes(Species, PetalWidth, fill = Normal)) +
  geom_boxplot() +
  scale_fill_yes_no_na() +
  theme(axis.text.x = styles$mono) -> fig
note_that("Outliers were observed.") %>%
  note_normal() %>%
  note_intro() -> notes
add_figure(fig, "fBoxplotPetalWidthBySpecies", title, styles,
           notes = notes, wide = TRUE)

#######################################################################
# Generate sections for Tables, Figures, and Appendices in APA 7 style.
#######################################################################
apa_docx(here = function() {})
