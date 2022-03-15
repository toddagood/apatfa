<!-- badges: start -->
[![R-CMD-check](https://github.com/toddagood/apatfa/workflows/R-CMD-check/badge.svg)](https://github.com/toddagood/apatfa/actions)
<!-- badges: end -->

The `apatfa` (a-pat-fa) package automates the creation of "Tables",
"Figures", and "Appendix" sections for scholarly papers written in
MS Word using the APA style guide.  The name `apatfa` is an abbreviation
for American Psychological Association Tables, Figures, and Appendices.
According to APA style, tables
and figures can either be placed within the body of the paper or
they can be grouped into end sections.  The `apatfa` package takes the
approach of always putting tables and figures in end sections (along
with the appendices) in order to keep a clear separation between
input and output documents in the document workflow.  Your manually
entered body content is kept in an input docx file and the
content generated by R is kept in a separate output docx file.  The
output docx file is then included (by reference, with auto-refresh)
back into the input docx file by using the `IncludeText` feature of
Word.


The main function `apa_docx()` generates an output docx file
containing bookmarked content (tables, figures, and appendices)
numbered and organized in APA style sections.  Within each output
section, the content is ordered by the first bookmark reference to
the content from within the input docx file.  After the output file is
included into the end of the input file using a Word
`IncludeText` field, the bookmarked content in the
output docx file will be available for reference from within the
input docx file in the usual way (by using the `Word > Insert > Cross Reference > Bookmark`
menu).


This package primarily uses the `officer` and `flextable` packages, but
the difference here is the obsession with conforming to the APA
style guide.  For example, in APA style, numeric table columns
should be center-aligned and at the same time should be
decimal-aligned, which flextables do not support, while `apatfa`
supports that and does it by default.


The `apatfa` package uses the `officer` package to read and write docx
files, uses the `flextable` package to style tables, and uses the
`ggplot2` package to style figures.  I really appreciate the work
of David Gohel and the other contributors to the officer and
flextable packages.


Here are the basic steps for using `apatfa` to create a paper:

* To get started, download (usually from your company or institution)
  a Word (docx) file containing a title page template, headers,
  footers, and pre-defined APA styles for paragraphs and fonts.  To
  achieve seamless operation with `apatfa`, check the following settings
  within the input docx file:
  * Ensure that the default
    paragraph style is named 'Normal' and uses the 'Times New Roman'
    12pt font with double-spaced lines, left-aligned, with 0.5"
    indentation for the first line.
  * Ensure that a paragraph style named 'Section' is defined based on
    the 'Normal' style but bold, center-aligned, and with the
    'Begin on new page' option enabled.
  * Ensure that a paragraph style named 'Caption' is defined based on
    the 'Normal' style but bold, left-aligned, and with 0" indentation
    for the first line.  Turn off any auto-numbering because `apatfa` will
    directly number the content in the right order using plain text.
  * For convenience, define a font style named 'Code' in the 'Courier New"
    10pt font and
    use that to style factor levels and code fragments within the body
    of the document because `apatfa` will do the same for factor levels
    in the output tables and figures and for code listings in the appendices.
  * Note that you don't need to worry about defining font settings for figure text
    because all figures will be in the output docx file only and the figure text
    will be styled (by default) using Arial 12pt by `apatfa`.

* Use `Word > Insert > Quick Parts > Field`
  to insert an `{IncludeText "output_file.docx"}` field right after the 'References'
  section in your paper.  Note that `apatfa` does not attempt to automate generation of
  the 'References' section because there are several good reference
  management tools available that integrate directly into the `Word > References`
  menu.

* Install and load the `apatfa` package in R:
  * `install.packages("devtools")`
  * `install_github("toddagood/apatfa")`
  * `library(apatfa)`

* Ensure that Arial, Courier New, and Times New Roman
  fonts are installed on your computer (such as in the Windows font registry),
  that the `extrafont` R package is installed, and that `extrafont::loadfonts()`
  can load those fonts.
  Confirm that the fonts have been loaded by calling `extrafont::fonts()` in R.

* Create a simple 'R script' (not 'R Markdown' or 'R Notebook') containing
  the following calls:
  * Call `get_styles() -> styles` to get a customizable styles list.
  * Call `set_apa_defaults()` to set APA themes as the defaults for
    flextables and ggplots.
  * Call `init_tfas()` to initialize/reset the list of table,
    figure, and appendix content.
  * Call `add_styling(styles, df) -> styles` to add automatic
    styling for column names and values in a data frame based on
    the class of the column (factor, logical, numeric, string) or
    directly manipulate the styles list to adjust styling as needed.
  * Call `tibble(...) %>% flextable() %>% styler(styles)` to create
    an APA styled flextable.  Numeric columns will be center-aligned
    and decimal aligned by default.  The `styles` list will control other
    styling options.  The styling can also be touched-up using
    flextable functions if needed.
  * Short-cuts are available to create specially formatted analysis
    result tables.  For example: `aov(...) %>% as_flextable_aov()`.
  * Call `add_table(x, bookmark, title, styles, notes = NULL, wide = FALSE, ...)`
    to add an APA styled flextable, `x`, as a bookmarked table with
    the given `title`, `styles`, and `notes`. If the table is wide, use
    `wide = TRUE` to display that table in landscape orientation.
    There are functions available to assist with preparing the title
    and notes args. The `bookmark` string must start with "t" (for
    table) and be less than 40 characters long.
  * Call `add_figure(fig, bookmark, title, styles, notes = NULL, wide = FALSE, ...)`
    to add a ggplot as a bookmarked figure with the given `title`,
    `styles`, and `notes`.  If the figure is wide, use `wide = TRUE` to
    display that figure in landscape orientation.  The
    `bookmark` string must start with "f" (for figure) and be less than
    40 characters long.
  * Call `add_appendix(bookmark, fun, wide = FALSE)`
    to add a bookmarked appendix.  The bookmark string must start
    with "a" (for appendix) and be less than 40 characters long.
    The `fun` function must accept an
    rdocx object and use commands from the officer
    package to add appendix content and return the updated rdocx object.
    Within the `fun` function, you can use add_md_normal() to add 'Normal'
    style paragraphs to the appendix with markdown styling support
    and use add_code_file() to add the contents of a
    source-code file to the appendix as 'Code' styled paragraphs (Courier
    New, 10pt).
  * Call `apa_docx("input_file.docx", "output_file.docx")` which will
    read the bookmark references from the input docx file and generate
    the output docx file with the referenced tables, figures, and
    appendices arranged in the right order.  As a short-hand notation,
    calling `apa_docx(here = function() {})` from within an R script
    named `paper_name.R` will automatically use `paper_name.docx` as the
    input file and `paper_name.tfa.docx` as the output file.  So a
    convenient approach is to create an R project per paper and
    name the input files `paper.docx` and `paper.R` and the output file
    will be `paper.tfa.docx`, which you will include into `paper.docx`.

* Once the output file has been generated, open the input docx file,
  and if the `IncludeText` field does not auto-update then press
  `Ctrl-A` (to select all) once and press `F9` (to update all fields) twice.

* Use `Word > Insert > Cross Reference` to insert or remove references to
  bookmarks defined in the included file.  Regenerate the output docx
  file after adding, removing, or re-ording bookmark references.


The documentation for the functions can be opened with the
command `help(package = "apatfa")`.


To learn more about `apatfa`, start with the vignettes (coming soon):
`browseVignettes(package = "apatfa")`.
