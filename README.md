<!-- badges: start -->
[![R-CMD-check](https://github.com/toddagood/apatfa/workflows/R-CMD-check/badge.svg)](https://github.com/toddagood/apatfa/actions)
<!-- badges: end -->

apatfa is an R package that generates tables, figures, and appendix sections
following APA 7 conventions.

The main function is apa_docx() which generates an output Word file (docx)
containing bookmarked content (tables, figures, and appendices) numbered and
organized in APA style sections.  In each output section, the content is ordered
by the first reference to the content within an input Word file.  The output
file is designed to be included into the input file using a Word field
{INCLUDETEXT "output_file.docx"} so that bookmarks in the output file can be
referenced from the input file.
