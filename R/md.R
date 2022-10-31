# Adds support for using markdown to create titles and notes
# for tables and figures.

md_to_xml <- function(x) {
  # Supports **bold** and __bold__
  x <- gsub("\\*\\*(.*?)\\*\\*", "<bold>\\1</bold>", x)
  x <- gsub("__(.*?)__", "<bold>\\1</bold>", x)

  # Supports *italic* and _italic_
  x <- gsub("\\*(.*?)\\*", "<italic>\\1</italic>", x)
  x <- gsub("_(.*?)_", "<italic>\\1</italic>", x)

  # Supports ^^superscript^^ and ^superscript^
  x <- gsub("\\^\\^(.*?)\\^\\^", "<sup>\\1</sup>", x)
  x <- gsub("\\^(.*?)\\^", "<sup>\\1</sup>", x)

  # Supports ~~subscript~~ and ~subscript
  x <- gsub("~~(.*?)~~", "<sub>\\1</sub>", x)
  x <- gsub("~(.*?)~", "<sub>\\1</sub>", x)

  # Supports `code`
  x <- gsub("\\`(.*?)\\`", "<code>\\1</code>", x)

  # Enclose in a body node with white-space preserved.
  paste0(c("<body xml:space=\"preserve\">", x, "</body>"), collapse="")
}

update_b <-
  function(x) officer::ftext(x$value, stats::update(x$p, bold=TRUE))
update_i <-
  function(x) officer::ftext(x$value, stats::update(x$p, italic=TRUE))
update_v <-
  function(x, v) officer::ftext(x$value, stats::update(x$p, vertical.align=v))
update_code <-
  function(x) officer::ftext(x$value, stats::update(x$p, font.family="Courier New", font.size=10))

to_fpar <- function(node, prop) {
  if (xml2::xml_name(node) == "text") {
    return(list(officer::ftext(xml2::xml_text(node), prop)))
  }
  chunks <- purrr::flatten(lapply(xml2::xml_contents(node), to_fpar, prop=prop))
  switch(xml2::xml_name(node),
         body = officer::fpar(values=chunks,
                              fp_p=officer::fp_par(line_spacing=2)),
         bold = lapply(chunks, update_b),
         italic = lapply(chunks, update_i),
         sup = lapply(chunks, update_v, "superscript"),
         sub = lapply(chunks, update_v, "subscript"),
         code = lapply(chunks, update_code)
  )
}

md_pars <- function(..., prop) {
  # Collapse args into md.
  md <- stringr::str_trim(paste0(..., collapse=""))
  # Split md into paragraphs at double newlines.
  mds <- strsplit(md, "\n\n", fixed=TRUE)[[1]]
  paras <- lapply(mds, function(md) {
    # Convert md to xml.
    x <- md_to_xml(md)
    # Read the xml.
    x <- xml2::read_xml(x)
    # Convert xml to fpar.
    to_fpar(x, prop=prop)
  })
  do.call(officer::block_list, paras)
}

#' Creates a title from markdown.
#'
#' @param ... markdown content
#' @return an officer::block_list of paragraphs
#' @export
md_title <- function(...) {
  defaults <- flextable::get_flextable_defaults()
  italic_text <- officer::fp_text(font.family=defaults$font.family,
                                  font.size=defaults$font.size,
                                  italic=TRUE)
  md_pars(..., prop=italic_text)
}

#' Creates notes from markdown.
#'
#' @param ... markdown content
#' @return an officer::block_list of paragraphs
#' @export
md_notes <- function(...) {
  defaults <- flextable::get_flextable_defaults()
  normal_text <- officer::fp_text(font.family=defaults$font.family,
                                  font.size=defaults$font.size)
  md_pars(..., prop=normal_text)
}


