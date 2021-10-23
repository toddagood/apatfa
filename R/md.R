# Adds support for using markdown to create titles and notes
# for tables and figures.

md_to_xml <- function(x) {
  # Supports **bold** and __bold__,
  x <- gsub("\\*\\*(.*?)\\*\\*", "<bold>\\1</bold>", x)
  x <- gsub("__(.*?)__", "<bold>\\1</bold>", x)

  # Supports *italic* and _italic_,
  x <- gsub("\\*(.*?)\\*", "<italic>\\1</italic>", x)
  x <- gsub("_(.*?)_", "<italic>\\1</italic>", x)

  # Supports ^^superscript^^ and ^superscript^,
  x <- gsub("\\^\\^(.*?)\\^\\^", "<sup>\\1</sup>", x)
  x <- gsub("\\^(.*?)\\^", "<sup>\\1</sup>", x)

  # Supports ~~subscript~~ and ~subscript~
  x <- gsub("~~(.*?)~~", "<sub>\\1</sub>", x)
  x <- gsub("~(.*?)~", "<sub>\\1</sub>", x)

  # Enclose in a body node with white-space preserved.
  paste0(c("<body xml:space=\"preserve\">", x, "</body>"), collapse="")
}

update_b <-
  function(x) officer::ftext(x$value, stats::update(x$p, bold=TRUE))
update_i <-
  function(x) officer::ftext(x$value, stats::update(x$p, italic=TRUE))
update_v <-
  function(x, v) officer::ftext(x$value, stats::update(x$p, vertical.align=v))

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
         sub = lapply(chunks, update_v, "subscript")
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

if (FALSE) {
  print(md_title("Plain *Italic* **bold**   plain__*Bold Italic*__ E=mc^^2^^ Water=H~2~O n=", 22))

  print(md_title("
This is the first paragraph.
Still **para1**.

Now in para2.
Still ~para2~.
Also in ^para2^.
"))
}


