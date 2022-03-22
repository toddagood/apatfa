# Planned enhancement:
# Also use the styles list to italicize statistics and variable names
# in axis titles and the legend title, similar to
# ggtext::element_markup() but driving the formatting from a styles list
# without requiring any markup.  Also use mono font for factor values
# in the axis text and legend text based on the styles list.  Today
# these can mostly be handled manually by applying themes to the axes
# and legend, but some things cannot be handled, such as italicizing
# only a variable name within a longer axis title.

#' @importFrom broom tidy
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr first
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr rename_with
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom exactci poisson.exact
#' @importFrom flextable add_footer_lines
#' @importFrom flextable align
#' @importFrom flextable as_chunk
#' @importFrom flextable as_flextable
#' @importFrom flextable as_i
#' @importFrom flextable as_paragraph
#' @importFrom flextable as_sup
#' @importFrom flextable autofit
#' @importFrom flextable border_remove
#' @importFrom flextable colformat_double
#' @importFrom flextable delete_part
#' @importFrom flextable flextable
#' @importFrom flextable font
#' @importFrom flextable fontsize
#' @importFrom flextable italic
#' @importFrom flextable line_spacing
#' @importFrom flextable merge_at
#' @importFrom flextable merge_h
#' @importFrom flextable merge_v
#' @importFrom flextable mk_par
#' @importFrom flextable ncol_keys
#' @importFrom flextable nrow_part
#' @importFrom flextable padding
#' @importFrom flextable set_formatter
#' @importFrom flextable set_header_df
#' @importFrom flextable set_header_labels
#' @importFrom flextable valign
#' @importFrom flextable width
#' @importFrom ftExtra as_paragraph_md
#' @importFrom gdtools m_str_extents
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_abline
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_label
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 layer_data
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous theme
#' @importFrom ggplot2 stat_qq
#' @importFrom ggplot2 stat_qq_line
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggrepel geom_text_repel
#' @importFrom graphics abline
#' @importFrom grid rasterGrob
#' @importFrom gtools capwords
#' @importFrom moments kurtosis
#' @importFrom moments skewness
#' @importFrom officer docx_dim
#' @importFrom officer fp_text_lite
#' @importFrom officer styles_info
#' @importFrom purrr flatten
#' @importFrom purrr keep
#' @importFrom purrr imap
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_depth
#' @importFrom rlang .data
#' @importFrom scales pvalue_format
#' @importFrom stats AIC
#' @importFrom stats BIC
#' @importFrom stats confint
#' @importFrom stats formula
#' @importFrom stats hatvalues
#' @importFrom stats logLik
#' @importFrom stats nobs
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom stats resid
#' @importFrom stats rstandard
#' @importFrom stats sd
#' @importFrom stats setNames
#' @importFrom stats shapiro.test
#' @importFrom tibble add_column
#' @importFrom tibble as_tibble
#' @importFrom tibble as_tibble_col
#' @importFrom tibble as_tibble_row
#' @importFrom tibble rowid_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr everything
#' @importFrom tidyr unnest
#' @importFrom utils flush.console
#' @importFrom utils help
apatfa_help <- function() {
  help(package = "apatfa")
}

#' Initializes the list of tables, figures, and appendices
#' @export
init_tfas <- identity # Stubbed function

#' Gets a tfa
#' @param bookmark Bookmark of the table, figure, or appendix
#' @export
get_tfa <- identify # Stubbed function

# Functions to set and get the tfas.
set_tfas <- get_tfas <- identity # Stubbed functions

# The stubbed functions are redefined here,
# with tfas in their shared context
(function() {
  # The list of tables, figures, and appendices
  tfas <- list()

  # Initializes the tfas
  init_tfas <<- function() {
    tfas <<- list()
  }
  # Sets the tfas
  set_tfas <<- function(val) {
    tfas <<- val
  }
  # Gets the tfas
  get_tfas <<- function() {
    tfas
  }
  # Gets a tfa
  get_tfa <<- function(bookmark) {
    tfas[[bookmark]]
  }
})()

#' Applies the APA theme to a flextable
#'
#' The APA theme is similar to the booktabs theme, except:
#' * all headers are center aligned
#' * the left body column is left aligned
#' * all other body columns are center aligned
#' * numbers are padded to align on the decimal point
#'
#' @param x Input flextable
#' @return Output flextable in APA style
#' @export
theme_apa <- function (x) {
  if (!inherits(x, "flextable")) {
    stop("theme_apa supports only flextable objects.")
  }
  defaults <- flextable::get_flextable_defaults()
  if (defaults$font.family != "Times New Roman" ||
      defaults$font.size != 12 ||
      defaults$line_spacing != 2 ||
      defaults$text.align != "center") {
    stop("Use set_apa_defaults()")
  }
  big_border <- officer::fp_border(width = 2,
                                   color = defaults$border.color)
  std_border <- stats::update(big_border, width = 1)
  h_nrow <- flextable::nrow_part(x, "header")
  f_nrow <- flextable::nrow_part(x, "footer")
  b_nrow <- flextable::nrow_part(x, "body")
  x <- flextable::border_remove(x)
  if (h_nrow > 0) {
    x <- flextable::hline_top(x, border = big_border, part = "header")
    x <- flextable::hline(x, border = std_border, part = "header")
    x <- flextable::hline_bottom(x, border = big_border, part = "header")
  }
  if (f_nrow > 0) {
    x <- flextable::hline_bottom(x, border = big_border, part = "footer")
  }
  if (b_nrow > 0) {
    x <- flextable::hline_bottom(x, border = big_border, part = "body")
    # Left align the first column in the body.
    x <- flextable::align(x, j = 1, align = "left", part = "body")
    # Adjust left padding to align numbers on the decimal point.
    ds <- x$body$dataset[x$col_keys]
    which_j <- which(vapply(ds, is.numeric, c(TRUE)))
    purrr::walk(which_j, function(j) {
      content <- x$body$content[[1]]$data[,x$col_keys[[j]]]
      cnt <- purrr::map_int(content, function(d) nchar(d[["txt"]]))
      num <- purrr::map_lgl(content, function(d) grepl("^[-0-9]", d[["txt"]]))
      i <- which(num)
      p <- x$body$styles$pars$padding.left$data[,x$col_keys[[j]]]
      p <- p[i]
      cnt <- cnt[i]
      if (min(p) == max(p)) {
        # The padding coefficient only works for 12pt.
        p <- 5.4 * (max(cnt) - cnt) + p
        x <<- flextable::padding(x, i = i, j = x$col_keys[[j]],
                                 padding.left = p, part = "body")
      }
    })
  }
  flextable::fix_border_issues(x)
}

#' Sets APA style defaults for flextable and ggplot2
#'
#' @param digits significant digits for tables
#' @param na_str string to be used for NA values in tables
#' @param nan_str string to be used for NaN values in tables
#' @param fmt_datetime format string for datatime values
#' @param fig_theme additional ggplot2::theme settings for figures
#' @param styles Styles to use.
#' @param ... args to pass to set_flextable_defaults()
#' @export
set_apa_defaults <- function(digits = 2,
                             na_str = "NA",
                             nan_str = "NaN",
                             fmt_datetime = "%Y-%m-%d %H:%M:%S %Z",
                             fig_theme = ggplot2::theme(),
                             styles = get_styles(),
                             ...) {
  extrafont::loadfonts(device = styles$device, quiet = TRUE)
  flextable::set_flextable_defaults(theme_fun = theme_apa,
                                    font.family = "Times New Roman",
                                    font.size = 12,
                                    line_spacing = 2,
                                    text.align = "center",
                                    digits = digits,
                                    na_str = na_str,
                                    nan_str = nan_str,
                                    fmt_datetime = fmt_datetime,
                                    ...)
  fig_theme <-
    ggplot2::theme_minimal(base_size = 12, base_family = "Arial") +
    ggplot2::theme(axis.title = ggplot2::element_text(face = "bold.italic"),
                   panel.spacing = ggplot2::unit(12, "points"),
                   panel.border = ggplot2::element_rect(fill = NA),
                   legend.direction = "horizontal",
                   legend.position = "top",
                   legend.text = styles$mono,
                   legend.title = styles$bold.italic,
                   strip.text = styles$mono) +
    fig_theme
  ggplot2::theme_set(fig_theme)
  return()
}

end_portrait <- function (x, type = "nextPage") {
  d <- docx_dim(x)
  psize <- officer::page_size(width = d$page["width"],
                              height = d$page["height"],
                              orient = "portrait")
  pmar <- officer::page_mar(bottom = d$margins["bottom"],
                            top = d$margins["top"],
                            right = d$margins["right"],
                            left = d$margins["left"],
                            header = d$margins["header"],
                            footer = d$margins["footer"],
                            gutter = 0)
  bs <- officer::block_section(
    officer::prop_section(page_size = psize,
                          page_margins = pmar,
                          type = type))
  officer::body_end_block_section(x, bs)
}

end_landscape <- function (x, type = "nextPage") {
  d <- docx_dim(x)
  psize <- officer::page_size(width = d$page["width"],
                              height = d$page["height"],
                              orient = "landscape")
  pmar <- officer::page_mar(bottom = d$margins["bottom"],
                            top = d$margins["top"],
                            right = d$margins["right"],
                            left = d$margins["left"],
                            header = d$margins["header"],
                            footer = d$margins["footer"],
                            gutter = 0)
  bs <- officer::block_section(
    officer::prop_section(page_size = psize,
                          page_margins = pmar,
                          type = type))
  officer::body_end_block_section(x, bs)
}

table_adder <- function(x, obj, ...) {
  flextable::body_add_flextable(x, obj$ft, align = "left",
                                split = TRUE, keepnext = FALSE)
}

figure_adder <- function(x, obj, i) {
  if (i == 1) {
    # This is the first figure, so reserve one line for the Figures
    # section heading.
    obj$height <- obj$height - obj$line.height
  }
  png_file <- paste0(obj$img, ".png")
  rsvg::rsvg_png(obj$img, png_file,
                 width = 300 * obj$width,
                 height = 300 * obj$height)
  img <- officer::external_img(src = png_file,
                               width = obj$width,
                               height = obj$height)
  officer::body_add_fpar(x, officer::fpar(img))
}

word_fields <- function(x) {
  nodes <- xml2::xml_find_all(x$doc_obj$get(),
                              "//w:fldChar | //w:instrText")
  a <- xml2::xml_attr(nodes, "fldCharType", default = "")
  nt <- xml2::xml_text(nodes, trim = TRUE)
  nt[cumsum(a == "begin") > cumsum(a == "end") + 1 & a == ""] <- ""
  a <- a[-length(a)]
  fact <- c(0, cumsum(cumsum(a == "begin") == (cumsum(a == "end"))))
  nt <- split(nt, fact)
  as.character(purrr::map(nt, function(x) paste(x, collapse = "")))
}

#' Converts a list of strings to a list of styled paragraphs.
#'
#' @param notes A list of strings.
#' @param styles The styles to apply.
#' @param as_title If TRUE, apply inverse italics for titles.
#'
#' @return A list of styled paragraphs.
#' @export
note_paras <- function(notes, styles, as_title = FALSE) {
  defaults <- flextable::get_flextable_defaults()
  props <- officer::fp_text(font.family = defaults$font.family,
                            font.size = defaults$font.size)
  iprops <- stats::update(props, italic = TRUE)
  gregexpr("\\b", notes, perl = TRUE) -> m
  regmatches(notes, m, invert = TRUE) %>%
    lapply(function(z) {z[z != ""]}) -> notes_chunks

  map(notes_chunks, function(note_chunks) {
    imap(note_chunks, function(chunk, i) {
      if(xor(chunk %in% styles$italic.cols, as_title) ||
         i == 1 && !as_title && chunk == "Note") {
        flextable::as_chunk(chunk, props = iprops)
      } else {
        flextable::as_chunk(chunk, props = props)
      }})}) -> notes_chunks
  map(notes_chunks, function(note_chunks) {
    flextable::as_paragraph(list_values = note_chunks)
  })
}

#' Converts a list of paragraphs to a flextable with one column.
#'
#' @param paras A list of paragraphs.
#'
#' @return A flextable.
#' @export
note_table <- function(paras) {
  tibble(paras = do.call(c, paras)) %>%
    flextable() %>%
    delete_part(part = "header") %>%
    border_remove() %>%
    padding(padding = 0) %>%
    mk_par(value = paras)
}

note_widths <- function(x) {
  x$body$content[[1]]$data %>%
    tibble() %>%
    mutate(ft_row_id = row_number()) %>%
    unnest(cols = 1) -> txt_data
  widths <- txt_data$width
  heights <- txt_data$height
  txt_data$width <- NULL
  txt_data$height <- NULL
  fontsize <- txt_data$font.size
  fontsize[!(txt_data$vertical.align %in% "baseline")] <-
    fontsize[!(txt_data$vertical.align %in% "baseline")] / 2
  str_extents_ <-
    m_str_extents(
      txt_data$txt,
      fontname = txt_data$font.family,
      fontsize = fontsize,
      bold = txt_data$bold,
      italic = txt_data$italic
    ) / 72
  str_extents_[, 1] <-
    ifelse(is.na(str_extents_[, 1]) & !is.null(widths),
           widths,
           str_extents_[, 1])
  str_extents_[, 2] <-
    ifelse(is.na(str_extents_[, 2]) & !is.null(heights),
           heights,
           str_extents_[, 2])
  dimnames(str_extents_) <- list(NULL, c("width", "height"))
  txt_data <- cbind(txt_data, str_extents_)
  txt_data %>%
    group_by("ft_row_id") %>%
    summarize(width = sum(width)) %>%
    pull(width)
}

#' Creates an docx file with APA sections containing bookmarked
#' table, figure, and appendix content.
#'
#' @param path Path to the input docx file.
#' @param target Path to the output docx file.
#' @param here Find the input docx file beside getSrcFilename(here).
#' @param do_print If TRUE, the docx will be printed to the target.
#' @param do_browse If TRUE, also browse the target.
#' @param drop If TRUE, drop content not referenced by any bookmark.
#' @return The output rdocx document
#' @export
apa_docx <- function(path = NULL, target = NULL, here = NULL,
                     do_print = TRUE, do_browse = TRUE, drop = FALSE) {
  if (is.null(path) && is.function(here)) {
    path <- utils::getSrcFilename(here, full.names = TRUE)
    path <- paste0(tools::file_path_sans_ext(path), ".docx")
  }
  if (do_print && is.null(target)) {
    # Determine output file target.
    target <- paste0(tools::file_path_sans_ext(path), ".tfa.docx")
  }
  # Read input file.
  x <- officer::read_docx(path = path)

  # Find all bookmark references.
  info <- word_fields(x)
  info <- info[grep("REF +[tfa][a-zA-Z0-9_]+", info)]
  bookmarks <- unique(gsub("REF +([tfa][a-zA-Z0-9_]+).*", "\\1", info))
  # Consider only references to available tfas.
  tfas <- get_tfas()
  bookmarks <- intersect(bookmarks, names(tfas))
  # Identify available tables, figures, and appendices.
  tobjs <- names(tfas)[grep("^t", names(tfas))]
  fobjs <- names(tfas)[grep("^f", names(tfas))]
  aobjs <- names(tfas)[grep("^a", names(tfas))]
  # Identify referenced tables, figures, and appendices.
  tbookmarks <- bookmarks[grep("^t", bookmarks)]
  fbookmarks <- bookmarks[grep("^f", bookmarks)]
  abookmarks <- bookmarks[grep("^a", bookmarks)]
  if (drop == FALSE) {
    # Generate referenced tfas first, followed by extra tfas.
    tbookmarks <- c(tbookmarks, setdiff(tobjs, tbookmarks))
    fbookmarks <- c(fbookmarks, setdiff(fobjs, fbookmarks))
    abookmarks <- c(abookmarks, setdiff(aobjs, abookmarks))
  }
  # Determine if the document ends in landscape.
  x <- officer::cursor_end(x)
  landscape <- docx_dim(x)$landscape
  # The output file will initially be a copy of the input file
  # but with all body content removed.  Use an {INCLUDETEXT}
  # field in Word to include the output file at the end of the
  # input file.
  while(length(x)>1) {
    x <- officer::cursor_end(x)
    x <- officer::body_remove(x)
  }
  # Get default formatting info.
  defaults <- flextable::get_flextable_defaults()
  for (item in list(
    list(bookmarks = tbookmarks,
         caption = "Table",
         adder = table_adder),
    list(bookmarks = fbookmarks,
         caption = "Figure",
         adder = figure_adder))) {
    section <- paste0(item$caption, "s")
    for (i in seq_along(item$bookmarks)) {
      bookmark <- item$bookmarks[[i]]
      if (bookmark %in% bookmarks) {
        print(paste("Bookmark:", bookmark))
      } else {
        print(paste("Extra Bookmark:", bookmark))
      }
      obj <- tfas[[bookmark]]
      if (i == 1) {
        if (length(x) == 1) {
          if (xor(obj$wide, landscape)) {
            x <- end_portrait(x)
          } else {
            x <- officer::body_add_break(x)
          }
        }
        x <- officer::body_add_par(x, section, style = "heading 1")
      }
      x <- officer::body_add_par(x, paste(item$caption, i),
                                 style = "caption")
      x <- officer::body_bookmark(x, bookmark)
      x <- table_adder(x, list(ft = obj$title))
      fp_p <- officer::fp_par(line_spacing = 0)
      blank_line <- officer::fpar("", fp_p = fp_p)
      x <- officer::body_add_fpar(x, blank_line)
      x <- item$adder(x, obj, i)
      if (!is.null(obj$notes)) {
        fp_p <- officer::fp_par(line_spacing = 1)
        blank_line <- officer::fpar("", fp_p = fp_p)
        x <- officer::body_add_fpar(x, blank_line)
        x <- table_adder(x, list(ft = obj$notes))
      }
      if (obj$wide) {
        x <- end_landscape(x)
      } else {
        x <- end_portrait(x)
      }
    }
  }

  for (i in seq_along(abookmarks)) {
    bookmark <- abookmarks[[i]]
    brief <- FALSE
    if (bookmark %in% bookmarks) {
      print(paste("Bookmark:", bookmark))
    } else {
      print(paste("Extra Bookmark:", bookmark))
      brief <- TRUE
    }
    obj <- tfas[[bookmark]]
    if (i == 1) {
      if (length(x) == 1 && obj$wide) {
        x <- end_portrait(x)
      }
    }
    section <- "Appendix"
    if (length(abookmarks)>1) {
      section <- paste(section, LETTERS[[i]])
    }
    x <- officer::body_add_par(x, section, style = "heading 1")
    x <- officer::body_bookmark(x, bookmark)
    x <- obj$fun(x, brief = brief)
    if (obj$wide) {
      x <- end_landscape(x)
    } else {
      x <- end_portrait(x)
    }
  }

  if (do_print) {
    print(x, target = target)
    if (do_browse) {
      utils::browseURL(target)
    }
  }
  return(x)
}

#' Begins capturing a figure using svg
#'
#' The title and notes args accept flextable objects.
#' If a string is provided instead it will be converted
#' to a flextable with one row and italics based on the styles.
#' If a list of strings is provided instead it will be converted
#' to a flextable with a row for each list item.
#'
#' @param bookmark Bookmark for the figure
#' @param title Title for the figure
#' @param styles A styles list to use.
#' @param notes Notes about the figure.
#' @param wide Should the figure be displayed in landscape?
#' @param width Width for the figure in inches
#' @param height Height for the figure in inches
#' @param reserve Amount to subtract from the height
#' @export
begin_figure <- function(bookmark,
                         title,
                         styles,
                         notes = NULL,
                         wide = FALSE,
                         width = NULL,
                         height = NULL,
                         reserve = 0) {
  if (substr(bookmark, 1, 1) != "f") {
    stop("bookmark must start with the letter 'f'.")
  }
  if (nchar(bookmark) >= 40) {
    stop("bookmark must be less than 40 characters long.")
  }
  if (is.null(width)) {
    width <-
      if (wide) styles$landscape.width else styles$portrait.width
  }
  if (is.null(height)) {
    height <-
      if (wide) styles$landscape.height else styles$portrait.height
  }
  height <- height - reserve
  extra_lines <- 0
  title <- to_note_table(title, styles, as_title = TRUE, width)
  title %>% note_widths() -> nws
  tibble(w = nws) %>%
    mutate(nlines = as.integer(.data$w / width + 1)) %>%
    summarize(nlines = sum(nlines)) %>%
    pull(nlines) -> nlines
  extra_lines <- extra_lines + (nlines - 1)
  if (!is.null(notes)) {
    notes <- to_note_table(notes, styles, as_title = FALSE, width)
    notes %>% note_widths() -> nws
    tibble(w = nws) %>%
      mutate(nlines = as.integer(.data$w / width + 1)) %>%
      summarize(nlines = sum(nlines)) %>%
      pull(nlines) -> nlines
    extra_lines <- extra_lines + nlines
  }
  # Reduce the figure height to allow for extra title and notes lines.
  height <- height - styles$line.height * extra_lines
  fig_dir <- "./Figures"
  dir.create(fig_dir, showWarnings = FALSE)
  svg_file <- file.path(fig_dir, paste0(bookmark, ".svg"))
  meta_file <- file.path(fig_dir, paste0(bookmark, ".meta"))
  svg_file <- normalizePath(svg_file, "/", mustWork = FALSE)
  grDevices::svg(svg_file, width = width, height = height)
  extrafont::loadfonts(device = styles$device, quiet = TRUE)
  obj <- list(title = title, wide = wide, notes = notes,
              img = svg_file, width = width, height = height,
              line.height = styles$line.height)
  save(obj, file = meta_file)
  tfas <- get_tfas()
  tfas[[bookmark]] <- obj
  set_tfas(tfas)
  return()
}

#' Stops capturing a figure
#'
#' @return Previous graphic device
#' @export
end_figure <- function() {grDevices::dev.off()}

#' Adds a figure
#'
#' @param fig The figure
#' @param bookmark Bookmark for the figure
#' @param title Title for the figure
#' @param styles A styles list to use.
#' @param notes Notes about the figure
#' @param wide Should the figure be displayed in landscape?
#' @param width Width for the figure in inches
#' @param height Height for the figure in inches
#' @param reserve Amount to subtract from the height
#' @return The figure
#' @export
add_figure <- function(fig, bookmark, title, styles, notes = NULL,
                       wide = FALSE, width = NULL,
                       height = NULL, reserve = 0) {
  print(fig)
  if (is.null(fig$layout)) {
    begin_figure(bookmark, title, styles, notes = notes,
                 wide = wide, width = width,
                 height = height, reserve = reserve)
    print(fig)
    end_figure()
  } else {
    step <- prod(fig$layout)
    last <- prod(dim(fig))
    iseq <- seq(1, last, step)
    for (i in iseq) {
      afig <- fig[seq(i, min(last, i+step-1))]
      bookmarki <-
        if (length(iseq) == 1) bookmark else paste0(bookmark, i)
      titlei <- if (is.list(title) && !inherits(title, "flextable")) {
        title[[i]]
      } else {
        title
      }
      notesi <-
        if (is.list(notes) && !inherits(notes, "flextable")) {
          notes[[i]]
        } else {
          notes
        }
      begin_figure(bookmarki, titlei, styles, notes = notesi,
                   wide = wide, width = width,
                   height = height, reserve = reserve)
      print(afig)
      end_figure()
    }
  }
  return(fig)
}

#' Loads a figure
#'
#' @param bookmark Bookmark of the figure
#' @return The figure
#' @export
load_figure <- function(bookmark) {
  fig_dir <- "./Figures"
  meta_file <- file.path(fig_dir, paste0(bookmark, ".meta"))
  if (!file.exists(meta_file)) {
    stop("File not found: ", meta_file)
  }
  obj <- NULL
  load(meta_file)
  tfas <- get_tfas()
  tfas[[bookmark]] <- obj
  set_tfas(tfas)
  return()
}

#' Rotates and aligns the header of a flextable
#'
#' @param x A flextable
#' @param rotation Header rotation (tbrl or btlr)
#' @param align Header alignment (left, center, or right)
#' @return A flextable
#' @export
rotate_header <- function(x, rotation = "tbrl", align = "right") {
  stopifnot(inherits(x, "flextable"))
  h <- max(flextable::dim_pretty(x, part = "header")$widths)
  x <- flextable::align(x, align = align, part = "header")
  x <- flextable::rotate(x, rotation = rotation,
                         align = "center", part = "header")
  x <- flextable::height(x, height = h, part = "header")
  x <- flextable::hrule(x, rule = "exact", part = "header")
}

#' Fits the column widths of a flextable using dim_pretty
#'
#' @param x A flextable
#' @param body_only Only use the body for measuring pretty widths
#' @return A flextable
#' @export
autofit_width <- function(x, body_only = FALSE) {
  stopifnot(inherits(x, "flextable"))
  if (!inherits(x$body$dataset, "grouped_data")) {
    wb <- flextable::dim_pretty(x, part = "body")$widths
  } else {
    # Account for the table spanner.
    spanner <- names(x$body$dataset)[[1]]
    is_spanner <- stats::formula(paste("~ !is.na(", spanner, ")"))
    y <- flextable::mk_par(x, i = is_spanner, j=1, part="body",
                           value = flextable::as_paragraph(""))
    y <- flextable::mk_par(y, i = NULL, j = 1, part = "header",
                           value = flextable::as_paragraph(y$col_keys[[1]]))
    wb <- flextable::dim_pretty(y, part = "body")$widths
  }

  if (body_only) {
    return(flextable::width(x, width = wb))
  }

  df <- data.frame(wb = wb)
  h_nrow <- flextable::nrow_part(x, "header")
  if (h_nrow == 1) {
    df$wh <- flextable::dim_pretty(x, part = "header")$widths
    df$w <- pmax(df$wb, df$wh)
  } else {
    # Account for spanned headers.
    j <- grep("\\.", x$col_keys)
    df$wx <- flextable::dim_pretty(x, part = "header")$widths
    y <- flextable::mk_par(x, i = 1, j = j, part = "header",
                           value = flextable::as_paragraph(""))
    df$wy <- flextable::dim_pretty(y, part = "header")$widths
    df$grp <- gsub("^([^\\.]*).*", "\\1", x$col_keys)
    df <- dplyr::group_by(df, .data$grp)
    df <- dplyr::mutate(df, sy = sum(.data$wy),
                        cnt = length(.data$wy),
                        fx = dplyr::first(.data$wx))
    df <- dplyr::ungroup(df)
    df$wh <- dplyr::if_else(df$fx <= df$sy, df$wy,
                            df$wy + (df$fx - df$sy) / df$cnt)
    df$w <- pmax(df$wb, df$wh)
  }

  flextable::width(x, width = df$w)
}

to_note_table <- function(notes, styles, as_title, width) {
  if (inherits(notes, "flextable")) {
    notes
  } else {
    if (!is.vector(notes)) {
      list(notes) -> notes
    }
    map(notes, function (note) {
      if (inherits(note, "paragraph")) {
        note
      } else {
        note_paras(note, styles, as_title = as_title) %>% first()
      }
    }) %>%
      note_table() %>%
      flextable::width(width = width) -> title
  }
}

#' Adds a flextable
#'
#' @param x A flextable.
#' @param bookmark Bookmark for the table.
#' @param title Title for the table.
#' @param styles A styles list to use.
#' @param notes Notes about the table.
#' @param wide Should the table be displayed in landscape?
#' @param width Width for the table in inches.
#' @return A flextable.
#' @export
add_table <- function(x, bookmark, title, styles,
                      notes = NULL, wide = FALSE, width = NULL) {
  if (!inherits(x, "flextable")) {
    stop("add_table supports only flextable objects.")
  }
  if (substr(bookmark, 1, 1) != "t") {
    stop("bookmark must start with the letter 't'.")
  }
  if (nchar(bookmark) >= 40) {
    stop("bookmark must be less than 40 characters long.")
  }
  if (is.null(width)) {
    width <-
      if (wide) styles$landscape.width else styles$portrait.width
  }
  title <- to_note_table(title, styles, as_title = TRUE, width)
  if (!is.null(notes)) {
    notes <- to_note_table(notes, styles, as_title = FALSE, width)
  }
  table_dir <- "./Tables"
  dir.create(table_dir, showWarnings = FALSE)
  meta_file <- file.path(table_dir, paste0(bookmark, ".meta"))
  obj <- list(title = title, notes = notes, wide = wide, ft = x)
  save(obj, file = meta_file)
  tfas <- get_tfas()
  tfas[[bookmark]] <- obj
  set_tfas(tfas)
  return(x)
}

#' Loads a flextable
#'
#' @param bookmark Bookmark of the table
#' @return A flextable
#' @export
load_table <- function(bookmark) {
  table_dir <- "./Tables"
  dir.create(table_dir, showWarnings = FALSE)
  meta_file <- file.path(table_dir, paste0(bookmark, ".meta"))
  if (!file.exists(meta_file)) {
    stop("File not found: ", meta_file)
  }
  obj <- NULL
  load(meta_file)
  tfas <- get_tfas()
  tfas[[bookmark]] <- obj
  set_tfas(tfas)
  return(obj$ft)
}

#' Adds an appendix
#'
#' @param bookmark Bookmark for the appendix
#' @param fun Function to add the appendix content to an rdocx object
#' @param wide Should the appendix be displayed in landscape?
#' @export
add_appendix <- function(bookmark, fun, wide = FALSE) {
  if (substr(bookmark, 1, 1) != "a") {
    stop("bookmark must start with the letter 'a'.")
  }
  if (nchar(bookmark) >= 40) {
    stop("bookmark must be less than 40 characters long.")
  }
  obj <- list(wide = wide, fun = fun)
  tfas <- get_tfas()
  tfas[[bookmark]] <- obj
  set_tfas(tfas)
  return()
}

#' Adds markdown content as paragraphs in the default style.
#'
#' @param x The rdocx document.
#' @param ... Markdown content.
#' @return The rdocx document.
#' @export
add_md <- function(x, ...) {
  def_style <- styles_info(x, type = "paragraph",
                           is_default = TRUE)[1, "style_name"]
  paras <- md_notes(...)
  officer::body_add_blocks(x, do.call(officer::block_list, paras))
}

#' Adds the text of a code file in a mono 10pt font, single-spaced.
#'
#' @param x The rdocx document.
#' @param file_name The name of the code file.
#' @param head If TRUE, only add the first few lines of the file.
#' @return The rdocx document.
#' @export
add_code_file <- function(x, file_name, head = FALSE) {
  fp_t <- officer::fp_text(font.family = "Courier New", font.size = 10)
  txt <- readr::read_lines(file_name, lazy = FALSE,
                           n_max = ifelse(head, 6, Inf))
  paras <- purrr::map(txt, officer::fpar, fp_t = fp_t)
  officer::body_add_blocks(x, do.call(officer::block_list, paras))
}
