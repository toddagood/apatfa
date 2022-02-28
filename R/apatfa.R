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
    # Load windows fonts.
    extrafont::loadfonts(device = "win", quiet = TRUE)
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
    which_j <- which(sapply(ds, function(x) is.numeric(x)))
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
#' @param ... args to pass to set_flextable_defaults()
#' @export
set_apa_defaults <- function(digits = 2,
                             na_str = "NA",
                             nan_str = "NaN",
                             fmt_datetime = "%Y-%m-%d %H:%M:%S %Z",
                             fig_theme = ggplot2::theme(),
                             ...) {
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
                   legend.text = ggplot2::element_text(family = "Courier New"),
                   legend.title = ggplot2::element_text(face = "bold.italic"),
                   strip.text = ggplot2::element_text(family = "Courier New")) +
    fig_theme
  ggplot2::theme_set(fig_theme)
  return()
}

end_portrait <- function (x) {
  psize <- officer::page_size(width = 8.5, height = 11.0,
                              orient = "portrait")
  bs <- officer::block_section(
    officer::prop_section(page_size = psize, type = "nextPage"))
  officer::body_end_block_section(x, bs)
}

end_landscape <- function (x) {
  psize <- officer::page_size(width = 11.0, height = 8.5,
                              orient = "landscape")
  bs <- officer::block_section(
    officer::prop_section(page_size = psize, type = "nextPage"))
  officer::body_end_block_section(x, bs)
}

table_adder <- function(x, obj) {
  flextable::body_add_flextable(x, obj$ft, align = "left", split = TRUE)
}

figure_adder <- function(x, obj) {
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

#' Creates an docx file with APA sections containing bookmarked
#' table, figure, and appendix content
#'
#' @param path Path to the input docx file
#' @param target Path to the output docx file
#' @param here Find the input docx file beside getSrcFilename(here)
#' @param do_print If TRUE, the docx will be printed to the target
#' @param do_browse If TRUE, also browse the target
#' @return The output rdocx document
#' @export
apa_docx <- function(path = NULL, target = NULL, here = NULL,
                     do_print = TRUE, do_browse = TRUE) {
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
  # Generate referenced tfas first, followed by extra tfas.
  tbookmarks <- c(tbookmarks, setdiff(tobjs, tbookmarks))
  fbookmarks <- c(fbookmarks, setdiff(fobjs, fbookmarks))
  abookmarks <- c(abookmarks, setdiff(aobjs, abookmarks))
  # The output file will initially be a copy of the input file
  # but with all body content removed.  Use an {INCLUDETExT}
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
        if (length(x) == 1 && obj$wide) {
          x <- end_portrait(x)
        }
        x <- officer::body_add_par(x, section, style = "Section")
      }
      x <- officer::body_add_par(x, paste(item$caption, i),
                                 style = "caption")
      x <- officer::body_bookmark(x, bookmark)
      title <-
        if (inherits(obj$title, "block_list")) {
          obj$title
        } else {
          md_title(obj$title)
        }
      x <- officer::body_add_blocks(x, title)
      x <- item$adder(x, obj)
      if (!is.null(obj$notes)) {
        fp_p = officer::fp_par(line_spacing = 1)
        blank_line <- officer::fpar("", fp_p = fp_p)
        x <- officer::body_add_fpar(x, blank_line)
        notes <-
          if (inherits(obj$notes, "block_list")) {
            obj$notes
            } else {
              md_notes(obj$notes)
            }
        x <- officer::body_add_blocks(x, notes)
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
    if (bookmark %in% bookmarks) {
      print(paste("Bookmark:", bookmark))
    } else {
      print(paste("Extra Bookmark:", bookmark))
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
    x <- officer::body_add_par(x, section, style = "Section")
    x <- officer::body_bookmark(x, bookmark)
    if (bookmark %in% bookmarks) {
      x <- obj$fun(x)
    }
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
#' @param bookmark Bookmark for the figure
#' @param title Title for the figure
#' @param wide Should the figure be displayed in landscape?
#' @param width Width for the figure in inches
#' @param height Height for the figure in inches
#' @param reserve Amount to subtract from the height
#' @param notes Notes about the figure (string, fpar, or block_list)
#' @export
begin_figure <- function(bookmark,
                         title,
                         wide = FALSE,
                         width = NULL,
                         height = NULL,
                         reserve = 0,
                         notes = NULL) {
  portrait_width <- 6.5
  portrait_height <- 7.0
  landscape_width <- 9.0
  landscape_height <- 7.0
  if (is.null(width)) {
    width <- if (wide) landscape_width else portrait_width
  }
  if (is.null(height)) {
    height <- if (wide) landscape_height else portrait_height
  }
  height <- height - reserve
  fig_dir <- "./Figures"
  dir.create(fig_dir, showWarnings = FALSE)
  svg_file <- file.path(fig_dir, paste0(bookmark, ".svg"))
  meta_file <- file.path(fig_dir, paste0(bookmark, ".meta"))
  svg_file <- normalizePath(svg_file, "/", mustWork = FALSE)
  grDevices::svg(svg_file, width = width, height = height)
  extrafont::loadfonts(device = "win", quiet = TRUE)
  obj <- list(title = title, wide = wide, notes = notes,
              img = svg_file, width = width, height = height)
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
#' @param wide Should the figure be displayed in landscape?
#' @param width Width for the figure in inches
#' @param height Height for the figure in inches
#' @param reserve Amount to subtract from the height
#' @param notes Notes about the figure
#' @return The figure
#' @export
add_figure <- function(fig, bookmark, title, wide = FALSE, width = NULL,
                       height = NULL, reserve = 0, notes = NULL) {
  print(fig)
  if (is.null(fig$layout)) {
    begin_figure(bookmark, title, wide = wide, width = width,
                 height = height, reserve = reserve, notes = notes)
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
      titlei <- if (is.list(title) && !inherits(title, "block_list")) {
        title[[i]]
      } else {
        title
      }
      notesi <-
        if (is.list(notes) && !inherits(notes, "block_list")) {
          notes[[i]]
        } else {
          notes
        }
      begin_figure(bookmarki, titlei, wide = wide, width = width,
                   height = height, reserve = reserve, notes = notesi)
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
#'
#' @importFrom rlang .data
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

#' Adds a flextable
#'
#' @param x A flextable
#' @param bookmark Bookmark for the table
#' @param title Title for the table
#' @param notes Notes about the table
#' @param wide Should the table be displayed in landscape?
#' @return A flextable
#' @export
add_table <- function(x, bookmark, title, notes = NULL, wide = FALSE) {
  if (!inherits(x, "flextable")) {
    stop("add_table supports only flextable objects.")
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
    stop("The bookmark for the appendix must start with the letter a")
  }
  obj <- list(wide = wide, fun = fun)
  tfas <- get_tfas()
  tfas[[bookmark]] <- obj
  set_tfas(tfas)
  return()
}

#' Adds markdown content as paragraphs with style = "Normal"
#'
#' @param x the rdocx document
#' @param ... markdown content
#' @return the rdocx document
#' @export
add_md_normal <- function(x, ...) {
  for(para in md_notes(...)) {
    x <- officer::body_add_fpar(x, para, style = "Normal")
  }
  return(x)
}

#' Adds the text of a code file in a mono 10pt font, single-spaced
#'
#' @param x the rdocx document
#' @param file_name the name of the code file
#' @return the rdocx document
#' @export
add_code_file <- function(x, file_name) {
  fp_t <- officer::fp_text(font.family = "Courier New", font.size = 10)
  txt <- readr::read_lines(file_name, lazy = FALSE)
  paras <- purrr::map(txt, officer::fpar, fp_t = fp_t)
  officer::body_add_blocks(x, do.call(officer::block_list, paras))
}
