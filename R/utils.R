#############################################################################
#' Gets a styles list
#'
#' @import dplyr
#' @import flextable
#' @import ggplot2
#' @importFrom grid rasterGrob
#' @importFrom moments kurtosis skewness
#' @import officer
#' @importFrom purrr flatten keep map map_depth
#' @importFrom scales pvalue_format
#' @importFrom stats sd setNames quantile shapiro.test formula
#' @import tibble
#' @import tidyr
#' @importFrom utils flush.console
#'
#' @return A list of style elements
#' @export
get_styles <- function() {
  list(italic.cols = c(),
       mono.cols = c(),
       mono.fontname = "Courier New",
       mono.fontsize = 12,
       mono.fontsize.geom_text = 12 * 0.3,
       mono = element_text(family = "Courier New"),
       bold = element_text(face = "bold"),
       bold.italic = element_text(face = "bold.italic"),
       italic = element_text(face = "italic"),
       plain = element_text(family = "Arial", face = "plain"),
       colors.yes_no_na = c("Yes" = "#4DB6D0", "No" = "#D9717D",
                            "NA" = "grey"),
       colors.true_false = c("TRUE" = "blue", "FALSE" = "red")
  )
}

#############################################################################
#' Adds styling for the columns of a data frame.  Factor and logical
#' columns will use mono face for values.  All column names will be
#' in italics.
#'
#' @param styles Existing styles
#' @param df The data frame to add
#' @return Updated styles
#' @export
add_styling <- function(styles, df) {
  factor_cols <- df %>% keep(~ is.factor(.)) %>% names()
  logical_cols <- df %>% keep(~ is.logical(.)) %>% names()
  styles$italic.cols <- unique(c(styles$italic.cols, names(df)))
  styles$mono.cols <- unique(c(styles$mono.cols,
                               factor_cols, logical_cols))
  return(styles)
}

#############################################################################
#' Runs a Shapiro-Wilk test of normality.
#'
#' @param v The values to test
#' @param alpha The alpha level
#' @export
is_normal <- function(v, alpha = 0.05) {
  if (length(v) < 3 | length(v) > 5000) return("NA")
  else return(tryCatch(ifelse(shapiro.test(v)$p.value > alpha,
                              "Yes", "No"),
                       error = function(cond) "NA"))
}

#############################################################################
#' Appends a note about a Shapiro-Wilk test.
#'
#' @param notes Previous notes.
#' @param alpha The alpha level used for testing.
#' @param what The aesthetic used to indicate the test result.
#' @export
normal_note <- function(notes = NULL, alpha = 0.05, what = "Coloring") {
  msg <-
    paste(what, "indicated if a Shapiro-Wilk test of normality",
          "failed to reject the null hypothesis that the data were",
          "sampled from a population that was normally distributed",
          paste0("(*p*>", alpha, ")."))
  ifelse(is.null(notes) || (nchar(notes) == 0),
         msg, paste0(notes, "  ", msg))
}

#' Appends a suffix regarding the size of a data frame.
#'
#' @param df The data frame.
#' @param n The name to use to label the size, usually "n" or "N".
#' @export
n_is <- function(title = NULL, df, n = "n") {
  msg <-
    nrow(df) %>%
    format(big.mark = ",") %>%
    paste0("(*", n, "* = ", . , ")")
  ifelse(is.null(title) || (nchar(title) == 0),
         msg, paste(title, msg))
}

#############################################################################
#' Prefixes notes with an intro: Note.
#'
#' @param notes Existing notes.
#' @export
as_notes <- function(notes) {
  ifelse(!is.null(notes) && startsWith(notes, "Note."), notes, paste0("Note.  ", notes))
}

#############################################################################
#' Gets a scale_fill_manual with Yes, No, NA aesthetic keys.
#'
#' @param styles Can use custom colors from a styles list.
#' @return A scale_fill_manual object
#' @export
scale_fill_yes_no_na <- function(styles = get_styles()) {
  scale_fill_manual(values = styles$colors.yes_no_na)
}

#############################################################################
#' Gets a scale_color_manual with Yes, No, NA aesthetic keys.
#'
#' @param styles Can use custom colors from a styles list.
#' @return A scale_color_manual object
#' @export
scale_color_yes_no_na <- function(styles = get_styles()) {
  scale_color_manual(values = styles$colors.yes_no_na)
}

#############################################################################
#' Gets a scale_fill_manual with TRUE and FALSE aesthetic keys.
#'
#' @param styles Can use custom colors from a styles list.
#' @return A scale_fill_manual object
#' @export
scale_fill_true_false <- function(styles = get_styles()) {
  scale_fill_manual(values = styles$colors.true_false)
}

#############################################################################
#' Gets a scale_color_manual with TRUE and FALSE aesthetic keys.
#'
#' @param styles Can use custom colors from a styles list.
#' @return A scale_color_manual object
#' @export
scale_color_true_false <- function(styles = get_styles()) {
  scale_color_manual(values = styles$colors.true_false)
}

#############################################################################
#' Conditionally blanks the x axis.
#'
#' @param cond The condition when the x axis should be blank.
#' @return A theme that either blanks the x axis or does not.
#' @export
blank_axis_x <- function(cond = TRUE) {
  if (cond) {
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  } else theme()
}

#############################################################################
#' Conditionally blanks the y axis.
#'
#' @param cond The condition when the x axis should be blank.
#' @return A theme that either blanks the x axis or does not.
#' @export
blank_axis_y <- function(cond = TRUE) {
  if (cond) {
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  } else theme()
}

#############################################################################
#' Converts text to APA title case.
#'
#' @param t The text to convert.
#' @returns The text in title case
#' @export
tcase <- function(t) {
  s <- strsplit(strsplit(t, " ")[[1]], "-")
  wf <- function(w) {
    ifelse(w %in% c("and", "as", "but", "for", "if", "nor", "or", "so",
                    "yet", "a", "an", "the", "as", "at", "by", "for", "in",
                    "of", "off", "on", "per", "to", "up", "via"),
           w,
           paste0(toupper(substring(w, 1, 1)), substring(w, 2)))
  }
  map_depth(s, 1, wf) %>%
    map_depth(1, paste, collapse = "-") %>%
    paste(collapse = " ") %>%
    wf()
}

#############################################################################
#' Performs a dependency check.
#'
#' @param target The target file
#' @param dependency The dependency file
#' @return TRUE if the target is out of date
#' @export
out_of_date <- function(target, dependency) {
  if (!file.exists(dependency)) {
    stop(paste("Dependency", dependency, "for target",
               target, "does not exist.\n"))
  }
  if (!file.exists(target)) {
    cat(paste("Building", target, "\n"))
    flush.console()
    return(TRUE)
  }
  if (file.mtime(target) < file.mtime(dependency)) {
    cat(paste("Rebuilding", target, "\n"))
    flush.console()
    return(TRUE)
  }
  return(FALSE)
}

#############################################################################
#' Gets descriptive statistics for variable v.
#'
#' @param v The values to describe
#' @return A tibble of descriptive stats
#' @export
dstats <- function(v) {
  quantile(v, names = FALSE, na.rm = TRUE) %>%
    setNames(c("Min", "Q1", "Median", "Q3", "Max")) %>%
    as_tibble_row() %>%
    add_column(n = length(v),
               NAs = sum(is.na(v)),
               .before = 1) %>%
    add_column(Mean = mean(v, na.rm = TRUE),
               .after = "Median") %>%
    add_column(Range = .[["Max"]] - .[["Min"]],
               IQR = .[["Q3"]] - .[["Q1"]],
               SD = sd(v, na.rm = TRUE),
               Skewness = skewness(v, na.rm = TRUE),
               Kurtosis = kurtosis(v, na.rm = TRUE))
}

#############################################################################
#' Gets a tibble containing the name of the variable (in the first column)
#' and descriptive statistics for the named variable in the remaining
#' columns
#'
#' @param name The variable name
#' @param df The data frame
#' @return A tibble of descriptive stats
#' @export
dstats_row <- function(name, df) {
  df %>%
    summarize(across(all_of(!!name), dstats)) %>%
    unnest(everything()) %>%
    mutate(Variable = !!name, .before = 1)
}

#############################################################################
#' Applies styles to a flextable.
#'
#' @param x A flextable
#' @param styles A styles list to use
#' @param i A column selector for the flextable
#' @param fit Fit to the width if TRUE
#' @return The styled flextable
#' @export
styler <- function(x, styles, i = NULL, fit = TRUE) {
  pattern <- function(cols, exact = FALSE) {
    paste0(c(paste0("^", cols, "$"),
             if (exact) c() else paste0("^", cols, "\\.")),
           collapse = "|")
  }
  italic_cols <- grep(pattern(styles$italic.cols), x$col_keys)
  mono_cols <- grep(pattern(styles$mono.cols), x$col_keys)
  mono_cols_exact <- grep(pattern(styles$mono.cols, TRUE), x$col_keys)
  # Italicize variable names in the header.
  x <- italic(x, i = 1, j = italic_cols, part = "header")
  # Use a mono font for the levels of factors in the body.
  x <- font(x, i = i, j = mono_cols_exact,
            fontname = styles$mono.fontname,  part = "body")
  x <- fontsize(x, i = i, j = mono_cols_exact,
                size = styles$mono.fontsize,  part = "body")
  h_nrow <- nrow_part(x, "header")
  if (h_nrow > 1) {
    # Use a mono font for the levels of factors in the header.
    x <- font(x, i = 2:h_nrow, j = mono_cols,
              fontname = styles$mono.fontname,  part = "header")
    x <- fontsize(x, i = 2:h_nrow, j = mono_cols,
                  size = styles$mono.fontsize,  part = "header")
  }

  if (fit) {
    # Fit to the content width.
    x <- autofit_width(x)
  }
  return(x)
}

#############################################################################
#' Applies styles to a flextable with a table spanner.
#'
#' @param x A flextable
#' @param styles A styles list to use
#' @param spanner The spanner variable name
#' @return The styled flextable
#' @export
styler_with_spanner <- function(x, styles, spanner) {
  # A formula that matches spanner rows.
  is_spanner <- formula(paste("~ !is.na(", spanner, ")"))

  # A formula that matches non-spanner rows.
  is_not_spanner <- formula(paste("~ is.na(", spanner, ")"))

  # Apply regular styles to non-spanner rows,
  # center spanner rows across the table width,
  # italicize the spanner variable name, and
  # re-fit the width of the first column.
  styler(x, styles, i = is_not_spanner) %>%
    align(i = is_spanner, align = "center") %>%
    mk_par(i = is_spanner, use_dot = TRUE,
           value = as_paragraph(spanner, ": ", as_i(.))) %>%
    autofit_width()
}

#############################################################################
#' Rotates the header of a flextable.
#'
#' @param x A flextable
#' @return The flextable with rotated headers
#' @export
hrotate <- function(x) {
  rotate_header(x) %>%
    autofit_width(body_only = TRUE)
}

#############################################################################
#' Converts p values to formatted paragraphs.
#'
#' @param pvals The p values
#' @return Formatted paragraphs
#' @export
pval_pars <- function(pvals) {
  # Start with pvalue format.
  z <- pvalue_format(prefix = c("<", "=", ">"))(pvals)
  # Remove the leading zeros.
  z <- gsub("0\\.", ".", z)
  # Create paragraphs with the pvals prefixed with an italic p.
  italic_p <- as_chunk("p", props = fp_text_lite(italic = TRUE))
  z <- as_paragraph(italic_p, z)
  # Use a blank paragraph for NA and NaN values.
  z[!is.finite(pvals)] <- as_paragraph("")
  return(z)
}

#############################################################################
#' Converts an raov to a flextable
#'
#' @param x An raov
#' @param effect_size The effect size
#' @return A flextable
#' @export
as_flextable.raov <- function(x, effect_size) {
  styler <- function(x) {
    # Round doubles to three digits.
    x <- colformat_double(x, digits = 3, na_str = "")
    # Improve header labels.
    x <- set_header_labels(x, DF = "Df", `p-value` = "Sig.",
                           effect_size = "Effect Size")
    # Italicize statistics in the header.
    x <- italic(x, j = 2:7, part = "header")
    # Italicize variables in the first body column.
    x <- italic(x, j = 1, part = "body")
    # Use special formatting for p values.
    x <- mk_par(x, j = "p-value", part = "body", use_dot = TRUE,
                value = pval_pars(.))
    # Fit to width.
    return(autofit_width(x))
  }
  x$table %>%
    as_tibble() %>%
    add_column(Term = rownames(x$table), .before = 1) %>%
    add_column(effect_size = effect_size) %>%
    mutate(across(DF, as.integer)) %>%
    flextable() %>%
    styler()
}

#############################################################################
#' Converts a summary.rfit to a flextable
#'
#' @param x An rfit summary
#' @return A flextable
#' @export
###########################################################################
as_flextable.summary.rfit <- function(x) {
  styler <- function(x, R2) {
    # Get number of body rows.
    b_nrow <- nrow_part(x, "body")
    # Round doubles to three digits.
    x <- colformat_double(x, digits = 3, na_str = "", )
    # Improve header labels.
    x <- set_header_labels(x, term = "Term", `Std. Error` = "SE",
                           t.value = "t", p.value = "Sig.")
    # Italicize statistics in the header.
    x <- italic(x, j = 2:5, part = "header")
    # Italicize variables in the first body column.
    x <- italic(x, i = seq.int(2, b_nrow), j = 1, part = "body")
    # Use special formatting for p values.
    x <- mk_par(x, j = "p.value", part = "body", use_dot = TRUE,
                value = pval_pars(.))
    # Add multiple R squared in the footer.
    x <- add_footer_lines(x, values = paste("Multiple R-squared (Robust):",
                                            round(R2, 2)))
    # Highlight significant results.
    # x <- highlight(x, ~ p.value < 0.05, ~ p.value)
    # Fit to width.
    return(autofit_width(x))
  }
  x$coefficients %>%
    as_tibble() %>%
    add_column(Term = rownames(x$coefficients), .before = 1) %>%
    flextable() %>%
    styler(x$R2)
}

#############################################################################
#' Converts a vector of grouping variables to a list of combinations
#' of the grouping variables taken m at a time for each m in range r.
#'
#' @param gv A vector of group variable names
#' @param r The combinations range, defaults to all combinations
#' @return A list of combinations of the grouping variables
#' @export
grouped_combn <- function(gv, r = seq.int(0, length(gv))) {
  map(r, ~ combn(gv, ., simplify = FALSE)) %>%
    flatten()
}

#############################################################################
#' Gets a title for a grouping combination.
#'
#' @param title The base title
#' @param g A vector of grouping variable names
#' @return A title suffixed with grouping information
#' @export
grouped_title <- function(title, g) {
  len_g <- length(g)
  if (len_g == 0) title
  else if (len_g == 1) paste(title, "by", g[[1]])
  else if (len_g == 2) paste(title, "by", g[[1]], "and", g[[2]])
  else paste0(title, " by ",
              paste(g[1:len_g-1], collapse = ", "),
              ", and ", g[[len_g]])
}

#############################################################################
#' Gets a bookmark for a grouping combination.
#'
#' @param bookmark The base bookmark
#' @param g A vector of grouping variable names
#' @return A bookmark suffixed with grouping information
#' @export
grouped_bookmark <- function(bookmark, g) {
  len_g <- length(g)
  if (len_g == 0) bookmark
  else paste0(bookmark, "By", paste0(g, collapse = ""))
}

#############################################################################
#' Creates an attribution label for maps provided by Stamen Design.
#'
#' @param x The x coordinate for the right edge of the label
#' @param y The y coordinate for the bottom edge of the label
#' @param size The label size
#' @return A geom_label
#' @export
StamenAttribution <- function(x, y, size = 3) {
  label <- paste("Map tiles by Stamen Design, under CC BY 3.0. Data",
                 "by OpenStreetMap, under ODbL.")
  geom_label(x = x, y = y, label = label, size = size,
             hjust = "right", vjust = "bottom")
}

#############################################################################
#' Converts a static image to a ggplot with fixed coordinates.
#'
#' @param img An image, such as from png::readPNG
#' @return A ggplot of the image
#' @export
###########################################################################
ggimg <- function(img) {
  rg <- rasterGrob(img)
  h <- dim(rg$raster)[1]
  w <- dim(rg$raster)[2]
  fig <-
    ggplot(data = tibble(x = c(0, 1), y = c(0, 1))) +
    coord_fixed(h/w) +
    annotation_custom(rg, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  return(fig)
}

#############################################################################
#' Compute standardized scores
#'
#' @param x The values
#' @return The Z-scored values
#' @export
zscore <- function(x) {
  return((x - mean(x)) / sd(x))
}
