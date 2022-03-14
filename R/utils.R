
#' Adds a stats::anova(fit1, fit2) table.
#'
#' @import broom
#' @import dplyr
#' @import flextable
#' @importFrom ftExtra as_paragraph_md
#' @import ggplot2
#' @importFrom graphics abline
#' @importFrom grid rasterGrob
#' @importFrom gtools capwords
#' @importFrom moments kurtosis skewness
#' @import officer
#' @importFrom purrr flatten keep map map_chr map_depth
#' @importFrom scales pvalue_format
#' @importFrom stats AIC BIC formula hatvalues logLik nobs
#' @importFrom stats predict quantile rstandard sd setNames shapiro.test
#' @import tibble
#' @import tidyr
#' @importFrom utils flush.console
#'
#' @param x An anova comparing fit1 to fit2.
#' @param ... Args for add_table().
#'
#' @return An Anova flextable.
#' @export
add_anova_table <- function(x, ...) {
  models <- attr(x, "heading")[[2]]
  x <- rowid_to_column(x, "Model")
  x <- as_flextable(x)
  x <- italic(x, j = 2:6, part = "header")
  x <- colformat_double(x, j = 3:5, na_str = "")
  x <- mk_par(x, j = 6, part = "body", use_dot = TRUE,
              value = pval_pars(.))
  x <- add_footer_lines(x, models)
  x <- autofit(x)
  add_table(x, ...)
}

#' Adds a plot of Cook's distance by observation number.
#'
#' @param fit A fit
#' @inheritParams begin_figure
#'
#' @export
add_fit_cook_fig <- function(fit, bookmark, title) {
  notes <- "
Note.  Labels indicated the ID of the example in the dataset."
  apatfa::begin_figure(bookmark, title, notes = notes)
  plot(fit, which = 4)
  abline(h = c(0.5, 1.0), lty = 2, col = 2)
  apatfa::end_figure()
  return()
}

#' Add plots for fits.
#'
#' @param fit A fit
#' @param num The fit number
#'
#' @export
add_fit_figs <- function(fit, num) {
  title <- "Plot of Observed by Fitted for Fit"
  add_fit_obf_fig(fit, paste0("fOFFit", num), paste(title, num))

  title <- "Plot of Residual by Predicted for Fit"
  add_fit_rp_fig(fit, paste0("fRPFit", num), paste(title, num))

  if (diff(range(hatvalues(fit))) > 1e-10) {
    title <- "Plot of Residual by Leverage for Fit"
    add_fit_rl_fig(fit, paste0("fRLFit", num), paste(title, num))
  } else {
    title <- "Plot of Cook's Distance by Observation for Fit"
    add_fit_cook_fig(fit, paste0("fCookFit", num), paste(title, num))
  }
  return()
}

#' Adds an observed versus fitted plot.
#'
#' @param fit A fit
#' @inheritParams add_figure
#'
#' @return A figure.
#' @export
add_fit_obf_fig <- function(fit, bookmark, title) {
  notes <- "
Note.  Labels indicated the ID of the example in the dataset."
  fig <-
    ggplot(mapping = aes(x = fit$fitted.values,
                         y = fit$y,
                         label = names(fit$y))) +
    geom_point() +
    geom_abline(aes(slope = 1, intercept = 0), linetype = 2) +
    geom_text(check_overlap = TRUE, nudge_y = 0.25) +
    scale_x_continuous(breaks = integers_in_range) +
    scale_y_continuous(breaks = integers_in_range) +
    xlab("Fitted") +
    ylab("Observed")
  add_figure(fig, bookmark, title, notes = notes)
}

#' Adds a plot of residual by leverage.
#'
#' @param fit A fit
#' @inheritParams begin_figure
#'
#' @export
add_fit_rl_fig <- function(fit, bookmark, title) {
  notes <- "
Note.  Labels indicated the ID of the example in the dataset."
  apatfa::begin_figure(bookmark, title, notes = notes)
  plot(fit, which = 5)
  apatfa::end_figure()
  return()
}

#' Adds a plot of residual by predicted value.
#'
#' @param fit A fit
#' @inheritParams begin_figure
#' @param type The type of residual to plot.
#'
#' @return A figure.
#' @export
add_fit_rp_fig <- function(fit, bookmark, title, type = "pearson") {
  notes <- "
Note.  Labels indicated the ID of the example in the dataset."
  p <- predict(fit)
  r <- rstandard(fit, type = type)
  fig <-
    ggplot(mapping = aes(x = p, y = r, label = names(r))) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "glm", se = FALSE) +
    geom_text(check_overlap = TRUE, nudge_y = 0.25) +
    xlab("Predicted") +
    ylab(paste("Std.", capwords(type), "Residual"))
  add_figure(fig, bookmark, title, notes = notes)
}

#' Adds an lm table.
#'
#' @param x An lm fit.
#' @param ... Args for add_table().
#'
#' @return A flextable.
#' @export
add_lm_table <- function(x, ...) {
  x <- as_flextable(x)
  ncol <- ncol_keys(x)
  b_nrow <- nrow_part(x, "body")
  x <- set_header_labels(x, term = "Term", std.error = "SE", statistic = "t")
  x <- italic(x, j = 2:ncol, part = "header")
  if (b_nrow > 1) x <- italic(x, i = 2:b_nrow, j = 1, part = "body")
  x <- autofit_width(x)
  add_table(x, ...)
}

#' Adds a glm table.
#'
#' @param x A glm fit.
#' @param ... Args for add_table().
#'
#' @return A flextable.
#' @export
add_glm_table <- function(x, ...) {
  aic <- formatC(AIC(x), digits = 3, format = "f")
  bic <- formatC(BIC(x), digits = 3, format = "f")
  loglik <- formatC(logLik(x), digits = 2, format = "f")
  n <- nobs(x)
  sep <- paste0(rep("&nbsp;", 6), collapse = "")
  glanced <- paste0("AIC: ", aic, sep,
                    "BIC: ", bic, sep,
                    "log(likelihood): ", loglik, sep,
                    "n: ", n)
  fm <- paste("Model:", deparse1(x$formula))
  x <- as_flextable(x)
  ncol <- ncol_keys(x)
  b_nrow <- nrow_part(x, "body")
  x <- set_header_labels(x, term = "Term", std.error = "SE", statistic = "z")
  x <- italic(x, j = 2:ncol, part = "header")
  if (b_nrow > 1) x <- italic(x, i = 2:b_nrow, j = 1, part = "body")
  x <- mk_par(x, i = 2, j = 1, value = as_paragraph_md(glanced),
              part = "footer")
  x <- add_footer_lines(x, fm)
  x <- autofit(x)
  add_table(x, ...)
}

#' Adds styling for the columns of a data frame.
#'
#' Factor and logical columns will use mono face for values.  All column
#' names will be in italics.
#'
#' @param styles Existing styles.
#' @param df The data frame to add.
#'
#' @return Updated styles.
#' @export
add_styling <- function(styles, df) {
  factor_cols <- df %>% keep( ~ is.factor(.)) %>% names()
  logical_cols <- df %>% keep( ~ is.logical(.)) %>% names()
  styles$italic.cols <- unique(c(styles$italic.cols, names(df)))
  styles$mono.cols <- unique(c(styles$mono.cols,
                               factor_cols, logical_cols))
  return(styles)
}

#' Converts an aov to a flextable.
#'
#' @param x An aov.
#'
#' @return A flextable.
#' @export
as_flextable_aov <- function(x) {
  styler <- function(x) {
    # Get number of cols.
    ncol <- ncol_keys(x)
    # Get number of body rows.
    b_nrow <- nrow_part(x, "body")
    # Round doubles to three digits.
    x <- colformat_double(x, na_str = "")
    # Improve header labels.
    x <- set_header_labels(x, term = "Term", `df` = "Df", sumsq = "Sum Sq",
                           meansq = "Mean Sq", statistic = "F",
                           p.value = "Sig.")
    # Italicize statistics in the header.
    x <- italic(x, j = seq.int(2, ncol), part = "header")
    # Italicize variables in the first body column.
    if (b_nrow > 1) {
      x <- italic(x, i = seq.int(1, b_nrow - 1), j = 1, part = "body")
    }
    # Use special formatting for p values.
    x <- mk_par(x, j = "p.value", part = "body", use_dot = TRUE,
                value = pval_pars(.))
    # Fit to width.
    return(autofit_width(x))
  }
  tidy(x) %>%
    mutate(across("df", as.integer)) %>%
    flextable() %>%
    styler()
}

#' Converts an effectsize_anova to a flextable.
#'
#' @param x An effectsize_anova, such as from effectsize::eta_squared().
#'
#' @return A flextable.
#' @export
as_flextable.effectsize_anova <- function(x) {
  styler <- function(x) {
    x <- colformat_double(x)
    # Italicize statistics in the header.
    x <- italic(x, j = 2:4, part = "header")
    # Italicize the variable in the body.
    x <- italic(x, j = 1, part = "body")
    # Fit to width.
    return(autofit_width(x))
  }
  x %>%
    rename_with(function(ns) gsub("CI_", "Conf ", ns, fixed = TRUE)) %>%
    select(-c("CI")) %>%
    rename_with(function(ns) map_chr(ns, title_case)) %>%
    flextable() %>%
    styler()
}

#' Converts an htest to a flextable.
#'
#' @param htest An htest.
#'
#' @return A flextable
#' @export
as_flextable_htest <- function(htest) {
  styler <- function(x) {
    # Round doubles to three digits.
    x <- colformat_double(x, digits = 3, na_str = "")
    # Improve header labels.
    name <- htest$statistic %>% attr("name") %>% title_case()
    pname <- htest$parameter %>% attr("name") %>% title_case()
    x <- set_header_labels(x, statistic = name, parameter = pname,
                           p.value = "Sig.", method = "Method")
    # Italicize statistics in the header.
    x <- italic(x, j = 1:3, part = "header")
    # Use special formatting for p values.
    x <- mk_par(x, j = "p.value", part = "body", use_dot = TRUE,
                value = pval_pars(.))
    # Fit to width.
    return(autofit_width(x))
  }
  tidy(htest) %>%
    flextable() %>%
    styler()
}

#' Converts a kruskal_effsize to a flextable.
#'
#' @param x A kruskal_effsize object.
#'
#' @return A flextable.
#' @export
as_flextable.kruskal_effsize <- function(x) {
  styler <- function(x) {
    # Round doubles to three digits.
    x <- colformat_double(x, digits = 3)
    # Improve header labels.
    x <- set_header_labels(x, .y. = "Variable",
                           effsize = "Effect Size",
                           method = "Method",
                           magnitude = "Magnitude")
    # Italicize statistics in the header.
    x <- italic(x, j = c(2, 3), part = "header")
    # Italicize the variable in the body.
    x <- italic(x, j = 1, part = "body")
    # Fit to width.
    return(autofit_width(x))
  }
  flextable(x) %>%
    styler()
}

#' Converts a power.htest to a flextable.
#'
#' @param x A power.htest such as from power.anova.test().
#'
#' @return A flextable.
#' @export
as_flextable.power.htest <- function(x) {
  tibble(Groups = x$g,
         n = x$n,
         `Between Var.` = x$between.var,
         `Within Var.` = x$within.var,
         `Sig. Level` = x$sig.level,
         Power = x$power) %>%
    flextable() %>%
    italic(j = 3:6, part = "header") %>%
    colformat_double() %>%
    # Use special formatting for the power values.
    mk_par(j = "Power", part = "body", use_dot = TRUE,
           value = pval_pars(., with_p = FALSE))
}

#' Converts a Levene Test to a flextable.
#'
#' @param x A Levene test.
#'
#' @return A flextable.
#' @export
as_flextable_leveneTest <- function(x) {
  mutate(x, Term = c("Group", "Residuals"), .before = 1) %>%
    flextable() %>%
    italic(i = 1, j = 1) %>%
    set_header_labels(`F value` = "F", `Pr(>F)` = "Sig.") %>%
    italic(j = 2:4, part = "header") %>%
    colformat_double(na_str = "") %>%
    mk_par(j = "Pr(>F)", part = "body", use_dot = TRUE,
           value = pval_pars(.)) %>%
    autofit_width()
}

#' Converts an raov to a flextable.
#'
#' @param x An raov.
#' @param effect_size The effect size.
#'
#' @return A flextable.
#' @export
as_flextable.raov <- function(x, effect_size) {
  styler <- function(x) {
    # Round doubles to three digits.
    x <- colformat_double(x, digits = 3, na_str = "")
    # Improve header labels.
    x <- set_header_labels(x, `DF` = "Df", `p-value` = "Sig.",
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
    mutate(across("DF", as.integer)) %>%
    flextable() %>%
    styler()
}

#' Converts a summary.rfit to a flextable.
#'
#' @param x An rfit summary.
#'
#' @return A flextable.
#' @export
as_flextable.summary.rfit <- function(x) {
  styler <- function(x, R2) {
    # Get number of body rows.
    b_nrow <- nrow_part(x, "body")
    # Round doubles to three digits.
    x <- colformat_double(x, digits = 3, na_str = "")
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

#' Converts a TukeyHSD to a flextable.
#'
#' @param x A TukeyHSD.
#'
#' @return A flextable.
#' @export
as_flextable.TukeyHSD <- function(x) {
  styler <- function(x) {
    x <- colformat_double(x)
    # Italicize statistics in the header.
    x <- italic(x, j = 3:7, part = "header")
    # Use special formatting for p values.
    x <- mk_par(x, j = 7, part = "body", use_dot = TRUE,
                value = pval_pars(.))
    # Italicize the variable in the body.
    x <- italic(x, j = 1, part = "body")
    # Fit to width.
    return(autofit_width(x))
  }
  tidy(x) %>%
    rename_with(function(ns) gsub(".", " ", ns, fixed = TRUE)) %>%
    rename_with(function(ns) map_chr(ns, title_case)) %>%
    flextable() %>%
    styler()
}

#' Conditionally blanks the x axis.
#'
#' @param cond The condition when the x axis should be blank.
#'
#' @return A theme that either blanks the x axis or does not.
#' @export
blank_axis_x <- function(cond = TRUE) {
  if (cond) {
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  } else theme()
}

#' Conditionally blanks the y axis.
#'
#' @param cond The condition when the x axis should be blank.
#'
#' @return A theme that either blanks the x axis or does not.
#' @export
blank_axis_y <- function(cond = TRUE) {
  if (cond) {
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  } else theme()
}

#' Gets descriptive statistics for variable v.
#'
#' @param v The values to describe.
#'
#' @return A tibble of descriptive statistics.
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

#' Gets a tibble row summarizing the statistics of a named variable.
#'
#' The first column will be named "Variable" and will contain
#' the name of the variable.  The remaining columns will provide
#' the descriptive statistics for the named variable.
#'
#' @param name The variable name.
#' @param df The data frame.
#'
#' @return A tibble row of descriptive statistics for the named variable.
#' @export
#'
#' @examples
#' dstats_row("x", data.frame(x = c(1, 2, 3)))
#' purrr::map_dfr(c("Sepal.Length", "Sepal.Width"), dstats_row, iris)
dstats_row <- function(name, df) {
  df %>%
    summarize(across(all_of(!!name), dstats)) %>%
    unnest(everything()) %>%
    mutate(Variable = !!name, .before = 1)
}

#' Gets a styles list.
#'
#' @return A list of style elements.
#' @export
get_styles <- function() {
  list(
    italic.cols = c("n", "N", "NAs",
                    "Min", "Q1", "Median", "Mean", "Q3", "Max",
                    "Range", "IQR", "SD", "Skewness", "Kurtosis",
                    "p", "r", "t", "H", "W", "F", "df"),
    mono.cols = c(),
    mono.fontname = "Courier New",
    mono.fontsize = 12,
    mono.fontsize.geom_text = 12 * 0.3,
    mono = element_text(family = "Courier New"),
    bold = element_text(face = "bold"),
    bold.italic = element_text(face = "bold.italic"),
    italic = element_text(face = "italic"),
    plain = element_text(family = "Arial", face = "plain"),
    colors.yes_no_na = c(
      "Yes" = "#4DB6D0",
      "No" = "#D9717D",
      "NA" = "grey"
    ),
    colors.true_false = c("TRUE" = "blue", "FALSE" = "red")
  )
}

#' Converts a static image to a ggplot with fixed coordinates.
#'
#' @param img An image, such as from png::readPNG.
#'
#' @return A ggplot of the image.
#' @export
ggimg <- function(img) {
  rg <- rasterGrob(img)
  h <- dim(rg$raster)[1]
  w <- dim(rg$raster)[2]
  ggplot(data = tibble(x = c(0, 1), y = c(0, 1))) +
    coord_fixed(h/w) +
    annotation_custom(rg, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
}

#' Gets a bookmark for a grouping combination.
#'
#' @param bookmark The base bookmark.
#' @param g A vector of grouping variable names.
#'
#' @return The base bookmark suffixed with grouping information.
#' @export
grouped_bookmark <- function(bookmark, g) {
  len_g <- length(g)
  if (len_g == 0)
    bookmark
  else
    paste0(bookmark, "By", paste0(g, collapse = ""))
}

#' Converts a vector of grouping variables to a list of combinations
#' of the grouping variables taken m at a time for each m in range r.
#'
#' @param gv A vector of group variable names.
#' @param r The combinations range, defaults to all combinations.
#'
#' @return A list of combinations of the grouping variables.
#' @export
grouped_combn <- function(gv, r = seq.int(0, length(gv))) {
  map(r, ~ combn(gv, ., simplify = FALSE)) %>%
    flatten()
}

grouped_title <- function(title, g) {
  #' Gets a title for a grouping combination.
  #'
  #' @param title The base title.
  #' @param g A vector of grouping variable names.
  #'
  #' @return The base title suffixed with grouping information.
  #' @export
  len_g <- length(g)
  if (len_g == 0) title
  else if (len_g == 1) paste(title, "by", g[[1]])
  else if (len_g == 2) paste(title, "by", g[[1]], "and", g[[2]])
  else paste0(title, " by ",
              paste(g[1:len_g-1], collapse = ", "),
              ", and ", g[[len_g]])
}

#' Rotates the header of a flextable.
#'
#' @param x A flextable.
#'
#' @return The flextable with rotated headers, fit to the body width.
#' @export
hrotate <- function(x) {
  rotate_header(x) %>%
    autofit_width(body_only = TRUE)
}

#' Returns the integer values in the range of x, expanded.
#'
#' @inherit integers_in_range
#' @export
integers_in_extended_range <-
  function(x) integers_in_range(x, extend = TRUE)

#' Returns the integer values in the range of x.
#'
#' @param x The axis values.
#' @param extend If TRUE, lean out on the borders.
#'
#' @return A sequence of values in the range.
#' @export
integers_in_range <- function(x, extend = FALSE) {
  lo <- min(x, na.rm = TRUE)
  hi <- max(x, na.rm = TRUE)
  if (extend) seq(floor(lo), ceiling(hi)) else seq(ceiling(lo), floor(hi))
}

#' Runs a Shapiro-Wilk test of normality.
#'
#' Returns "Yes" if v is normal, "No" if v is not normal,
#' or "NA" if the test cannot be run.
#'
#' @param v The values to test.
#' @param alpha The alpha level.
#'
#' @export
is_normal <- function(v, alpha = 0.05) {
  if (length(v) < 3 | length(v) > 5000)
    return("NA")
  else
    return(tryCatch(
      ifelse(shapiro.test(v)$p.value > alpha,
             "Yes", "No"),
      error = function(cond)
        "NA"
    ))
}

#' Uses the geom_boxplot() algorithm to identify the outlier points.
#'
#' @param x A vector of numeric data.
#' @param ... Additional parameters to pass to geom_boxplot().
#'
#' @return A logical vector the same length as x (TRUE if outlier).
#' @export
is_outlier <- function(x, ...) {
  tibble(d = x) %>%
    ggplot(aes(d)) + geom_boxplot(...) -> fig
  layer_data(fig) %>% as.list() -> d
  x < d$xmin | x > d$xmax
}

#' Appends a note about that.
#'
#' @param notes The notes.
#' @param ... That to note.
#'
#' @return The notes, with a note about that appended.
#' @export
note_that <- function(notes = NULL, ...) {
  paste(...) -> that
  if(!is.null(notes) && nchar(notes) > 0 &&
     substr(notes, nchar(notes), nchar(notes)) == ".")
    paste(c(notes, that), collapse = "  ") -> notes
  else
    paste(c(notes, that), collapse = " ") -> notes
  if(!is.null(notes) && nchar(notes) > 0 &&
     substr(notes, nchar(notes), nchar(notes)) != ".")
    paste0(notes, ".")
  else
    notes
}

#' Performs a correlation test and appends a note about it.
#'
#' @param notes The notes.
#' @param df A data frame.
#' @param x The column name for the x variable.
#' @param y The column name for the y variable.
#' @param alpha The alpha level for the correlation test.
#' @param ... Additional args for stats::cor.test().
#'
#' @return The notes, with a correlation test note appended.
#' @export
note_cor_test <- function(notes, df, x, y, alpha = 0.05, ...) {
  stats::cor.test(df[[x]], df[[y]], ...) -> h
  w <- ifelse(h$p.value < alpha, "correlated", "not correlated")
  paste0(x, " and ", y, " were ", w) %>%
    note_estimate_htest(h) %>%
    note_statistic_htest(h) %>%
    note_p_value(h$p.value) %>%
    paste0(".") -> note
  if(!is.null(notes) && nchar(notes) > 0)
    paste0(notes, "  ", note)
  else
    note
}

#' Appends a note about an estimate from a hypothesis test.
#'
#' @param notes The notes.
#' @param h A hypothesis test.
#'
#' @return The notes, with a note about an estimate appended.
#' @export
note_estimate_htest <- function(notes, h) {
  name <- h$estimate %>% attr("name") %>% gsub("cor", "r", .)
  format(h$estimate, digits = 2) -> val
  ifelse(name == "r",
         val %>%
           gsub("^0\\.", ".", .) %>%
           gsub("^-0\\.", "-.", .),
         val) %>%
    paste0(name, "(", h$parameter, ")=", .) -> note
  if(!is.null(notes) && nchar(notes) > 0)
    paste0(notes, ", ", note)
  else
    note
}

#' Prefixes notes with an intro: Note.
#'
#' @param notes The notes.
#'
#' @return The notes, prefixed if the intro was missing.
#' @export
note_intro <- function(notes) {
  ifelse(!is.null(notes) && startsWith(notes, "Note."),
         notes,
         paste0("Note.  ", notes))
}

#' Appends a note about a p value.
#'
#' @param notes The notes.
#' @param p The p value.
#'
#' @return The notes, with a note about a p value appended.
#' @export
note_p_value <- function(notes, p) {
  scales::pvalue_format(prefix = c("<", "=", ">"))(p) %>%
    gsub("^(.)0\\.", "\\1.", .) %>%
    paste0("p", .) -> note
  if(!is.null(notes) && nchar(notes) > 0)
    paste0(notes, ", ", note)
  else
    note
}

#' Appends a note about a statistic from a hypothesis test.
#'
#' @param notes The notes.
#' @param h A hypothesis test.
#'
#' @return The notes, with a note about the statistic appended.
#' @export
note_statistic_htest <- function(notes, h) {
  name <- h$statistic %>% attr("name")
  format(h$statistic, digits = 2) %>%
    paste0(name, "(", h$parameter, ")=", .) -> note
  if(!is.null(notes) && nchar(notes) > 0)
    paste0(notes, ", ", note)
  else
    note
}

#' Appends a note about a Shapiro-Wilk test of normality.
#'
#' @param notes Previous notes.
#' @param alpha The alpha level used for testing.
#' @param what The aesthetic used to indicate the test result.
#'
#' @return The previous notes with a normality note appended.
#' @export
note_normal <- function(notes = NULL, alpha = 0.05, what = "Coloring") {
  note <-
      paste(
        what,
        "indicated if a Shapiro-Wilk test of normality",
        "failed to reject the null hypothesis that the data were",
        "sampled from a population that was normally distributed",
        paste0("(p>", alpha, ").")
      )
    ifelse(is.null(notes) || (nchar(notes) == 0),
           note, paste0(notes, "  ", note))
  }

#' Performs a dependency check.
#'
#' @param target The target file.
#' @param dependency The dependency file.
#'
#' @return TRUE if the target is out of date.
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

#' Converts p values to formatted paragraphs.
#'
#' @param pvals The p values.
#' @param with_p Logical.  Prefix with the p character?
#'
#' @return Formatted paragraphs.
#' @export
pval_pars <- function(pvals, with_p = TRUE) {
  # Start with pvalue format.
  eq <- if(with_p) "=" else ""
  z <- pvalue_format(prefix = c("<", eq, ">"))(pvals)
  # Remove the leading zeros.
  z <- gsub("0\\.", ".", z)
  if (with_p) {
    # Create paragraphs with the pvals prefixed with an italic p.
    italic_p <- as_chunk("p", props = fp_text_lite(italic = TRUE))
    z <- as_paragraph(italic_p, z)
  } else {
    z <- as_paragraph(z)
  }
  # Use a blank paragraph for NA and NaN values.
  z[!is.finite(pvals)] <- as_paragraph("")
  return(z)
}

#' Gets a scale_color_manual with TRUE and FALSE aesthetic keys.
#'
#' @param styles Can use custom colors from a styles list.
#'
#' @return A scale_color_manual object.
#' @export
scale_color_true_false <- function(styles = get_styles()) {
  scale_color_manual(values = styles$colors.true_false)
}

#' Gets a scale_color_manual with Yes, No, NA aesthetic keys.
#'
#' @param styles Can use custom colors from a styles list.
#'
#' @return A scale_color_manual object.
#' @export
scale_color_yes_no_na <- function(styles = get_styles()) {
  scale_color_manual(values = styles$colors.yes_no_na)
}

#' Gets a scale_fill_manual with TRUE and FALSE aesthetic keys.
#'
#' @param styles Can use custom colors from a styles list.
#'
#' @return A scale_fill_manual object.
#' @export
scale_fill_true_false <- function(styles = get_styles()) {
  scale_fill_manual(values = styles$colors.true_false)
}

#' Gets a scale_fill_manual with Yes, No, NA aesthetic keys.
#'
#' @param styles Can use custom colors from a styles list.
#'
#' @return A scale_fill_manual object.
#' @export
scale_fill_yes_no_na <- function(styles = get_styles()) {
  scale_fill_manual(values = styles$colors.yes_no_na)
}

#' Creates an attribution label for maps provided by Stamen Design.
#'
#' @param x The x coordinate for the right edge of the label.
#' @param y The y coordinate for the bottom edge of the label.
#' @param size The label size.
#'
#' @return A geom_label.
#' @export
StamenAttribution <- function(x, y, size = 3) {
  label <- paste("Map tiles by Stamen Design, under CC BY 3.0. Data",
                 "by OpenStreetMap, under ODbL.")
  geom_label(x = x, y = y, label = label, size = size,
             hjust = "right", vjust = "bottom")
}

#' Applies styles to a flextable.
#'
#' @param x A flextable.
#' @param styles A styles list to use.
#' @param i A column selector for the flextable.
#' @param fit Fit to the width if TRUE.
#'
#' @return The styled flextable.
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

#' Applies styles to a flextable with a table spanner.
#'
#' @param x A flextable.
#' @param styles A styles list to use.
#' @param spanner The spanner variable name.
#'
#' @return The styled flextable.
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

#' Converts a title to APA title case.
#'
#' @param title The title to convert.
#'
#' @returns The title in title case.
#' @export
title_case <- function(title) {
  s <- strsplit(strsplit(title, " ")[[1]], "-")
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

#' Suffixes a title with information about the number of observations.
#'
#' @param title The title.
#' @param df The data frame.
#' @param n The name to use to label the size, usually "n" or "N".
#'
#' @export
title_n <- function(title = NULL, df, n = "n") {
  msg <-
    nrow(df) %>%
    format(big.mark = ",") %>%
    paste0("(", n, " = ", . , ")")
  ifelse(is.null(title) || (nchar(title) == 0),
         msg, paste(title, msg))
}

#' Computes standardized scores.
#'
#' Subtracts the mean and then divides by the standard deviation.
#'
#' @inheritParams stats::sd
#'
#' @return The standardized scores for the values.
#' @export
zscore <- function(x, na.rm = FALSE) {
  return((x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm))
}
