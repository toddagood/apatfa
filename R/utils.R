#' Adds a stats::anova(fit1, fit2) table.
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
              value = pval_pars(.data$.))
  x <- add_footer_lines(x, models)
  x <- autofit(x)
  add_table(x, ...)
}

#' Adds a plot of Cook's distance by observation number.
#'
#' @param fit A fit
#' @param outliers A vector of labels for outlier points.
#' @inheritParams begin_figure
#'
#' @export
add_fit_cook_fig <- function(fit, bookmark, title, styles,
                             outliers = NULL) {
  notes <- if(!is.null(outliers)) {
    note_that("Outliers were labeled by observation ID.")
  } else NULL
  notes %>% note_fit_model(fit) -> notes
  apatfa::begin_figure(bookmark, title, styles,
                       notes = note_intro(notes))
  plot(fit, which = 4, sub.caption = "")
  abline(h = c(0.5, 1.0), lty = 2, col = 2)
  apatfa::end_figure()
  return()
}

#' Add plots for fits.
#'
#' @param fit A fit.
#' @param num The fit number.
#' @param outliers A vector of labels for outlier points.
#' @inheritParams add_figure
#' @param type Type of residuals to use.
#'
#' @export
add_fit_figs <- function(fit, num, styles, outliers = NULL,
                         type = NULL) {
  title <- "Plot of Observed by Predicted for Fit"
  add_fit_op_fig(fit, paste0("fOPFit", num), paste(title, num),
                 styles, outliers = outliers)

  title <- "Plot of Residual by Predicted for Fit"
  add_fit_rp_fig(fit, paste0("fRPFit", num), paste(title, num),
                 styles, outliers = outliers, type = type)

  title <- "Normal Q-Q Plot of Residuals for Fit"
  add_fit_qq_fig(fit, paste0("fQQFit", num), paste(title, num),
                 styles)

  if (diff(range(hatvalues(fit))) > 1e-10) {
    title <- "Plot of Residual by Leverage for Fit"
    add_fit_rl_fig(fit, paste0("fRLFit", num), paste(title, num),
                   styles, outliers = outliers)
  } else {
    title <- "Plot of Cook's Distance by Observation for Fit"
    add_fit_cook_fig(fit, paste0("fCookFit", num), paste(title, num),
                     styles, outliers = outliers)
  }
  return()
}

#' Adds an observed versus predicted plot.
#'
#' @param fit A fit.
#' @param outliers A vector of labels for outlier points.
#' @inheritParams add_figure
#'
#' @return A figure.
#' @export
add_fit_op_fig <- function(fit, bookmark, title, styles,
                            outliers = NULL) {
  notes <- if(!is.null(outliers)) {
    note_that("Outliers were labeled by observation ID.")
  } else NULL
  notes %>% note_fit_model(fit) -> notes
  f <- predict(fit)
  ob <- f + resid(fit)
  opt_outliers <- function() {
    if (is.null(outliers))
      theme()
    else
      geom_text_repel(aes(label = outliers), na.rm = TRUE)
  }
  fig <-
    ggplot(mapping = aes(f, ob)) +
    geom_point() +
    geom_abline(aes(slope = 1, intercept = 0)) +
    xlab("Predicted") +
    ylab("Observed") +
    opt_outliers() -> fig
  add_figure(fig, bookmark, title, styles,
             notes = note_intro(notes))
}

#' Adds a normal Q-Q plot of residuals.
#'
#' Outliers among residuals are labeled.
#'
#' @param fit A fit.
#' @inheritParams add_figure
#'
#' @return A figure.
#' @export
add_fit_qq_fig <- function(fit, bookmark, title, styles) {
  if (!inherits(fit, "lm")) {
    stop("Only fits of class lm are supported.")
  }
  tibble(sres = rstandard(fit)) %>%
    mutate(label = row_number()) -> data
  ggplot(data, aes(sample = .data$sres)) +
    stat_qq() +
    stat_qq_line() +
    labs(y = "Standard Residual",
         x = "Theoretical Quantile") -> fig
  data %>%
    arrange(.data$sres) %>%
    mutate(outlier = is_outlier(.data$sres),
           rank = rank(-abs(.data$sres))) %>%
    mutate(label = ifelse(.data$outlier & .data$rank <= 3,
                          .data$label, NA)) -> data
  notes <- if(!is.null(data$label)) {
    note_that("Outlier residuals were labeled by observation ID.")
  } else NULL
  notes %>% note_fit_model(fit) -> notes
  fig +
    geom_text_repel(inherit.aes = FALSE,
                    data = layer_data(fig), aes(.data$x, .data$y),
                    label = data$label, na.rm = TRUE) -> fig
  add_figure(fig, bookmark, title, styles,
             notes = note_intro(notes))
}

#' Adds a plot of residual by leverage.
#'
#' @param fit A fit
#' @param outliers A vector of labels for outlier points.
#' @inheritParams add_figure
#'
#' @export
add_fit_rl_fig <- function(fit, bookmark, title, styles,
                           outliers = NULL) {
  notes <- if(!is.null(outliers)) {
    note_that("Outlier residuals were labeled by observation ID.")
  } else NULL
  notes %>% note_fit_model(fit) -> notes
  apatfa::begin_figure(bookmark, title, styles,
                       notes = note_intro(notes))
  plot(fit, which = 5, sub.caption = "")
  apatfa::end_figure()
  return()
}

#' Adds a plot of residual by predicted value.
#'
#' @param fit A fit
#' @param outliers A vector of labels for outlier points.
#' @param type The type of residual to plot.
#' @inheritParams add_figure
#'
#' @return A figure.
#' @export
add_fit_rp_fig <- function(fit, bookmark, title,
                           styles, outliers = NULL,
                           type = NULL) {
  notes <- if(!is.null(outliers)) {
    note_that("Outliers were labeled by observation ID.")
  } else NULL
  notes %>% note_fit_model(fit) -> notes
  p <- predict(fit)
  r <- if (is.null(type)) {
    t <- "Standard"
    rstandard(fit)
  } else {
    t <- type
    rstandard(fit, type = type)
  }
  opt_outliers <- function() {
    if (is.null(outliers))
      theme()
    else
      geom_text_repel(aes(label = outliers), na.rm = TRUE)
  }
  fig <-
    ggplot(mapping = aes(x = p, y = r)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "glm", se = FALSE) +
    xlab("Predicted") +
    ylab(paste(t, "Residual")) +
    opt_outliers()
  add_figure(fig, bookmark, title, styles,
             notes = note_intro(notes))
}

#' Converts a string to a chunk of text in the default table font.
#'
#' @param x A string.
#' @param props The fp_text properties to use (NULL for default).
#'
#' @return A chunk of text.
#' @export
as_t <- function(x, props = NULL) {
  if (is.null(props)) {
    defaults <- flextable::get_flextable_defaults()
    props <- officer::fp_text_lite(font.family = defaults$font.family,
                                   font.size = defaults$font.size)
  }
  as_chunk(x, props = props)
}

#' Adds an lm table.
#'
#' @param x An lm fit.
#' @param ... Additional args for add_table().
#' @inheritParams add_table
#'
#' @return A flextable.
#' @export
add_lm_table <- function(x, bookmark, title, styles,
                         notes = NULL, wide = FALSE, width = NULL,
                         ...) {
  ft <- as_flextable_lm(x)

  g <- broom::glance(x)

  list(title) %>%
    note_paras(styles = styles, as_title = TRUE) %>%
    first() -> header_para

  list("Model:", "Summary:", " ", " ", " ") %>%
    note_paras(styles = styles) -> c1_paras

  note_fit_model(fit = x) %>%
    note_paras(styles = styles) %>%
    first() -> model_para

  paste0("Test statistic: *F*(",
         format(g$df, big.mark = ","),
         ", ",
         format(g$df.residual, big.mark = ","),
         ")=",
         formatC(g$statistic, format = "f", digits = 2),
         ", *p*",
         note_p_value(p = g$p.value, with_p = FALSE,
                      with_eq = TRUE)) %>%
    as_paragraph_md() -> summary_para1

  paste0("Multiple *R*^2^=",
         format(g$r.squared, format = "f", digits = 2),
         ".  Adjusted *R*^2^=",
         format(g$adj.r.squared, format = "f", digits = 2),
         ".  *AIC*=",
         formatC(g$AIC, format = "f", digits = 2),
         ".  *BIC*=",
         formatC(g$BIC, format = "f", digits = 2),
         ".") %>%
    as_paragraph_md() -> summary_para2

  paste0("Residual standard error: ",
         formatC(g$sigma, format = "f", digits = 2),
         " on ",
         format(g$df.residual, big.mark = ","),
         " degrees of freedom.") %>%
    as_paragraph_md() -> summary_para3

  quants <- quantile(x$residuals)
  names(quants) <- c("Min", "Q1", "Median", "Q3", "Max")
  paste0("Residuals: ",
         paste0("*", names(quants),
                "*=",
                formatC(quants, format = "f", digits = 2),
                collapse = ", "),
         ".") %>%
    as_paragraph_md() -> summary_para4

  do.call(c, c1_paras) -> c1_paras

  c(model_para,
    summary_para1,
    summary_para2,
    summary_para3,
    summary_para4) -> c2_paras

  if (is.null(width)) {
    width <-
      if (wide) styles$landscape.width else styles$portrait.width
  }

  defaults <- flextable::get_flextable_defaults()
  big_border <- officer::fp_border(width = 2,
                                   color = defaults$border.color)
  lw <- 0.9
  tibble(c1 = c1_paras, c2 = c2_paras) %>%
    flextable() %>%
    border_remove() %>%
    padding(padding.left = 0, part = "header") %>%
    mk_par(value = .data$., use_dot = TRUE) %>%
    valign(valign = "top") %>%
    width(j = 1, width = lw) %>%
    width(j = 2, width = width - lw) %>%
    mk_par(j = 1, value = header_para, part = "header") %>%
    merge_at(j = 1:2, part = "header") %>%
    align(align = "left", part = "all") %>%
    flextable::hline_bottom(border = big_border, part = "header") %>%
    flextable::hline(i = 1, border = big_border, part = "body") -> title

  note_p_levels() -> stat_para

  notes %>%
    note_paras(styles = styles) -> paras
  paras[[length(paras) + 1]] <- stat_para
  note_table(paras) %>%
    width(width = width) -> notes

  styler(ft, styles) %>%
    merge_at(i = 1, j = 5:6, part = "header") %>%
    align(j = "p.value", align = "right") %>%
    padding(j = "p.value", padding.right = 0) %>%
    padding(j = "signif", padding.left = 0) %>%
    add_table(bookmark, title, styles, notes = notes,
              wide = wide, width = width, ...)
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
  x <- set_header_labels(x,
                         term = "Term",
                         std.error = "SE",
                         statistic = "z")
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
#' Factor and logical columns will use mono face for values.  All
#' column names will be in italics.
#'
#' @param styles Existing styles.
#' @param df The data frame to add.
#'
#' @return Updated styles.
#' @export
add_styling <- function(styles, df) {
  df %>% keep(is.factor) %>% names() -> factor_cols
  df %>% keep(is.logical) %>% names() -> logical_cols
  df %>% keep(is.factor) %>% map(levels) %>%
    unlist(use.names = FALSE) %>% unique() -> factor_levs
  logical_levs <-
    if(length(logical_cols) > 0) c("TRUE", "FALSE") else c()
  styles$italic.cols <- unique(c(styles$italic.cols, names(df)))
  styles$mono.cols <- unique(c(styles$mono.cols,
                               factor_cols, logical_cols))
  styles$mono.words <- unique(c(styles$mono.words,
                                factor_levs, logical_levs))
  return(styles)
}

#' Converts markdown to a paragraph in APA style.
#'
#' Calls ftExtra::as_paragraph_md(...) and then applies APA
#' styles.
#'
#' @param styles The styles list to use.
#' @param ... The args to pass to ftExtra::as_paragraph_md().
#' @inheritParams ftExtra::as_paragraph_md
#' @return A paragraph.
#' @export
apa_paragraph_md <- function(x, styles, ...) {
  p <- ftExtra::as_paragraph_md(x, ...)
  i <- which(p[[1]]$font.family == "monospace")
  p[[1]][i, "font.family"] <- styles$mono.fontname
  p[[1]][i, "font.size"] <- styles$mono.fontsize
  p[[1]][i, "shading.color"] <- NA
  p
}

#' Converts an aov to a flextable.
#'
#' @param x An aov.
#'
#' @return A flextable.
#' @export
as_flextable_aov <- function(x) {
  styler <- function(x) {
    # Get number of body rows.
    b_nrow <- nrow_part(x, "body")
    # Round doubles to three digits.
    x <- colformat_double(x, na_str = "")
    # Improve header labels.
    x <- set_header_labels(x,
                           term = "Term",
                           sumsq = "SS",
                           statistic = "F",
                           p.value = "Pr(>F)")
    x <- set_formatter(x, p.value = function(x) {
      ifelse(is.na(x),
             "",
             sub("e-(..)$", "e-0\\1", sprintf("%.2e", x)))
    })
    x <- mk_par(x, j = "signif",
                value = as_paragraph(as_sup(as_t(signif_format(.data$p.value)))))
    # Italicize statistics in the header.
    x <- italic(x, j = seq.int(2, 5), part = "header")
    # Italicize variables in the first body column.
    if (b_nrow > 2) {
      x <- italic(x, i = seq.int(2, b_nrow - 1), j = 1, part = "body")
    }
    autofit_width(x) %>%
      merge_at(j = 5:6, part = "header") %>%
      align(j = 5, align = "right") %>%
      align(j = 6, align = "left") %>%
      padding(j = 5, padding.right = 0) %>%
      padding(j = 6, padding.left = 0)
  }
  tidy(x) %>%
    mutate(across("df", as.integer)) -> tab
  flextable(tab, col_keys = c(names(tab), "signif")) %>%
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
    rename_with(
      function(ns) gsub("CI_", "Conf ", ns, fixed = TRUE)) %>%
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
                value = pval_pars(.data$.))
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

signif_format <- function(x){
  z <- cut(x, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
           labels = c("***", "**", "*", "\u2020", ""))
  z <- as.character(z)
  z[is.na(x)] <- ""
  z
}

#' Converts an lm to a flextable.
#'
#' @param x An lm object.
#'
#' @return A flextable.
#' @export
as_flextable_lm <- function(x){

  if( !requireNamespace("broom", quietly = TRUE) ){
    stop(paste("broom package should be installed to create",
               "a flextable from an lm object."))
  }

  data_t <- broom::tidy(x)

  ft <- flextable(data_t,
                  col_keys = c("term", "estimate", "std.error",
                               "statistic", "p.value", "signif"))
  ft <- colformat_double(ft)
  ft <- set_formatter(ft, p.value = function(x) {
    sub("e-(..)$", "e-0\\1", sprintf("%.2e", x))
    })
  ft <- mk_par(ft, j = "signif",
               value = as_paragraph(as_sup(as_t(signif_format(.data$p.value)))))
  ft <- align(ft, j = "signif", align = "left")
  ft <- set_header_labels(ft, term = "Term", estimate = "Estimate",
                          std.error = "SE", statistic = "t",
                          p.value = "Pr(>|t|)", signif = "" )
  ncol <- ncol_keys(ft)
  b_nrow <- nrow_part(ft, "body")
  ft <- italic(ft, j = 2:ncol, part = "header")
  if (b_nrow > 1) ft <- italic(ft, i = 2:b_nrow, j = 1, part = "body")
  ft
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
           value = pval_pars(.data$., with_p = FALSE))
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
           value = pval_pars(.data$.)) %>%
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
    x <- set_header_labels(x, `DF` = "df", `p-value` = "Sig.",
                           effect_size = "Effect Size")
    # Italicize statistics in the header.
    x <- italic(x, j = 2:7, part = "header")
    # Italicize variables in the first body column.
    x <- italic(x, j = 1, part = "body")
    # Use special formatting for p values.
    x <- mk_par(x, j = "p-value", part = "body", use_dot = TRUE,
                value = pval_pars(.data$.))
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
  styler <- function(x) {
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
                value = pval_pars(.data$.))
    # Highlight significant results.
    # x <- highlight(x, ~ p.value < 0.05, ~ p.value)
    # Fit to width.
    return(autofit_width(x))
  }
  x$coefficients %>%
    as_tibble() %>%
    add_column(Term = rownames(x$coefficients), .before = 1) %>%
    flextable() %>%
    styler()
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
                value = pval_pars(.data$.))
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
    mutate(n = length(v),
           NAs = sum(is.na(v)),
           .before = 1) %>%
    mutate(Mean = mean(v, na.rm = TRUE),
           .after = "Median") %>%
    mutate(Range = .data$Max - .data$Min,
           IQR = .data$Q3 - .data$Q1,
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
#' @return A tibble row of descriptive statistics for the variable.
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
    italic = element_text(face = "italic"),
    mono.cols = c(),
    mono.words = c("NA"),
    mono = element_text(family = "Courier New", size = 10),
    mono.fontname = "Courier New",
    mono.fontsize = 10,
    mono.fontsize.geom_text = 10 * 0.3,
    bold = element_text(face = "bold"),
    bold.italic = element_text(face = "bold.italic"),
    plain = element_text(family = "Arial", size = 12, face = "plain"),
    colors.yes_no_na = c(
      "Yes" = "#4DB6D0",
      "No" = "#D9717D",
      "NA" = "grey"
    ),
    colors.true_false = c("TRUE" = "blue", "FALSE" = "red"),
    portrait.width = 6.5,
    portrait.height = 8.0,
    landscape.width = 9.0,
    landscape.height = 5.5,
    line.height = 0.4,
    device = "win"
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
  if (extend)
    seq(floor(lo), ceiling(hi))
  else
    seq(ceiling(lo), floor(hi))
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
note_cor_test <- function(notes = NULL, df, x, y, alpha = 0.05, ...) {
  stats::cor.test(df[[x]], df[[y]], ...) -> h
  w <- ifelse(h$p.value < alpha, "correlated", "not correlated")
  paste0(x, " and ", y, " were ", w) %>%
    note_estimate_htest(h) %>%
    note_statistic_htest(h) %>%
    note_p_value(h$p.value) %>%
    paste0(".") -> note
  note_that(notes, note)
}

#' Appends a note about an estimate from a hypothesis test.
#'
#' @param notes The notes.
#' @param h A hypothesis test.
#'
#' @return The notes, with a note about an estimate appended.
#' @export
note_estimate_htest <- function(notes = NULL, h) {
  name <- gsub("cor", "r", h$estimate %>% attr("name"))
  format(h$estimate, digits = 2) -> val
  paste0(name, "(", h$parameter, ")=",
         ifelse(name == "r",
                gsub("^-0\\.", "-.", gsub("^0\\.", ".", val)),
                val)) -> note
  note_that(notes, note)
}

#' Appends a note about that.
#'
#' @param notes The notes.
#' @param fit The model of this fit will be noted.
#'
#' @return The updated notes.
#' @export
note_fit_model <- function(notes = NULL, fit) {
  fit$terms %>%
    deparse(width.cutoff = 100L) %>%
    as_tibble_col() %>%
    mutate(i = row_number()) %>%
    subset(`i` == min(`i`) | `i` == max(`i`)) %>%
    pull(.data$value) %>%
    paste(collapse = "... + ") -> note
  gsub(" +", " ", note) -> note
  paste("The model was", note) -> fit_model
  note_that(notes, fit_model)
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

#' Appends a note about a Shapiro-Wilk test of normality.
#'
#' @param notes Previous notes.
#' @param what The aesthetic used to indicate the test result.
#' @param alpha The alpha level used for testing.
#'
#' @return The previous notes with a normality note appended.
#' @export
note_normal <- function(notes = NULL, what = "Coloring",
                        alpha = 0.05) {
  paste(what,
        "indicated if a Shapiro-Wilk test of normality",
        "failed to reject the null hypothesis that the data were",
        "sampled from a population that was normally distributed",
        paste0("(p>", alpha, ").")
  ) -> note
  note_that(notes, note)
}

#' Returns a paragraph of significance levels for p.
#'
#' @return A paragraph.
#' @export
note_p_levels <- function() {
  list(as_t("***") %>% as_sup(),
       as_t("p") %>% as_i(),
       as_t("<.001.  "),
       as_t("**") %>% as_sup(),
       as_t("p") %>% as_i(),
       as_t("<.01.  "),
       as_t("*") %>% as_sup(),
       as_t("p") %>% as_i(),
       as_t("<.05.  "),
       as_t("\u2020") %>% as_sup(),
       as_t("p") %>% as_i(),
       as_t("<.10.")) -> note
  as_paragraph(list_values = note)
}

#' Appends a note about a p value.
#'
#' @param notes The notes.
#' @param p The p value.
#' @param with_p Logical.  Prefix with the p character?
#' @param with_eq Logical.  Use equal sign?
#'
#' @return The notes, with a note about a p value appended.
#' @export
note_p_value <- function(notes = NULL, p, with_p = TRUE,
                         with_eq = with_p) {
  # Start with pvalue format.
  eq <- if(with_eq) "=" else ""
  scales::pvalue_format(prefix = c("<", eq, ">"))(p) -> note
  # Remove the leading zeros.
  gsub("0\\.", ".", note) -> note
  if (with_p) {
    paste0("p", note) -> note
  }
  note_that(notes, note)
}

#' Appends a note about a statistic from a hypothesis test.
#'
#' @param notes The notes.
#' @param h A hypothesis test.
#'
#' @return The notes, with a note about the statistic appended.
#' @export
note_statistic_htest <- function(notes = NULL, h) {
  name <- h$statistic %>% attr("name")
  format(h$statistic, digits = 2) -> note
  paste0(name, "(", h$parameter, ")=", note) -> note
  note_that(notes, note)
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

#' Returns the estimated Poisson lambda parameter with CI.
#'
#' @param x A numeric vector of data.
#' @param ... Other args for poisson.exact.
#' @return A tibble with ymin, y, and ymax columns.
#' @export
poisson_lambda_ci <- function(x, ...) {
  pt <- poisson.exact(x = sum(x), T = length(x), ...)
  tibble(y = pt$estimate,
         ymin = pt$conf.int[[1]],
         ymax = pt$conf.int[[2]])
}

#' Converts p values to formatted paragraphs.
#'
#' @param pvals The p values.
#' @param with_p Logical.  Prefix with the p character?
#'
#' @return Formatted paragraphs.
#' @export
pval_pars <- function(pvals, with_p = TRUE) {
  z <- note_p_value(p = pvals, with_p = with_p)
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
           value = as_paragraph(spanner, ": ", as_i(.data$.))) %>%
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
    ifelse(
      w %in% c(
        "a", "an", "and", "as", "at", "but", "by", "for", "if", "in",
        "nor", "of", "off", "on", "or", "per", "so", "the", "to",
        "up", "via", "yet"
      ),
      w,
      paste0(toupper(substring(w, 1, 1)), substring(w, 2))
    )
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
  nrow(df) %>%
    format(big.mark = ",") -> msg
  paste0("(", n, " = ", msg , ")") -> msg
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
