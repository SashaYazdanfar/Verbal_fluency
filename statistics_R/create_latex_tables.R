# =============================================================================
# Export lm() results as APA-7 LaTeX tables in the journal template format
# (\processtable + \colhead).
#
# Expects two named lists of lm objects in your workspace:
#   models       -> main models          -> results_tables.tex
#   models_t  -> models with Aufgabe  -> supp_task_tables.tex
# with the names lm_{zipf|aoa}_{3_1|3_2}_{lex|sem}.
#
# Usage:  source("export_lm_tables.R")   (after fitting the models)
# =============================================================================
library(broom)


# Group comparisons encoded in the model names (3 = adults, 1 = children,
# 2 = adolescents).
comparisons <- c("3_1" = "Adults--children",
                 "3_2" = "Adults--adolescents")

# Dependent variables encoded in the model names.
outcomes <- c(zipf = "Zipf frequency difference",
              aoa  = "AoA difference")

# Fluency tasks encoded in the model names.
tasks <- c(lex = "letter fluency",
           sem = "semantic fluency")

# Readable labels for non-task terms (regular expressions -> label).
term_labels <- c("^\\(Intercept\\)$"           = "Intercept",
                 "^scale\\(diff?_aoa_"         = "AoA difference",
                 "^scale\\(diff?_zipf_"        = "Zipf frequency difference",
                 "^scale\\(diff?_wordlength_"  = "Word length difference")

# Name of the task factor in models_task and English names of its levels.
# Single letters (letter fluency) are shown as "Letter H" automatically;
# levels missing here are shown as they are.
task_factor <- "Aufgabe"
task_level_labels <- c("Kleidung"              = "Clothes",
                       "Sportarten"            = "Sports",
                       "Süßigkeiten" = "Sweets",
                       "Tiere"                 = "Animals")

include_intercept <- TRUE   # set to FALSE to drop the intercept rows
conf_level        <- 0.95

# -----------------------------------------------------------------------------
# 2. Number formatting (APA 7)
# -----------------------------------------------------------------------------

# Plain number with `digits` decimals; LaTeX minus; never "-0.00".
fmt_num <- function(x, digits = 2) {
  r <- round(x, digits)
  r[r == 0] <- 0                                  # turns -0 into 0
  out <- formatC(abs(r), format = "f", digits = digits)
  ifelse(r < 0, paste0("$-$", out), out)
}

# Number without leading zero (for values bounded by 1, e.g. R squared).
fmt_noLead <- function(x, digits = 2) {
  out <- fmt_num(x, digits)
  sub("(^|\\$)0\\.", "\\1.", out)
}

# p value: 3 decimals, no leading zero, "< .001" for very small values.
fmt_p <- function(p) {
  ifelse(p < .001, "$<$ .001", fmt_noLead(p, 3))
}

# Escape LaTeX special characters in labels.
latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  gsub("([&%$#_{}])", "\\\\\\1", x)
}

# -----------------------------------------------------------------------------
# 3. Helpers
# -----------------------------------------------------------------------------

# Return one model from a list; stop with a clear message if it is missing.
get_model <- function(model_list, name, list_name) {
  if (!name %in% names(model_list)) {
    stop(sprintf(
      "Model '%s' not found in '%s'.\n  Available models: %s",
      name, list_name,
      if (length(model_list)) paste(names(model_list), collapse = ", ") else "(none)"),
      call. = FALSE)
  }
  m <- model_list[[name]]
  if (!inherits(m, "lm")) {
    stop(sprintf("'%s' in '%s' is not an lm object (class: %s).",
                 name, list_name, paste(class(m), collapse = "/")), call. = FALSE)
  }
  m
}

# Readable label for one term of model `m`.
label_term <- function(term, m) {
  # Task contrasts, e.g. "Aufgabeh" or "AufgabeKleidung"
  if (startsWith(term, task_factor) && !is.null(m$xlevels[[task_factor]])) {
    lvl_names <- function(l) {
      l <- enc2utf8(l)                       # robust to non-UTF-8 locales
      names(task_level_labels) <- enc2utf8(names(task_level_labels))
      ifelse(l %in% names(task_level_labels), task_level_labels[l],
             ifelse(nchar(l) == 1, paste("Letter", toupper(l)), l))
    }
    level <- substring(term, nchar(task_factor) + 1)
    ref   <- m$xlevels[[task_factor]][1]
    return(sprintf("Task: %s (vs. %s)", lvl_names(level), lvl_names(ref)))
  }
  hit <- vapply(names(term_labels), grepl, logical(1), x = term)
  if (!any(hit)) {
    stop(sprintf("No readable label for term '%s'. Add a pattern to 'term_labels'.",
                 term), call. = FALSE)
  }
  unname(term_labels[which(hit)[1]])
}

# Coefficients + fit of one model, extracted with broom (numeric).
extract_model <- function(m) {
  co <- broom::tidy(m, conf.int = TRUE, conf.level = conf_level)
  if (!include_intercept) co <- co[co$term != "(Intercept)", ]
  co$label <- vapply(co$term, label_term, character(1), m = m)
  list(coefs = co, adj_r2 = broom::glance(m)$adj.r.squared)
}

# -----------------------------------------------------------------------------
# 4. Building the LaTeX tables
# -----------------------------------------------------------------------------

# Block order: adults-children Zipf, adults-children AoA,
#              adults-adolescents Zipf, adults-adolescents AoA.
block_specs <- function(task) {
  grid <- expand.grid(outcome = names(outcomes), comp = names(comparisons),
                      stringsAsFactors = FALSE)
  grid$model <- sprintf("lm_%s_%s_%s", grid$outcome, grid$comp, task)
  grid$title <- sprintf("%s: %s", comparisons[grid$comp], outcomes[grid$outcome])
  grid
}

make_block <- function(m, title, n_col = 7) {
  x  <- extract_model(m)
  co <- x$coefs
  rows <- sprintf("%s & %s & %s & %s & %s & %s & %s\\\\",
                  latex_escape(co$label),
                  fmt_num(co$estimate), fmt_num(co$std.error),
                  fmt_num(co$statistic),
                  fmt_num(co$conf.low), fmt_num(co$conf.high),
                  fmt_p(co$p.value))
  c(sprintf("\\multicolumn{%d}{l}{\\textit{%s}}\\\\", n_col, title),
    rows,
    sprintf("\\multicolumn{%d}{l}{$R^{2}_{\\mathrm{adj}}$ = %s}\\\\[3pt]",
            n_col, fmt_noLead(x$adj_r2, 2)))
}

make_table <- function(model_list, list_name, task, caption, label) {
  specs  <- block_specs(task)
  # Check all models first, so a missing one is reported before any work.
  mods   <- lapply(specs$model, get_model, model_list = model_list,
                   list_name = list_name)
  blocks <- unlist(Map(make_block, mods, specs$title))
  ci_lab <- sprintf("%d\\%% CI", round(conf_level * 100))
  
  c("\\begin{table}[!h]",
    "\\processtable{\\tabcolsep6pt\\begin{tabular}{lrrrrrr}",
    "\\hline",
    sprintf("\\colhead{} & \\colhead{} & \\colhead{} & \\colhead{} & \\multicolumn{2}{c}{\\colhead{%s}} & \\colhead{}\\\\", ci_lab),
    "\\cline{5-6}",
    "\\colhead{Predictor} & \\colhead{Estimate} & \\colhead{\\textit{SE}} & \\colhead{\\textit{t}} & \\colhead{\\textit{LL}} & \\colhead{\\textit{UL}} & \\colhead{\\textit{p}}\\\\",
    "\\hline",
    blocks,
    "\\hline",
    sprintf("\\end{tabular}}{\\caption{%s}\\label{%s}}{}", caption, label),
    "\\end{table}",
    "")
}

write_tables <- function(model_list, list_name, file, caption_fmt, label_fmt) {
  out <- unlist(lapply(names(tasks), function(tk) {
    make_table(model_list, list_name, tk,
               caption = sprintf(caption_fmt, tasks[[tk]]),
               label   = sprintf(label_fmt, tk))
  }))
  writeLines(c("% Generated automatically by export_lm_tables.R -- do not edit by hand",
               "", out), file, useBytes = FALSE)
  message("Written: ", normalizePath(file))
  invisible(out)
}

# -----------------------------------------------------------------------------
# 5. Write the two files
# -----------------------------------------------------------------------------

if (!exists("models"))      stop("Object 'models' not found. Fit the main models first.", call. = FALSE)
if (!exists("models_t")) stop("Object 'models_t' not found. Fit the task models first.", call. = FALSE)

write_tables(models, "models", "results_tables.tex",
             caption_fmt = "Linear regression results for %s",
             label_fmt   = "tab:results_%s")

write_tables(models_t, "models_t", "supp_task_tables.tex",
             caption_fmt = "Linear regression results for %s including task as a predictor",
             label_fmt   = "tab:supp_task_%s")

# -----------------------------------------------------------------------------
# 6. Check: table values vs. summary(model)  (independent of broom)
# -----------------------------------------------------------------------------
# For every model, recomputes all cells from summary() and confint(), formats
# them with the same rules and confirms the exact rows are in the .tex file.
# Also prints one model side by side so you can compare by eye.

check_tables <- function(model_list, list_name, file, show = NULL) {
  tex  <- readLines(file)
  n_ok <- 0
  for (k in seq_along(tasks)) {
    specs <- block_specs(names(tasks)[k])
    for (b in seq_len(nrow(specs))) {
      nm <- specs$model[b]
      m  <- get_model(model_list, nm, list_name)
      s  <- summary(m)
      cf <- coef(s)
      ci <- confint(m, level = conf_level)
      keep <- if (include_intercept) rownames(cf) else setdiff(rownames(cf), "(Intercept)")
      expected <- c(
        sprintf("\\multicolumn{7}{l}{\\textit{%s}}\\\\", specs$title[b]),
        vapply(keep, function(term) sprintf(
          "%s & %s & %s & %s & %s & %s & %s\\\\",
          latex_escape(label_term(term, m)),
          fmt_num(cf[term, "Estimate"]), fmt_num(cf[term, "Std. Error"]),
          fmt_num(cf[term, "t value"]),
          fmt_num(ci[term, 1]), fmt_num(ci[term, 2]),
          fmt_p(cf[term, "Pr(>|t|)"])), character(1)),
        sprintf("\\multicolumn{7}{l}{$R^{2}_{\\mathrm{adj}}$ = %s}\\\\[3pt]",
                fmt_noLead(s$adj.r.squared, 2)))
      # Block b of table k = k-th occurrence of its title line in the file
      idx <- which(tex == expected[1])[k]
      found <- if (is.na(idx)) character(0) else tex[idx:(idx + length(expected) - 1)]
      if (!identical(unname(found), unname(expected))) {
        stop(sprintf("MISMATCH in %s, model %s.\n  expected:\n    %s\n  found:\n    %s",
                     file, nm, paste(expected, collapse = "\n    "),
                     paste(found, collapse = "\n    ")), call. = FALSE)
      }
      n_ok <- n_ok + 1
      if (identical(nm, show)) {
        cat("\n--- summary(", nm, ") ---\n", sep = "")
        print(round(cbind(cf, ci), 4))
        cat("Adjusted R squared:", round(s$adj.r.squared, 4), "\n")
        cat("--- corresponding lines in ", file, " ---\n", sep = "")
        cat(found, sep = "\n")
        cat("\n")
      }
    }
  }
  message(sprintf("OK: %s -- all %d model blocks match summary() and confint().",
                  file, n_ok))
  invisible(TRUE)
}

check_tables(models,      "models",      "results_tables.tex",   show = "lm_zipf_3_1_lex")
check_tables(models_t, "models_task", "supp_task_tables.tex", show = "lm_zipf_3_1_sem")