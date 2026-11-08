

# ---- converting the tables to APA latex format

# results - summary table with columns: model, term, estimate, p.value, conf.low, conf.high

# 2 after comma signs
fmt_num <- function(x) {
  s <- sprintf("%.2f", x)
  s[s == "-0.00"] <- "0.00"
  gsub("-", "$-$", s)
}

fmt_p <- function(p) {
  ifelse(p < .001, "$<$ .001", sub("^0", "", sprintf("%.3f", p)))
}

# Change predictor names from e.g. scale(diff_aoa_3_1) etc.
term_label <- function(term) {
  case_when(
    term == "(Intercept)"      ~ "Intercept",
    grepl("aoa", term)         ~ "AoA difference",
    grepl("zipf", term)        ~ "Zipf difference",
    grepl("wordlength", term)  ~ "Word length difference"
  )
}

# For each task the following table
blocks <- tibble::tribble(
  ~dv,    ~grp,  ~title,
  "zipf", "3_1", "Adults $-$ children: Zipf",
  "aoa",  "3_1", "Adults $-$ children: AoA",
  "zipf", "3_2", "Adults $-$ teenagers: Zipf",
  "aoa",  "3_2", "Adults $-$ teenagers: AoA"
)

# Bind the table

make_table <- function(res, task, caption, label) {
  rows <- c()
  for (i in seq_len(nrow(blocks))) {
    b <- blocks[i, ]
    m <- res %>% filter(model == paste0("lm_", b$dv, "_", b$grp, "_", task))
    if (nrow(m) == 0) stop("Не знайдено модель: ",
                           paste0("lm_", b$dv, "_", b$grp, "_", task))
    
    if (i > 1) rows <- c(rows, "\\noalign{\\vspace{4pt}}")
    rows <- c(rows,
              sprintf("\\multicolumn{6}{l}{\\textit{%s}}\\\\", b$title),
              sprintf("\\quad %s & %s & %s & %s & %s & %s\\\\",
                      term_label(m$term), fmt_num(m$estimate), fmt_num(m$std.error),
                      fmt_num(m$conf.low), fmt_num(m$conf.high), fmt_p(m$p.value))
    )
  }
  
  c("\\begin{table}[!h]",
    "\\processtable{\\tabcolsep8pt\\begin{tabular}{lccccc}",
    "\\hline",
    # перший рядок заголовка: 95% CI над двома стовпцями
    "\\colhead{Predictor} & \\colhead{Estimate} & \\colhead{$SE$} & \\multicolumn{2}{c}{95\\% CI} & \\colhead{$p$}\\\\",
    "\\cline{4-5}",
    # другий рядок заголовка: LL і UL
    " & & & \\colhead{$LL$} & \\colhead{$UL$} & \\\\",
    "\\hline",
    rows,
    "\\hline",
    sprintf("\\end{tabular}}{\\caption{%s}\\label{%s}}{}", caption, label),
    "\\end{table}")
}
# Bind to tables to one file
model_names <- c("lm_zipf_3_1_lex", "lm_zipf_3_2_lex", "lm_aoa_3_1_lex", "lm_aoa_3_2_lex",
                 "lm_zipf_3_1_sem", "lm_zipf_3_2_sem", "lm_aoa_3_1_sem", "lm_aoa_3_2_sem")
models <- list(lm_zipf_3_1, lm_zipf_3_2, lm_aoa_3_1, lm_aoa_3_2,
               lm_zipf_3_1_sem, lm_zipf_3_2_sem, lm_aoa_3_1_sem, lm_aoa_3_2_sem)

results <- bind_rows(lapply(seq_along(models), function(i) {
  out <- tidy(models[[i]], conf.int = TRUE)   # таблиця для однієї моделі
  out$model <- model_names[i]                 # дописуємо її назву
  out
}))
unique(results$model) 

writeLines(
  c(make_table(results, "lex", "Regression results for the lexical task",  "tab:results_lexical"),
    "",
    make_table(results, "sem", "Regression results for the semantic task", "tab:results_semantic")),
  "results/results_tables.tex"
)


# converting to Latex the models with fixed effect of Aufgabe
# 1. Назви предикторів, тепер із рівнями завдання
term_label <- function(term) {
  case_when(
    term == "(Intercept)"             ~ "Intercept",
    term == "AufgabeKleidung"         ~ "Task: clothing",
    term == "AufgabeSportarten"       ~ "Task: sports",
    term == "AufgabeSüßigkeiten"      ~ "Task: sweets",
    grepl("^Aufgabe", term)           ~ paste0("Task: letter ", sub("^Aufgabe", "", term)),
    grepl("aoa", term)                ~ "AoA difference",
    grepl("zipf", term)               ~ "Zipf difference",
    grepl("wordlength", term)         ~ "Word length difference"
  )
}

# p для рядка з показниками моделі: "= .023" або "< .001"
fmt_p_eq <- function(p) {
  ifelse(p < .001, "$<$ .001", paste0("= ", sub("^0", "", sprintf("%.3f", p))))
}

# 2–3. Таблиця з t і рядком показників моделі
make_table_supp <- function(res, fit, task, caption, label) {
  rows <- c()
  for (i in seq_len(nrow(blocks))) {
    b <- blocks[i, ]
    name <- paste0("lm_", b$dv, "_", b$grp, "_", task)
    m <- res %>% filter(model == name)
    g <- fit %>% filter(model == name)
    if (nrow(m) == 0) stop("Не знайдено модель: ", name)
    
    if (i > 1) rows <- c(rows, "\\noalign{\\vspace{4pt}}")
    rows <- c(rows,
              sprintf("\\multicolumn{7}{l}{\\textit{%s}}\\\\", b$title),
              sprintf("\\quad %s & %s & %s & %s & %s & %s & %s\\\\",
                      term_label(m$term), fmt_num(m$estimate), fmt_num(m$std.error),
                      fmt_num(m$statistic), fmt_num(m$conf.low), fmt_num(m$conf.high),
                      fmt_p(m$p.value)),
              sprintf("\\multicolumn{7}{l}{\\quad\\footnotesize $R^2 = %s$, $F(%d, %d) = %s$, $p$ %s}\\\\",
                      sub("^0", "", sprintf("%.2f", g$r.squared)),
                      as.integer(g$df), as.integer(g$df.residual),
                      sprintf("%.2f", g$statistic), fmt_p_eq(g$p.value))
    )
  }
  
  c("\\begin{table}[!h]",
    "\\processtable{\\tabcolsep6pt\\begin{tabular}{lcccccc}",
    "\\hline",
    "\\colhead{Predictor} & \\colhead{Estimate} & \\colhead{$SE$} & \\colhead{$t$} & \\multicolumn{2}{c}{95\\% CI} & \\colhead{$p$}\\\\",
    "\\cline{5-6}",
    " & & & & \\colhead{$LL$} & \\colhead{$UL$} & \\\\",
    "\\hline",
    rows,
    "\\hline",
    sprintf("\\end{tabular}}{\\caption{%s}\\label{%s}}{}", caption, label),
    "\\end{table}")
}

# 4. Моделі із завданням: підставте ваші справжні назви об'єктів
models_task <- list(  lm_zipf_3_1_lex     = lm_zipf_3_1t,
                      lm_zipf_3_2_lex      = lm_zipf_3_2t,
                      lm_aoa_3_1_lex       = lm_aoa_3_1t,
                      lm_aoa_3_2_lex       = lm_aoa_3_2t,
                      lm_zipf_3_1_sem = lm_zipf_3_1t_sem,
                      lm_zipf_3_2_sem = lm_zipf_3_2t_sem,
                      lm_aoa_3_1_sem  = lm_aoa_3_1t_sem,
                      lm_aoa_3_2_sem  = lm_aoa_3_2t_sem)

results_task <- bind_rows(lapply(seq_along(models_task), function(i) {
  out <- tidy(models_task[[i]], conf.int = TRUE)
  out$model <- model_names[i]
  out
}))

fit_task <- bind_rows(lapply(seq_along(models_task), function(i) {
  out <- glance(models_task[[i]])
  out$model <- model_names[i]
  out
}))

writeLines(
  c(make_table_supp(results_task, fit_task, "lex",
                    "Regression results for the lexical task including task as a predictor",
                    "tab:supp_task_lexical"),
    "",
    make_table_supp(results_task, fit_task, "sem",
                    "Regression results for the semantic task including task as a predictor",
                    "tab:supp_task_semantic")),
  "results/supp_task_tables.tex"
)




# -----------------------------------------------------------------------------
# Compare with van Heuven et al. (2014): does a Zipf value of 2 correspond to early-acquired words?
zipf_simple <- round(log10((df$freq + 1) / N) + 3, digits = 2)
df$zipf_simple <- zipf_simple

median_aoa <- median(df$AoA, na.rm = TRUE)

mean_distr <- df %>%
  mutate(aoa_group = case_when(
    AoA <= median_aoa ~ "early",
    AoA > median_aoa ~ "late",
    TRUE ~ NA_character_
  ))

early_late_aoa <- mean_distr %>%
  filter(!is.na(aoa_group)) %>%
  group_by(aoa_group) %>%
  summarise(mean_zipf = mean(zipf_simple, na.rm = TRUE), # compresses the input data
            sd_zipf = sd(zipf_simple, na.rm = TRUE),
            n = n())

# dividing to low, middle and high frequency words

# Source - https://stackoverflow.com/a/62574380
# Posted by AlexB
# Retrieved 2026-08-30, License - CC BY-SA 4.0

median_df_zipf = df %>%
  mutate(tertiles = ntile(zipf_simple, 3)) %>%
  mutate(tertiles = if_else(tertiles == 1, 'Low', if_else(tertiles == 2, 'Medium', 'High'))) %>%
  arrange(zipf_simple)

high_low_middle_freqs <- median_df_zipf %>%
  filter(!is.na(tertiles)) %>%
  group_by(tertiles) %>%
  summarise(mean_zipf = mean(zipf_simple, na.rm = TRUE),
            sd_zipf = sd(zipf_simple, na.rm = TRUE),
            n = n())

write_xlsx(
  list(early_late_aoa, high_low_middle_freqs),
  "results/2026_11_08mean_zipf_aoa.xlsx"
)


citation("car")

