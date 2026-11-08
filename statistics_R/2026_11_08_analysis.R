library(broom)
library(writexl)
library(tidyverse)
library(performance) # multicollinearity
library(readxl)
library(lme4) 
library(lmerTest) 
library(dplyr)
library(stringr) # average word length
library(ggpubr)
library(car)
set.seed(42) # EB: set for every code run  --> for ties.method = "random" in rankings

df = as.data.frame(read_excel("data/processed/2026_08_24_fluencyAoAFreqs_df.xlsx")) # EB: as.data.frame() makes baseR more usable
colnames(df) 
# drop first two columns 
df <- df[ , -c(1:2)]
# Check Nones
nrow(df) # 6900
sum(is.na(df$AoA)) #201/6900
sum(is.na(df$freq)) #712/6900 # after imputation will be: 98/6900
# Normalize task labels so that spelling variants are treated as the same category.
df$Aufgabe[df$Aufgabe == "SÜßigkeiten"] <- "Süßigkeiten"
df$Aufgabe[df$Aufgabe == "Sport"] <- "Sportarten"
unique(df$Aufgabe) #10 --> 8

# Normalize task labels so that spelling variants are treated as the same category.
df$Aufgabe[df$Aufgabe == "SÜßigkeiten"] <- "Süßigkeiten"
df$Aufgabe[df$Aufgabe == "Sport"] <- "Sportarten"

#-------------------------------------------------------------------------------
# Frequency Imputation
#-------------------------------------------------------------------------------

# Linear model for missing frequency values, comparing models with and without outliers.
# The model fit improves when outliers are kept, so the analysis uses the full data set.
outliers <- boxplot.stats(log(df$freq))$out # 108 outliers remain

# Filter out extreme values in the log-frequency distribution and remove rows with missing frequency.
df_clean <- df[!(log(df$freq) %in% outliers) & is.na(df$freq) == F, ] # filter 108 outliers and 712 missing values

# For the predictive model, nchar(Response) is also log-transformed because it slightly improves model fit.
summary(model_lm_NoOutliers  <- lm(log(freq) ~ AoA + log(nchar(Response)), data = df_clean)) # R2adj = 0.4201

# R2adj = 0.4341 --> model with outliers is retained
summary(model_lm  <- lm(log(freq) ~ AoA + log(nchar(Response)), data = df))
write_xlsx(
  list(
    coefficients = tidy(model_lm, conf.int = TRUE),
    model_fit    = glance(model_lm)
  ),
  "results/2026_11_08freq_imputation_model.xlsx"
)
# to get r squared and residual from the moodel
broom::glance(model_lm)
# Save diagnostic plots for the linear model.
for (w in 1:4) {
  png(paste0("results/plots/2026_11_08_PredictFreqs", w, ".png"), width = 800, height = 600)
  plot(model_lm, which = w)
  dev.off()
}

# Predict missing frequency values using AoA and response length.
sum(!is.na(df$freq) & !is.na(df$AoA))   # learned 
sum( is.na(df$freq) & !is.na(df$AoA))   # imputed
sum( is.na(df$freq) &  is.na(df$AoA))   # could not be imputed

nrow(df[!is.na(df$freq), ])
missing_rows <- df[is.na(df$freq), ] # EB: 798 missing values
predicted <- predict(model_lm, newdata = missing_rows)
predicted_exp <- exp(predicted)
df$freq[is.na(df$freq)] <- predicted_exp

sum(is.na(df$freq)) # EB: 98 remain


# Check that the predicted values are valid and non-negative.
missing_rows$Response # Responses without frequency values
max(df$freq, na.rm = TRUE)
min(predicted_exp, na.rm = TRUE)

#-------------------------------------------------------------------------------
# Rank words within groups and tasks.
#-------------------------------------------------------------------------------

  
# Count how often each response appears within each task/group combination.
df <- df %>% add_count(Aufgabe, Group, Response, name = "counts")

# Create a rank for each word within each group and task.
# This is useful for comparing response prominence across age groups.
set.seed(42) 
df_ranks <- df %>%
  distinct(Group, Aufgabe, Response, counts, .keep_all = TRUE) %>% # keeps only unique words
  group_by(Group, Aufgabe) %>%
  mutate(rank_raw = rank(-counts, ties.method = "random")) %>%
  ungroup()

# Calculate the number of ranks in the child group.
# This is used as a common denominator for the three groups in the ntile split.
n_per_aufgabe <- df_ranks %>%
  filter(Group == 1) %>%
  distinct(Aufgabe, Response) %>%
  count(Aufgabe, name = "N")


# df_ranks has 2928 rows
df_ranks <- df_ranks %>%
  ungroup() %>%
  left_join(n_per_aufgabe, by = "Aufgabe")

# Convert ranks into equal-sized bins using ntile().
df_bins <- df_ranks %>%
  group_by(Group, Aufgabe) %>%
  mutate(bin = ntile(rank_raw, unique(N))) %>%
  ungroup()

head(as.data.frame(df_ranks))
head(as.data.frame(df_bins)) # identical with rank for children ...
tail(as.data.frame(df_bins)) # ... but not for adults, with bin < rank, as it should be. 

# Compute the mean frequency and AoA for each rank bin.
averaged <- df_bins %>% group_by(Aufgabe, Condition, Group, bin) %>%
  summarise(
    mean_freq    = mean(freq, na.rm = TRUE),
    mean_aoa     = mean(AoA, na.rm = TRUE),
    aggreg_words = paste(Response, collapse = ", "),
    n_words      = n())

averaged <- as.data.frame(averaged)

# ------------------------------------------------------------------------------
# ---- Zipf transformation ----
#-------------------------------------------------------------------------------

# Convert mean frequencies into Zipf values.
# This makes the frequency scale more interpretable and comparable across tasks and groups.
childlex_freqs<- as.data.frame(read_excel("data/norms/childlex_0.17.01c.xlsx", sheet = "All"))
n_lemmas <- nrow(childlex_freqs) #164798 types, 
nrow(childlex_freqs)

corpus_tokens_millions <- 7.8          # no punctuation, 10 Mio. with punctuation (Schroeder et al., 2015)
corpus_types_millions  <- 120000 / 1000000  # word types according to (Schroeder et al., 2015)

N <- corpus_tokens_millions + corpus_types_millions

# convert normalized childlex freqs to raw
freq_childlex_raw <- averaged$mean_freq * corpus_tokens_millions   
zipf  <- round(log10((freq_childlex_raw + 1) / N) + 3, digits = 2)

averaged["zipf"] <- zipf

max(zipf, na.rm = TRUE) - min(zipf, na.rm = TRUE) # max: 8.02, min:2.17
max(averaged["zipf"], na.rm = TRUE) - min(averaged["zipf"], na.rm = TRUE)
max(averaged["mean_freq"], na.rm = TRUE) - min(averaged["mean_freq"], na.rm = TRUE)
max(averaged["mean_aoa"], na.rm = TRUE) - min(averaged["mean_aoa"], na.rm = TRUE)


#-------------------------------------------------------------------------------
# Convert to wide format and calculate differences between groups
#-------------------------------------------------------------------------------

# ---- Prepare wide-format comparisons ----
# Long- to wide-format: each rank bin becomes one row and groups are in separate columns.
wide_df <- averaged %>%
  select(-mean_freq) %>%
  pivot_wider(
    names_from = Group,
    values_from = c(zipf, mean_aoa, aggreg_words, n_words)
  )

# Differences between adults and children / adults and adolescents are calculated here.
wide_df <- wide_df %>%
  mutate(
    diff_zipf_3_1 = zipf_3 - zipf_1,       # Adults - children
    diff_zipf_3_2 = zipf_3 - zipf_2,       # Adults - adolescents
    diff_aoa_3_1  = mean_aoa_3 - mean_aoa_1,
    diff_aoa_3_2  = mean_aoa_3 - mean_aoa_2,
  )


# Calculate mean word length for each group and derive group differences.
wide_df <- mutate(wide_df, word_length_1 = map_dbl(aggreg_words_1, ~ mean(str_length(str_split(.x, ", ")[[1]]), na.rm = TRUE)))
wide_df <- mutate(wide_df, word_length_2 = map_dbl(aggreg_words_2, ~ mean(str_length(str_split(.x, ", ")[[1]]), na.rm = TRUE)))
wide_df <- mutate(wide_df, word_length_3 = map_dbl(aggreg_words_3, ~ mean(str_length(str_split(.x, ", ")[[1]]), na.rm = TRUE)))

wide_df <- wide_df %>%
  mutate(
    dif_wordlength_3_1 = word_length_3 - word_length_1,       # Erwachsene - Kinder
    dif_wordlength_3_2 = word_length_3 - word_length_2,       # Erwachsene - Jugendliche
  )


# EB: Next, return to long format, with differences 3_1 and 3_2 in one column
# begin
colnames(wide_df)
range(wide_df$word_length_3, na.rm = T)

df_wide_3_1 <- wide_df[ , c(1:3, 16, 18, 23)]
colnames(df_wide_3_1)[4:6] <- c("diff_zipf", "diff_aoa", "diff_length")
df_wide_3_1$diff = "adults_children"
df_wide_3_2 <- wide_df[ , c(1:3, 17, 19, 24)]
colnames(df_wide_3_2)[4:6] <- c("diff_zipf", "diff_aoa", "diff_length")
df_wide_3_2$diff = "adults_adolescents"

df_diff <- rbind(df_wide_3_1, df_wide_3_2)

## EB begin
df_diff$exclusions <- "none"
df_diff$exclusions <- ifelse(is.na(df_diff$diff_zipf) == T & is.na(df_diff$diff_aoa) == F, "no_zipf_diff",
                             ifelse(is.na(df_diff$diff_aoa) == T & is.na(df_diff$diff_zipf) == F, "no_aoa_diff",
                                    ifelse(is.na(df_diff$diff_aoa) == T & is.na(df_diff$diff_zipf) == T, "both_diffs_missing", "ok")))
nrow(df_diff[df_diff$exclusions != "ok", ])

# after exclusion of missing data for mean frequency or mean aoa per rank, 
# 628 (95.2) and 638 (93.7) data points remained for the comparison of 
# adults and children and adults and adolescents, respectively. 

addmargins(table(df_diff$exclusions, df_diff$diff)) 
addmargins(round(prop.table(table(df_diff$exclusions, df_diff$diff),2)*100, 2))
addmargins(table(df_diff$exclusions, df_diff$diff, df_diff$Condition)) 

## carry out all following analyses with non-missing cases only, as freq and aoa are included in all analyses
df_diff <- df_diff[df_diff$exclusions == "ok", ]

## do we get the sasme number of exclusions for wide_df? 
nrow(wide_df[is.na(wide_df$diff_zipf_3_1) == T | is.na(wide_df$diff_aoa_3_1) == T, ])
nrow(wide_df[is.na(wide_df$diff_zipf_3_2) == T | is.na(wide_df$diff_aoa_3_2) == T, ])

# wide_df 670 cases
wide_df <- wide_df[!(is.na(wide_df$diff_zipf_3_1) == T | is.na(wide_df$diff_aoa_3_1) == T) & 
                     # wide_df 670 cases
                     !(is.na(wide_df$diff_zipf_3_2) == T | is.na(wide_df$diff_aoa_3_2) == T), ]
# wide_df 660 cases --> clearly, some of the 74 n.a. cases concernced the same rank! 
sum(is.na(wide_df))

colnames(wide_df)

dput(unique(wide_df$Aufgabe))
wide_df$Aufgabe <- factor(wide_df$Aufgabe, 
                          levels = c("Kleidung", "Sportarten", "Süßigkeiten", "Tiere", "d", "h", "n", "s"))

# levels umsortieren, Tiere zuerst
wide_df$Aufgabe <- factor(wide_df$Aufgabe, levels = c("Tiere", "Kleidung", "Sportarten", "Süßigkeiten", "d", "h", "n", "s")) # ggf. andere Baseline

# -----------------------------------------------------------------------------
# ---- Linear models for the lexical task ----
# Fit linear models for the lexical task.
# -----------------------------------------------------------------------------

# scaling of fixed effects necessary to ensure that they are at 0 
lm_zipf_3_1 <- lm(diff_zipf_3_1 ~ 1 + scale(diff_aoa_3_1) + scale(dif_wordlength_3_1), data = wide_df %>% filter(Condition == 1))

# exploratory: effect of task
lm_zipf_3_1t <- lm(diff_zipf_3_1 ~ 1 + scale(diff_aoa_3_1) + scale(dif_wordlength_3_1)   + Aufgabe, data = wide_df %>% filter(Condition == 1))

lm_zipf_3_2 <- lm(diff_zipf_3_2 ~ 1 + scale(diff_aoa_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 1))
# exploratory: effect of task
lm_zipf_3_2t <- lm(diff_zipf_3_2 ~ 1 + scale(diff_aoa_3_2) + scale(dif_wordlength_3_2)   + Aufgabe, data = wide_df %>% filter(Condition == 1))

summary(lm_zipf_3_1)
summary(lm_zipf_3_1t)

summary(lm_zipf_3_2)
summary(lm_zipf_3_2t)

lm_aoa_3_1 <- lm(diff_aoa_3_1 ~ 1 + scale(diff_zipf_3_1) + scale(dif_wordlength_3_1), data = wide_df %>% filter(Condition == 1))
# exploratory: effect of task
lm_aoa_3_1t <- lm(diff_aoa_3_1 ~ 1 + scale(diff_zipf_3_1) + scale(dif_wordlength_3_1) + Aufgabe, data = wide_df %>% filter(Condition == 1))

lm_aoa_3_2 <- lm(diff_aoa_3_2 ~ 1 + scale(diff_zipf_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 1))
# exploratory: effect of task
lm_aoa_3_2t <- lm(diff_aoa_3_2 ~ 1 + scale(diff_zipf_3_2) + scale(dif_wordlength_3_2) + Aufgabe, data = wide_df %>% filter(Condition == 1))

summary(lm_aoa_3_1)
summary(lm_aoa_3_1t)

summary(lm_aoa_3_2)
summary(lm_aoa_3_2t)


# ---- Linear models for the semantic task ----
# Fit the same type of models for the semantic task.
lm_zipf_3_1_sem <- lm(diff_zipf_3_1 ~ 1 + scale(diff_aoa_3_1) + scale(dif_wordlength_3_1), 
                      data = wide_df %>% filter(Condition == 2))
# exploratory: effect of task
lm_zipf_3_1t_sem <- lm(diff_zipf_3_1 ~ 1 + scale(diff_aoa_3_1) + scale(dif_wordlength_3_1) + Aufgabe, 
                       data = wide_df %>% filter(Condition == 2))

lm_zipf_3_2_sem <- lm(diff_zipf_3_2 ~ 1 + scale(diff_aoa_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 2))
# exploratory: effect of task
lm_zipf_3_2t_sem <- lm(diff_zipf_3_2 ~ 1 + scale(diff_aoa_3_2) + scale(dif_wordlength_3_2) + Aufgabe, data = wide_df %>% filter(Condition == 2))

summary(lm_zipf_3_1_sem)
summary(lm_zipf_3_1t_sem)

summary(lm_zipf_3_2_sem)
summary(lm_zipf_3_2t_sem)

lm_aoa_3_1_sem <- lm(diff_aoa_3_1 ~ 1 + scale(diff_zipf_3_1) + scale(dif_wordlength_3_1), data = wide_df %>% filter(Condition == 2))
# exploratory: effect of task
lm_aoa_3_1t_sem <- lm(diff_aoa_3_1 ~ 1 + scale(diff_zipf_3_1) + scale(dif_wordlength_3_1) + Aufgabe, data = wide_df %>% filter(Condition == 2))

lm_aoa_3_2_sem <- lm(diff_aoa_3_2 ~ 1 + scale(diff_zipf_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 2))
# exploratory: effect of task
lm_aoa_3_2t_sem <- lm(diff_aoa_3_2 ~ 1 + scale(diff_zipf_3_2) + scale(dif_wordlength_3_2) + Aufgabe, data = wide_df %>% filter(Condition == 2))

summary(lm_aoa_3_1_sem)
summary(lm_aoa_3_1t_sem)

summary(lm_aoa_3_2_sem)
summary(lm_aoa_3_2t_sem)

#### EB: We can conclude here that task has no specific effect (possibly due to a lack of statistical power). In any event, carry on with prior code. 

# ---- Model tables ----
# Tables

models <- list(
  lm_zipf_3_1_lex     = lm_zipf_3_1,
  lm_zipf_3_2_lex      = lm_zipf_3_2,
  lm_aoa_3_1_lex       = lm_aoa_3_1,
  lm_aoa_3_2_lex       = lm_aoa_3_2,
  lm_zipf_3_1_sem = lm_zipf_3_1_sem,
  lm_zipf_3_2_sem = lm_zipf_3_2_sem,
  lm_aoa_3_1_sem  = lm_aoa_3_1_sem,
  lm_aoa_3_2_sem  = lm_aoa_3_2_sem
)
for (model in models) {
  print(vif(model))
}
models_t <- list(  lm_zipf_3_1_lex     = lm_zipf_3_1t,
                   lm_zipf_3_2_lex      = lm_zipf_3_2t,
                   lm_aoa_3_1_lex       = lm_aoa_3_1t,
                   lm_aoa_3_2_lex       = lm_aoa_3_2t,
                   lm_zipf_3_1_sem = lm_zipf_3_1t_sem,
                   lm_zipf_3_2_sem = lm_zipf_3_2t_sem,
                   lm_aoa_3_1_sem  = lm_aoa_3_1t_sem,
                   lm_aoa_3_2_sem  = lm_aoa_3_2t_sem)


coef_list <- list()
fit_list <- list()
intercepts <- list()

for (name in names(models)) {
  mod <- models[[name]]
  
  coef_list[[name]] <- tidy(mod, conf.int = TRUE) %>%
    mutate(model = name, .before = 1)
  
  fit_list[[name]] <- glance(mod) %>%
    mutate(model = name, .before = 1)
  
}
coef_table <- as.data.frame(bind_rows(coef_list))
fit_table <- bind_rows(fit_list)

write_xlsx(
  list(coefficients = coef_table, fit_stats = fit_table),
  "results/2026_11_08_model_summaries.xlsx"
)

# TASK AS FIXED EFFECT: EXPLORATORY ANALYSIS
coef_list_t <- list()
fit_list <- list()
intercepts <- list()

for (name in names(models_t)) {
  mod <- models_t[[name]]
  
  coef_list_t[[name]] <- tidy(mod, conf.int = TRUE) %>%
    mutate(model = name, .before = 1)
  
  fit_list[[name]] <- glance(mod) %>%
    mutate(model = name, .before = 1)
  
}

coef_table_t <- as.data.frame(bind_rows(coef_list_t))
fit_table <- bind_rows(fit_list)

write_xlsx(
  list(coefficients = coef_table_t, fit_stats = fit_table),
  "results/2026_11_08_TaskEffect_model_summaries.xlsx"
)


# -----------------------------------------------------------------------------
# ---- plot difference scores as histograms
# -----------------------------------------------------------------------------

colnames(df_diff)

df_plot<- df_diff%>%
  mutate(
    diff= factor(df_diff$diff, levels = c("adults_children", "adults_adolescents"), 
                 labels = c("adults - children", "adults - adolescents")),
    Condition= factor(df_diff$Condition, levels = c("1", "2"), 
                      labels = c("Letter fluency", "Semantic fluency")),
  )
table(df_plot$Condition, df_plot$Aufgabe)


levels(df_plot$diff)

histograms_paper <- list()

(intercepts_table <- coef_table[c(1,4,7,10,13,16,19,22), c("model", "term", "estimate")]) #1-4 letter fluency, 2*zipf, 2*aoa
intercepts_table <- cbind(intercepts_table$estimate[1:4], intercepts_table$estimate[5:8])
colnames(intercepts_table) <- c("Letter fluency", "Semantic fluency")


for (i in 1:2){
  j = levels(df_plot$Condition)[i]
  max_xlim <- max(max(abs(range(df_plot[df_plot$diff == "adults - children", "diff_zipf"]))), 
                  max(abs(range(df_plot[df_plot$diff == "adults - adolescents", "diff_zipf"]))))
  
  # hist Zipf, children
  # coord_cartesian and use/pre-calculation of the maximum values from ChaptGPT
  (Z_C <- ggplot(df_plot[df_plot$diff == "adults - children" & df_plot$exclusions == "ok" & df_plot$Condition == j, ], aes(x = diff_zipf)) +
      geom_histogram(bins = 100, fill = "lightgrey") +
      geom_vline(xintercept = 0, color = "grey40", linewidth = 1, linetype = "dashed") +
      geom_vline(xintercept = intercepts_table[1,colnames(intercepts_table) == j], color = "red", linewidth = 1) +
      xlab("Adults minus children") + 
      theme_minimal() +
      theme(axis.title.y = element_blank()) + 
      coord_cartesian(xlim = c(-max_xlim, max_xlim),
                      ylim = c(0,20))) 
  
  # hist Zipf, children
  (Z_A <- ggplot(df_plot[df_plot$diff == "adults - adolescents" & df_plot$exclusions == "ok" & df_plot$Condition == j, ], aes(x = diff_zipf)) +
      geom_histogram(bins = 100, fill = "lightgrey") +
      geom_vline(xintercept = 0, color = "grey40", linewidth = 1, linetype = "dashed") +
      geom_vline(xintercept = intercepts_table[2,colnames(intercepts_table) == j], color = "red", linewidth = 1) +
      xlab("Adults minus adolescents") + 
      theme_minimal() +
      theme(axis.title.y = element_blank()) + 
      coord_cartesian(xlim = c(-max_xlim, max_xlim),
                      ylim = c(0,20)))
  
  max_xlim <- max(max(abs(range(df_plot[df_plot$diff == "adults - children", "diff_aoa"]))), 
                  max(abs(range(df_plot[df_plot$diff == "adults - adolescents", "diff_aoa"]))))
  # hist aoa, children
  (AoA_C <- ggplot(df_plot[df_plot$diff == "adults - children" & df_plot$exclusions == "ok" & df_plot$Condition == j, ], aes(x = diff_aoa)) +
      geom_histogram(bins = 100, fill = "lightgrey") +
      geom_vline(xintercept = 0, color = "grey40", linewidth = 1, linetype = "dashed") +
      geom_vline(xintercept = intercepts_table[3,colnames(intercepts_table) == j], color = "red", linewidth = 1) +
      xlab("Adults minus children") + 
      theme_minimal() +
      theme(axis.title.y = element_blank()) + 
      coord_cartesian(xlim = c(-max_xlim, max_xlim),
                      ylim = c(0,20))) 
  
  # hist Zipf, children
  (AoA_A <- ggplot(df_plot[df_plot$diff == "adults - adolescents" & df_plot$exclusions == "ok" & df_plot$Condition == j, ], aes(x = diff_aoa)) + 
      geom_histogram(bins = 100, fill = "lightgrey") +
      geom_vline(xintercept = 0, color = "grey40", linewidth = 1, linetype = "dashed") +
      geom_vline(xintercept = intercepts_table[4,colnames(intercepts_table) == j], color = "red", linewidth = 1) +
      xlab("Adults minus adolescents") + 
      theme_minimal() +
      theme(axis.title.y = element_blank()) + 
      coord_cartesian(xlim = c(-max_xlim, max_xlim),
                      ylim = c(0,20)))
  
  histograms_paper[[i]] <- ggarrange(
    ggarrange(Z_C, Z_A, ncol = 2) %>% 
      annotate_figure(top = text_grob(paste0(j, ": rank-based Zipf-score differences"),
                                      face = "bold", size = 14)),
    ggarrange(AoA_C, AoA_A, ncol = 2) %>% 
      annotate_figure(top = text_grob(paste0(j, ": rank-based AoA differences"),
                                      face = "bold", size = 14)),
    nrow = 2)
}
histograms_paper[[2]]
histograms_paper[[1]]

png("results/plots/Histogram_LF.png", width = 800, height = 800)
histograms_paper[[1]]
dev.off()
png("results/plots/Histogram_SF.png", width = 800, height = 800)
histograms_paper[[2]]
dev.off()


# -----------------------------------------------------------------------------
# Compare with van Heuven et al. (2014): does a Zipf value of 2 correspond to early-acquired words?
df$zipf_simple <- round(log10((df$freq * corpus_tokens_millions + 1) / N) + 3, digits = 2)

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


# Claude (Anthropic; Claude Opus 5.5) and ChatGPT (OpenAI) were used to assist 
# with debugging and reviewing the R analysis code. 