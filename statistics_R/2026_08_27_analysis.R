#install.packages("readxl") 
#install.packages("see")
#install.packages("performance")
#install.packages("lme4")
#install.packages("tidyr")
#install.packages("tidytable")
# Signifikanz ausgeben (lmer does not produce significance)
#install.packages("lmerTest")
#install.packages("broom") #for tibbles
#install.packages("writexl")

library(broom)
library(writexl)
library(dplyr) # data manipulation, pipe usage
library(tidyr) #conversion to wide format
library(performance) #multicollinearity
library(readxl) # XLSX data
library(lme4) 
library(lmerTest) 
library(stringr) # average word length
library(tidytable)

set.seed(42)
setwd("~/Studium_withoutBackup/SS_26/Wortflüssigkeit/21.08.2026_data_analysis")

# Read the processed dataset containing AoA and frequency values.
# This is the main file used for the subsequent analyses.
df = as.data.frame(read_excel("data/processed/2026_08_24_fluencyAoAFreqs_df.xlsx")) # EB: hier as.data.frame() hinzufuegen, dann ist baseR besser anwendbar auf die Daten
colnames(df)
head(df, 20)

# Missing values in frequency and AoA
# Check the number of rows and the amount of missing lexical data.
# This helps verify whether the preprocessing pipeline produced complete values.
nrow(df) # 6900
sum(is.na(df$AoA)) #201/6900
sum(is.na(df$freq)) #712/6900 # now: 98/6900
unique(df$Aufgabe) #10 --> 8
# Normalize task labels so that spelling variants are treated as the same category.
df$Aufgabe[df$Aufgabe == "SÜßigkeiten"] <- "Süßigkeiten"
df$Aufgabe[df$Aufgabe == "Sport"] <- "Sportarten"

# Inspect the number of unique responses within each task and group.
for (i in 5:8) {
  print(unique(df[df$Aufgabe == unique(df$Aufgabe)[i], "Aufgabe"]))
  print(length(unique(df[df$Condition == 2 & df$Group == 1 & df$Aufgabe == unique(df$Aufgabe)[i], "Response"])))
  print(length(unique(df[df$Condition == 2 & df$Group == 2 & df$Aufgabe == unique(df$Aufgabe)[i], "Response"])))
  print(length(unique(df[df$Condition == 2 & df$Group == 3 & df$Aufgabe == unique(df$Aufgabe)[i], "Response"])))
}

for (i in 1:4) {
  print(unique(df[df$Aufgabe == unique(df$Aufgabe)[i], "Aufgabe"]))
  print(length(unique(df[df$Condition == 1 & df$Group == 1 & df$Aufgabe == unique(df$Aufgabe)[i], "Response"])))
  print(length(unique(df[df$Condition == 1 & df$Group == 2 & df$Aufgabe == unique(df$Aufgabe)[i], "Response"])))
  print(length(unique(df[df$Condition == 1 & df$Group == 3 & df$Aufgabe == unique(df$Aufgabe)[i], "Response"])))
}

# Outliers and log-frequency checks
# These plots help judge whether the frequency distribution is skewed and whether a log transform is appropriate.
boxplot(df$AoA)
boxplot(df$freq)
boxplot(log(df$freq), main = "Log Frequency")

plot(df$AoA, log(df$freq)) # Lower frequency words tend to be acquired later.
plot(df$AoA[log(df$freq) > 2], log(df$freq)[log(df$freq) > 2])

# Linear model for missing frequency values, comparing models with and without outliers.
# The model fit improves when outliers are kept, so the analysis uses the full model.
outliers <- boxplot.stats(log(df$freq))$out # EB: log added to reduce outliers; 108 outliers remain
# Filter out extreme values in the log-frequency distribution and remove rows with missing frequency.
df_clean <- df[!(log(df$freq) %in% outliers) & is.na(df$freq) == F, ] # filter 108 outliers and 712 missing values
nrow(df_clean) # 6080 rows (820 omitted)
length(boxplot.stats(df_clean$freq)$out) # 938 outliers items in raw frequencies
length(boxplot.stats(log(df_clean$freq))$out) # 108 outliers in log frequencies
#summary(model_lm  <- lm(log(freq) ~ AoA + log(nchar(Response)), data = df_clean)) # R2adj = 0.4214

# For the predictive model, nchar(Response) is also log-transformed because it slightly improves model fit.
# For model comparison, the adjusted R-squared is the preferred criterion.
#summary(model_lm_NoOutliers  <- lm(log(freq) ~ AoA + log(nchar(Response)), data = df_clean)) # R2adj = 0.4201
summary(model_lm  <- lm(log(freq) ~ AoA + log(nchar(Response)), data = df)) # R2adj = 0.4341 --> model with outliers is retained

# Save diagnostic plots for the linear model.
for (w in 1:4) {
  png(paste0("results/plots/PredictFreqs", w, ".png"), width = 800, height = 600)
  plot(model_lm, which = w)
  dev.off()
}

hist(log(df$freq))
hist(log(nchar(df$Response))) # mit log auch besser! 


# Predict missing frequency values using AoA and response length.
missing_rows <- df[is.na(df$freq), ]
predicted <- predict(model_lm, newdata = missing_rows)
predicted_exp <- exp(predicted)
df$freq[is.na(df$freq)] <- predicted_exp

# Check that the predicted values are valid and non-negative.
missing_rows$Response # Responses without frequency values
max(df$freq, na.rm = TRUE)
min(predicted_exp, na.rm = TRUE)

# Rank words within groups and tasks.
# Count how often each response appears within each task/group combination.
df <- df %>% add_count(Aufgabe, Group, Response, name = "counts")

# Create a rank for each word within each group and task.
# This is useful for comparing response prominence across age groups.
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

df_ranks <- df_ranks %>%
  ungroup() %>%
  left_join(n_per_aufgabe, by = "Aufgabe")

# Convert ranks into equal-sized bins using ntile().
df_bins <- df_ranks %>%
  group_by(Group, Aufgabe) %>%
  mutate(bin = ntile(rank_raw, unique(N))) %>%
  ungroup()

# Compute the mean frequency and AoA for each rank bin.
averaged <- as.data.frame(df_bins %>%
                            group_by(Aufgabe, Condition, Group, bin) %>%
                            summarise(
                              mean_freq    = mean(freq, na.rm = TRUE),
                              mean_aoa     = mean(AoA, na.rm = TRUE),
                              aggreg_words = paste(Response, collapse = ", "),
                              n_words      = n(),
                              .groups = "drop"
                            ))

# ---- Zipf transformation ----
# Convert mean frequencies into Zipf values.
# This makes the frequency scale more interpretable and comparable across tasks and groups.
childlex_freqs<- as.data.frame(read_excel("data/norms/childlex_0.17.01c.xlsx", sheet = "All"))
n_lemmas <- nrow(childlex_freqs) #164798 types, 
nrow(childlex_freqs)

corpus_tokens_millions <- 7.8          # ohne Punktuation, 10 Mio mit Punktuation (Schroeder et al., 2015)
corpus_types_millions  <- 120000 / 1000000  # word types according to (Schroeder et al., 2015)

N<- corpus_tokens_millions + corpus_types_millions
zipf <- round(log10((averaged$mean_freq + 1) / N) + 3, digits = 2)
averaged["zipf"] <- zipf

max(zipf, na.rm = TRUE)-min(zipf, na.rm = TRUE) # max: 7.13, min:2.11
max(averaged["zipf"], na.rm = TRUE)-min(averaged["zipf"], na.rm = TRUE)
max(averaged["mean_freq"], na.rm = TRUE)-min(averaged["mean_freq"], na.rm = TRUE)
max(averaged["mean_aoa"], na.rm = TRUE)-min(averaged["mean_aoa"], na.rm = TRUE)


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
    diff_zipf_3_1 = zipf_3 - zipf_1,       # Erwachsene - Kinder
    diff_zipf_3_2 = zipf_3 - zipf_2,       # Erwachsene - Jugendliche
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

# ---- Linear models for the lexical task ----
# Fit linear models for the lexical task.
lm_zipf_3_1 <- lm(diff_zipf_3_1 ~ 1 + scale(diff_aoa_3_1) + scale(dif_wordlength_3_1), data = wide_df %>% filter(Condition == 1))
lm_zipf_3_2 <- lm(diff_zipf_3_2 ~ 1 + scale(diff_aoa_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 1))

summary(lm_zipf_3_1)
summary(lm_zipf_3_2)

lm_aoa_3_1 <- lm(diff_aoa_3_1 ~ 1 + scale(diff_zipf_3_1) + scale(dif_wordlength_3_1), data = wide_df %>% filter(Condition == 1))
lm_aoa_3_2 <- lm(diff_aoa_3_2 ~ 1 + scale(diff_zipf_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 1))

summary(lm_aoa_3_1)
summary(lm_aoa_3_2)

# ---- Linear models for the semantic task ----
# Fit the same type of models for the semantic task.
lm_zipf_3_1_sem <- lm(diff_zipf_3_1 ~ 1 + scale(diff_aoa_3_1) + scale(dif_wordlength_3_1), data = wide_df %>% filter(Condition == 2))
lm_zipf_3_2_sem <- lm(diff_zipf_3_2 ~ 1 + scale(diff_aoa_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 2))

summary(lm_zipf_3_1_sem)
summary(lm_zipf_3_2_sem)

lm_aoa_3_1_sem <- lm(diff_aoa_3_1 ~ 1 + scale(diff_zipf_3_1) + scale(dif_wordlength_3_1), data = wide_df %>% filter(Condition == 2))
lm_aoa_3_2_sem <- lm(diff_aoa_3_2 ~ 1 + scale(diff_zipf_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 2))

summary(lm_aoa_3_1_sem)
summary(lm_aoa_3_2_sem)


# ---- Model tables ----
# Tables

models <- list(
  lm_zipf_3_1     = lm_zipf_3_1,
  lm_zipf_3_2     = lm_zipf_3_2,
  lm_aoa_3_1      = lm_aoa_3_1,
  lm_aoa_3_2      = lm_aoa_3_2,
  lm_zipf_3_1_sem = lm_zipf_3_1_sem,
  lm_zipf_3_2_sem = lm_zipf_3_2_sem,
  lm_aoa_3_1_sem  = lm_aoa_3_1_sem,
  lm_aoa_3_2_sem  = lm_aoa_3_2_sem
)

coef_list <- list()
fit_list <- list()

for (name in names(models)) {
  mod <- models[[name]]
  
  coef_list[[name]] <- tidy(mod, conf.int = TRUE) %>%
    mutate(model = name, .before = 1)
  
  fit_list[[name]] <- glance(mod) %>%
    mutate(model = name, .before = 1)
}

coef_table <- bind_rows(coef_list)
fit_table <- bind_rows(fit_list)

write_xlsx(
  list(coefficients = coef_table, fit_stats = fit_table),
  "results/model_summaries.xlsx"
)

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
  "results/mean_zipf_aoa.xlsx"
)


