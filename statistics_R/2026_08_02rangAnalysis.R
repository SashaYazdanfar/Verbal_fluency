#install.packages("readxl") 
#install.packages("ordinal")
#install.packages("see")
#install.packages("performance")
#install.packages("lme4")
#install.packages("tidyr")
#install.packages("tidytable")
# Signifikanz ausgeben (lmer does not produce significance)
#install.packages("lmerTest")
library(dplyr)
library(tidyr)
library(performance)
library(readxl)
library(ordinal)
library(lme4)
library(lmerTest)
library(stringr)
library(tidytable)

set.seed(42)
setwd("~/Studium_withoutBackup/SS_26/Wortflüssigkeit/21.08.2026_data_analysis")
# Df mit AoA und Frequenzen lesen
df = as.data.frame(read_excel("results/2026_08_24_fluencyAoAFreqs_df.xlsx")) # EB: hier as.data.frame() hinzufuegen, dann ist baseR besser anwendbar auf die Daten
colnames(df)
head(df, 20)

# Fehlende Werte in Frequenz und AoA zählen
sum(is.na(df$AoA)) #201/6900
sum(is.na(df$freq)) #712/6900 # now: 98/6900
nrow(df)
unique(df$Aufgabe) #10 --> 8
df$Aufgabe[df$Aufgabe == "SÜßigkeiten"] <- "Süßigkeiten"
df$Aufgabe[df$Aufgabe == "Sport"] <- "Sportarten"

# EB: Anzahl unique Antworten ansehen, separat für semantische und formale Fluency und pro Gruppe
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

# Ausreißer, log-Frequenz berechnen
boxplot(df$AoA)
boxplot(df$freq)
boxplot(log(df$freq), main = "Log Frequency") # EB

plot(df$AoA, log(df$freq)) # je niedriger frequent, desto später erworben. 
plot(df$AoA[log(df$freq) > 2], log(df$freq)[log(df$freq) > 2])

# Lineares Model für fehlende Werte in Frequenz mit und ohne Ausreißer vergleichen
# R-squared ist höher mit Ausreißer als ohne -- > Lineares Model mit Außreiser benutzen
outliers <- boxplot.stats(log(df$freq))$out# EB: log hinzugefuegt --> weniger Outlier
nrow(df_clean) # 49ers 5958 rows (942 omitted )
length(boxplot.stats(df$freq)$out) # 938 Outliers in freqs
length(boxplot.stats(log(df$freq))$out) # 108 Outliers in log freqs
df_clean <- df[!(log(df$freq) %in% outliers) & is.na(df$freq) == F, ] # EB: das Schließt nichts aus ... --> is.na(df$freq) == F hinzufuegen
nrow(df_clean)

# fuer die Modelle habe ich auch die nchar(Response) logarithmiert, das verbessert sie ein wenig. 
# Fuer Model fit besser *Adjusted* R-squared nehmen:  
#summary(model_lm_NoOutliers  <- lm(log(freq) ~ AoA + log(nchar(Response)), data = df_clean)) # R2adj = 0.4201
summary(model_lm  <- lm(log(freq) ~ AoA + log(nchar(Response)), data = df)) # R2adj =   0.4035

# Die Grafiken von lmm(s) speichern
for (w in 1:4) {
  png(paste0("results/plots/PredictFreqs", w, ".png"), width = 800, height = 600)
  plot(model_lm, which = w)
  dev.off()
}

hist(log(df$freq))
hist(log(nchar(df$Response))) # mit log auch besser! 


# Fehlende Werte in Frequenz mit AoA vorhersagen
missing_rows <- df[is.na(df$freq), ]
predicted <- predict(model_lm, newdata = missing_rows)    
predicted_exp <- exp(predicted)
df$freq[is.na(df$freq)] <- predicted_exp 

# Kontrollieren, dass es keine Negative Werte gibt
missing_rows$Response # Antworte ohne Frequenzen
max(df$freq, na.rm = TRUE)
min(predicted_exp, na.rm = TRUE)

# Ränge innerhalb Gruppen und Aufgaben erstellen
# Häufigkeit von Wörter innerhalb der Gruppen berechnen
df <- df %>% add_count(Aufgabe, Group, Response, name = "counts")
# Spalte mit Rängen erstellen für jede Gruppe und Aufgabe
# min/ max/ average = kleinster/ größter/ Durchschnitt der Rängen der gleichrangigen Wörter
df_ranks <- df %>%
  distinct(Group, Aufgabe, Response, counts, .keep_all = TRUE) %>%
  group_by(Group, Aufgabe) %>%
  mutate(rank_raw = rank(-counts, ties.method = "random")) %>%
  ungroup()

# Anzahl der Rängen in Kindergruppe berechnen
# wird als gemeinsames N für drei Gruppen verwendet (ntile)
n_per_aufgabe <- df_ranks %>%
  filter(Group == 1) %>%
  distinct(Aufgabe, Response) %>%
  count(Aufgabe, name = "N")

df_ranks <- df_ranks %>%
  ungroup() %>%
  left_join(n_per_aufgabe, by = "Aufgabe")

# In gleiche Bins umformen mithilfe von ntile
df_bins <- df_ranks %>%
  group_by(Group, Aufgabe) %>%
  mutate(bin = ntile(rank_raw, unique(N))) %>%
  ungroup()

# Mittlere Frequenz und AoA für jeden Rangplatz berechnen
# Außreißer ? 
averaged <- as.data.frame(df_bins %>%
  group_by(Aufgabe, Condition, Group, bin) %>%
  summarise(
    mean_freq    = mean(freq, na.rm = TRUE),
    mean_aoa     = mean(AoA, na.rm = TRUE),
    aggreg_words = paste(Response, collapse = ", "),
    n_words      = n(),
    .groups = "drop"
  ))

# Mittlere Freqzuenzen in Zipf konvertieren
childlex_freqs<- as.data.frame(read_excel("data/norms/childlex_0.17.01c.xlsx", sheet = "All"))
N <- nrow(childlex_freqs)/1000000 + sum(is.na(averaged$mean_freq)/1000000)
zipf <- round(log10((averaged$mean_freq + 1) / N) + 3, digits = 2)
averaged["zipf"] = zipf
max(averaged["zipf"], na.rm = TRUE)-min(averaged["zipf"], na.rm = TRUE)
max(averaged["mean_freq"], na.rm = TRUE)-min(averaged["mean_freq"], na.rm = TRUE)
max(averaged["mean_aoa"], na.rm = TRUE)-min(averaged["mean_aoa"], na.rm = TRUE)


# Long- zu Wide-Format: pro Rang (bin) eine Zeile, Gruppen als separate Spalten
wide_df <- averaged %>%
  select(-mean_freq) %>%
  pivot_wider(
    names_from = Group,
    values_from = c(zipf, mean_aoa, aggreg_words, n_words)
  )

#   Differenz zwischen Jugendlichen und Kinder NICHT behalten
wide_df <- wide_df %>%
  mutate(
    diff_zipf_3_1 = zipf_3 - zipf_1,       # Erwachsene - Kinder
    diff_zipf_3_2 = zipf_3 - zipf_2,       # Erwachsene - Jugendliche
    diff_aoa_3_1  = mean_aoa_3 - mean_aoa_1,
    diff_aoa_3_2  = mean_aoa_3 - mean_aoa_2,
  )




# Die Spalten mit durchschnittlichen Wortlänge und ihrer Differenz erstellen
wide_df = mutate(wide_df, word_length_1  = map_dbl(aggreg_words_1, ~ mean(str_length(str_split(.x, ", ")[[1]]), na.rm = TRUE)))
wide_df = mutate(wide_df, word_length_2  = map_dbl(aggreg_words_2, ~ mean(str_length(str_split(.x, ", ")[[1]]), na.rm = TRUE)))
wide_df = mutate(wide_df, word_length_3  = map_dbl(aggreg_words_3, ~ mean(str_length(str_split(.x, ", ")[[1]]), na.rm = TRUE)))

wide_df <- wide_df %>%
  mutate(
    dif_wordlength_3_1 = word_length_3 - word_length_1,       # Erwachsene - Kinder
    dif_wordlength_3_2 = word_length_3 - word_length_2,       # Erwachsene - Jugendliche
  )

# Lineare Modelle anpassen für lexikalische Aufgabe
lm_zipf_3_1 = lm( diff_zipf_3_1 ~ 1 + scale(diff_aoa_3_1) + scale(dif_wordlength_3_1) , data = wide_df %>% filter(Condition == 1))
lm_zipf_3_2 = lm( diff_zipf_3_2 ~ 1 +  scale(diff_aoa_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 1))


summary(lm_zipf_3_1)
summary(lm_zipf_3_2)

lm_aoa_3_1 = lm( diff_aoa_3_1 ~ 1 + scale(diff_zipf_3_1) + scale(dif_wordlength_3_1), data = wide_df %>% filter(Condition == 1))
lm_aoa_3_2 = lm( diff_aoa_3_2 ~ 1 + scale(diff_zipf_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 1))

summary(lm_aoa_3_1)
summary(lm_aoa_3_2)

# Lineare Modelle anpassen für semantische Aufgabe
lm_zipf_3_1_sem = lm( diff_zipf_3_1 ~ 1 + scale(diff_aoa_3_1) + scale(dif_wordlength_3_1) , data = wide_df %>% filter(Condition == 2))
lm_zipf_3_2_sem = lm( diff_zipf_3_2 ~ 1 + scale(diff_aoa_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 2))

summary(lm_zipf_3_1_sem)
summary(lm_zipf_3_2_sem)

lm_aoa_3_1_sem = lm( diff_aoa_3_1 ~ 1 + scale(diff_zipf_3_1)+ scale(dif_wordlength_3_1), data = wide_df %>% filter(Condition == 2))
lm_aoa_3_2_sem = lm( diff_aoa_3_2 ~ 1 + scale(diff_zipf_3_2) + scale(dif_wordlength_3_2), data = wide_df %>% filter(Condition == 2))

summary(lm_aoa_3_1_sem)
summary(lm_aoa_3_2_sem)


# Tables
# install.packages(c("broom", "purrr", "writexl"))
install.packages("broom")
install.packages("purrr")
install.packages("writexl")

library(broom)
library(purrr)
library(writexl)

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

# Коефіцієнти (ß, CI, SE, t, p) для кожної моделі
coef_table <- imap_dfr(models, function(mod, name) {
  tidy(mod, conf.int = TRUE) %>%
    mutate(model = name, .before = 1)
})

# R², adj. R², тощо, для кожної моделі
fit_table <- imap_dfr(models, function(mod, name) {
  glance(mod) %>%
    mutate(model = name, .before = 1)
})

write_xlsx(
  list(coefficients = coef_table, fit_stats = fit_table),
  "results/plots/model_summaries.xlsx"
)
# install.packages("modelsummary")
library(modelsummary)

modelsummary(
  models,
  output = "results/model_summaries.docx",   # або .html, .xlsx, .csv
  statistic = c("SE = {std.error}", "t = {statistic}", "p = {p.value}"),
  gof_omit = "AIC|BIC|Log.Lik|F"
)
