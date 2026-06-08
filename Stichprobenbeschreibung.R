# Stichprobenbeschreibung für Ergebnisteil

# load MRS-II data
data <- read.csv(
  file.choose(),
  sep = ",",
  stringsAsFactors = FALSE
)

# 1. Stichprobengrösse ####
nrow(data)

# 2. Alter (deskriptive Kennwerte) ####
mean(data$Age.at.baseline..years., na.rm = TRUE)
sd(data$Age.at.baseline..years., na.rm = TRUE)
min(data$Age.at.baseline..years., na.rm = TRUE)
max(data$Age.at.baseline..years., na.rm = TRUE)

# 3. MRS-II Scores (deskriptiv) ####
# Zusammenfassung
summary(data$score_somato)
summary(data$score_psych)
summary(data$score_uro)
summary(data$score_total)

# Standardabweichungen
sd(data$score_somato, na.rm = TRUE)
sd(data$score_psych, na.rm = TRUE)
sd(data$score_uro, na.rm = TRUE)
sd(data$score_total, na.rm = TRUE)

# 4. Dichotome Entscheidungsvariablen ####
table(data$dec_somato)
table(data$dec_psych)
table(data$dec_uro)
table(data$dec_total)
