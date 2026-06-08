# Datei laden
data <- read.csv(file.choose(),
                 sep = ";",            # CSV nutzt ; als Trennzeichen 
                 stringsAsFactors = FALSE)  # Zeichenketten nicht automatisch als Faktor

# Daten anschauen
View(data)      # Tabellenansicht in RStudio
head(data)      # erste 6 Zeilen
str(data)       # Struktur: Spalten & Typen
names(data)     # Spaltennamen anzeigen

# Diese 11 Spalten sind die einzelnen Items der MRS-II Skala
mrs_items <- c(
  "X1..Hot.flushes..sweating",
  "X2..Heart.discomfort",
  "X3..Sleep.problems",
  "X4..Depressive.mood",
  "X5..Irritability",
  "X6..Anxiety",
  "X7..Physical.and.mental.exhaustion",
  "X8..Sexual.problems",
  "X9..Bladder.problems",
  "X10..Dryness.of.vagina",
  "X11..Joint.and.muscular.discomfort"
)

## Alle 11 Items des MRS-II automatisch umcodieren (0-4)

# Schleife, die für jedes der 11 Items ausgeführt wird:
for (nm in mrs_items) {
  
  # Text bereinigen
  x <- tolower(trimws(as.character(data[[nm]])))
  
  # Antworten in Zahlen 0–4 umwandeln
  data[[nm]] <- ifelse(x == "0 (none)", 0,
                       ifelse(x == "1 (mild)", 1,
                              ifelse(x == "2 (moderate)", 2,
                                     ifelse(x == "3 (severe)", 3,
                                            ifelse(x == "4 (very severe)", 4, NA)))))
  
  # Numerisch sicherstellen
  data[[nm]] <- as.numeric(data[[nm]])
}

## Verfügbarkeits-Spalte umcodieren

# Text bereinigen wie oben
x <- tolower(trimws(as.character(data$MRS.II.questionnaire.answers.available.)))

# Direkt überschreiben
data$MRS.II.questionnaire.answers.available. <- ifelse(
  x == "yes", 1,
  ifelse(x == "no", 0, NA)
)

# Numerisch sicherstellen
data$MRS.II.questionnaire.answers.available. <- 
  as.numeric(data$MRS.II.questionnaire.answers.available.)

## MRS-II-Spalten umbenennen:
names(data)[names(data) == "MRS.II.questionnaire.answers.available."] <- "mrsii_available"
names(data)[names(data) == "X1..Hot.flushes..sweating"]              <- "mrsii_1"
names(data)[names(data) == "X2..Heart.discomfort"]                   <- "mrsii_2"
names(data)[names(data) == "X3..Sleep.problems"]                     <- "mrsii_3"
names(data)[names(data) == "X4..Depressive.mood"]                    <- "mrsii_4"
names(data)[names(data) == "X5..Irritability"]                       <- "mrsii_5"
names(data)[names(data) == "X6..Anxiety"]                            <- "mrsii_6"
names(data)[names(data) == "X7..Physical.and.mental.exhaustion"]     <- "mrsii_7"
names(data)[names(data) == "X8..Sexual.problems"]                    <- "mrsii_8"
names(data)[names(data) == "X9..Bladder.problems"]                   <- "mrsii_9"
names(data)[names(data) == "X10..Dryness.of.vagina"]                 <- "mrsii_10"
names(data)[names(data) == "X11..Joint.and.muscular.discomfort"]     <- "mrsii_11"

## Scores berechnen und neue Spalten machen
# Vegetativ (Items 1, 2, 3, 11)
data$mrsii_vegetative <- data$mrsii_1 + data$mrsii_2 + data$mrsii_3 + data$mrsii_11
# Psychologisch (Items 4, 5, 6, 7)
data$mrsii_psychological <- data$mrsii_4 + data$mrsii_5 + data$mrsii_6 + data$mrsii_7
# Urogenital (Items 8, 9, 10)
data$mrsii_urogenital <- data$mrsii_8 + data$mrsii_9 + data$mrsii_10
# Total Score
data$mrsii_total <- data$mrsii_vegetative + data$mrsii_psychological + data$mrsii_urogenital

# zeige alle Werte in Spalte Event.Name
unique(data$Event.Name)

# Filtere nach Baseline Visit - Mth 0
data <- data[data$Event.Name == "Baseline Visit - Mth 0", ]

# Nur diese Spalten im Datensatz behalten
data <- data[c(
  "Record.ID",
  "Age.at.baseline..years.",
  "Reproductive.age",
  "SERM",
  "mrsii_available",
  "mrsii_1", "mrsii_2", "mrsii_3", "mrsii_4", "mrsii_5",
  "mrsii_6", "mrsii_7", "mrsii_8", "mrsii_9", "mrsii_10", "mrsii_11",
  "mrsii_vegetative",
  "mrsii_psychological",
  "mrsii_urogenital",
  "mrsii_total"
)]

# Nur Fälle behalten, bei denen mrsii_available = 1
data <- data[data$mrsii_available == 1, ]

# Spalten "Reproductive.age" und "SERM" löschen, da unvollständig
data$Reproductive.age <- NULL
data$SERM <- NULL

#Anzahl Zeilen?
nrow(data)

# Nur vollständige Zeilen für mrsii_1–mrsii_11 behalten
# relevanten Spalten definieren
mrs_items <- c(
  "mrsii_1", "mrsii_2", "mrsii_3", "mrsii_4", "mrsii_5",
  "mrsii_6", "mrsii_7", "mrsii_8", "mrsii_9", "mrsii_10", "mrsii_11"
)

# Nur Zeilen behalten, bei denen KEIN NA in diesen Spalten vorkommt
data <- data[complete.cases(data[, mrs_items]), ]

# Kontrolle: Gibt es jetzt noch NAs?
colSums(is.na(data[, mrs_items]))

#Datensatz als CSV speichern
write.csv(data, file = file.choose(new = TRUE), row.names = FALSE)
