## Abbildungen
#Vorbereitung
getwd() # Kontrolle
setwd("/Users/jennypernusch/Library/Mobile Documents/com~apple~CloudDocs/Studium/MSc/00_Masterarbeit/Thesis_Pernusch")

## Abbildung F2: Entscheidungsgenauigkeit bei reduzierter Itemzahl ####
# Mean Accuracy vs. mean number of selected items
# inkl. fixe Itemselektionen

path_output <- "./Output/"

if (!dir.exists(path_output)) {
  stop("Output-Ordner nicht gefunden: ", path_output)
}

# Daten einlesen
perf_select <- read.csv(
  file.path(path_output, "perfSelect_03_03_26_nTheta1000_nItems_10.csv"),
  stringsAsFactors = FALSE
)

perf_fixed <- read.csv(
  file.path(path_output, "perfFixed_03_03_26_nTheta1000_nItems_10.csv"),
  stringsAsFactors = FALSE
)

# Adaptive Ergebnisse vorbereiten
perf_f1 <- perf_select[, c("cM", "nItems", "acc_mean")]
perf_f1 <- perf_f1[!duplicated(perf_f1), ]
perf_f1 <- perf_f1[order(perf_f1$nItems), ]

# Fixe Verfahren vorbereiten
perf_fixed_f1 <- perf_fixed[, c("type", "nItems", "acc_mean")]
perf_fixed_f1 <- perf_fixed_f1[!duplicated(perf_fixed_f1), ]

fixed_types <- c("full", "somato", "psych", "uro")

labels <- c(
  full = "Vollversion MRS-II",
  somato = "Somato-vegetative Subskala",
  psych = "Psychologische Subskala",
  uro = "Urogenitale Subskala"
)

colors <- c(
  adaptiv = "black",
  full = "#D55E00",
  somato = "#0072B2",
  psych = "#009E73",
  uro = "#CC79A7"
)

point_types <- c(
  full = 17,
  somato = 15,
  psych = 18,
  uro = 8
)

# Grafik speichern
png(
  filename = file.path(path_output, "Figure_F2_accuracy_items_adaptive_fixed.png"),
  width = 2400,
  height = 1400,
  res = 220
)

# rechts mehr Platz für Legende, aber Linien NICHT ausserhalb zeichnen
par(mar = c(5, 5, 3, 10), xpd = FALSE)

# Leerer Plot zuerst
plot(
  perf_f1$nItems,
  perf_f1$acc_mean,
  type = "n",
  xaxt = "n",
  yaxt = "n",
  xlim = c(0, 11.5),
  ylim = c(0, 1.02),
  xlab = "Anzahl ausgewählter Items",
  ylab = "Mittlere Genauigkeit",
  main = ""
)

# Achsen
axis(1, at = 0:11)
axis(2, at = seq(0, 1, by = 0.2), las = 1)

# Grid zuerst, damit Daten darüber liegen
grid()

# Horizontale Referenzlinien fixer Verfahren NUR innerhalb des Plotbereichs
for (typ in fixed_types) {
  
  dat_i <- perf_fixed_f1[perf_fixed_f1$type == typ, ]
  
  segments(
    x0 = 0,
    x1 = 11.5,
    y0 = dat_i$acc_mean,
    y1 = dat_i$acc_mean,
    col = colors[typ],
    lwd = 2,
    lty = 2
  )
}

# Adaptive Kurve darüber zeichnen
lines(
  perf_f1$nItems,
  perf_f1$acc_mean,
  type = "b",
  pch = 16,
  lwd = 3,
  col = colors["adaptiv"]
)

# Fixe Itemselektionen als Punkte ergänzen
for (typ in fixed_types) {
  
  dat_i <- perf_fixed_f1[perf_fixed_f1$type == typ, ]
  
  points(
    dat_i$nItems,
    dat_i$acc_mean,
    pch = point_types[typ],
    col = colors[typ],
    cex = 1.8,
    lwd = 2
  )
}

# Vertikale Linie für Vollversion NUR im Plotbereich
segments(
  x0 = 11,
  x1 = 11,
  y0 = 0,
  y1 = 1.02,
  col = colors["full"],
  lwd = 2,
  lty = 3
)

box()

# Legende ausserhalb erlauben
par(xpd = TRUE)

legend(
  "bottomright",
  inset = c(0.18, 0),
  legend = c("Adaptive Itemselektion", labels[fixed_types]),
  col = c(colors["adaptiv"], colors[fixed_types]),
  pch = c(16, point_types[fixed_types]),
  lwd = c(3, rep(2, length(fixed_types))),
  lty = c(1, rep(2, length(fixed_types))),
  bty = "n",
  cex = 0.9
)

dev.off()


## Abbildung F1: Vergleich Gesamtkosten adaptive vs. fixe Itemselektionen a) und b) ####
## (a) Gesamtverlauf und (b) Detailansicht niedriger Messkosten

path_output <- "./Output/"

if (!dir.exists(path_output)) {
  stop("Output-Ordner nicht gefunden: ", path_output)
}

# Daten einlesen
perf_select <- read.csv(
  file.path(path_output, "perfSelect_03_03_26_nTheta1000_nItems_10.csv"),
  stringsAsFactors = FALSE
)

perf_fixed <- read.csv(
  file.path(path_output, "perfFixed_03_03_26_nTheta1000_nItems_10.csv"),
  stringsAsFactors = FALSE
)

# Adaptive Ergebnisse vorbereiten
perf_select_f2 <- perf_select[, c("cM", "totCost")]
perf_select_f2 <- perf_select_f2[!duplicated(perf_select_f2$cM), ]
perf_select_f2 <- perf_select_f2[order(perf_select_f2$cM), ]

# Fixe Verfahren vorbereiten
perf_fixed_f2 <- perf_fixed[, c("type", "cM", "totCost")]
perf_fixed_f2 <- perf_fixed_f2[order(perf_fixed_f2$type, perf_fixed_f2$cM), ]

# Verfahren, Labels, Farben, Linien und Symbole
fixed_types <- c("full", "somato", "psych", "uro")

labels <- c(
  full = "Vollversion MRS-II",
  somato = "Somato-vegetative Subskala",
  psych = "Psychologische Subskala",
  uro = "Urogenitale Subskala"
)

colors <- c(
  adaptiv = "black",
  full = "#D55E00",
  somato = "#0072B2",
  psych = "#009E73",
  uro = "#CC79A7"
)

line_types <- c(
  full = 1,
  somato = 2,
  psych = 3,
  uro = 4
)

point_types <- c(
  full = 17,
  somato = 15,
  psych = 18,
  uro = 8
)

# Hilfsfunktion für beide Panels
plot_total_cost <- function(xlim, ylim, x_ticks, panel_label, show_legend = FALSE) {
  
  plot(
    perf_select_f2$cM,
    perf_select_f2$totCost,
    type = "n",
    xlim = xlim,
    ylim = ylim,
    xaxt = "n",
    xlab = "Messkosten pro Item",
    ylab = "Mittlere Gesamtkosten",
    main = panel_label
  )
  
  axis(1, at = x_ticks, labels = sprintf("%.3f", x_ticks))
  grid()
  
  # Adaptive Itemselektion
  lines(
    perf_select_f2$cM,
    perf_select_f2$totCost,
    type = "o",
    lwd = 3,
    pch = 16,
    col = colors["adaptiv"]
  )
  
  # Fixe Verfahren
  for (typ in fixed_types) {
    
    dat_i <- perf_fixed_f2[perf_fixed_f2$type == typ, ]
    
    lines(
      dat_i$cM,
      dat_i$totCost,
      type = "o",
      lwd = 2,
      lty = line_types[typ],
      pch = point_types[typ],
      col = colors[typ]
    )
  }
  
  if (show_legend) {
    legend(
      "topleft",
      legend = c("Adaptive Itemselektion", labels[fixed_types]),
      col = c(colors["adaptiv"], colors[fixed_types]),
      lwd = c(3, rep(2, length(fixed_types))),
      lty = c(1, line_types[fixed_types]),
      pch = c(16, point_types[fixed_types]),
      bty = "n",
      cex = 0.8
    )
  }
  
  box()
}

# Achsenbereiche
ylim_max <- max(
  perf_select_f2$totCost,
  perf_fixed_f2$totCost,
  na.rm = TRUE
)

# Grafik speichern
png(
  filename = file.path(path_output, "Figure_F1_total_cost_adaptive_fixed_panels.png"),
  width = 3000,
  height = 1400,
  res = 220
)

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2), xpd = FALSE)

# (a) Gesamtverlauf
plot_total_cost(
  xlim = c(0, 0.12),
  ylim = c(0, ylim_max),
  x_ticks = seq(0, 0.12, by = 0.02),
  panel_label = "(a) Gesamtverlauf",
  show_legend = TRUE
)

# (b) Detailansicht niedriger Messkosten
plot_total_cost(
  xlim = c(0, 0.01),
  ylim = c(0, 0.26),
  x_ticks = seq(0, 0.01, by = 0.001),
  panel_label = "(b) Detailansicht niedriger Messkosten",
  show_legend = FALSE
)

dev.off()




## Abbildung S1: Sensitivität und Spezifität in Abhängigkeit der Itemanzahl ####
## (a) Sensitivität und (b) Spezifität

path_output <- "./Output/"

if (!dir.exists(path_output)) {
  stop("Output-Ordner nicht gefunden: ", path_output)
}

# Daten einlesen
perf_select <- read.csv(
  file.path(path_output, "perfSelect_03_03_26_nTheta1000_nItems_10.csv"),
  stringsAsFactors = FALSE
)

perf_fixed <- read.csv(
  file.path(path_output, "perfFixed_03_03_26_nTheta1000_nItems_10.csv"),
  stringsAsFactors = FALSE
)

# Benötigte Spalten prüfen
needed_select <- c("nItems", "sens_mean", "spec_mean")
needed_fixed <- c("type", "nItems", "sens_mean", "spec_mean")

missing_select <- setdiff(needed_select, names(perf_select))
missing_fixed <- setdiff(needed_fixed, names(perf_fixed))

if (length(missing_select) > 0) {
  stop("Folgende Spalten fehlen in perf_select: ",
       paste(missing_select, collapse = ", "))
}

if (length(missing_fixed) > 0) {
  stop("Folgende Spalten fehlen in perf_fixed: ",
       paste(missing_fixed, collapse = ", "))
}

# Adaptive Ergebnisse vorbereiten
perf_select_supp <- perf_select[, c("nItems", "sens_mean", "spec_mean")]
perf_select_supp <- perf_select_supp[!duplicated(perf_select_supp$nItems), ]
perf_select_supp <- perf_select_supp[order(perf_select_supp$nItems), ]

# Fixe Verfahren vorbereiten
fixed_types <- c("full", "somato", "psych", "uro")

perf_fixed_supp <- perf_fixed[
  perf_fixed$type %in% fixed_types,
  c("type", "nItems", "sens_mean", "spec_mean")
]

# Pro Verfahren nur eine Zeile behalten
perf_fixed_supp <- perf_fixed_supp[!duplicated(perf_fixed_supp$type), ]

# Labels, Farben und Symbole
labels <- c(
  full = "Vollversion MRS-II",
  somato = "Somato-vegetative Subskala",
  psych = "Psychologische Subskala",
  uro = "Urogenitale Subskala"
)

colors <- c(
  adaptiv = "black",
  full = "#D55E00",
  somato = "#0072B2",
  psych = "#009E73",
  uro = "#CC79A7"
)

point_types <- c(
  full = 17,
  somato = 15,
  psych = 18,
  uro = 8
)

# Hilfsfunktion für beide Panels
plot_sens_spec_panel <- function(metric,
                                 ylab,
                                 panel_label,
                                 show_legend = FALSE) {
  
  plot(
    perf_select_supp$nItems,
    perf_select_supp[[metric]],
    type = "n",
    xlim = c(0, 11),
    ylim = c(0.5, 1.02),
    xaxt = "n",
    xlab = "Mittlere Anzahl Items",
    ylab = ylab,
    main = panel_label
  )
  
  axis(1, at = 0:11)
  grid()
  
  # Adaptive Itemselektion
  lines(
    perf_select_supp$nItems,
    perf_select_supp[[metric]],
    type = "o",
    lwd = 3,
    pch = 16,
    col = colors["adaptiv"]
  )
  
  # Fixe Verfahren als Referenzpunkte
  for (typ in fixed_types) {
    
    dat_i <- perf_fixed_supp[perf_fixed_supp$type == typ, ]
    
    points(
      dat_i$nItems,
      dat_i[[metric]],
      pch = point_types[typ],
      col = colors[typ],
      cex = 1.4,
      lwd = 2
    )
  }
  
  if (show_legend) {
    
    legend(
      "bottomright",
      legend = c("Adaptive Itemselektion", labels[fixed_types]),
      col = c(colors["adaptiv"], colors[fixed_types]),
      lwd = c(3, rep(NA, length(fixed_types))),
      lty = c(1, rep(NA, length(fixed_types))),
      pch = c(16, point_types[fixed_types]),
      pt.cex = c(1, rep(1.2, length(fixed_types))),
      bty = "n",
      cex = 0.65
    )
  }
  
  box()
}

# Grafik speichern
png(
  filename = file.path(
    path_output,
    "Figure_S1_sens_spec_adaptive_fixed_panels.png"
  ),
  width = 3000,
  height = 1400,
  res = 220
)

par(
  mfrow = c(1, 2),
  mar = c(5, 5, 4, 2),
  xpd = FALSE
)

# Panel Sensitivität
plot_sens_spec_panel(
  metric = "sens_mean",
  ylab = "Mittlere Sensitivität",
  panel_label = "(a) Sensitivität",
  show_legend = FALSE
)

# Panel Spezifität
plot_sens_spec_panel(
  metric = "spec_mean",
  ylab = "Mittlere Spezifität",
  panel_label = "(b) Spezifität",
  show_legend = TRUE
)

dev.off()

## Abbildung S2: Accuracy pro Entscheidungsvariable ####

path_output <- "./Output/"

if (!dir.exists(path_output)) {
  stop("Output-Ordner nicht gefunden: ", path_output)
}

# Daten einlesen
perf_select <- read.csv(
  file.path(path_output, "perfSelect_03_03_26_nTheta1000_nItems_10.csv"),
  stringsAsFactors = FALSE
)

perf_fixed <- read.csv(
  file.path(path_output, "perfFixed_03_03_26_nTheta1000_nItems_10.csv"),
  stringsAsFactors = FALSE
)

# Benötigte Spalten prüfen
needed_select <- c("nItems", "acc_1", "acc_2", "acc_3", "acc_4")
needed_fixed <- c("type", "nItems", "acc_1", "acc_2", "acc_3", "acc_4")

missing_select <- setdiff(needed_select, names(perf_select))
missing_fixed <- setdiff(needed_fixed, names(perf_fixed))

if (length(missing_select) > 0) {
  stop("Folgende Spalten fehlen in perf_select: ",
       paste(missing_select, collapse = ", "))
}

if (length(missing_fixed) > 0) {
  stop("Folgende Spalten fehlen in perf_fixed: ",
       paste(missing_fixed, collapse = ", "))
}

# Adaptive Ergebnisse vorbereiten
perf_select_acc <- perf_select[, c("nItems", "acc_1", "acc_2", "acc_3", "acc_4")]
perf_select_acc <- perf_select_acc[!duplicated(perf_select_acc$nItems), ]
perf_select_acc <- perf_select_acc[order(perf_select_acc$nItems), ]

# Fixe Verfahren vorbereiten
fixed_types <- c("full", "somato", "psych", "uro")

perf_fixed_acc <- perf_fixed[
  perf_fixed$type %in% fixed_types,
  c("type", "nItems", "acc_1", "acc_2", "acc_3", "acc_4")
]

# Pro Verfahren nur eine Zeile behalten
perf_fixed_acc <- perf_fixed_acc[!duplicated(perf_fixed_acc$type), ]

# Panel-Beschriftungen
decision_labels <- c(
  acc_1 = "(a) Gesamtscore",
  acc_2 = "(b) Somato-vegetativ",
  acc_3 = "(c) Psychologisch",
  acc_4 = "(d) Urogenital"
)

# Legendenlabels
fixed_labels <- c(
  full = "Vollversion MRS-II",
  somato = "Somato-vegetative Subskala",
  psych = "Psychologische Subskala",
  uro = "Urogenitale Subskala"
)

# Farben und Symbole
colors <- c(
  adaptiv = "black",
  full = "#D55E00",
  somato = "#0072B2",
  psych = "#009E73",
  uro = "#CC79A7"
)

point_types <- c(
  full = 17,
  somato = 15,
  psych = 18,
  uro = 8
)

# Hilfsfunktion für Panels
plot_accuracy_panel <- function(metric, panel_label, show_legend = FALSE) {
  
  plot(
    perf_select_acc$nItems,
    perf_select_acc[[metric]],
    type = "n",
    xlim = c(0, 11),
    ylim = c(0.5, 1.02),
    xaxt = "n",
    xlab = "Mittlere Anzahl Items",
    ylab = "Accuracy",
    main = panel_label
  )
  
  axis(1, at = 0:11)
  grid()
  
  # Adaptive Itemselektion
  lines(
    perf_select_acc$nItems,
    perf_select_acc[[metric]],
    type = "o",
    lwd = 3,
    pch = 16,
    col = colors["adaptiv"]
  )
  
  # Fixe Verfahren als Referenzpunkte
  for (typ in fixed_types) {
    
    dat_i <- perf_fixed_acc[perf_fixed_acc$type == typ, ]
    
    points(
      dat_i$nItems,
      dat_i[[metric]],
      pch = point_types[typ],
      col = colors[typ],
      cex = 1.4,
      lwd = 2
    )
  }
  
  if (show_legend) {
    legend(
      "bottomright",
      legend = c("Adaptive Itemselektion", fixed_labels[fixed_types]),
      col = c(colors["adaptiv"], colors[fixed_types]),
      lwd = c(3, rep(NA, length(fixed_types))),
      lty = c(1, rep(NA, length(fixed_types))),
      pch = c(16, point_types[fixed_types]),
      pt.cex = c(1, rep(1.2, length(fixed_types))),
      bty = "n",
      cex = 0.65
    )
  }
  
  box()
}

# Grafik speichern
png(
  filename = file.path(path_output, "Figure_S2_accuracy_by_decision_panels.png"),
  width = 3000,
  height = 2200,
  res = 220
)

par(mfrow = c(2, 2), mar = c(5, 5, 4, 2), xpd = FALSE)

plot_accuracy_panel("acc_1", decision_labels["acc_1"], show_legend = FALSE)
plot_accuracy_panel("acc_2", decision_labels["acc_2"], show_legend = FALSE)
plot_accuracy_panel("acc_3", decision_labels["acc_3"], show_legend = FALSE)
plot_accuracy_panel("acc_4", decision_labels["acc_4"], show_legend = TRUE)

dev.off()
