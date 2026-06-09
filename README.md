# Master Thesis – Adaptive Classification of Menopausal Symptoms

This repository contains the R scripts used for my master's thesis:
**"Entscheidungsorientierte adaptive Itemselektion zur Klassifikation menopausaler Symptomatik auf Basis der MRS-II"**

## Project Overview
The project investigates decision-oriented adaptive item selection for the classification of menopausal symptom severity based on the Menopause Rating Scale II (MRS-II). The performance of adaptive item selection is evaluated and compared with fixed item selections derived from the MRS-II total scale and subscales.

## Repository Structure

| File | Description |
|--------|-------------|
| `01_prepareData.R` | Data preparation and preprocessing |
| `02_fitModels.R` | Model fitting and calibration |
| `03_predAndPerf.R` | Adaptive predictions and performance evaluation |
| `03_predAndPerf_fixedSelections.R` | Evaluation of fixed item selections |
| `Abbildungen.R` | Generation of figures and plots |
| `DataManagement_Cimbolic.R` | Data management and dataset preparation |
| `Stichprobenbeschreibung.R` | Sample description and descriptive statistics |
| `dedaptiveExtended.R` | Core functions of the dedaptive framework used in the analyses   |

## Software
Analyses were conducted in **R** using the **dedaptive** framework and related statistical packages.

The adaptive testing procedures are based on existing R scripts provided by Patric Wyss. These scripts served as the methodological foundation for the analyses and were adapted for the present application to the MRS-II.

## Author
Jennifer Pernusch
Mater's Thesis
Fachhochschule Nordwestschweiz, 2026
