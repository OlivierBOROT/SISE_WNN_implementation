# OlivierBorot

**R Package for Time Series Forecasting using Weighted Nearest Neighbors (WNN)**

> **[Version française disponible ci-dessous](#En-Français)**

---

## Table of Contents

- [Context](#context)
- [Installation](#installation)
- [Quick Example](#quick-example)
- [Algorithm](#algorithm)
- [Vignettes](#vignettes)
- [License](#license)
- [Reference](#reference)
- [Version Française](#olivierborot-français)

---

## Context

This package was developed as part of the **Master 2 SISE** (Statistique et Informatique pour la Science des donnéEs) program at Université Lumière Lyon 2, for the **Time Series** course.

The objective is to implement the WNN algorithm as described in Talavera-Llames et al. (2016) in a fully functional R package.

---

## Installation

### Prerequisites

```r
install.packages("R6")
```

### From GitHub

```r
devtools::install_github("OlivierBOROT/SISE_WNN_implementation")
```

### From Source

```r
devtools::install("path/to/OlivierBorot")
```

### From Built Package

```bash
R CMD build OlivierBorot
R CMD INSTALL OlivierBorot_1.0.0.tar.gz
```

---

## Quick Example

```r
library(OlivierBorot)

# Load data
data(AirPassengers)
train <- window(AirPassengers, end = c(1959, 12))

# Create and fit model
model <- WNN$new(horizon = 12, window = 24, k = 5)
forecast <- model$fit_predict(train)

# View results
print(forecast)
model$summary()
```

---

## Algorithm

The WNN algorithm predicts future values by finding similar patterns in historical data.

#### Step 1: Reference Window

Extract the last `w` observations as the reference pattern:

$$CC_i = [c_{i-w+1}, c_{i-w+2}, \ldots, c_i]$$

#### Step 2: Distance Calculation

For each historical window $CC_j$, compute the Euclidean distance:

$$dist(i,j) = \sqrt{\sum_{l=1}^{w}(CC_i[l] - CC_j[l])^2}$$

#### Step 3: Weighted Forecast

Select the `k` nearest neighbors and compute the weighted average of their subsequent values:

$$\hat{C}_i = \frac{\sum_{j=1}^{k} \alpha_j \cdot C_{q_j}}{\sum_{j=1}^{k} \alpha_j}$$

Where the weights are defined as:

$$\alpha_j = \frac{1}{dist(CC_{q_j}, CC_i)^2}$$

---

## Vignettes

Two vignettes are available with detailed examples and comparisons:

```r
vignette("WNN_EN", package = "OlivierBorot")  # English
vignette("WNN_FR", package = "OlivierBorot")  # French
```

---

## License

This project is licensed under the **MIT License**. See the [LICENSE](LICENSE) file for details.

---

## Reference

Talavera-Llames, R.L., Pérez-Chacón, R., Martínez-Ballesteros, M., Troncoso, A., Martínez-Álvarez, F. (2016). **A Nearest Neighbours-Based Algorithm for Big Time Series Data Forecasting**. In: Hybrid Artificial Intelligent Systems. HAIS 2016. Lecture Notes in Computer Science, vol 9648. Springer, Cham.

---
  
# En Français
## Contexte

Ce package a été développé dans le cadre du **Master 2 SISE** (Statistique et Informatique pour la Science des donnéEs) à l'Université Lumière Lyon 2, pour le cours de **Séries Temporelles**.

L'objectif est d'implémenter l'algorithme WNN tel que décrit dans Talavera-Llames et al. (2016) sous forme d'un package R fonctionnel.

---

## Installation

### Prérequis

```r
install.packages("R6")
```

### Depuis GitHub

```r
devtools::install_github("OlivierBOROT/SISE_WNN_implementation")
```

### Depuis le Code Source

```r
devtools::install("chemin/vers/OlivierBorot")
```

### Depuis le Package Compilé

```bash
R CMD build OlivierBorot
R CMD INSTALL OlivierBorot_1.0.0.tar.gz
```

---

## Exemple Rapide

```r
library(OlivierBorot)

# Charger les données
data(AirPassengers)
train <- window(AirPassengers, end = c(1959, 12))

# Créer et entraîner le modèle
model <- WNN$new(horizon = 12, window = 24, k = 5)
forecast <- model$fit_predict(train)

# Afficher les résultats
print(forecast)
model$summary()
```

---

## Algorithme

L'algorithme WNN prédit les valeurs futures en recherchant des motifs similaires dans les données historiques.

#### Étape 1 : Fenêtre de Référence

Extraire les `w` dernières observations comme motif de référence :

$$CC_i = [c_{i-w+1}, c_{i-w+2}, \ldots, c_i]$$

#### Étape 2 : Calcul des Distances

Pour chaque fenêtre historique $CC_j$, calculer la distance euclidienne :

$$dist(i,j) = \sqrt{\sum_{l=1}^{w}(CC_i[l] - CC_j[l])^2}$$

#### Étape 3 : Prévision Pondérée

Sélectionner les `k` plus proches voisins et calculer la moyenne pondérée de leurs valeurs suivantes :

$$\hat{C}_i = \frac{\sum_{j=1}^{k} \alpha_j \cdot C_{q_j}}{\sum_{j=1}^{k} \alpha_j}$$

Où les poids sont définis par :

$$\alpha_j = \frac{1}{dist(CC_{q_j}, CC_i)^2}$$

---

## Vignettes

Deux vignettes sont disponibles avec des exemples détaillés et des comparaisons :

```r
vignette("WNN_EN", package = "OlivierBorot")  # Anglais
vignette("WNN_FR", package = "OlivierBorot")  # Français
```

---

## Licence

Ce projet est sous licence **MIT**. Voir le fichier [LICENSE](LICENSE) pour plus de détails.

---

## Référence

Talavera-Llames, R.L., Pérez-Chacón, R., Martínez-Ballesteros, M., Troncoso, A., Martínez-Álvarez, F. (2016). **A Nearest Neighbours-Based Algorithm for Big Time Series Data Forecasting**. In: Hybrid Artificial Intelligent Systems. HAIS 2016. Lecture Notes in Computer Science, vol 9648. Springer, Cham.
