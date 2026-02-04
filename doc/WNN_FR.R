## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(OlivierBorot)
library(forecast)
library(ggplot2)

## ----data-exploration---------------------------------------------------------
# Charger le jeu de données AirPassengers
data(AirPassengers)

# Informations de base
cat("Jeu de données : AirPassengers\n")
cat("Longueur :", length(AirPassengers), "observations\n")
cat("Fréquence :", frequency(AirPassengers), "(mensuelle)\n")
cat("Période :", paste(start(AirPassengers), collapse = "-"), "à",
    paste(end(AirPassengers), collapse = "-"), "\n")

# Visualiser les données
autoplot(AirPassengers) +
  ggtitle("Passagers Aériens Internationaux Mensuels (1949-1960)") +
  xlab("Année") +
  ylab("Passagers (milliers)")

## ----train-test-split---------------------------------------------------------
# Séparer les données
train_end <- c(1959, 12)
train <- window(AirPassengers, end = train_end)
test <- window(AirPassengers, start = c(1960, 1))

cat("Ensemble d'entraînement :", length(train), "observations\n")
cat("Ensemble de test :", length(test), "observations\n")

# Visualiser la séparation
autoplot(train) +
  autolayer(test, series = "Test") +
  ggtitle("Séparation Train-Test") +
  xlab("Année") +
  ylab("Passagers")

## ----wnn-model----------------------------------------------------------------
# Créer le modèle WNN
# - horizon = 12 (prédire 12 mois)
# - window = 24 (utiliser 24 mois pour la correspondance de motifs)
# - k = 5 (utiliser 5 plus proches voisins)
wnn_model <- WNN$new(horizon = 12, window = 24, k = 5)

# Afficher les paramètres du modèle
print(wnn_model)

# Entraîner et prédire
wnn_forecast <- wnn_model$fit_predict(train)

# Afficher le résumé complet
wnn_model$summary()

## ----visualization------------------------------------------------------------
# Comparer prévision et valeurs réelles
autoplot(train) +
  autolayer(test, series = "Réel") +
  autolayer(wnn_forecast, series = "Prévision WNN") +
  ggtitle("Prévision WNN vs Valeurs Réelles") +
  xlab("Année") +
  ylab("Passagers") +
  guides(colour = guide_legend(title = "Série"))

## ----accuracy-----------------------------------------------------------------
# Calculer les métriques de précision
errors <- test - wnn_forecast

# Erreur Absolue Moyenne (MAE)
mae <- mean(abs(errors))

# Racine de l'Erreur Quadratique Moyenne (RMSE)
rmse <- sqrt(mean(errors^2))

# Erreur Absolue Moyenne en Pourcentage (MAPE)
mape <- mean(abs(errors / test)) * 100

cat("Métriques de Précision des Prévisions :\n")
cat("---------------------------------------\n")
cat("MAE  :", round(mae, 2), "\n")
cat("RMSE :", round(rmse, 2), "\n")
cat("MAPE :", round(mape, 2), "%\n")

## ----comparison---------------------------------------------------------------
# Modèle ETS
ets_model <- ets(train)
ets_forecast <- forecast(ets_model, h = 12)

# Modèle ARIMA
arima_model <- auto.arima(train)
arima_forecast <- forecast(arima_model, h = 12)

# Calculer le RMSE pour chaque méthode
rmse_wnn <- sqrt(mean((test - wnn_forecast)^2))
rmse_ets <- sqrt(mean((test - ets_forecast$mean)^2))
rmse_arima <- sqrt(mean((test - arima_forecast$mean)^2))

# Créer le tableau de comparaison
comparison <- data.frame(
  Methode = c("WNN (k=5, w=24)", "ETS", "ARIMA"),
  RMSE = round(c(rmse_wnn, rmse_ets, rmse_arima), 2)
)

print(comparison)

## ----comparison-plot----------------------------------------------------------
# Comparaison visuelle
autoplot(train) +
  autolayer(test, series = "Réel") +
  autolayer(wnn_forecast, series = "WNN") +
  autolayer(ets_forecast$mean, series = "ETS") +
  autolayer(arima_forecast$mean, series = "ARIMA") +
  ggtitle("Comparaison des Méthodes de Prévision") +
  xlab("Année") +
  ylab("Passagers") +
  guides(colour = guide_legend(title = "Méthode"))

## ----hyperparameter-k---------------------------------------------------------
k_values <- c(1, 3, 5, 7, 10)
results <- data.frame(k = integer(), RMSE = numeric())

for (k in k_values) {
  model <- WNN$new(horizon = 12, window = 24, k = k)
  pred <- model$fit_predict(train)
  rmse <- sqrt(mean((test - pred)^2))
  results <- rbind(results, data.frame(k = k, RMSE = rmse))
}

print(results)

ggplot(results, aes(x = k, y = RMSE)) +
  geom_line() +
  geom_point(size = 3) +
  ggtitle("Effet du Nombre de Voisins (k) sur le RMSE") +
  xlab("Nombre de Voisins (k)") +
  ylab("RMSE")

## ----hyperparameter-w---------------------------------------------------------
window_values <- c(12, 18, 24, 36, 48)
results_w <- data.frame(window = integer(), RMSE = numeric())

for (w in window_values) {
  model <- WNN$new(horizon = 12, window = w, k = 5)
  pred <- model$fit_predict(train)
  rmse <- sqrt(mean((test - pred)^2))
  results_w <- rbind(results_w, data.frame(window = w, RMSE = rmse))
}

print(results_w)

ggplot(results_w, aes(x = window, y = RMSE)) +
  geom_line() +
  geom_point(size = 3) +
  ggtitle("Effet de la Taille de Fenêtre (w) sur le RMSE") +
  xlab("Taille de Fenêtre (w)") +
  ylab("RMSE")

