### Coelbe Wetterstation - wichtige Parameter extrahieren
library(dplyr)
data <- read.table("C:/Users/Lena/Documents/Uni_Marburg/WS_2021/Globaler Wandel/Lokaler_Teil/Coelbe_Wetterstation/produkt_klima_monat_18810101_20191231_03164.txt", header = T, dec = ".", sep = ";")

### Parameter: MO_TT = Monatsmittel der Lufttemperatur in 2m Hoehe; MO_N = Monatsmittel des Bedeckunsgrades in Achtel; MO_RR = Monatssumme der Niederschlagshoehe in mm

data$DATUM <- strptime(data$MESS_DATUM_BEGINN, format = "%Y%m%d")

data_clean <- subset(data, DATUM > "1987-12-01" & DATUM < "2019-01-01")
data_clean_fertig <- data.frame(Datum = data_clean$DATUM,
                                Niederschlag_mm = data_clean$MO_RR,
                                Temperatur_Grad_Celsius = data_clean$MO_TT,
                                Bedeckungsgrad_Achteln = data_clean$MO_N)
data_clean_fertig[data_clean_fertig == -999] <- NA
data_clean_fertig$Month_Year <- substr(data_clean_fertig$Datum, 1,7)
### -> Bedeckungsgrad ist nur bis einschließlich 2011 verfügbar
### langjaehriges Mittel erstellen für die Sommermonate Juni, Juli und August von 1988-2018

Summer_month <- data_clean_fertig %>%
  filter(substr(data_clean_fertig$Datum,6,7) == "06" | substr(data_clean_fertig$Datum,6,7) == "07" | substr(data_clean_fertig$Datum,6,7) == "08")

### Datenlücke von 2000-08 bis 2006-05

lang_Mittel <- data.frame(Temperatur = mean(Summer_month$Temperatur_Grad_Celsius),
                          Niederschlag = mean(Summer_month$Niederschlag_mm))
saveRDS(lang_Mittel, "C:/Users/Lena/Documents/Uni_Marburg/WS_2021/Globaler Wandel/Lokaler_Teil/Coelbe_Wetterstation/lang_Mittel.rds")

### Mittelwerte für die Untersuchungsjahre erstellen (2000, 2006, 2012, 2018)
# 2000
Sommer_2000 <- Summer_month %>%
  filter(substr(Summer_month$Datum,1,4) == "2000")
Sommer_2000 <- data.frame(Niederschlag = mean(Sommer_2000$Niederschlag_mm),
                          Temperatur = mean(Sommer_2000$Temperatur_Grad_Celsius))
row.names(Sommer_2000) <- "2000"
# 2006 
Sommer_2006 <- Summer_month %>%
  filter(substr(Summer_month$Datum,1,4) == "2006")
Sommer_2006 <- data.frame(Niederschlag = mean(Sommer_2006$Niederschlag_mm),
                          Temperatur = mean(Sommer_2006$Temperatur_Grad_Celsius))
row.names(Sommer_2006) <- "2006"
# 2012
Sommer_2012 <- Summer_month %>%
  filter(substr(Summer_month$Datum,1,4) == "2012")
Sommer_2012 <- data.frame(Niederschlag = mean(Sommer_2012$Niederschlag_mm),
                          Temperatur = mean(Sommer_2012$Temperatur_Grad_Celsius))
row.names(Sommer_2012) <- "2012"
# 2018
Sommer_2018 <- Summer_month %>%
  filter(substr(Summer_month$Datum,1,4) == "2018")
Sommer_2018 <- data.frame(Niederschlag = mean(Sommer_2018$Niederschlag_mm),
                          Temperatur = mean(Sommer_2018$Temperatur_Grad_Celsius))
row.names(Sommer_2018) <- "2018"

### einen DF erstellen

DF_Sommer_Mittelwerte <- do.call("rbind", list(Sommer_2000, Sommer_2006, Sommer_2012, Sommer_2018))
DF_Sommer_Mittelwerte$Datum <- c("2000", "2006", "2012", "2018")
saveRDS(DF_Sommer_Mittelwerte, 
          "C:/Users/Lena/Documents/Uni_Marburg/WS_2021/Globaler Wandel/Lokaler_Teil/Coelbe_Wetterstation/Mittelwerte_Sommer.rds")

### plot it 
par(mfrow = c(1,1))
# Niederschlag
niederschlag_lang_mittel_line <- c(lang_Mittel$Niederschlag, lang_Mittel$Niederschlag, lang_Mittel$Niederschlag, lang_Mittel$Niederschlag)
par(mar = c(5, 4, 4, 4) + 0.3)
df_bar <- barplot(DF_Sommer_Mittelwerte$Niederschlag,
                  ylab = "Niederschlag (in mm)",
                  ylim = c(0, 100),
                  xlab = "Jahr",
                  names.arg = c("2000", "2006", "2012", "2018"),
                  main  = "Mittlerer Niederschlag der Sommermonate \n Juni, Juli und August in Coelbe \n (rote Linie = langjaehriges Mittel von 1988-2018)",
                  col = "darkblue")
lines(x = df_bar, y = niederschlag_lang_mittel_line, col = "red", lwd = 3)
lines(x = 0.5 + df_bar, y = niederschlag_lang_mittel_line, col = "red", lwd = 3)
lines(x = df_bar - 0.5, y = niederschlag_lang_mittel_line, col = "red", lwd = 3)

# Temperatur
Temperatur_lang_mittel_line <- c(lang_Mittel$Temperatur, lang_Mittel$Temperatur, lang_Mittel$Temperatur, lang_Mittel$Temperatur)
par(mar = c(5, 4, 4, 4) + 0.3)
df_bar <- barplot(DF_Sommer_Mittelwerte$Temperatur,
                  ylab = "Temperatur (in ?C)",
                  ylim = c(0, 30),
                  xlab = "Jahr",
                  names.arg = c("2000", "2006", "2012", "2018"),
                  main  = "Mittlerer Temperatur der Sommermonate \n Juni, Juli und August in Coelbe \n (rote Linie = langjaehriges Mittel von 1988-2018)",
                  col = "orange")
lines(x = df_bar, y = Temperatur_lang_mittel_line, col = "red", lwd = 3)
lines(x = 0.5 + df_bar, y = Temperatur_lang_mittel_line, col = "red", lwd = 3)
lines(x = df_bar - 0.5, y = Temperatur_lang_mittel_line, col = "red", lwd = 3)
