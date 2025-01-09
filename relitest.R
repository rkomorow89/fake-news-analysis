# Reliabilitätstests mit R

library(tidycomm)
library(openxlsx)
library(tidyverse)
library(kableExtra) # optional für nette Tabellenansicht der Ergebnisse

# Wenn mehrere, getrennte Dateien (z. B. jeder Codierer mit einer eigenen Excel-Datei), dann Auflistung der Dateien ebenfalls so möglich
liste <- list.files(pattern = "Pretest.xlsx")

# Hiermit können mehrere Dateien gleichzeitig eingelesen werden; die "ID" entspricht hier dem Dateinamen (nützlich, wenn meine Excel-Dateien nach Codierern benannt sind, beispielsweise "codierungen_nathalie.xlsx")
d_fake <- 
  liste %>% 
  purrr::map(readxl::read_excel, skip = 0) %>% 
  purrr::map2_df(liste, ~ mutate(.x, ID = .y))

# Am besten ist es, wenn jede Spalte auch einen Namen hat. In diesem Fall brauchen wir beispielsweise die erste Spalte, weil darin die Codierer festgehalten sind. Die Spalte heißt nach dem Einlesen `...1`.
colnames(d_fake)

# Jetzt werden wir die Dinge raus, die wir nicht brauchen: Zwischenmarkierungen der Artikel, leere Zeilen. Als erstes benennen wir aber die Codierer-Spalte um.

d_fake <- 
  d_fake %>% 
  rename(codierer = `...1`) %>% 
  filter(!grepl("Art", codierer), !is.na(codierer)) %>% 
  select(-13:-15)

# Wir wiederholen das gleiche für die True News (allerdings hier alles in einem Schritt)
d_true <- 
  liste %>% 
  purrr::map(readxl::read_excel, skip = 0, sheet = 2) %>% 
  purrr::map2_df(liste, ~ mutate(.x, ID = .y)) %>% 
  rename(codierer = `...1`) %>% 
  filter(!grepl("Art", codierer), !is.na(codierer)) %>% 
  select(-13:-15)

# Danach können wir die beiden Datensätze zusammenfügen
d_ges <- 
  d_fake %>% 
  rbind(d_true)

# Stimmt das einigermaßen? Wir testen.
d_ges %>% group_by(codierer) %>% count # 20 Codierungen pro Codierer
d_ges %>% group_by(`A1 Codiernummer`) %>% count # je 5 Mal 20 Artikel

# Und jetzt die Reliabilität. Wir gehen nach Skalenniveau vor. Dann können wir am einfachsten mehrere Variablen auf einmal berücksichtigen.

colnames(d_ges)

# Nominal:
d_ges %>% 
  select(codierer, `A1 Codiernummer`:`B1 Darstellung`) %>% 
  tidycomm::test_icr(unit_var = `A1 Codiernummer`, coder_var = "codierer") %>% View()

# Wir sehen, dass bei dem Datum der Veröfentlichung ein fehlender Wert zu finden ist. Das meldet uns tidycomm. Außerdem finden wir einen fehlenden Wert bei Holsti und der prozentualen Übereinstimmung, weil diese Koeffizienten nicht mit fehlenden Werten umgehen können. Wenn wir die Auswertung nur aufgrund der Fälle machen wollen, die keine fehlenden Werte haben, können wir a) na.omit = TRUE verwenden. Oder vorab b) solche Texte ausschließen, bei denen nicht alle Codierer alles notwendige codiert haben. Da ihr mit Krippendorff arbeiten möchtet, müssten wir im Grunde nichts weiter machen und es passt, wie oben geschrieben.

# Ordinal; die Variablen müssen dafür als Zahlen formatiert sein
d_ges %>% 
  select(codierer, `A1 Codiernummer`, `C1 Aktualität`:`C5 Nähe`) %>% 
  dplyr::mutate_at(3:7, as.numeric) %>% # hier werden die Variablen 3-7 numerisch gemacht
  tidycomm::test_icr(unit_var = `A1 Codiernummer`, coder_var = "codierer",
                     levels = c(`C1 Aktualität` = "ordinal", 
                                `C2 Schaden` = "ordinal", 
                                `C3 Kontroverse` = "ordinal", 
                                `C4 Aggression` = "ordinal", 
                                `C5 Nähe` = "ordinal")) %>% View()

# Übrigens, als nette Tabelle:
a <- d_ges %>% 
  select(codierer, `A1 Codiernummer`:`B1 Darstellung`) %>% 
  tidycomm::test_icr(unit_var = `A1 Codiernummer`, coder_var = "codierer")

b <- d_ges %>% 
  select(codierer, `A1 Codiernummer`, `C1 Aktualität`:`C5 Nähe`) %>% 
  dplyr::mutate_at(3:7, as.numeric) %>% # hier werden die Variablen 3-7 numerisch gemacht
  tidycomm::test_icr(unit_var = `A1 Codiernummer`, coder_var = "codierer",
                     levels = c(`C1 Aktualität` = "ordinal", 
                                `C2 Schaden` = "ordinal", 
                                `C3 Kontroverse` = "ordinal", 
                                `C4 Aggression` = "ordinal", 
                                `C5 Nähe` = "ordinal"))

a %>% rbind(b) %>% 
  select(-Level, -Agreement) %>% 
  dplyr::mutate_at(5:6, round, 2) %>% 
  filter(!Variable %in% c("codierer", "A2 Datum Codierung")) %>% 
  as.data.frame() %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_classic(full_width = F, html_font = "Cambria")

# Oder einfacher und direkt nach Excel:
a %>% rbind(b) %>% 
  select(-Level, -Agreement) %>% 
  dplyr::mutate_at(5:6, round, 2) %>% 
  filter(!Variable %in% c("codierer", "A2 Datum Codierung")) %>% 
  as.data.frame() %>% 
  write.xlsx("test.xlsx")
