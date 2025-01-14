# Working Directory
DATA_DIR <- "C:/path/to/your/data/directory"
setwd(DATA_DIR)
getwd()

# Pakete
list.of.packages <- c("quanteda", "plyr","tidyverse","udpipe","textclean","qdapRegex","stringi","performance",
                      "sjPlot","ggeffects","tidycomm","report","dplyr","koRpus","LSAfun","spacyr","writexl","openxlsx", 
                      "kableExtra","purrr","see","patchwork","sjmisc","sjlabelled","Hmisc","webshot","questionr","lsr")

#nur installieren, wenn noch nicht in RStudio vorhanden
install.packages(setdiff(list.of.packages, rownames(installed.packages()))) 

install.packages(
  "koRpus.lang.de",
  repo=c(
    getOption("repos"),
    l10n="https://undocumeantit.github.io/repos/l10n"
  )
)

library(quanteda)
library(plyr)
library(tidyverse)
library(udpipe)
library(textclean)
library(qdapRegex)
library(stringi)
library(performance)
library(sjPlot)
library(ggeffects)
library(tidycomm)
library(report)
library(dplyr)
library(koRpus)
library(koRpus.lang.de)
library(LSAfun)
library(spacyr)
library(writexl)
library(openxlsx)
library(kableExtra)
library(purrr)
library(see)
library(patchwork)
library(sjmisc)
library(sjlabelled)
library(Hmisc)
library(webshot)
library(questionr)
library(lsr)

# udpipe-Sprachmodell
udmodel <- "german-ud-2.0-170801.udpipe"

# Zusatzdateien und Pakete: Alternativ 
# Worthaeufigkeitsberechnung
LCC.de <- read.corp.LCC("deu_news_2015_1M.tar.gz")
# Kohaesion
# Semantischer Raum fuer LSA
# Heruntergeladen von https://sites.google.com/site/fritzgntr/software-resources/semantic_spaces
load("dewak100k_lsa.rda")
# NER mit spacyr
# Anaconda 3.x muss auf Rechner installiert sein, herunterladbar von:
# https://www.anaconda.com/products/individual#windows
#folgende Befehle müssen nur einmal durchlaufen werden
spacy_install() 
spacy_download_langmodel("de") 
spacy_initialize(model = "de_core_news_sm") 

#2 Funktionen
#2.2 Bereinigungsfunktion
putzibot <- function(text){
  tryCatch( { !require(textclean) }
            , warning = function(w) { print("Bitte installiere textclean.") })
  
  #if( !require(textclean) ){ stop("Bitte installiere textclean.")}
  x <- text
  
  yes_orig <- c("- bis", "- und", "- oder", "- bzw.", "- als", "- noch", " - ")
  yes_ers <- c("-#keep#bis", "-#keep#und", "-#keep#oder", "-#keep#bzw.", "-#keep#als", "-#keep#noch", "#keep#-#keep#")
  
  # Loeschen von Punkten in Ordinalzahlen
  x <- gsub("((^| )[[:digit:]]{1,2})(\\.)", "\\1", x)
  
  # Loeschen von Links
  x <- textclean::replace_url(x)
  
  # Falsche Leerzeichen
  x <- gsub("(\\.|\\?|!)(?=[A-Za-z])", ". ", x, perl = TRUE)
  x <- textclean::mgsub(x, yes_orig, yes_ers)
  x <- gsub("(- )([[:upper:]])", "-\\2", x)
  x <- gsub("(- )([[:upper:]])", "-\\2", x)
  x <- gsub("(- )([[:alnum:]])", "\\2", x)
  x <- gsub("#keep#", " ", x)
  
  # Leerzeichen trimmen
  x <- stringr::str_squish(x)
  return(x)
} # wo der Name wohl herkommt? ;-)

#2.3 Zaehlen von Grossschreibung
n_gross <- function(text){
  x <- unlist(gregexpr("[A-Z]", text))
  if (x == -1) {
    return(0)
  } else {
    return(length(x))
  }
}

#2.4 Lesbarkeitsanalyse
klartext <- function(text, sprachmodell, ner_spacyr = FALSE, LSA = NULL, worthaeufigkeit = NULL, zeig_mal_text = FALSE){
  
  # Zuweisung der Werte
  x <- text
  udmodel <- sprachmodell
  
  # Benoetigte Funktion
  n_gross <- function(text){
    x <- unlist(gregexpr("[A-Z]", text))
    if (x == -1) {
      return(0)
    } else {
      return(length(x))
    }
  }
  
  ##########################################
  # 1. Korrektur fehlender Satzendezeichen #
  ##########################################
  # Bspw. wenn Zwischenueberschriften ohne Punkt enden.
  # Hier wird vereinfacht nach grossgeschriebenen Artikeln und Personalpronomen gesucht. 
  # Das ist natuerlich nicht perfekt! 
  # Haeufigster Fehler: Siezen wird faelschlicherweise als Satzende markiert (Sie = Personalpronomen vs. Sie = Siezen).
  d <- 
    udpipe(x, object = udmodel) %>% 
    filter(!is.na(start)) %>% 
    rowwise() %>% 
    dplyr::mutate(fehlende_satztrennung = case_when(xpos %in% c("PPER", "ART") & n_gross(substr(token, 1, 1)) == 1 & term_id != 1 & token_id != 1 ~ 1)) %>% 
    ungroup()
  
  x2 <- 
    d %>% 
    dplyr::mutate(token2 = case_when(is.na(fehlende_satztrennung) ~ token, 
                                     TRUE ~ paste0(". ", token))) %>% 
    dplyr::summarise(text = paste(token2, collapse = " "))
  
  x2 <- qdapRegex::rm_white(x2)
  
  x2 <- udpipe(x2, udmodel) %>% group_by(doc_id, sentence_id) %>% dplyr::summarize(satz = unique(sentence)) %>% ungroup() %>% select(satz) %>% as.matrix() %>% as.vector() %>% gsub(". ", " ", ., fixed = TRUE)
  #x2 ist der korrigierte Text, aufgesplittet in Saetze
  
  ############################################################
  # 2. Textstatistiken auf Grundlage des korrigierten Textes #
  ############################################################
  d <- 
    udpipe(x2, object = udmodel) %>% 
    filter(!is.na(start)) %>% 
    dplyr::mutate(id = paste(doc_id, paragraph_id, sentence_id, sep = "_"),
                  n_char = nchar(token)) %>% 
    dplyr::group_by(id) %>% # 
    dplyr::mutate(words = case_when(upos != "PUNCT" ~ 1, # schreibe 1, wenn es sich nicht um ein Satzzeichen handelt 
                                    TRUE ~ 0),
                  n_word = sum(words), # zaehle dann die 1er zusammen, um die Wortzahl pro Satz zu bekommen
                  upos = case_when(is.na(upos) ~ "ADP", # da udpipe bestimmte Appositionen (zum, dem) etwas merkwuerdig markiert, vergib dann, wenn keine Wortart erkannt wird das Label ADP
                                   TRUE ~ upos)) %>% 
    ungroup()
  
  n_satzzahl <- d %>% group_by(id) %>% dplyr::summarize(satz = unique(sentence)) %>% ungroup() %>% select(satz) %>% nrow()
  
  n_wortzahl <-  d %>% filter(upos != "PUNCT") %>% dplyr::summarize(n_word = length(token)) %>% as.matrix() %>% as.vector()
  
  m_satzlaenge <- n_wortzahl/n_satzzahl %>% as.matrix() %>% as.vector()
  
  n_buchstabenzahl <- d %>% filter(upos != "PUNCT") %>% dplyr::summarize(n_buchstaben = sum(n_char)) %>% as.matrix() %>% as.vector()
  
  m_wortlaenge <- n_buchstabenzahl/n_wortzahl 
  
  wortzahl_gr6 <- d %>% filter(n_char >= 7) %>% dplyr::count() %>% as.matrix() %>% as.vector() # Anzahl der Woerter mit mehr als 6 Buchstaben
  
  perc_wortzahl_gr6 <- wortzahl_gr6/n_wortzahl*100 # Anteil der Woerter mit mehr als 6 Buchstaben
  
  LIX <- m_satzlaenge + perc_wortzahl_gr6
  
  ##################################################
  # 3. Analysen des ersten Satzes (Annahme: Titel) #
  ##################################################
  satz1 <- d %>% filter(id == "doc1_1_1") %>% select(sentence) %>% unique() %>% as.matrix() %>% as.vector()
  
  n_buchstaben_satz1 <- d %>% filter(id == "doc1_1_1", upos != "PUNCT") %>% dplyr::mutate(buchstaben = nchar(token)) %>% select(buchstaben) %>% as.matrix() %>% as.vector() %>% sum()
  
  n_buchstaben_satz1_KAP <- d %>% filter(id == "doc1_1_1", upos != "PUNCT") %>% select(token) %>% n_gross
  
  n_wortzahl_satz1 <- d %>% filter(id == "doc1_1_1", upos != "PUNCT") %>% select(n_word) %>% unique() %>% as.matrix() %>% as.vector()
  
  n_wortzahl_gr6_satz1 <- d %>% filter(id == "doc1_1_1", n_char >= 7) %>% dplyr::count() %>% as.matrix() %>% as.vector()
  
  ##########
  # 4. NER #
  ##########
  if (ner_spacyr == TRUE) {
    n_eigennamen_satz1 <- spacyr::spacy_parse(satz1, dependency = TRUE, lemma = FALSE, tag = TRUE) %>% entity_extract(concatenator = "_") %>% filter(entity_type == "PER") %>% nrow()
  } else {
    n_eigennamen_satz1 <- udpipe(satz1, object = udmodel) %>% filter(upos == "PROPN") %>% nrow()
  }
  
  #####################
  # 5. Worthaeufigkeit #
  #####################
  
  if (is.null(worthaeufigkeit)) {
    anzahl_hk_bis_8_satz1 <- NA
    anteil_hk_bis_8_satz1 <- NA
  } else {
    LCC.de <- worthaeufigkeit
    # Haeufigstes Wort (der)
    freq.der <- LCC.de@words %>%  mutate(rank = num-100) %>% filter(rank >= 0) %>% filter(freq == max(freq)) %>% select(freq) %>% as.numeric()
    
    hk_data <- 
      LCC.de@words %>% 
      dplyr::mutate(leer = grepl(" ", word)) %>%
      filter(leer == FALSE) %>% 
      dplyr::mutate(rank = num-100, HK = round(log2(freq.der/freq), -.01)) %>% 
      filter(num > 62) %>% 
      select(word, HK)
    
    # num > 62 == Satzzeichen weg
    
    # Worthaeufigkeit
    tok <- koRpus::tokenize(satz1, format="obj", lang="de")
    
    hk_8 <- 
      hk_data %>% 
      merge(taggedText(tok) %>% filter(wclass == "word"), by.x = "word", by.y = "token", all.y = TRUE) %>% 
      dplyr::mutate(n_words = length(HK)) %>% 
      group_by(HK) %>% 
      dplyr::summarize(n_hk = length(HK), n_words = unique(n_words)) %>% 
      ungroup() %>% 
      filter(HK < 9)
    
    anzahl_hk_bis_8_satz1 <- sum(hk_8$n_hk)
    anteil_hk_bis_8_satz1 <- anzahl_hk_bis_8_satz1/n_wortzahl_satz1 
  }
  
  ############### 
  # 6. Kohaesion #
  ###############
  
  if (is.null(LSA)) {
    LSA_coh <- NA
  } else {
    semraum <- LSA
    x3 <- gsub("([[:alnum:]])(-)([[:alnum:]])", "\\1\\3", x2)
    x3 <- gsub("[^.[:alpha:][:space:]]", " ", x3)
    x3 <- stringr::str_squish(x3)
    x3 <- gsub(" .", ".", x3, fixed = TRUE)
    x4 <- tolower(x3)
    
    cohasion <- coherence(x4, tvectors=semraum)$global %>% as.numeric()
    LSA_coh <- data.frame(LSA_coh = cohasion)
  }
  
  ###################
  # Ergebnisausgabe #
  ###################
  
  result <- data.frame(n_satzzahl, n_wortzahl, n_buchstabenzahl, m_satzlaenge, m_wortlaenge, wortzahl_gr6, perc_wortzahl_gr6, LIX, LSA_coh, satz1, n_buchstaben_satz1, n_buchstaben_satz1_KAP, n_wortzahl_satz1, n_wortzahl_gr6_satz1, n_eigennamen_satz1, anzahl_hk_bis_8_satz1, anteil_hk_bis_8_satz1)
  
  if (zeig_mal_text == FALSE) {
    return(result)
  } else {
    result2 <- list(result, x2)
    names(result2) <- c("lesbarkeit", "text_bereinigt")
    return(result2)
  }
}

#3 Textanalysen
#3.1 GermanTRC
germanfakenc_corpus <- readRDS("germanfakenc_corpus.rds")
docvars(germanfakenc_corpus, "ID_2") <- docvars(germanfakenc_corpus) %>% group_by(type, ID) %>% dplyr::mutate(n = c(1:length(type))) %>% ungroup() %>% dplyr::mutate(ID_2 = paste(ID, type, n, sep = "_")) %>% select(ID_2)
docnames(germanfakenc_corpus) <- docvars(germanfakenc_corpus)$ID_2
germanfakenc_corpus_red <- quanteda::corpus_subset(germanfakenc_corpus, type != "CRAWLED")
docvars(germanfakenc_corpus_red, "sammlung") <- "GermanTRC"

d_germanfakenc <- NULL
i <- NA
pb = txtProgressBar(min = 0, max = length(germanfakenc_corpus_red), initial = 0, style = 3) 

#Durchlauf dauert ca. 70 Minuten (Ladebalken beachten!)
for (i in c(1:length(germanfakenc_corpus_red))) {
  doc <- docvars(germanfakenc_corpus_red)[i,] %>% select(ID_2, sammlung, Title)
  text <- paste(gsub(":|\\.|!|\\?", "", doc$Title), texts(germanfakenc_corpus_red)[i], sep = ". ")
  text <- putzibot(text)
  klar <- klartext(text = text, sprachmodell = udmodel, ner_spacyr = TRUE, LSA = NULL, worthaeufigkeit = LCC.de, zeig_mal_text = FALSE)
  temp <- doc %>% cbind(klar)
  d_germanfakenc <- rbind(d_germanfakenc, temp)
  setTxtProgressBar(pb,i) 
}

#3.2 Corona-News
#Dateinamen im Ordner mit den Corona-News auflisten
liste <- list.files(path = "Textsammlung 1", recursive = TRUE, full.names = TRUE)
corona <- map_df(liste, ~ data_frame(text = readLines(.x, encoding = "UTF-8")) %>% mutate(ID_2 = gsub("(.*)(T_|F_)(.*)(\\.txt)", "\\2\\3", .x)))
corona2 <- 
  corona %>% 
  as_tibble() %>% 
  filter(nchar(text) != 0) %>% 
  dplyr::mutate(text = stringi::stri_trim_both(text)) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(text = gsub("\u2028", ".\n", text),
                text = case_when(grepl("[[:punct:]]$", text) ~ text, TRUE ~ paste0(text, "."))) %>%
  ungroup() %>% 
  group_by(ID_2) %>% 
  dplyr::summarize(text = paste(text, collapse = "\n")) %>% 
  ungroup()
corona_corpus <- corpus(corona2)
docvars(corona_corpus, "sammlung") <- "Corona"
docnames(corona_corpus) <- docvars(corona_corpus)$ID_2
d_corona <- NULL
i <- NA
pb = txtProgressBar(min = 0, max = length(corona_corpus), initial = 0, style = 3) 

#Durchlauf dauert ca. 10 Minuten (Ladebalken beachten!)
for (i in c(1:length(corona_corpus))) {
  doc <- docvars(corona_corpus)[i,]
  
  x <- unlist(stringi::stri_split_lines(texts(corona_corpus)[i]))
  x <- add_missing_endmark(x, replacement = ".", endmarks = c("?", ".", "!"))
  text <- gsub(":.", ".", x, fixed = TRUE)
  text <- putzibot(text)
  text <- text[which(nchar(text) > 0)]
  
  if (grepl("F_", doc$ID_2)) {
    Title <- text[1]
    text[1] <- paste0(gsub(":|\\.|!|\\?", "", Title), ".")
  } else {
    Title <- paste(text[1:2], collapse = " ")
    text[2] <- paste0(gsub(":|\\.|!|\\?", "", Title), ". ")
    text <- text[-1]
  }
  doc <- doc %>% cbind(Title)
  
  klar <- klartext(text = text, sprachmodell = udmodel, ner_spacyr = TRUE, LSA = NULL, worthaeufigkeit = LCC.de, zeig_mal_text = FALSE)
  temp <- doc %>% cbind(klar)
  d_corona <- rbind(d_corona, temp)
  setTxtProgressBar(pb,i)
}

#4 Zusammenfuehrung und Begutachtung der Ergebnisse
daten_gesamt <- d_germanfakenc %>% rbind(d_corona)

# Als .rds-Datei speichern
saveRDS(daten_gesamt, "~/path/to/your/data/rds_files/verstaendlichkeitsanalyse_fake_und_true_komplett.rds")

# Gespeicherte .rds-Datei aufrufen
daten_gesamt <- readRDS("~/path/to/your/data/verstaendlichkeitsanalyse_fake_und_true_komplett.rds")

#als Excel-Datei in WD speichern
write_xlsx(daten_gesamt, "analyse_komplett.xlsx")

#Fuege neue Spalte "art" hinzu
d_test <- 
  daten_gesamt %>% 
  dplyr::mutate(art = case_when(grepl("FAKE|F_", ID_2) ~ "Fake News",
                                TRUE ~ "True News")) 

#Spaltenbeschreibungen hinzufuegen
var.labels <- c(ID_2="ID_2",
                sammlung="Textsammlung",
                Title="Nachrichtentitel",
                n_satzzahl="Anzahl der Saetze",
                n_wortzahl="Anzahl der Woerter",
                n_buchstabenzahl="Anzahl der Buchstaben",
                m_satzlaenge="Durchschn. Satzlaenge in Woertern",
                m_wortlaenge="Durchschn. Wortlaenge in Buchstaben",
                wortzahl_gr6="Anzahl der Woerter mit mehr als 6 Buchstaben (= lange Woerter nach Bjoernsson)",
                perc_wortzahl_gr6="Anteil der Woerter mit mehr als 6 Buchstaben",
                LIX="LIX Lesbarkeitsindex",
                LSA_coh="Kohaesion (LSA)",
                satz1="Erster Satz des Textes (soll sein: Titel)",
                n_buchstaben_satz1="Anzahl der Buchstaben im ersten Satz",
                n_buchstaben_satz1_KAP="Anzahl der gross geschriebenen Buchstaben im ersten Satz",
                n_wortzahl_satz1="Anzahl der Woerter im ersten Satz",
                n_wortzahl_gr6_satz1="Anzahl der Woerter mit mehr als 6 Buchstaben im ersten Satz",
                n_eigennamen_satz1="Anzahl der Eigennamen im ersten Satz",
                ner="Verwendete NER-Variante",
                anzahl_hk_bis_8_satz1="Anzahl der Woerter bis zur Haeufigkeitsklasse 8 (sehr haeufig, eher einfach)",
                anteil_hk_bis_8_satz1="Anteil der Woerter bis zur Haeufigkeitsklasse 8",
                art="Artikelart")

label(d_test) <- as.list(var.labels[match(names(d_test), names(var.labels))])
label(d_test)

#Regression für LIX
m <- lm(LIX ~ art + sammlung, d_test)
performance::check_model(m)

summary(m)

sjPlot::tab_model(m, auto.label = F, pred.labels = c("(Intercept)","Artikelart: True News","Textsammlung: GermanTRC"), file = "html/Verstaendlichkeit (LIX).html")

#als Bilddatei abspeichern
webshot::install_phantomjs()
webshot("html/Verstaendlichkeit (LIX).html", "plots/Verstaendlichkeit (LIX).png")

#Mittelwerte
round(mean(d_test$LIX[d_test$art=="Fake News"]),1)
round(mean(d_test$LIX[d_test$art=="True News"]),1)

report::report(m)

sjPlot::plot_model(m, type = "emm", terms = c("art", "sammlung"))

ggeffects::ggemmeans(m, terms = c("art", "sammlung"))

ggeffects::ggemmeans(m, terms = c("art", "sammlung")) %>% plot()

ggeffects::ggemmeans(m, terms = c("art", "sammlung")) %>% plot() +
  xlab("Verstaendlichkeit") + 
  ylab("LIX") + 
  scale_colour_discrete(name = "Sammlung") +
  theme_bw() + 
  ggtitle("")

#Regression für Titellänge
m <- lm(n_wortzahl_satz1 ~ art + sammlung, d_test)
sjPlot::tab_model(m, auto.label = F, pred.labels = c("(Intercept)","Artikelart: True News","Textsammlung: GermanTRC"), file = "html/Titel-Laenge.html")
webshot("html/Titel-Laenge.html", "plots/Titel-Laenge.png")

round(mean(d_test$n_wortzahl_satz1[d_test$art=="Fake News"]),2)
round(mean(d_test$n_wortzahl_satz1[d_test$art=="True News"]),2)

#Regression für Anteil einfache Wörter im Titel
m <- lm(anteil_hk_bis_8_satz1 ~ art + sammlung, d_test)
sjPlot::tab_model(m, auto.label = F, pred.labels = c("(Intercept)","Artikelart: True News","Textsammlung: GermanTRC"), file = "html/Anteil einfache Woerter im Titel.html")
webshot("html/Anteil einfache Woerter im Titel.html", "plots/Anteil einfache Woerter im Titel.png")

round(mean(d_test$anteil_hk_bis_8_satz1[d_test$art=="Fake News"]),3)
round(mean(d_test$anteil_hk_bis_8_satz1[d_test$art=="True News"]),3)

#Regression für Anzahl der groß geschriebenen Buchstaben im ersten Satz
m <- lm(n_buchstaben_satz1_KAP ~ art + sammlung, d_test)
sjPlot::tab_model(m, auto.label = F, pred.labels = c("(Intercept)","Artikelart: True News","Textsammlung: GermanTRC"), file = "html/Anzahl der gross geschriebenen Buchstaben im ersten Satz.html")
webshot("html/Anzahl der gross geschriebenen Buchstaben im ersten Satz.html", "plots/Anzahl der gross geschriebenen Buchstaben im ersten Satz.png")

round(mean(d_test$n_buchstaben_satz1_KAP[d_test$art=="Fake News"]),2)
round(mean(d_test$n_buchstaben_satz1_KAP[d_test$art=="True News"]),2)

#5 Alternative

i <- NA
result_korpus <- NULL
for (i in c(1:length(corona_corpus))) {
  x <- unlist(stringi::stri_split_lines(texts(corona_corpus)[i]))
  LIX_koRpus <- koRpus::LIX(koRpus::tokenize(x, format = "obj", lang = "de", ign.comp = TRUE))@LIX[["index"]]
  doc <- names(texts(corona_corpus)[i])
  temp <- data.frame(doc, LIX_koRpus)
  result_korpus <- rbind(result_korpus, temp)
}
head(result_korpus)

# Reliabilitätstests

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

# Wir sehen, dass bei dem Datum der Veröffentlichung ein fehlender Wert zu finden ist. Das meldet uns tidycomm. Außerdem finden wir einen fehlenden Wert bei Holsti und der prozentualen Übereinstimmung, weil diese Koeffizienten nicht mit fehlenden Werten umgehen können. Wenn wir die Auswertung nur aufgrund der Fälle machen wollen, die keine fehlenden Werte haben, können wir a) na.omit = TRUE verwenden. Oder vorab b) solche Texte ausschließen, bei denen nicht alle Codierer alles notwendige codiert haben. Da ihr mit Krippendorff arbeiten möchtet, müssten wir im Grunde nichts weiter machen und es passt, wie oben geschrieben.

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

#Ergebnisse
#Häufigkeiten der Darstellungsform
liste <- list.files(pattern = "Codebogen.xlsx")

#Daten einlesen
d_code <- 
  liste %>% 
  purrr::map(readxl::read_excel, skip = 0) %>% 
  purrr::map2_df(liste, ~ mutate(.x, ID = .y))

#Fuege neue Spalte "art" hinzu
d_code <- 
  d_code %>% 
  dplyr::mutate(art = case_when(grepl("F", `A1 Codiernummer`) ~ "Fake News",
                                TRUE ~ "True News")) 

#Häufigkeiten der Darstellungsarten
#1 Bericht, Nachricht 
#2 Reportage, Feature, Doku
#3 Kommentar, Leitartikel 
#4 Interview 
#5 Sonstiges/Unklar

#Anteile an Fake News
fake_freq<-round(table(d_code[d_code$art=="Fake News",]$`B1 Darstellung`),2)
fake_rel.freq<-round(fake_freq/nrow(d_code[d_code$art=="Fake News",])*100,2)

#Anteile an True News
true_freq<-round(table(d_code[d_code$art=="True News",]$`B1 Darstellung`),2)
true_rel.freq<-round(true_freq/nrow(d_code[d_code$art=="True News",])*100,2)

#zusammenfassen
comb <-rbind(fake_freq,true_freq)

#Nullen hinzufügen
comb[1,4]=0
comb[1,5]=0

#Chi-Quadrat-Test durchführen
test <- chisq.test(comb)
#Chi-Quadrat-Wert
test$statistic
#Freiheitsgrade
test$parameter
#p-Wert
test$p.value

#Cramer's V
round(cramer.v(comb),2)

#Balkendiagramm
bar<-barplot(comb,
        main = "Darstellungsform",
        xlab = "Anzahl",
        col = c("red","green"),
        beside = TRUE,
        ylim=c(0,180)
)

#Legende hinzufügen
legend("topright",
       c("Fake News","True News"),
       fill = c("red","green")
)

#Zahlenwerte hinzufügen
text(x=bar, y=comb+5, labels=as.character(comb))

#Welch t-Test Schaden
#1 kein Schaden
#2 geringer Schaden
#3 großer Schaden

#Anteile an Fake News
schaden_freq_fake<-round(table(d_code[d_code$art=="Fake News",]$`C2 Schaden`),2)
schaden_rel.freq_fake<-round(schaden_freq_fake/nrow(d_code[d_code$art=="Fake News",])*100,2)

#Anteile an True News
schaden_freq_true<-round(table(d_code[d_code$art=="True News",]$`C2 Schaden`),2)
schaden_rel.freq_true<-round(schaden_freq_true/nrow(d_code[d_code$art=="True News",])*100,2)

#Gesamt
schaden_freq <- schaden_freq_fake+schaden_freq_true

#zusammenfassen
comb <-rbind(schaden_freq_fake,schaden_freq_true)

#Chi-Quadrat-Test durchführen
test <- chisq.test(comb)
#Chi-Quadrat-Wert
test$statistic
#Freiheitsgrade
test$parameter
#p-Wert
test$p.value

#Cramer's V
round(cramer.v(comb),2)

#Mittelwerte
schaden_fake <- d_code[d_code$art=="Fake News",]$`C2 Schaden`
schaden_true <- d_code[d_code$art=="True News",]$`C2 Schaden`

mean1 <- round(mean(schaden_fake),2)
mean2 <- round(mean(schaden_true),2)

#Standardabweichungen
sd1 <- round(sd(schaden_fake),2)
sd2 <- round(sd(schaden_true),2)

#zusammenfassen
comb <-rbind(mean1,mean2)

test <- t.test(schaden_fake, schaden_true)

#t-Wert
test$statistic
#Freiheitsgrade
test$parameter
#p-Wert
test$p.value

#Cohen's d
round(cohensD(schaden_fake, schaden_true),2)

#Balkendiagramm
bar<-barplot(comb,
             main = "Schaden",
             xlab = "Mittelwert",
             col = c("blue","green"),
             beside = TRUE,
             ylim=c(0,2.5)
)

#Legende hinzufügen
legend("topright",
       c("Fake News","True News"),
       fill = c("blue","green")
)

#Zahlenwerte hinzufügen
text(x=bar, y=comb+0.2, labels=as.character(comb))

#Welch t-Test Aggression
#1 keine Aggression
#2 geringe Aggression
#3 mittlere Aggression
#4 höchste Aggression

#Anteile an Fake News
aggression_freq_fake<-round(table(d_code[d_code$art=="Fake News",]$`C4 Aggression`),2)
aggression_rel.freq_fake<-round(aggression_freq_fake/nrow(d_code[d_code$art=="Fake News",])*100,2)

#Anteile an True News
aggression_freq_true<-round(table(d_code[d_code$art=="True News",]$`C4 Aggression`),2)
aggression_rel.freq_true<-round(aggression_freq_true/nrow(d_code[d_code$art=="True News",])*100,2)

#Gesamt
aggression_freq <- aggression_freq_fake+aggression_freq_true

#zusammenfassen
comb <-rbind(aggression_freq_fake,aggression_freq_true)

#Chi-Quadrat-Test durchführen
test <- chisq.test(comb)
#Chi-Quadrat-Wert
test$statistic
#Freiheitsgrade
test$parameter
#p-Wert
test$p.value

#Cramer's V
round(cramer.v(comb),2)

#Mittelwerte
aggression_fake <- d_code[d_code$art=="Fake News",]$`C4 Aggression`
aggression_true <- d_code[d_code$art=="True News",]$`C4 Aggression`

mean1 <- round(mean(aggression_fake),2)
mean2 <- round(mean(aggression_true),2)

#Standardabweichungen
sd1 <- round(sd(aggression_fake),2)
sd2 <- round(sd(aggression_true),2)

#zusammenfassen
comb <-rbind(mean1,mean2)

test <- t.test(aggression_fake, aggression_true)

#t-Wert
test$statistic
#Freiheitsgrade
test$parameter
#p-Wert
test$p.value

#Cohen's d
round(cohensD(aggression_fake, aggression_true),2)

#Balkendiagramm
bar<-barplot(comb,
             main = "Aggression",
             xlab = "Mittelwert",
             col = c("blue","green"),
             beside = TRUE,
             ylim=c(0,2.5)
)

#Legende hinzufügen
legend("topright",
       c("Fake News","True News"),
       fill = c("blue","green")
)

#Zahlenwerte hinzufügen
text(x=bar, y=comb+0.2, labels=as.character(comb))

#Welch t-Test Kontroverse
#1 keine Kontroverse
#2 geringe Kontroverse
#3 große Kontroverse 

#Anteile an Fake News
kontroverse_freq_fake<-round(table(d_code[d_code$art=="Fake News",]$`C3 Kontroverse`),2)
kontroverse_rel.freq_fake<-round(kontroverse_freq_fake/nrow(d_code[d_code$art=="Fake News",])*100,2)

#Anteile an True News
kontroverse_freq_true<-round(table(d_code[d_code$art=="True News",]$`C3 Kontroverse`),2)
kontroverse_rel.freq_true<-round(kontroverse_freq_true/nrow(d_code[d_code$art=="True News",])*100,2)

#Gesamt
kontroverse_freq <- kontroverse_freq_fake+kontroverse_freq_true

#zusammenfassen
comb <-rbind(kontroverse_freq_fake,kontroverse_freq_true)

#Chi-Quadrat-Test durchführen
test <- chisq.test(comb)
#Chi-Quadrat-Wert
test$statistic
#Freiheitsgrade
test$parameter
#p-Wert
test$p.value

#Cramer's V
round(cramer.v(comb),2)

#Mittelwerte
kontroverse_fake <- d_code[d_code$art=="Fake News",]$`C3 Kontroverse`
kontroverse_true <- d_code[d_code$art=="True News",]$`C3 Kontroverse`

mean1 <- round(mean(kontroverse_fake),2)
mean2 <- round(mean(kontroverse_true),2)

#Standardabweichungen
sd1 <- round(sd(kontroverse_fake),2)
sd2 <- round(sd(kontroverse_true),2)

#zusammenfassen
comb <-rbind(mean1,mean2)

test <- t.test(kontroverse_fake, kontroverse_true)

#t-Wert
test$statistic
#Freiheitsgrade
test$parameter
#p-Wert
test$p.value

#Cohen's d
round(cohensD(kontroverse_fake, kontroverse_true),2)

#Balkendiagramm
bar<-barplot(comb,
             main = "Kontroverse",
             xlab = "Mittelwert",
             col = c("blue","green"),
             beside = TRUE,
             ylim=c(0,2.5)
)

#Legende hinzufügen
legend("topright",
       c("Fake News","True News"),
       fill = c("blue","green")
)

#Zahlenwerte hinzufügen
text(x=bar, y=comb+0.2, labels=as.character(comb))

#Welch t-Test Aktualitaet
#1 Nicht aktuelle Thematik --> 1,2,3,4,5,9
#2 Aktuelle Thematik --> 6,7,8

#Aktualitaet umkodieren
d  <- d_code%>% mutate_at(vars(d_code$`C1 Aktualität`), funs(dplyr::recode(., '6'=1,'7'=1,'8'=1, .default = 0)))

#Anteile an Fake News
aktualitaet_freq_fake<-round(table(d[d_code$art=="Fake News",]$`C1 Aktualität`),2)
aktualitaet_rel.freq_fake<-round(aktualitaet_freq_fake/nrow(d[d_code$art=="Fake News",])*100,2)

#Anteile an True News
aktualitaet_freq_true<-round(table(d[d_code$art=="True News",]$`C1 Aktualität`),2)
aktualitaet_rel.freq_true<-round(aktualitaet_freq_true/nrow(d[d_code$art=="True News",])*100,2)

#Gesamt
aktualitaet_freq <- aktualitaet_freq_fake+aktualitaet_freq_true

#zusammenfassen
comb <-rbind(aktualitaet_freq_fake,aktualitaet_freq_true)

#Chi-Quadrat-Test durchführen
test <- chisq.test(comb)
#Chi-Quadrat-Wert
test$statistic
#Freiheitsgrade
test$parameter
#p-Wert
test$p.value

#Cramer's V
round(cramer.v(comb),2)

#Mittelwerte
aktualitaet_fake <- d[d$art=="Fake News",]$`C1 Aktualität`
aktualitaet_true <- d[d$art=="True News",]$`C1 Aktualität`

mean1 <- round(mean(aktualitaet_fake),2)
mean2 <- round(mean(aktualitaet_true),2)

#Standardabweichungen
sd1 <- round(sd(aktualitaet_fake),2)
sd2 <- round(sd(aktualitaet_true),2)

#zusammenfassen
comb <-rbind(mean1,mean2)

test <- t.test(aktualitaet_fake, aktualitaet_true)

#t-Wert
test$statistic
#Freiheitsgrade
test$parameter
#p-Wert
test$p.value

#Cohen's d
round(cohensD(aktualitaet_fake, aktualitaet_true),2)

#Balkendiagramm
bar<-barplot(comb,
             main = "Aktualitaet",
             xlab = "Mittelwert",
             col = c("blue","green"),
             beside = TRUE,
             ylim=c(0,0.8)
)

#Legende hinzufügen
legend("topright",
       c("Fake News","True News"),
       fill = c("blue","green")
)

#Zahlenwerte hinzufügen
text(x=bar, y=comb+0.1, labels=as.character(comb))

#Welch t-Test Naehe
#1 Bezugsraum ausserhalb von Deutschland --> 1,2,3,4
#2 Bezugsraum Deutschland --> 5

#Naehe umkodieren
d  <- d_code%>% mutate_at(vars(d_code$`C5 Nähe`), funs(dplyr::recode(., '5'=1, .default = 0)))

#Anteile an Fake News
naehe_freq_fake<-round(table(d[d_code$art=="Fake News",]$`C5 Nähe`),2)
naehe_rel.freq_fake<-round(naehe_freq_fake/nrow(d[d_code$art=="Fake News",])*100,2)

#Anteile an True News
naehe_freq_true<-round(table(d[d_code$art=="True News",]$`C5 Nähe`),2)
naehe_rel.freq_fake<-round(naehe_freq_true/nrow(d[d_code$art=="True News",])*100,2)

#Gesamt
naehe_freq <- naehe_freq_fake+naehe_freq_true

#zusammenfassen
comb <-rbind(naehe_freq_fake,naehe_freq_true)

#Chi-Quadrat-Test durchführen
test <- chisq.test(comb)
#Chi-Quadrat-Wert
test$statistic
#Freiheitsgrade
test$parameter
#p-Wert
test$p.value

#Cramer's V
round(cramer.v(comb),2)

#Mittelwerte
naehe_fake <- d_code[d$art=="Fake News",]$`C5 Nähe`
naehe_true <- d_code[d$art=="True News",]$`C5 Nähe`

mean1 <- round(mean(naehe_fake),2)
mean2 <- round(mean(naehe_true),2)

#Standardabweichungen
sd1 <- round(sd(naehe_fake),2)
sd2 <- round(sd(naehe_true),2)

#zusammenfassen
comb <-rbind(mean1,mean2)

test <- t.test(naehe_fake, naehe_true)

#t-Wert
test$statistic
#Freiheitsgrade
test$parameter
#p-Wert
test$p.value

#Cohen's d
round(cohensD(naehe_fake, naehe_true),2)









