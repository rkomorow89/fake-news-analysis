# Verständlichkeitsanalyse von Fake News und True News

# Pakete
library(quanteda)
library(plyr)
library(tidyverse)
library(udpipe)
library(textclean) # Bereinigungsfunktionen
library(qdapRegex) # Bereinigungsfunktionen
library(stringi) # Textmanipulation

# WD: Hier sind alle meine Dateien
setwd("~/Schreibtisch/Claudi Test R/PolKomm")

#####################################
# Zusatzdateien: Zwingend notwendig #
#####################################

# udpipe-Sprachmodell
udmodel <- "~/Schreibtisch/Claudi Test R/PolKomm/german-ud-2.0-170801.udpipe" 

########################################
# Zusatzdateien und Pakete: Alternativ #
########################################

# Die folgenden Pakete und Dateien sind nicht zwingend notwendig, um die wesentlichen Textanalysen durchführen zu können.

# Worthäufigkeitsberechnung
library(koRpus)
library(koRpus.lang.de)

LCC.de <- read.corp.LCC("~/R/Run/Diss_Claudi/deu_news_2015_1M.tar.gz")

# Korpora findet man hier: https://wortschatz.uni-leipzig.de/de/download/German
# Es dient als Referenzpunkt um zu schauen, wie häufig die Wörter eines Textes im Vergleich in der deutschen Sprache vorkommen (= gemessen daran, wie häufig das Wort in einer Textsammlung vorkommt).

# Kohäsion
library(LSAfun) # Kohäsion; könnte bei Macs Probleme bei der Installation geben

# Semantischer Raum für LSA
load("dewak100k_lsa.rda")

# NER mit spacyr
#install.packages("spacyr")
library("spacyr")
spacy_install() # miniconda wird installiert
spacy_download_langmodel("de") # Deutsch herunterladen
spacy_initialize(model = "de_core_news_sm") # Deutsch laden

###########
# HINWEIS #
########### 

# Damit es hier nicht zu unübersichtlich wird, habe ich einige Funktionen geschrieben. Sie fassen Teilschritte zusammen, sodass man später nicht ganz so viel Wust auf einem Fleck hat. Ich erkläre euch gerne, was da jeweils gemacht wird, warne aber vor, dass einige Punkte aufgrund des Materials nicht ganz so einfach sind. Wir werden daher hier nicht alle Punkte berücksichtigen, die man theoretisch berücksichtigen könnte.

# Grundlegende Punkte, die berücksichtigt sind:
# falsch gesetzte (fehlende) Leerzeichen
# Links
# fehlende Satzendezeichen

# Eventuell fallen euch auch noch ein paar Punkte ein, die ich jetzt noch nicht berücksichtige, weil sie mir nicht direkt ins Auge gefallen sind.

# Bereinigungsfunktion
putzibot <- function(text){
  tryCatch( { !require(textclean) }
            , warning = function(w) { print("Bitte installiere textclean.") })
  
  #if( !require(textclean) ){ stop("Bitte installiere textclean.")}
  x <- text
  
  yes_orig <- c("- bis", "- und", "- oder", "- bzw.", "- als", "- noch", " - ")
  yes_ers <- c("-#keep#bis", "-#keep#und", "-#keep#oder", "-#keep#bzw.", "-#keep#als", "-#keep#noch", "#keep#-#keep#")
  
  # Löschen von Punkten in Ordinalzahlen
  x <- gsub("((^| )[[:digit:]]{1,2})(\\.)", "\\1", x)
  
  # Löschen von Links
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

# Zählen von Großschreibung
n_gross <- function(text){
  x <- unlist(gregexpr("[A-Z]", text))
  if (x == -1) {
    return(0)
  } else {
    return(length(x))
  }
}

# Lesbarkeitsanalyse
klartext <- function(text, sprachmodell, ner_spacyr = FALSE, LSA = NULL, worthäufigkeit = NULL, zeig_mal_text = FALSE){
  
  # Zuweisung der Werte
  x <- text
  udmodel <- sprachmodell
  
  # Benötigte Funktion
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
  # Bspw. wenn Zwischenüberschriften ohne Punkt enden.
  # Hier wird vereinfacht nach großgeschriebenen Artikeln und Personalpronomen gesucht. 
  # Das ist natürlich nicht perfekt! 
  # Häufigster Fehler: Siezen wird fälschlicherweise als Satzende markiert (Sie = Personalpronomen vs. Sie = Siezen).
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
  #x2 ist der korrigierte Text, aufgesplittet in Sätze
  
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
                  n_word = sum(words), # zähle dann die 1er zusammen, um die Wortzahl pro Satz zu bekommen
                  upos = case_when(is.na(upos) ~ "ADP", # da udpipe bestimmte Appositionen (zum, dem) etwas merkwürdig markiert, vergib dann, wenn keine Wortart erkannt wird das Label ADP
                                   TRUE ~ upos)) %>% 
    ungroup()
  
  n_satzzahl <- d %>% group_by(id) %>% dplyr::summarize(satz = unique(sentence)) %>% ungroup() %>% select(satz) %>% nrow()
  
  n_wortzahl <-  d %>% filter(upos != "PUNCT") %>% dplyr::summarize(n_word = length(token)) %>% as.matrix() %>% as.vector()
  
  m_satzlaenge <- n_wortzahl/n_satzzahl %>% as.matrix() %>% as.vector()
  
  n_buchstabenzahl <- d %>% filter(upos != "PUNCT") %>% dplyr::summarize(n_buchstaben = sum(n_char)) %>% as.matrix() %>% as.vector()
  
  m_wortlaenge <- n_buchstabenzahl/n_wortzahl 
  
  wortzahl_gr6 <- d %>% filter(n_char >= 7) %>% dplyr::count() %>% as.matrix() %>% as.vector() # Anzahl der Wörter mit mehr als 6 Buchstaben
  
  perc_wortzahl_gr6 <- wortzahl_gr6/n_wortzahl*100 # Anteil der Wörter mit mehr als 6 Buchstaben
  
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
  # 5. Worthäufigkeit #
  #####################
  
  if (is.null(worthäufigkeit)) {
    anzahl_hk_bis_8_satz1 <- NA
    anteil_hk_bis_8_satz1 <- NA
  } else {
    LCC.de <- worthäufigkeit
    # Häufigstes Wort (der)
    freq.der <- LCC.de@words %>%  mutate(rank = num-100) %>% filter(rank >= 0) %>% filter(freq == max(freq)) %>% select(freq) %>% as.numeric()
    
    hk_data <- 
      LCC.de@words %>% 
      dplyr::mutate(leer = grepl(" ", word)) %>%
      filter(leer == FALSE) %>% 
      dplyr::mutate(rank = num-100, HK = round(log2(freq.der/freq), -.01)) %>% 
      filter(num > 62) %>% 
      select(word, HK)
    
    # num > 62 == Satzzeichen weg
    
    # Worthäufigkeit
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
  # 6. Kohäsion #
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



#############
# GermanTRC #
#############

germanfakenc_corpus <- readRDS("germanfakenc_corpus.rds")

docvars(germanfakenc_corpus, "ID_2") <- docvars(germanfakenc_corpus) %>% group_by(type, ID) %>% dplyr::mutate(n = c(1:length(type))) %>% ungroup() %>% dplyr::mutate(ID_2 = paste(ID, type, n, sep = "_")) %>% select(ID_2)

docnames(germanfakenc_corpus) <- docvars(germanfakenc_corpus)$ID_2

germanfakenc_corpus_red <- quanteda::corpus_subset(germanfakenc_corpus, type != "CRAWLED")
docvars(germanfakenc_corpus_red, "sammlung") <- "GermanTRC"

# Analyse
d_germanfakenc <- NULL
i <- NA
pb = txtProgressBar(min = 0, max = length(germanfakenc_corpus_red), initial = 0, style = 3) 

for (i in c(1:length(germanfakenc_corpus_red))) {
  doc <- docvars(germanfakenc_corpus_red)[i,] %>% select(ID_2, sammlung, Title)
  text <- paste(gsub(":|\\.|!|\\?", "", doc$Title), texts(germanfakenc_corpus_red)[i], sep = ". ")
  text <- putzibot(text)
  klar <- klartext(text = text, sprachmodell = udmodel, ner_spacyr = TRUE, LSA = dewak100k_lsa, worthäufigkeit = LCC.de, zeig_mal_text = FALSE)
  temp <- doc %>% cbind(klar)
  d_germanfakenc <- rbind(d_germanfakenc, temp)
  setTxtProgressBar(pb,i) # Dies ist ein Ladebalken, denn die Analyse der 500 Texte dauert ein bisschen. Solltet ihr beim ersten Versuch, die Schleife zu starten, eine Fehlermeldung bekommen, for-Schleife einfach nochmal neu starten
}

# Aus Neugier: Was zeigt sich beim LIX?
spick <-
  d_germanfakenc %>% 
  dplyr::mutate(art = case_when(grepl("FAKE", ID_2) ~ "Fake News",
                                TRUE ~ "True News")) 

spick %>% 
  group_by(art) %>% 
  tidycomm::describe(LIX)

m <- t.test(LIX ~ art, spick)
report::report(m)

spick %>% 
  dplyr::mutate(anteil_groß_titel = n_buchstaben_satz1_KAP/n_buchstaben_satz1*100) %>% 
  group_by(art) %>% 
  tidycomm::describe(anteil_groß_titel)

###############
# Corona-News #
###############

liste <- list.files(path = "Corona-News", recursive = TRUE, full.names = TRUE)

corona <- map_df(liste, ~ data_frame(text = readLines(.x)) %>% mutate(ID_2 = gsub("(.*)(T_|F_)(.*)(\\.txt)", "\\2\\3", .x)))

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

# Für diese Texte müssen wir noch die Titel extrahieren. Das bietet sich nach der Bereinigung an. Dadurch fliegen bei den Texten nämlich die (teilweise) oben anstehenden Links weg.

d_corona <- NULL
i <- NA
pb = txtProgressBar(min = 0, max = length(corona_corpus), initial = 0, style = 3) 

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
  
  klar <- klartext(text = text, sprachmodell = udmodel, ner_spacyr = TRUE, LSA = dewak100k_lsa, worthäufigkeit = LCC.de, zeig_mal_text = FALSE)
  temp <- doc %>% cbind(klar)
  d_corona <- rbind(d_corona, temp)
  setTxtProgressBar(pb,i)
}

##################################
# Zusammenführung der Ergebnisse #
##################################

daten_gesamt <- d_germanfakenc %>% rbind(d_corona) %>% rename(anteil_hk_bis_8_satz1 = n_wortzahl_satz1.1)

saveRDS(daten_gesamt, "~/Schreibtisch/Claudi Test R/PolKomm/verständlichkeitsanalyse_fake_und_true_komplett.rds")

spick <-
  daten_gesamt %>% 
  dplyr::mutate(art = case_when(grepl("FAKE|F_", ID_2) ~ "Fake News",
                                TRUE ~ "True News")) 

spick %>% 
  group_by(art) %>% 
  tidycomm::describe(LIX)

m <- t.test(LIX ~ art, spick)
report::report(m)

spick %>% 
  dplyr::mutate(anteil_groß_titel = n_buchstaben_satz1_KAP/n_buchstaben_satz1*100) %>% 
  group_by(art) %>% 
  tidycomm::describe(anteil_groß_titel)

spick_2 <- 
  spick %>% 
  filter(sammlung == "GermanTRC")

spick_2 %>% 
  group_by(art) %>% 
  tidycomm::describe(LIX)

m <- t.test(LIX ~ art, spick)
report::report(m)

spick_2 %>% 
  dplyr::mutate(anteil_groß_titel = n_buchstaben_satz1_KAP/n_buchstaben_satz1*100) %>% 
  group_by(art) %>% 
  tidycomm::describe(anteil_groß_titel)


############
# Übrigens #
############

# Man könnte auch mal schauen, ob es wirklich einen so großen Unterschied in den Ergebnissen macht, wenn wir die Texte nicht weiter bereinigen. Das habe ich jetzt aber noch nicht mit den Daten getestet.
# Es ginge am einfachsten mit koRpus.

tok_text <- koRpus::tokenize(x, format = "obj", lang = "de", ign.comp = TRUE)

LIX_koRpus <- koRpus::LIX(tok_text)
LIX_koRpus
