## Daten bereinigen
# Erstmal installieren wor die Pakete, die wir benutzen wollen.
# Immer, wenn wir an ein Problem stoßen, für das wir ein 
# weiteres Paket brauchen, gehen wir hierher zurück, fügen es hinu
# und führen diese Zeile nochmals aus
#library(needs)
needs(tidyverse, magrittr, stringr)

Bundesland_ID <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16")
Bundesland_Name <- c("Schleswig-Holstein (SH)", "Hamburg (HH)", "Niedersachsen (NI)", "Bremen (HB)", "Nordrhein-Westfalen (NW)", "Hessen (HE)", "Rheinland-Pfalz (RP)", "Baden-Württemberg (BW)", "Bayern (BY)", "Saarland (SL)", "Berlin (BE)", "Brandenburg (BB)", "Mecklenburg-Vorpommern (MV)", "Sachsen (SN)", "Sachsen-Anhalt (ST)", "Thüringen (TH)")
Bundeslaender <- data.frame(Bundesland_ID, Bundesland_Name, stringsAsFactors = FALSE)

# Im Datenordner liegt ein Datensatz namens "ew19_untidy.csv"
#colnames(ew19_untidy)[3] <- "bundesland ---- so ändern wir den Namen der Spalte in "bundesland"
#by hilft danach zu verstehen, welche Spalte (Name), wir mit welcher Spalte (Name) von dem anderen Datensatz mergen wollen
#write.csv(lookup, "Daten/ew19_namen.csv", row.names = F) #row.names = F, speichert nicht die Zeile


ew19_untidy <- read.csv("Daten/ew19_untidy.csv", sep=";", skip = 2) %>% 
  filter(!(Nr %in% NA)) %>% 
  filter(Nr > 100) %>% 
  mutate(gehört.zu = str_pad(gehört.zu, width=2, side="left", pad="0")) %>% 
  left_join(., Bundeslaender, by=c("gehört.zu" = "Bundesland_ID")) %>% 
  separate(Bundesland_Name, into=c("Bundeslaend_Name", "Bundesland_ID"), " ") %>% 
  mutate(Bundesland_ID = gsub("\\(|\\)", "", Bundesland_ID))

#lookup <- data.frame(Spaltennamen = colnames(ew19_untidy))

lookup <- read.csv("Daten/ew19_namen.csv", sep=",")

names(ew19_untidy) <- lookup$Spaltennamen_neu[match(names(ew19_untidy), lookup$Spaltennamen)]
