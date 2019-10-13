## Daten bereinigen

needs(readxl, tidyverse, magrittr)

data1 <- read.csv2("Daten/merkmale.csv")
data1$StadtID <- as.character(data1$StadtID)
#data1 %<>% mutate(StadtID 0 as.character(StadtID))

data2 <- read_xlsx("Daten/daten_staedte.xlsx")

data1 %<>%
  mutate(StadtID = gsub("ID ", "", StadtID))

  data1$StadtID <- as.numeric(data1$StadtID)

  data_merge <- data1 %>% 
    left_join(., data2, by=c("StadtID" = "KRS"))

  bundeslaender <- read_csv2("Daten/ew19.csv") %>% select(1:3)  

  data_merge %<>%
    left_join(., bundeslaender, by=c("StadtID" = "Wahlgebiet_ID"))
  
  data_analysis <- data_merge %>% 
    select(StadtID, Wahlgebiet_Name, Wahlgebiet_Name3, Medianeinkommen, Medianeinkommen.Frauen, Medianeinkommen.Männer)
  
   ###### Wo verdienen Frauen mehr als Männer (2 Möglichkeiten zur Lösung zu kommen, einmal mit for-loop und einmal mit pipen)

  for(i in 1:nrow(data_analysis)){
  if(data_analysis$Medianeinkommen.Frauen[i] > data_analysis$Medianeinkommen.Männer[i]) {
    print(data_analysis$Wahlgebiet_Name[i])}
}
  
  data_analysis %>% 
  filter(Medianeinkommen.Frauen > Medianeinkommen.Männer)
  
  ##### Wo ist die Dönerdichte am höchsten
  
 data_merge[order(-data_merge$Döner_1000),] %>% 
   select(Wahlgebiet_Name, Döner_1000)
 
 data_merge %>% 
  arrange(desc(Döner_1000)) %>% 
   select(Wahlgebiet_Name, Döner_1000)

 #### Welche 10 Städte haben die meisten Delikte mit Rauschmitteln?
 
 spaltennamen <- data.frame(colnames(data_merge))

 
 data_merge %>% 
   arrange(desc(alt_Rauschgift)) %>% 
   slice(1:10) %>% select(c(Wahlgebiet_Name, alt_Rauschgift))
 
 #### Wie viele Ufos werden in Deutschland insgesamt gesichtet?
  
 data_merge %>%
   summarise(sum(Ufos * Einwohner))

#### Wie hoch ist die Nesthockerquote von 30 bis 40 Jährigen in eurer Heimatstadt?
 
 data_merge$`Nesthocker_beide_30 bis 40`[data_merge$Wahlgebiet_Name %in% "Köln, Stadt"]

 #### Wie hoch ist die "Einbrüche je Einwohner"-Quote je Bundesland?
 
data_merge %>%
   group_by(Wahlgebiet_Name3) %>%
   summarize(result = sum(Wohnungseinbruch*100) / sum(Einwohner))%>%
   arrange(desc(result))
 
 
  
  # Zeig alle StadtIDs, die in Datensatz 1 sind aber nicht in Datensatz 2
  bundeslaender$Wahlgebiet_ID[!(bundeslaender$Wahlgebiet_ID %in% data_merge$StadtID)]
  
  # Zeig alle KRSs, die in Datensatz 2 sind aber nicht in Datensatz 1
  data_merge$StadtID[!(data_merge$StadtID %in% bundeslaender$Wahlgebiet_ID)]

  data_merge %>%
    select(-Wahlgebiet_Name, -Wahlgebiet_Name3) %>%
    filter(StadtID %in% c(3152, 3156)) 
  ...
    
    
  
  
  
  
  
  
  
  
  