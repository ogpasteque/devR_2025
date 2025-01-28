# M7

rochelle_data <- resultats_pres2022[resultats_pres2022$Libelle == "La Rochelle", ]

col1 <- c("Hidalgo", "Jadot", "Melenchon")

col2 <- c(mean(rochelle_data$Part_Hidalgo), mean(rochelle_data$Part_Jadot), mean(rochelle_data$Part_Melenchon))

data <- data.frame(group=col1, value=col2)

ggplot(data= data, aes(x="", y=value, fill=group)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    geom_text(aes(label=round(value, digits=2)), position=position_stack(vjust=0.5), color="white", size=3.5) +
    scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157"))

# m8

    royan_data <- resultats_pres2022[resultats_pres2022$Libelle == "Royan", ]
voteRoyant_Candidats1_4 <- royan_data[, c("Code_BV", "Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse")]

reformat <- voteRoyant_Candidats1_4 %>% 
  pivot_longer(cols = c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse"), 
               names_to = 'candidats', 
               values_to = 'parts')

ggplot(data = reformat, aes(x = 2, y = parts, fill = candidats)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~Code_BV) +
  ggtitle("Parts de vote dans les BV de Royan") +
  theme(plot.title = element_text(family = "Trebuchet MS", face = "bold", size = 20, hjust = 0, color = "#555555")) +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  scale_fill_manual(values = c("#0a3895", "#b831f3", "#f33157", "#6091f6")) +
  xlim(0.5, 2.5) + 
  theme_void() +
  theme(plot.background = element_blank())





# Rapport R
```{r setup, include=FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## Parts de vote des principaux partis de gauche à l'échelle de la ville de La Rochelle
```{r echo=FALSE, message=FALSE, warning=FALSE}
library(readxl)
library(ggplot2)
resultats_pres2022 <- read_excel("C:/Users/Enzo/Desktop/LPSIG/Travail/cours_python/r/donnees/resultats_pres2022.xlsx")
voteLaRochelle <- resultats_pres2022 [resultats_pres2022$Libelle =='La Rochelle' & resultats_pres2022$Code_dep == '17' ,]

col1 <- c("Melenchon", "Jadot", "Hidalgo")
col2 <- c(mean(voteLaRochelle$Part_Melenchon), mean(voteLaRochelle$Part_Jadot), mean(voteLaRochelle$Part_Hidalgo))
data <- data.frame(group=col1, value=col2)

ggplot(data, aes(x="", y=value , fill=group)) + geom_bar(stat="identity", width=1) + geom_col() + coord_polar("y", start=0) +
geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) + scale_fill_manual(values = c("#ffadf8", "#0cb240", "#f03e18"))
```


# M10


data_filtered$moyennenote <- rowMeans(data_filtered[,c('Infos', 'Deplacement', 'Proprete', 'Agreable', 'Satisfaction', 'Services')], na.rm=TRUE)

stats_descriptives <- data_filtered %>%
    summarise(
        moyenne = mean(moyennenote, na.rm = TRUE),
        mediane = median(moyennenote, na.rm = TRUE),
        ecart_type = sd(moyennenote, na.rm = TRUE)
    )
print(stats_descriptives)


resultat_csv <- data_filtered %>% filter(moyennenote > 8.1)
write.csv2(resultat_csv, "C:/Users/Enzo/Desktop/LPSIG/Travail/cours_python/r/donnees/resultat.csv", row.names = FALSE)

data_long <- data_filtered %>%
  pivot_longer(cols = c("Infos", "Deplacement", "Proprete", "Agreable", "Satisfaction", "Services"), names_to = "type_note", values_to = "valeur_note")

ggplot(data_long, aes(x = type_note, y = valeur_note, fill = type_note)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Gare, scales = "free_x") +
  labs(title = "Notes par type pour chaque gare", x = "Type de note", y = "Note") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))