# Traitements statistiques des réponses au questionnaire -------------------------------------

# Encodage du fichier : UTF-8


## Importation des packages -----
library(here) # pour chemin valable partoutlibrary(tidyverse)
library(magrittr)
library(readr) # ouverture csv
library(readxl) # ouverture xlm
library(sjlabelled) # labels avec la description des variables
library(qessmasteR) # package disponible sur github : https://github.com/jrdavalos/qessmasteR
library(questionr)
library(expss) #export avec label + if_na

library(FactoMineR) # AGD
library(kableExtra) # tableaux
library(stargazer) # tableaux de régression
library(xtable) # autres tableaux
library(ggplot2) # visualisation 
library(ggeffects) # idem
library(ggrepel) # idem
library(RColorBrewer) # idem
library(extrafont) # pour le time new roman
library(factoextra)# dendogramme

## Chapitre 1 ----

# La cartographie est sur un fichier dédié

### Tableau 1.2.1 - non-réponses
# tableau
pages <- quest$lastpage %>% # dernière page ouverte par l'enquêté
  table(useNA = "ifany") %>% # tri à plat
  as.data.frame %>% 
  select(Freq) %>% 
  mutate(`Fréquence (%)` = Freq / sum(Freq) * 100, # proportion
         `Fréquence cumulée (%)` = cumsum(Freq) / sum(Freq) * 100) # proportion cumulée

pages[8,] <- c(sum(pages$Freq), sum(pages$`Fréquence (%)`), 100) # ajout de la somme

### noms des lignes 
rownames(pages) <- c("Demande de consentement","Socio-démographique", "Socialisation politique", "Pratiques politique et électorales", "Engagement à la France insoumise","Politique et idéologie", "Remarques", "Total")

pages %<>%
  rownames_to_column %>% 
  tibble
### et colonnes
names(pages)[1:2] <- c("Dernière page validée", "Effectif")

# lecture
note_pages <- "\\\\textsc{Note}: Valider une page n'implique pas nécessairement de s'y arrêter. Ainsi, il y a 19 personnes qui n'ont validé que la demande de consentement mais ont rempli la première page de sorte à ce qu'il y ait des informations traitables\\\\footnote{Précisons tout de même que certains individus dont les réponses étaient incohérentes ou les doublons ont été enlevés de la liste malgré le fait qu'ils avaient répondu aux questions qui nous semblaient d'intérêt, il est d'ailleurs probable que ces incohérences soient la raison de l'arrêt du questionnaire qui n'a pas accepté leurs réponses -- par exemple si les enquêtés mettaient plusieurs professions.}."

# mise en forme
pages %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Répartition des réponses au questionnaire selon la dernière page validée",
        label = "pages", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2,note_pages),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm")


### Tableau 1.2.2 - questionnaires complets ----
# tableau
implic_quest <- quest %$% 
  table(dipl_rec2,lastpage %in% 6:7)

# lecture
test_implic_quest <- paste("\\\\textsc{Tests}: $p(\\\\chi^2)=", round(chisq.test(implic_quest)$p.value, 2), "$\n")
note_implic_quest <- "\\\\textsc{Lecture}: 83\\\\% des titulaires d'un baccalauréat sont arrivés jusqu'à la dernière page (ou ont validé l'avant dernière) du questionnaire."

# mise en forme
quest %>% 
  mutate(lastpageT = lastpage %in% 6:7) %>% 
  lprop_pctot(dipl_rec2,lastpageT) %>% 
  slice(c(4,1,2,5,6,7,3,8)) %>%
  rename(`Complet` = `TRUE`,
         `Incomplet` = `FALSE`) %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "rrrr", digits = c(1,1,0,1),
        caption = "Part de questionnaires complets en fonction du diplôme", label = "implic_quest") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2, test_implic_quest, note_implic_quest),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "4.5cm") 


### Tableau 1.2.4 - taux de NA ----
# construction du tableau 
na <- questions %>% 
  slice(1:185) %>% 
  filter(bloc > 1) %>% 
  select(questions, pct_NA) %>% 
  filter(pct_NA > 0,
         !str_detect(questions,"engagé-e"), # filtre engage parti/synd
         !str_detect(questions,"Y avez-vous"), # filtre engage parti/synd
         !str_detect(questions,"licenciement"), # filtre
         !str_detect(questions,"deuxième parent"), # inutile
         !str_detect(questions,"activité"), # retraités
         !str_detect(questions,"influer"), # filtre connaitelu
         !str_detect(questions,"Laquelle"), !str_detect(questions,"Lequel"), !str_detect(questions,"genre"), !str_detect(questions,"Commentaire"), !str_detect(questions,"Autre")) %>% # toujours moins
  mutate(questions = str_replace(questions,"\\[(.*?)\\]","")) %>% 
  group_by(questions) %>%
  summarise(`Taux de NR (%)` = min(pct_NA)) %>%
  arrange(`Taux de NR (%)`) %>% 
  slice_tail(n = 12) %>%
  mutate(questions = str_replace_all(questions, "é","é"),
         questions = str_replace_all(questions, "ê","è"),
         questions = str_replace_all(questions, "â","à"),
         questions = paste(questions, c("[2021/2022]","","[types de scrutins]",rep("",9)))) %>% 
  rename(Question = questions) %>% 
  xtable(caption = "Questions ayant les plus forts taux de non-réponses (hors abandon du questionnaire)", label = "tab:na", align = c("","|m{13.5cm}","|>{\\raggedleft\\arraybackslash}m{1.5cm}|"), digits = c(0,0,1))

# mise en forme
print(na, caption.placement = "top", include.rownames = FALSE, size = "fontsize{9}{12}\\selectfont", hline.after = seq(-1,nrow(na),1))


### Tableau 1.2.5 - Commentaires ----
# tableau
quest %<>% # on recode tout ce qui est inférieur au Bac
  mutate(dipl_rec2 = case_when(dipl %in% c("Sans diplôme", "BEPC ou équivalent", "BEP/CAP ou équivalent") ~ "Inférieur au Bac", 
                               T ~ dipl))
rem_dipl <- quest %>% 
  filter(lastpage > 5) %>% # pages concernées
  mutate(remarque = !(is.na(remarque_mili) & is.na(remarque_questio))) %>% # ceux qui ont fait au moins une remarque
  lprop_pctot(dipl_rec2, remarque) %>% # tableau
  slice(c(4,1,2,5,6,7,3,8)) %>% # reordonne 
  rename(`Commentaire` = `TRUE`, # change les noms de colonne
         `Non réponse` = `FALSE`)

# lecture
test_rem_dipl <- quest %>% 
  filter(lastpage > 5) %$% 
  table(dipl_rec2, !(is.na(remarque_mili) & is.na(remarque_questio)))
test_rem_dipl <- paste("\\\\textsc{Tests}: $p(\\\\chi^2)=", round(chisq.test(test_rem_dipl)$p.value, 2), "$, $V=",round(cramer.v(test_rem_dipl), 2), "$\n")
note_rem_dipl <- paste0("\\\\textsc{Lecture}: ", round(100 * rem_dipl[1,2]/(rem_dipl[1,1] + rem_dipl[1,2]), 1), "\\\\% des titulaires d'un baccalauréat ont répondu à au moins une des deux questions proposant au répondant de commenter l'enquête ou d'apporter plus d'information.")

# mise en forme
quest %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "rrrr", digits = c(1,1,0,1),
        caption = "Part de commentaires remplis en fonction du diplôme (hors abandon du questionnaire)", label = "remarque_dipl") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2, test_rem_dipl, note_rem_dipl),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "4.5cm")

## Chapitre 2 ----
### Tableau 2.1.1 - comparaison PCS pop générale et échantillon ----

# Création du tableau en pop générale (agrégation enquête emploi, disponible sur le site de l'Insee)
demo <- read_excel(here("Donnees","demo.xlsx"), range = "N5:P52") %>% # excel moche du coup on garde un tableau 
  mutate(names = read_excel(here("Donnees","demo.xlsx"), range = "A5:A52") %>% 
           as.matrix %>% as.vector) # et on ajoute les pcs après

# Création des catégories agrégées en fonction de la question du questionnaire 
demo %<>%
  filter(names %in% c("Agriculteurs exploitants", "Artisans, commerçants, chefs d'entreprise",
                      "Cadres et professions intellectuelles supérieures",
                      "Professions libérales","Cadres de la fonction publique",
                      "Professeurs, professions scientifiques",
                      "Professions intermédiaires",
                      "Professeurs des écoles, instituteurs et assimilés","Employés","Ouvriers",
                      "Anciennes professions intermédiaires",
                      "Anciens employés","Anciens ouvriers",
                      "Chômeurs n'ayant jamais travaillé","Élèves ou étudiants",
                      "Anciens agriculteurs exploitants",
                      "Anciens artisans, commerçants, chefs d'entreprises",
                      "Anciens cadres, professions intellectuelles supérieures")) %>%
  select(names,Ensemble) %>% 
  bind_rows(tibble(names = c("Cadres du privé", "Autres professions intermédiaires", "Inactifs", "Retraités", "Chômeurs", "fonction publique", "privé"),
                   Ensemble = c(sum(demo %>% 
                                      filter(names %in% c("Cadres administratifs et commerciaux d'entreprise","Ingénieurs et cadres techniques d'entreprise")) %>% 
                                      select(Ensemble)) %>% 
                                  as.numeric, # addition tous les cadres du privé
                                demo %>% 
                                  filter(names == "Professions intermédiaires") %>% 
                                  select(Ensemble) - 
                                  demo %>% 
                                  filter(names == "Professeurs des écoles, instituteurs et assimilés") %>% 
                                  select(Ensemble) %>% 
                                  as.numeric, # prof intermédiaires sans compter les professeurs/instit
                                sum(demo %>% 
                                      filter(str_detect(names, "Ancien") |
                                               names == "Personnes n’ayant jamais travaillé") %>%
                                      select(Ensemble)), # inactifs
                                round(100 * 16160000 / 66980000, 1), # retraités non comptés donc ajouts manuel pour la même année (recensement 2017)
                                NA_integer_, # chomeurs non comptés non plus
                                sum(demo %>% 
                                      filter(str_detect(names, "Employés civils") |
                                               str_detect(names, "militaires")) %>% 
                                      select(Ensemble)),# somme employés fonction publique 
                                sum(demo %>% 
                                      filter(str_detect(names, "Employés administratifs ") | 
                                               str_detect(names, "Employés de commerce") |
                                               str_detect(names, "Personnels des services")) %>% 
                                      select(Ensemble)) # somme employés privé
                                ) %>% 
                     unlist))

demo %<>% # ajout de la ligne autres inactifs puisque l'on a l'ensemble des inactifs (cf infra)
  bind_rows(tibble(names = "Autres inactifs",
                   Ensemble = demo %>% 
                     filter(names == "Inactifs") %>% 
                     select(Ensemble) %>% 
                     as.numeric -
                     sum(demo %>% 
                           filter(names %in% c("Retraités","Chômeurs n'ayant jamais travaillé","Élèves ou étudiants")) %>% 
                           select(Ensemble)))) %>% 
  filter(names != "Chômeurs n'ayant jamais travaillé", !str_detect(names, "Ancien")) %>% # on enlève ces catégories
  slice(c(1:5,12,6:8,13,9,17,18,10,16,14,15,11,19)) # réordonne 
# demo présent la 3e colonne du tableau 2.1.1

# Table des enquêtés
pcs1 <- quest2 %>% select(pcs) %>% table() # effectifs (quest = bdd sans dom-tom, non communicable)
pcs2 <- pcs1 %>% prop.table() %>% as.data.frame() # proportions

pcs1 %<>% # transformation en data-frame
  as.data.frame() %>% 
  rename(N = `Freq`)

# pcs1 est alors :
#                                                   .   N
# 1                          Agriculteurs exploitants   3
# 2         Artisans, commerçants, chefs d'entreprise  31
# 3                                   Autres inactifs  16
# 4                 Autres professions intermédiaires  43
# 5                                            Cadres 117
# 6                                          Chômeurs  27
# 7                               Élèves ou étudiants  72
# 8                                          Employés  91
# 9                                          Ouvriers  13
# 10 Professeurs des écoles, instituteurs et assimilé  27
# 11           Professeurs, professions scientifiques  88
# 12                            Professions libérales  27
# 13                                        Retraités 169

# pcs2 : 
#                                                   .        Freq
# 1                          Agriculteurs exploitants 0.004143646
# 2         Artisans, commerçants, chefs d'entreprise 0.042817680
# 3                                   Autres inactifs 0.022099448
# 4                 Autres professions intermédiaires 0.059392265
# 5                                            Cadres 0.161602210
# 6                                          Chômeurs 0.037292818
# 7                               Élèves ou étudiants 0.099447514
# 8                                          Employés 0.125690608
# 9                                          Ouvriers 0.017955801
# 10 Professeurs des écoles, instituteurs et assimilé 0.037292818
# 11           Professeurs, professions scientifiques 0.121546961
# 12                            Professions libérales 0.037292818
# 13                                        Retraités 0.233425414


pcs <- pcs2 %>% # on colle
  bind_cols(pcs1) %>% 
  bind_rows(tibble(Var1 = c("Cadres et professions intellectuelles supérieures",
                            "Professions intermédiaires", "Inactifs", "Cadres de la fonction publique",
                            "Cadres du privé", "fonction publique", "privé"),
                   Freq = c(sum(pcs2$Freq[c(5,11,12)]), # ajout des freq des CPIS
                            sum(pcs2$Freq[c(4,10)]), # ajout des freq de l'ensemble des PI
                            sum(pcs2$Freq[c(3,7,13)]), # ajout de l'ensemble des inactifs 
                            pcs2$Freq[5] * prop.table(table(quest2$sect[quest2$pcs == "Cadres"]))[4],
                            # différence selon le secteur pour cadres
                            pcs2$Freq[5] * sum(prop.table(table(quest2$sect[quest2$pcs == "Cadres"]))[-4]),
                            pcs2$Freq[8] * prop.table(table(quest2$sect[quest2$pcs == "Employés"]))[4],
                            # différence selon le secteur pour employés
                            pcs2$Freq[8] * sum(prop.table(table(quest2$sect[quest2$pcs == "Employés"]))[-4])),
                   N = c(sum(pcs1$N[c(5,11,12)]), # idem pour effectifs
                         sum(pcs1$N[c(4,10)]),
                         sum(pcs1$N[c(3,7,13)]),
                         table(quest2$sect[quest2$pcs == "Cadres"])[4],
                         sum(table(quest2$sect[quest2$pcs == "Cadres"])[-4]),
                         table(quest2$sect[quest2$pcs == "Employés"])[4],
                         sum(table(quest2$sect[quest2$pcs == "Employés"])[-4])))) %>%
  
  mutate(Freq = round(100 * Freq, 1)) %>% # arrondi
  slice(c(1,2,14,12,17,18,11,15,10,4,8,19,20,9,6,16,13,7,3)) # même ordre que pour demo
# pcs donne les deux 1eres colonnes du tableau 2.1.1

# fusion des deux tableaux 
pcs %<>%
  bind_cols(demo) %>%
  select(names, N, Freq, Ensemble) %>% 
  mutate(names = if_else(names %in% c("Agriculteurs exploitants","Artisans, commerçants, chefs d'entreprise", 
                                      "Cadres et professions intellectuelles supérieures","Professions intermédiaires",
                                      "Employés","Ouvriers","Inactifs", "Chômeurs", "Autres professions intermédiaires"),
                         names, paste("dont", str_to_lower(names))),
         # ajout de "dont" devant les sous catégories
         Ensemble = if_else(is.na(Ensemble), "--", as.character(Ensemble)))

# notes du tableau
champ2 <- "\\\\textsc{Champ}: Adhérents à la France insoumise en 2021\n"
note_compa <- "\\\\textsc{Note}: Les données de population générales sont issues de l'enquête Emploi et du recensement 2017 pour les retraités (dans les deux cas, personnes de 15 ans et plus). Les chômeurs parmi les insoumis sont ici notés car on n'a pas leur dernière profession. À noter que le taux de chômage en 2017 était de 9.4\\\\%.\n"
lecture_compa <- paste("\\\\textsc{Lecture}: $", pcs$Freq[6], "$\\\\% des insoumis interrogés sont professeurs ou de profession scientifique alors que dans la population générale cette proportion est de $", demo$Ensemble[6],"$\\\\%.") 

# mise en forme pour kable
pcs %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrr", digits = c(1,1,1),
        caption = "Composition sociale à la FI et dans la population française", label = "compa", col.names = c("Profession et catégorie socio-professionnelle", "FI (eff)", "FI (%)", "France en 2017 (%)")) %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 8) %>%
  add_indent(which(str_detect(pcs$names,"dont") | 
                     str_detect(pcs$names, "Autres"))) %>% # ajoute indentation là où il y a "dont" ou autres PI
  footnote(general = c(champ2, note_compa, lecture_compa),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "10cm")



### Tableau 2.1.2 - comparaison diplômes ----
dipl <- quest2$dipl %>%
  table(useNA = "ifany") %>%
  prop.table() %>% # proportion des diplômes enquêtés
  data.frame() %>%
  rename(Diplôme = ".", FI = Freq) %>%
  slice(c(9,3:1,4,6,7,8,5,10)) %>%
  mutate(Diplôme = c(rep("BEP/CAP ou inférieur", 3), Diplôme[4:10] %>% as.vector())) %>% # combine sans diplôme, BEPC et BEP
  group_by(Diplôme) %>% # regroupe
  summarise(FI = sum(FI)) %>% # somme 
  bind_rows(tibble(Diplôme = "Supérieur à Bac+2", FI = sum(.[4:7,2]))) %>% # somme tous les bac +2 et +
  slice(c(2,1,3,9,5,6,7,4,8)) %>% # réordonne
  mutate(Diplôme = ifelse(Diplôme %in% c("Licence/licence professionnelle","Maîtrise/Master 1","Master et assimilé","Doctorat, HDR"), paste("dont", Diplôme), Diplôme),
         # idem que pour les pcs : "dont" devant les bac+2
         FI = round(100 * FI, 1),
         `France (2020)` = c(42.3,17.8,14.8,24.8,24.8,24.8,24.8,24.8,0.2)) %>% # enquête emploi 2020 (Insee), la répétition est corrigée après
  if_na("Non réponse") # Remplace le NA

# légende
note_dipl <- "\\\\textsc{Note}: Les données de population générales sont issues de l'enquête Emploi (personnes de 15 ans et plus). Le détail des diplômes supérieurs à Bac+2 n'y est pas précisé.\n"
lecture_dipl <- paste("\\\\textsc{Lecture}: $", dipl$FI[4], "$\\\\% des insoumis interrogés ont un diplôme supérieur à Bac+2 alors que dans la population générale cette proportion est de $", dipl$`France (2020)`[4],"$\\\\%.")

# mise en forme
dipl %>%
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrr", digits = c(1,1,1),
        caption = "Plus haut niveau de diplôme obtenu à la FI et dans la population française (\\%)", label = "compa_dipl") %>%
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2, note_dipl, lecture_dipl),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>%
  column_spec(1, width = "10cm") %>%
  add_indent(which(str_detect(dipl$Diplôme, "dont"))) %>% # comme supra
  collapse_rows(columns = 3, valign = "top", latex_hline = "none") # ne marche que sur LaTeX (combine tous les 24.8 répétés)


### Figure 2.1.1 - âge ----
quest2 %>%
  ggplot(aes(age)) +
  geom_histogram(binwidth = 2) +
  xlab("Age") +
  ylab("Effectifs") +
  scale_x_continuous(breaks = seq(20, 80, by = 10)) + 
  geom_vline(xintercept = summary(quest2$age)[c(2,3,5)], colour = "brown") +# ajout des droites rouges
  theme_minimal() +
  theme(text = element_text(family = "Times", size = 10))

### Tableau 2.1.3  -diplome x age ----
agecat_dipl <- quest2 %>%
  filter(pcs != "Élèves ou étudiants") %>% # enlève les étudiants
  mutate(agecat = if_else(agecat %in% c("16-25 ans", "25-35 ans"), "16-35 ans", agecat)) %>% # regroupement
  lprop_pctot(agecat, dipl_rec)

test_agecat_dipl <- quest2 %>% 
  filter(pcs != "Élèves ou étudiants") %$% 
  table(agecat, classes_soc) # pour les tests de chi2 et v de cramer 

# légende
test_agecat_dipl <- paste("\\\\textsc{Tests}: $p(\\\\chi^2)=", format(chisq.test(test_agecat_dipl)$p.value, digit = 2), "$, $V=",round(cramer.v(test_agecat_dipl), 2), "$\n")
lect_agecat_dipl <- paste0("\\\\textsc{Lecture}: ", round(100 * agecat_dipl[1,3]/sum(agecat_dipl[1,]), 1), "\\\\% des 16-35 ans de la FI sans compter les étudiants sont titulaires d'au moins un Master 1 ou équivalent.\n")
note_agecat_dipl <- "\\\\textsc{Note}: Ici comme il n'y a pas d'étudiants, on a fusionné les 16-25 ans et les 25-35 ans, les premiers n'étant que neuf."

# mise en forme
agecat_dipl %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "rrrr", digits = 1,
        caption = "Diplôme en fonction de la catégorie d'âge (\\% en ligne, hors étudiants)", label = "agecat_dipl") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2, test_agecat_dipl, lect_agecat_dipl, note_agecat_dipl),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "1.4cm")

### Tableau 2.1.4  -diplome x pcs ----
agecat_pcs <- quest2 %>% 
  filter(pcs != "Élèves ou étudiants") %>% 
  lprop_pctot(agecat, classes_soc) %>% 
  select(3,1,2,4:5)

test_agecat_pcs <- quest2 %>% 
  filter(pcs != "Élèves ou étudiants") %$% 
  table(agecat, classes_soc) # pour les tests de chi2 et v de cramer 

# légende
test_agecat_pcs <- paste("\\\\textsc{Tests}: $p(\\\\chi^2)=", format(chisq.test(test_agecat_pcs)$p.value, digit = 2), "$, $V=",round(cramer.v(test_agecat_pcs), 2), "$\n")
lect_agecat_pcs <- paste0("\\\\textsc{Lecture}: ", agecat_pcs[1,3], "\\\\% des 16-35 ans de la FI sont d'une CS moyenne supérieure\n")

# mise en forme
agecat_pcs %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "rrrr", digits = 1,
        caption = "Catégorie sociale en fonction de la catégorie d'âge (\\% en ligne)", label = "agecat_pcs") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2, test_agecat_pcs, lect_agecat_pcs),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "1.4cm")

### Tableau 2.2.5 - modèles logit ----
# regroupement des modalités
quest2 %<>%
  mutate(genre = factor(genre, levels = c("Homme", "Femme")),
         engagelfi_jlm2 = factor(engagelfi_jlm2, levels = c("Pas important","Important")),
         engagelfi_droits2 = factor(engagelfi_droits2, levels = c("Pas important","Important")),
         engagelfi_carriere2 = factor(engagelfi_carriere2, levels = c("Pas important","Important")),
         engagelfi_fam2 = factor(engagelfi_fam2, levels = c("Pas important","Important")),
         engagelfi_rencontre2 = factor(engagelfi_rencontre2, levels = c("Pas important","Important")),
         engagelfi_souple2 = factor(engagelfi_souple2, levels = c("Pas important","Important")),
         agecat_rec_f = factor(agecat_rec, levels = levels(factor(agecat_rec))[c(2,1,3,4)]),
         dipl_par_f = factor(dipl_par, levels = levels(factor(dipl_par))[c(1,5,2:4)]))

# modeles + modeles vides pour test chi2 
jlm <- glm(engagelfi_jlm2 ~ genre + agecat_rec_f + dipl_rec + engage + dipl_par_f + engage_par, binomial, data = quest2)

jlm_null <-  glm(engagelfi_jlm2 ~ 1, binomial, data = quest2, subset = !is.na(genre) & !is.na(agecat_rec_f) & !is.na(dipl_rec) & !is.na(engage) &!is.na(dipl_par_f) &!is.na(engage_par))

droits <- glm(engagelfi_droits2 ~ genre + agecat_rec_f + dipl_rec + engage + dipl_par_f + engage_par, binomial, data = quest2)

droits_null <-  glm(engagelfi_droits2 ~ 1, binomial, data = quest2, subset = !is.na(genre) & !is.na(agecat_rec_f) & !is.na(dipl_rec) & !is.na(engage) &!is.na(dipl_par_f) &!is.na(engage_par))

carriere <- glm(engagelfi_carriere2 ~ genre + agecat_rec_f + dipl_rec + engage + dipl_par_f + engage_par, binomial, data = quest2)

carriere_null <-  glm(engagelfi_carriere2 ~ 1, binomial, data = quest2, subset = !is.na(genre) & !is.na(agecat_rec_f) & !is.na(dipl_rec) & !is.na(engage) &!is.na(dipl_par_f) &!is.na(engage_par))

fam <- glm(engagelfi_fam2 ~ genre + agecat_rec_f + dipl_rec + engage + dipl_par_f + engage_par, binomial, data = quest2)

fam_null <-  glm(engagelfi_fam2 ~ 1, binomial, data = quest2, subset = !is.na(genre) & !is.na(agecat_rec_f) & !is.na(dipl_rec) & !is.na(engage) &!is.na(dipl_par_f) &!is.na(engage_par))

rencontre <- glm(engagelfi_rencontre2 ~ genre + agecat_rec_f + dipl_rec + engage + dipl_par_f + engage_par, binomial, data = quest2)

rencontre_null <-  glm(engagelfi_rencontre2 ~ 1, binomial, data = quest2, subset = !is.na(genre) & !is.na(agecat_rec_f) & !is.na(dipl_rec) & !is.na(engage) &!is.na(dipl_par_f) &!is.na(engage_par))

souple <- glm(engagelfi_souple2 ~ genre + agecat_rec_f + dipl_rec + engage + dipl_par_f + engage_par, binomial, data = quest2)

souple_null <-  glm(engagelfi_souple2 ~ 1, binomial, data = quest2, subset = !is.na(genre) & !is.na(agecat_rec_f) & !is.na(dipl_rec) & !is.na(engage) &!is.na(dipl_par_f) &!is.na(engage_par))


# mise en forme des modèles
N_models <- list(c("N", # récupère les effectifs
                   length(jlm$fitted.values),
                   length(droits$fitted.values),
                   length(carriere$fitted.values),
                   length(fam$fitted.values),
                   length(rencontre$fitted.values),
                   length(souple$fitted.values)),
                 c("$p(\\chi^2)$", #récupère les chi2 en comparant les modèles avec les modèles y = 1
                   format(anova(jlm_null,jlm, test = "Chisq")[2,5], digits = 2),
                   format(anova(droits_null,droits, test = "Chisq")[2,5], digits = 2),
                   format(anova(carriere_null,carriere, test = "Chisq")[2,5], digits = 2),
                   format(anova(fam_null,fam, test = "Chisq")[2,5], digits = 2),
                   format(anova(rencontre_null,rencontre, test = "Chisq")[2,5], digits = 2),
                   format(anova(souple_null,souple, test = "Chisq")[2,5], digits = 2)))

labs_tot <- rownames(jlm$R) %>% # change les étiquettes pour le tableau (jlm étant le 1er modèle indiqué)
  str_replace_all("genre", "") %>% 
  str_replace_all("\\[","") %>% 
  str_replace_all("\\)","") %>% 
  str_replace_all("\\(","") %>% 
  str_replace_all("Intercept","Constante") %>% 
  str_replace_all(",","-") %>% 
  str_replace_all("agecat_rec_f","Age: ") %>% 
  str_replace_all("dipl_rec","") %>% 
  str_replace_all("engageOui","Engagé") %>% 
  str_replace_all("dipl_par_f","Parents: ") %>% 
  str_replace_all("engage_parOui","Parents engagés") %>% 
  str_replace_all("supérieures","sup.") %>% 
  str_replace_all(" ou équivalent","/CEP") 

models <- c("JLM","Lutte", "Carrière","Famille","Sociabilité","Souplesse orga") # noms des modèles

# légende
champ <- "\\textsc{Champ}: Adhérents à la France insoumise en 2021"
note_motifs <- "\\multirow{1}{0.8\\textwidth}{\\textsc{Note}: Les modèles renvoient dans l'ordre à l'importance de: (1) la personnalité de Jean-Luc Mélenchon, (2) la lutte pour ses propres droits, (3) pouvoir \\enquote{vivre de son engagement}, (4) la poursuite de la tradition familiale, (5) rencontrer des personnes ayant des points communs et (6) la souplesse du mouvement vis-à-vis du parti traditionnel. Les tests du $\\chi^2$ renvoient à une analyse de la variance entre un modèle sans variables explicatives et le modèle considéré.}"
lecture_motifs <- paste0("\\multirow{1}{0.8\\textwidth}{\\textsc{Lecture}: les femmes ont un logit de $", round(droits$coefficients[2], 3), "$ concernant la lutter pour leurs propres droits. Autrement dit, elles ont $e^{", round(droits$coefficients[2], 3), "} = ", round(exp(droits$coefficients[2]),1), "$ fois plus de chances que les hommes de déclarer que le fait de lutter pour leurs droits a joué un rôle très important ou assez important dans leur engagement à la FI.}")

# tableau
stargazer(jlm,droits,carriere,fam,rencontre,souple,
          title = "Facteurs jugés importants pour s'engager à la FI (modèles logit)",
          style = "asr",
          column.sep.width = "1pt",
          covariate.labels = labs_tot,
          # column.labels = "",
          dep.var.labels = models,
          align = TRUE,
          intercept.top = TRUE,
          intercept.bottom = FALSE,
          font.size = "footnotesize", 
          label = "tab:motifs",
          model.numbers = TRUE,
          keep.stat = c("aic","rsq"),
          add.lines = N_models,
          table.placement = "!hp",
          notes = c(champ,note_motifs,rep("",3),lecture_motifs),
          float.env = "sidewaystable")

### Tableau 2.3.6 - engagements ----
# lecture
lecture_orgas <- paste0(" \\\\textsc{Lecture}: ", round(100 * prop.table(table(quest2$engagesynd))[2], 1), "\\\\% des militants insoumis déclarent avoir adhéré ou adhérer à un syndicat.")

# mise en forme du tableau
prop.table(table(quest2$engageparti)) %>% 
  bind_rows(prop.table(table(quest2$engagesynd)), #colle tous les engagements triés à plat
            prop.table(table(quest2$engagecoll)),
            prop.table(table(quest2$engageasso)),
            quest2 %$% 
              table(engageparti == "Oui" | engagesynd == "Oui"| engagecoll == "Oui"| engageasso == "Oui") %>%
              prop.table() %>% 
              `names<-`(c("Non","Oui")),
            quest2 %$% 
              table(engageparti == "Oui" | engagesynd == "Oui"| engagecoll == "Oui") %>%
              prop.table() %>% 
              `names<-`(c("Non","Oui"))) %>%  
  mutate(Type = c("Parti", "Syndicat", "Collectif", "Association","Au moins un type","Sauf association"),
         Oui = round(100 * Oui, 1), # transforme en %
         Non = round(100 * Non, 1), # idem
         `Effectifs de oui` = c(quest2 %>% 
                                  filter(engageparti == "Oui") %>% 
                                  nrow, # effectifs pour la modalité oui
                                quest2 %>% 
                                  filter(engagesynd == "Oui") %>% 
                                  nrow,
                                quest2 %>% 
                                  filter(engagecoll == "Oui") %>% 
                                  nrow,
                                quest2 %>% 
                                  filter(engageasso == "Oui") %>% 
                                  nrow,
                                quest2 %>% 
                                  filter(engageparti == "Oui" | engagesynd == "Oui"| engagecoll == "Oui"| engageasso == "Oui") %>%
                                  nrow,
                                quest2 %>% 
                                  filter(engageparti == "Oui" | engagesynd == "Oui"| engagecoll == "Oui") %>% 
                                  nrow)) %>% 
  select(3:1,4) %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrr", digits = c(1,1,1,0),
        caption = "Engagement des militants dans divers types d'organisations (\\% en ligne)", label = "orgas") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2,lecture_orgas),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "")

### Tableau 2.3.7 - engagements ----
# tableau 
eng_synd_parti <- quest2 %>% 
  lprop_pctot(engagesynd,engageparti) %>% 
  select(2,1,3:4) %>% 
  slice(2,1,3) %>% 
  `row.names<-`(c("Syndicat oui", "Syndicat non", "Ensemble")) %>% 
  `colnames<-`(c("Parti oui", "Parti non", "Effectifs","% tot."))

# lecture
test_eng_synd_parti <- quest2 %$% 
  table(engagesynd,engageparti)

test_eng_synd_parti <- paste("\\\\textsc{Tests}: $p(\\\\chi^2)=", format(chisq.test(test_eng_synd_parti)$p.value, digit = 2), "$, $V=",round(cramer.v(test_eng_synd_parti), 2), "$\n")
lecture_eng_synd_parti <- paste0("\\\\textsc{Lecture}: ", eng_synd_parti[1,1],"\\\\% des militants insoumis ayant été syndiqués ont également été dans un parti.")

# mise en forme 
eng_synd_parti %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrr", digits = c(1,1,0,1),
        caption = "Engagement dans un parti en fonction de l'engagement dans un syndicat (\\% en ligne)", label = "eng_synd_parti") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2,test_eng_synd_parti,lecture_eng_synd_parti),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "")

# Construction de la bd pour l'ACM
data_acm1 <- quest_acm %>% 
  filter(lastpage > 5) %>% 
  select(genre,dipl:etabens_sup,pcs,agecat, pcs_pere:dipl_conj, ville,parti_pere:asso_conj,
         connaitelu,actmili_actprincipale:actmili_plussatis,amis_lfi:amis_abseng,
         typemilitant,convaincre, -pcs_par2,
         engageparti:engageasso,responsparti,responssynd,comite:pas_espace,
         eluavant,freqreu:redac_atelierloi,groupefb,conventionfi,amfis,
         freqmanif,campagnelfi,covid_resoc:covid_manif,don,elulfi) %>% 
  mutate_all(funs(replace(., is.na(.), paste(substitute(.), "NA", sep = ".")))) %>% 
  mutate_if(is.character, as.factor)

### Tableau 2.3.8 - engagement selon les partis ----
# tableaux
trotsmao <- quest2 %>% # ext. gauche
  filter(parti_trotsmao != "Non concerné") %$%
  table(parti_trotsmao) %>% 
  prop.table()
gaucherad <- quest2 %>% # autre gauche de la gauche
  filter(parti_gaucherad != "Non concerné") %$%
  table(parti_gaucherad) %>% 
  prop.table()
gauche <- quest2 %>% # gauche non-communiste
  filter(parti_gauche != "Non concerné") %$%
  table(parti_gauche) %>% 
  prop.table()
autre <- quest2 %>%  # autres
  filter(parti_autre != "Non concerné") %$%
  table(parti_autre) %>% 
  prop.table()
pcf <- quest2 %>% # pcf
  filter(parti_pcf != "Non concerné") %$%
  table(parti_pcf) %>% 
  prop.table()

# fusion
partis <- tibble(`Catégorie` = c("PCF", "Extrême gauche", "Autre gauche de la gauche", "Gauche non communiste", "Autres"),
                 Oui = c(round(100 * pcf[2], 1), round(100 * trotsmao[2], 1), round(100 * gaucherad[2], 1), round(100 * gauche[2], 1), round(100 * autre[2], 1)),
                 Non = c(round(100 * pcf[1], 1), round(100 * trotsmao[1], 1), round(100 * gaucherad[1], 1), round(100 * gauche[1], 1), round(100 * autre[1], 1)))

# lecture
N_parti <- paste("$N=", quest2 %>% # effectifs totaux d'engagés
                   filter(engageparti == "Oui") %>% 
                   nrow, "$\n")

lecture_partis <- paste0(" \\\\textsc{Lecture}: ", round(100 * pcf[2]), "\\\\% des militants insoumis ayant été dans un parti politique déclarent avoir adhéré ou adhérer au \\\\acrshort{pcf}. Voir \\\\reftable{recodage} pour le détail du recodage des partis.")

# mise en forme
partis %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrr", digits = c(1,1,1),
        caption = "Répartion des militants dans les partis cités (\\% en ligne)", label = "partis") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2,N_parti,lecture_partis),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "")

### Tableau 2.3.9 - engagement selon les syndicats ----
# tableaux (même principe)
cgt <- quest2 %>% 
  filter(synd_cgt != "Non concerné") %$%
  table(synd_cgt) %>% 
  prop.table()
cfdt <- quest2 %>% 
  filter(synd_cfdt != "Non concerné") %$%
  table(synd_cfdt) %>% 
  prop.table()
fo <- quest2 %>% 
  filter(synd_fo != "Non concerné") %$%
  table(synd_fo) %>% 
  prop.table()
fsu <- quest2 %>% 
  filter(synd_fsu != "Non concerné") %$%
  table(synd_fsu) %>% 
  prop.table()
jeunes <- quest2 %>% 
  filter(synd_jeunes != "Non concerné") %$%
  table(synd_jeunes) %>% 
  prop.table() 
autre <- quest2 %>% 
  filter(synd_autre != "Non concerné") %$%
  table(synd_autre) %>% 
  prop.table() 

# fusion
synd <- tibble(`Catégorie` = c("CGT", "FO", "CFDT", "FSU", "Syndicat lycéen ou étudiant", "Autre"),
               Oui = c(round(100 * cgt[2], 1), round(100 * fo[2], 1), round(100 * cfdt[2], 1), round(100 * fsu[2], 1), round(100 * jeunes[2], 1), round(100 * autre[2], 1)),
               Non = c(round(100 * cgt[1], 1), round(100 * fo[1], 1), round(100 * cfdt[1], 1), round(100 * fsu[1], 1), round(100 * jeunes[1], 1), round(100 * autre[2], 1)))


# lecture
N_synd <- paste("$N=", quest2 %>% 
                  filter(engagesynd == "Oui") %>% 
                  nrow, "$\n")
lecture_synd <- paste0(" \\\\textsc{Lecture}: ", round(100 * cgt[2],1), "\\\\% des militants insoumis ayant été dans un syndicat déclarent avoir adhéré ou adhérer à la \\\\acrshort{cgt}.")

# mise en forme
synd %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrr", digits = c(1,1,1),
        caption = "Répartion des militants dans les syndicats cités (\\% en ligne)", label = "synd") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2,N_synd,lecture_synd),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "")


### Tableau 2.3.10 - modèle loglin ----
# Création du tableau de contingence
data_engage <- quest2 %>% 
  group_by(engageparti, engagesynd, engagecoll, engageasso) %>% 
  tally

# modèle loglin 2.2
PS_PC_PA_SC_SA_CA <- glm(n ~ engageparti + engagesynd + engagecoll + engageasso + engageparti:engagesynd + engageparti:engagecoll + engageparti:engageasso + engagesynd:engagecoll + engagesynd:engageasso + engagecoll:engageasso , poisson, data_engage)

# modèle saturé
PSCA <- glm(n ~ engageparti * engagesynd * engagecoll * engageasso, poisson, data_engage)

# calcul des indicateurs d'ajustement 
ajust_eng <- stat_ajust(PS_PC_PA_SC_SA_CA, ref = PSCA) %>% # PSCA aura ainsi BIC = AIC = 0
  bind_cols(tibble(N = data_engage[1:16,"n"] %>% sum)) %>% # ajoute les effectifs
  select(-1) %>% # enlève le num de ligne
  t %>% # retourne la matrice
  as.data.frame() %>% # transforme en data.frame sinon pas utilisable
  rownames_to_column %>% # nom de ligne dans une colonne
  mutate(rowname = c("$G^2$","Degrés de liberté","$p(G^2)$","Dissimilarité","AIC","BIC","N"),
         V1 = case_when(rowname == "$p(G^2)$" ~ format(V1, digit = 2),
                        rowname == "Dissimilarité" ~ paste0(round(100 * V1, 1),"\\%"),
                        T ~ as.character(round(V1)))) %>% # changement des noms
  group_split(rowname) %>% # crée une liste utilisable avec stargazer
  map(~discard(flatten_chr(.), . == "")) # change les tibbles de la liste en vecteurs

ajust_eng <- ajust_eng[c(7,5,6,1:4)] # changement de l'ordre

# légende
models_PSCA <- "\\eqref{loglin_engage}"
lecture_motifs <- paste0("\\multirow{1}{0.8\\textwidth}{\\textsc{Lecture}: les insoumis ayant été engagés dans un parti ont $e^{", round(PS_PC_PA_SC_SA_CA$coefficients[6], 3), "} = ", round(exp(PS_PC_PA_SC_SA_CA$coefficients[6]),1), "$ fois plus de chances que ceux qui n'y ont jamais été engagés d'avoir été engagés également dans un syndicat.}")

# changement des étiquettes
labs_tot_PSCA <- PS_PC_PA_SC_SA_CA$R %>%
  rownames %>% 
  str_replace_all(":"," $\\\\times$ ") %>% 
  str_replace_all("engagepartiOui","Parti") %>% 
  str_replace_all("engagesyndOui","Syndicat") %>% 
  str_replace_all("engageassoOui","Association") %>% 
  str_replace_all("engagecollOui","Collectif") %>% 
  str_replace_all("\\(Intercept\\)","Constante")

# mise en forme
stargazer(PS_PC_PA_SC_SA_CA,
          title = "Description des interactions entre les différents types d'engagement (modèle log-linéaire)",
          style = "asr",
          column.sep.width = "1pt",
          covariate.labels = labs_tot_PSCA,
          dep.var.labels = models_PSCA,
          align = TRUE,
          intercept.top = TRUE,
          intercept.bottom = FALSE,
          font.size = "footnotesize", 
          label = "tab:engagements",
          model.numbers = FALSE,
          add.lines = ajust_eng,
          keep.stat = "wald",
          table.placement = "!h",
          notes = c(champ,lecture_motifs,""))


### Tableau 2.3.11 - engagement x CS ----
# tableau
primo_classes <- quest2 %>% 
  mutate(pasengage = engageparti == "Non" & engagesynd == "Non") %>% 
  lprop_pctot(classes_soc, pasengage) %>% 
  slice(2,1,3,4) %>% 
  rename(Engagé = `FALSE`, `Pas engagé` = `TRUE`)

# lecture
test_primo_classes <- quest2 %$% 
  table(classes_soc,engageparti == "Non" & engagesynd == "Non")

test_primo_classes <- paste("\\\\textsc{Tests}: $p(\\\\chi^2)=", format(chisq.test(test_primo_classes)$p.value, digit = 2), "$, $V=",round(cramer.v(test_primo_classes), 2), "$\n")

lect_primo_classes <- paste0("\\\\textsc{Lecture}: ", primo_classes[3,1], "\\\\% des militants de la FI de CS populaires ont été engagés dans au moins un parti ou un syndicat.")

# mise en forme
primo_classes %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrr", digits = 1,
        caption = "Engagement ou non dans un parti ou syndicat en fonction de la catégorie sociale (\\% en ligne)", label = "primo_classes") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2, test_primo_classes, lect_primo_classes),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm")

### Tableau 2.3.12 - engagement x âge ----
# tableau
primo_age <- quest2 %>% 
  mutate(pasengage = engageparti == "Non" & engagesynd == "Non") %>% 
  lprop_pctot(agecat, pasengage) %>% 
  rename(Engagé = `FALSE`, `Pas engagé` = `TRUE`)

# lecture 
test_primo_age <- quest2 %$% 
  table(agecat,engageparti == "Non" & engagesynd == "Non")

test_primo_age <- paste("\\\\textsc{Tests}: $p(\\\\chi^2)=", format(chisq.test(test_primo_age)$p.value, digit = 2), "$, $V=",round(cramer.v(test_primo_age), 2), "$\n")

lect_primo_age<- paste0("\\\\textsc{Lecture}: ", primo_age[3,1], "\\\\% des militants de la FI de 35-45 ans ont été engagés dans au moins un parti un syndicat.")

# mise en forme
primo_age %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrr", digits = 1,
        caption = "Engagement ou non dans un parti ou syndicat en fonction de l'âge  (\\% en ligne)", label = "primo_age") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2, test_primo_age, lect_primo_age),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "")


# Chapitre 3 ----
### Tableau 3.1.1 - pratiques militantes ----
# tableau 
activites <- bind_rows(quest2 %>% select(freqmanif) %>% table %>% prop.table %>% round(3) * 100,
                       quest2 %>% select(freqtract_election) %>% table %>% prop.table %>% round(3) * 100,
                       quest2 %>% select(freqtract_paselection) %>% table %>% prop.table %>% round(3) * 100) %>% 
  mutate(Pratique = c("Manifestations ou rassemblements",
                      "Tractages ou collages en période électorale",
                      "Tractages ou collages hors période électorale"),
         Effectifs = c(quest2 %>% select(freqmanif) %>% table %>% sum,
                       quest2 %>% select(freqtract_election) %>% table %>% sum,
                       quest2 %>% select(freqtract_paselection) %>% table %>% sum)) %>% 
  select(5,1,3,2,4,6)

# lecture
lecture_activites <- paste0(" \\\\textsc{Lecture}: ", activites[1,2], "\\\\% des militants insoumis ne participent jamais à une manifestation ou rassemblement.")

# mise en forme
activites %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrrr", digits = c(0,rep(1,4),0),
        caption = "Fréquences de plusieurs pratiques militantes (\\% en ligne)", label = "activites") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2, lecture_activites),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "6.4cm") %>% 
  column_spec(4, width = "2cm")


### Tableau 3.1.2 - responsabilités ----
# tableaux
respons_parti2 <- quest2 %>% #freq
  filter(responsparti_rec != "Jamais encarté") %$% 
  table(responsparti_rec)

respons_parti <- respons_parti2 %>% # proportions
  prop.table %>%
  round(3) * 100

respons_synd2 <- quest2 %>% # freq
  filter(responssynd_rec != "Jamais syndiqué") %$% 
  table(responssynd_rec)

respons_synd <- respons_synd2 %>% # proportions
  prop.table %>% 
  round(3) * 100

# fusion
respons <- tibble(` ` = c("Pas de responsabilité", "Responsabilité locale", "Responsabilité nationale", "Responsabilité en entreprise", "Total"),
                  `%` = c(respons_parti[1:3], "--", 100), # ajout d'une ligne vide car pas les mêmes cat que pour les responsabilités syndicales
                  Effectif = c(respons_parti2[1:3], "--", sum(respons_parti2)),
                  `% ` = c(respons_synd[1:4], 100),
                  `Effectif ` = c(respons_synd2[1:4], sum(respons_synd2)))

# lecture
lecture_respons <- paste0("\\\\textsc{Lecture}: ", respons[1,2], "\\\\% des militants insoumis qui ont été dans un parti n'ont jamais eu de responsabilité partisane.")

# mise en forme
respons %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrrr", digits = c(0,1,0,1,0),
        caption = "Responsabilités des militants ayant été ou étant engagés dans un parti ou syndicat (\\% en colonne)", label = "respons") %>% 
  add_header_above(c(" " = 1, "Parti" = 2, "Syndicat" = 2), bold = TRUE, escape = FALSE) %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2, lecture_respons),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") 

### Tableau 3.1.3 - rédactions ----
# tableau (un par variable)
redac <- 
  bind_rows(quest2 %>% select(redac_tractfi) %>% table %>% prop.table %>% round(3) * 100, # proportions
            quest2 %>% filter(redac_tractparti != "Non concerné") %>% 
              select(redac_tractparti) %>% table %>% prop.table %>% round(3) * 100,
            quest2 %>% select(redac_communique) %>% table %>% prop.table %>% round(3) * 100,
            quest2 %>% select(redac_articlefi) %>% table %>% prop.table %>% round(3) * 100,
            quest2 %>% select(redac_articlega) %>% table %>% prop.table %>% round(3) * 100,
            quest2 %>% filter(redac_tractparti != "Non concerné") %>% 
              select(redac_articlepartisynd) %>% table %>% prop.table %>% round(3) * 100,
            quest2 %>% select(redac_articleperso) %>% table %>% prop.table %>% round(3) * 100,
            quest2 %>% select(redac_aec) %>% table %>% prop.table %>% round(3) * 100,
            quest2 %>% select(redac_atelierloi) %>% table %>% prop.table %>% round(3) * 100) %>% 
  mutate(` ` = c("Tracts pour la FI",# change les noms 
                 "Tracts pour parti ou syndicat",
                 "Communiqués",
                 "Article pour un média de la FI",
                 "Article pour groupe d'action",
                 "Article pour parti ou syndicat",
                 "Article sur blog personnel",
                 "Livret de l'Avenir en commun",
                 "Texte des ateliers des lois"),
         Effectifs = c(quest2 %>% select(redac_tractfi) %>% table %>% sum, # effectifs
                       quest2 %>% filter(redac_tractparti != "Non concerné") %>% 
                         select(redac_tractparti) %>% table %>% sum,
                       quest2 %>% select(redac_communique) %>% table %>% sum,
                       quest2 %>% select(redac_articlefi) %>% table %>% sum,
                       quest2 %>% select(redac_articlega) %>% table %>% sum,
                       quest2 %>% filter(redac_tractparti != "Non concerné") %>% 
                         select(redac_articlepartisynd) %>% table %>% sum,
                       quest2 %>% select(redac_articleperso) %>% table %>% sum,
                       quest2 %>% select(redac_aec) %>% table %>% sum,
                       quest2 %>% select(redac_atelierloi) %>% table %>% sum)) %>% 
  select(5,1,3,2,4,6)

# lecture
lecture_activites <- paste0("\\\\textsc{Lecture}: ", redac[1,2], "\\\\% des militants insoumis ne participent jamais à la rédaction de tracts pour la FI.\n")
note_activites <- "\\\\textsc{Note}: Les effectifs sont bien moindres concernant les questions qui n'avaient pas à être posées aux militants qui n'ont jamais adhéré à un syndicat ou un parti."

# mise en forme
redac %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrrr", digits = c(0,rep(1,4),0),
        caption = "Fréquences des activités de rédactions des militants insoumis (\\% en ligne)", label = "redac") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2, lecture_activites),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "6.4cm") %>% 
  column_spec(4, width = "2cm")

### Tableau 3.1.4 - confinements ----
# tableau (un par variable)
corona <- bind_rows(quest2 %$% table(covid_resoc) %>% prop.table, # proportions oui / non
                    quest2 %$% table(covid_num) %>% prop.table,
                    quest2 %$% table(covid_aide) %>% prop.table,
                    quest2 %$% table(covid_coll) %>% prop.table,
                    quest2 %$% table(covid_manif) %>% prop.table) %>% 
  mutate(` ` = c("Réseaux sociaux", "Meetings numériques et réunions à distance","Aide de personnes en difficultés", "Collectes pour les quartiers populaire", "Manifestations"),
         Oui = 100 * round(Oui, 3), # pourcentage
         Non = 100 * round(Non, 3)) %>% 
  select(3:1)

# lecture
N_corona <- paste0("$N=", quest2 %>% 
                     filter(!is.na(covid_resoc)) %>% 
                     nrow(), "$\n")
lecture_corona <- paste0("\\\\textsc{Lecture}: ", corona[2,2], "\\\\% des militants insoumis estiment avoir pu continué à militer grâce aux meetings numériques pendant les confinements.\n")
note_corona <- "\\\\textsc{Note}: Les réponses \\\\enquote{autre} ont été recodées quand elles revenaient à une de ces propositions. On n'a pas reproduit les pourcentages tant ils étaient dérisoires et comprenant également les individus qui disaient ne pas avoir fait d'action pendant la pendémie."

# mise en forme
corona %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrr",
        caption = "Continuation du militantisme pendant les confinements (\\% en ligne)", label = "corona") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2,N_corona, lecture_corona,note_corona),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "6.4cm") %>% 
  column_spec(4, width = "2cm")


### Tableau 3.1.5 - réunions ----
# tableau
reunions <- quest2 %>% 
  filter(freqreu != "Pas dans un groupe") %>% 
  lprop_pctot(freqparticipereu_rec,freqreu_rec) %>% 
  select(1:2,4,3,5:6)

# lecture
test_reunions <- quest2 %>% 
  filter(freqreu != "Pas dans un groupe") %$% 
  table(freqparticipereu_rec,freqreu_rec)

test_reunions <- paste("\\\\textsc{Tests}: $p(\\\\chi^2)=", format(chisq.test(test_reunions)$p.value, digit = 2), "$, $V=",round(cramer.v(test_reunions), 2), "$\n")
note_reunions <- "\\\\textsc{Note}: Les militants non membres de groupes ont été exclus."

lect_reunions <- paste0("\\\\textsc{Lecture}: ", reunions[2,1], "\\\\% des insoumis membres d'un groupe d'action et allant à presque toutes leurs réunions ont moins d'une réunion par mois.\n")

# mise en forme
reunions %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "rrrrr", digits = 1,
        caption = "Fréquence des réunions en fonction de la présence en réunion (\\% en ligne)", label = "reunions") %>% 
  add_header_above(c(" " = 1, "Fréquence de réunion du groupe" = 4, " " = 2), bold = TRUE, escape = FALSE) %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 8) %>%
  footnote(general = c(champ2, test_reunions, lect_reunions, note_reunions),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "3.8cm")


### Construction de l'ACM ----
# BDD ACM
data_acm1 <- quest_acm2 %>% # BDD sans les dom-tom
  filter(lastpage > 4) %>%  # enquêtés qui sont allés jusqu'aux questions concernées
  select( # variables actives :
    freqmanif, freqtract_election, campagnelfi, campagne_reg, campagne_pres, participgroupefb,
         responsparti_rec, responssynd_rec, eluavant, elulfi, redac_tractfi, redac_tractparti, redac_articlefi,
         redac_articlega,redac_articlepartisynd, redac_articleperso, redac_aec, redac_atelierloi, comite,
         conventionfi, amfis, don, covid_resoc, covid_num, covid_aide, covid_coll, covid_manif, freqreu_rec,
         freqparticipereu_rec, engageasso, engagecoll,
         # variables supp : 
         genre, dipl_rec, classes_soc, ville, engage_par, agecat, typemilitant, retraite,
         sect, parti_conj, synd_conj, connaitelu, anneelfi) %>% 
  mutate_all(funs(replace(., is.na(.), paste(substitute(.), "NA", sep = ".")))) %>% # met les NA en character
  mutate_if(is.character, as.factor) # tout en factor


# ACM
acm_pratiques <- MCA(data_acm1, graph = FALSE, quali.sup = 32:44, ncp = Inf, # quali.sup voir supra
                     excl = c("freqmanif.NA", "freqtract_election.NA","freqtract_paselection.NA", 
                              "campagnelfi.NA", "campagne_reg.NA", "campagne_pres.NA", "participgroupefb.NA",
                              "eluavant.NA", "elulfi.NA", "redac_tractfi.NA", "redac_tractparti.NA",
                              "redac_articlefi.NA", "redac_articlega.NA", "redac_articlepartisynd.NA",
                              "redac_articleperso.NA","redac_aec.NA", "redac_atelierloi.NA", "comite.NA",
                              "conventionfi.NA", "amfis.NA", "covid_resoc.NA", "covid_num.NA", "covid_aide.NA",
                              "covid_coll.NA", "covid_manif.NA","freqreu_rec.NA", "freqparticipereu_rec.NA",
                              "don.NA", # on exclue les NA de l'analyse 
                              # les modalités suivantes sont répétitives avec d'autres modalités
                              "Pas dans un groupe1",
                              "Tract parti/syndicat : non concerné",
                              "Article parti/syndicat : non concerné"))


# Extraction de toutes les données de l'ACM
tab_agd <- function(agd, data, sortie = "res") {# création de tous les tableaux pour les acm
  type <- class(agd)
  if(class(agd)[1] %>% str_detect("MCA")) {# ne marche que si c'est une ACM
    variances.acm <- as.data.frame(agd$eig) %>%
      rownames_to_column() %>% # récupérer les noms de lignes (dim 1, dim 2, etc) dans une colonne distincte
      slice(1:nrow(agd$eig)) %>% # tout conserver
      mutate(Axes = str_replace_all(rowname, "dim", "Axe")) %>% # créer une nouvelle variable à partir de rowname, qui prend les valeurs "Axe 1, Axe 2, etc" au lieu de "dim 1, dim 2, etc."
      select(-rowname) %>%# on enlève cette colonne dont on n'a plus besoin
      rename(`Valeurs propres` = eigenvalue) %>%
      rename(`% de variance` = `percentage of variance`) %>% # on renomme les autres colonnes
      rename(`% cumulé de variance` = `cumulative percentage of variance`) %>% 
      mutate(Axes = fct_relevel(Axes, paste("Axe", 1:nrow(agd$eig))))
    
    # variables actives
    frequences.acm <- # fréquences de chaque modalité
      pivot_longer(data, everything(), names_to = "variables", values_to = "modalites") %>% # prend les modalités 
      count(variables,modalites) %>% # compte
      group_by(variables) %>% # regroupe
      mutate(pourcentage = round(100 * n / nrow(data), 1)) %>% # tri à plat des modalités
      ungroup() %>% 
      select(variables,modalites,n,pourcentage)
    
    coordonnees.acm <- # coordonnée de chaque modalité sur chaque axe
      as.data.frame(round(agd$var$coord, 2)) %>% 
      rename_all(tolower) %>% # change les noms
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "coord", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    contributions.acm <- # contribution de chaque modalité à chaque axe
      as.data.frame(round(agd$var$contrib, 2)) %>% 
      rename_all(tolower) %>% # idem
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "contrib", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    cos2.acm <- # cos2 de chaque modalité sur chaque axe
      as.data.frame(round(agd$var$cos2, 2)) %>% 
      rename_all(tolower) %>% # idem
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "cos2", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    vtest.acm <- # test de chaque modalité sur chaque axe
      as.data.frame(round(agd$var$v.test, 2)) %>% 
      rename_all(tolower) %>% # idem
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "vtest", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    resultats_actives.acm <- # fusion variables actives
      frequences.acm %>% 
      right_join(coordonnees.acm) %>% 
      right_join(contributions.acm) %>% 
      right_join(cos2.acm) %>% 
      right_join(vtest.acm) %>% 
      mutate(type = "Active") # pour repérer les variables actives dans le grand tableau
    
    # variables sup
    coordonnees_sup.acm <-  # coordonnée de chaque modalité sur chaque axe
      as.data.frame(round(agd$quali.sup$coord, 2)) %>% 
      rename_all(tolower) %>% 
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "coord", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    cos2_sup.acm <-  # cos2 de chaque modalité sur chaque axe
      as.data.frame(round(agd$quali.sup$cos2, 2)) %>% 
      rename_all(tolower) %>% 
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "cos2", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    vtest_sup.acm <-  # test de chaque modalité sur chaque axe
      as.data.frame(round(agd$quali.sup$v.test, 2)) %>% 
      rename_all(tolower) %>% 
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "vtest", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    resultats_sup.acm <- # fusion variables sup
      frequences.acm %>% 
      right_join(coordonnees_sup.acm) %>% 
      right_join(cos2_sup.acm) %>% 
      right_join(vtest_sup.acm) %>% 
      mutate(type = "Supplémentaire")
    
    resultats_complet.acm <- bind_rows(resultats_actives.acm, resultats_sup.acm) # fusion des deux grands tableaux
  }
  else {message("Pas une ACM")}
  if(sortie == "res") {return(resultats_complet.acm)}
  if(sortie == "var") {return(variances.acm)}
}

res.ACM <- tab_agd(acm_pratiques,data_acm1) %>% # résultats complets
  filter(!str_detect(modalites,"NA"))

### Tableau 3.2.6 - inerties ----
# tableau 
var.ACM <- tab_agd(acm_pratiques, data_acm1, "var") %>% # récupération variance
  slice_head(n = 10) %>% # dix premiers axes
  rename(` ` = Axes) %>% 
  select(6,1:3) %>% # axes / valeurs propres / % variance / % variance cumulée
  mutate_if(is.numeric, ~round(., 2)) %>% # arrondir les num

# lecture
N_acm <- paste("$N=",nrow(data_acm1), "$\n")

# mise en forme
var.ACM %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = 2,
        caption = "Inerties des dix premiers axes issus de l’ACM", label = "inertie") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2, N_acm),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") 


### Tableau 3.2.7 - axe 1 ----
# tableaux (on divise en coord > 0 et coord < 0)
axe1_1 <- res.ACM %>% 
  filter(type == "Active") %>% 
  filter(dim1_contrib > 2, dim1_coord < 0) %>% # contrib > la moyenne
  select(modalites, n, dim1_coord, dim1_contrib, dim1_cos2, dim1_vtest) %>% 
  arrange(desc(dim1_contrib)) %>% # ordre décroissant
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))

axe1_2 <- res.ACM %>% # idem
  filter(type == "Active") %>% 
  filter(dim1_contrib > 2, dim1_coord > 0) %>% 
  select(modalites, n, dim1_coord, dim1_contrib, dim1_cos2, dim1_vtest) %>% 
  arrange(desc(dim1_contrib)) %>% 
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))

axe1 <- bind_rows(axe1_1, axe1_2) # fusion

# lecture
lect_axe1 <- paste0("\\\\textsc{Lecture}: La modalité \\\\enquote{ne jamais tracter en période d'élection} est la plus contributive de l'axe 1. Elle contribue à ", axe1[1,4], "\\\\% de son inertie, soit quasiment quatre fois la contribution moyenne. La contribution totale de ces variables à l'inertie de l'axe est ici de ", sum(axe1$Contribution), "\\\\%.")

# mise en forme
axe1 %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrrr", digits = c(1,4,2,2),
        caption = "Modalités les plus contributives au premier axe", label = "axe1") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  row_spec(row = nrow(axe1_1), hline_after = TRUE) %>% 
  footnote(general = c(champ2, N_acm, lect_axe1),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "6cm") 


### Tableau 3.2.8 - axe 2 ----
# tableaux (même principe)
axe2_1 <- res.ACM %>% 
  filter(type == "Active") %>% 
  filter(dim2_contrib > 2, dim2_coord < 0) %>% 
  select(modalites, n, dim2_coord, dim2_contrib, dim2_cos2, dim2_vtest) %>% 
  arrange(desc(dim2_contrib)) %>% 
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))

axe2_2 <- res.ACM %>% 
  filter(type == "Active") %>% 
  filter(dim2_contrib > 2, dim2_coord > 0) %>% 
  select(modalites, n, dim2_coord, dim2_contrib, dim2_cos2, dim2_vtest) %>% 
  arrange(desc(dim2_contrib)) %>% 
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))

axe2 <- bind_rows(axe2_1, axe2_2)

# lecture
lect_axe2 <- paste0("\\\\textsc{Lecture}: La modalité \\\\enquote{jamais syndiqué} est la plus contributive du côté négatif de l'axe 2. Elle contribue à ", axe2[1,4], "\\\\% de son inertie, soit quasiment six fois la contribution moyenne. La contribution totale de ces variables à l'inertie de l'axe est ici de ", sum(axe2$Contribution), "\\\\%.")

# mise en forme
axe2 %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrrr", digits = 2,
        caption = "Modalités les plus contributives au deuxième axe", label = "axe2") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  row_spec(row = nrow(axe2_1), hline_after = TRUE) %>% 
  footnote(general = c(champ2, N_acm, lect_axe2),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "6cm") 


### Tableau 3.2.9 - axe 3 ----
# tableaux (même principe)
axe3_1 <- res.ACM %>% 
  filter(type == "Active") %>% 
  filter(dim3_contrib > 2, dim3_coord < 0) %>% 
  select(modalites, n, dim3_coord, dim3_contrib, dim3_cos2, dim3_vtest) %>% 
  arrange(desc(dim3_contrib)) %>% 
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))

axe3_2 <- res.ACM %>% 
  filter(type == "Active") %>% 
  filter(dim3_contrib > 2, dim3_coord > 0) %>% 
  select(modalites, n, dim3_coord, dim3_contrib, dim3_cos2, dim3_vtest) %>% 
  arrange(desc(dim3_contrib)) %>% 
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))

axe3 <- bind_rows(axe3_1, axe3_2)

# lecture
lect_axe3 <- paste0("\\\\textsc{Lecture}: La modalité \\\\enquote{rédiger rarement un article pour le groupe d'action} est la plus contributive la partie négatice de l'axe 3. Elle contribue à ", axe3[1,4], "\\\\% de son inertie, soit quasiment trois fois la contribution moyenne. La contribution totale de ces variables à l'inertie de l'axe est ici de ", sum(axe3$Contribution), "\\\\%.")

# mise en forme
axe3 %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrrr", digits = 2,
        caption = "Modalités les plus contributives au troisième axe", label = "axe3") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  row_spec(row = nrow(axe3_1), hline_after = TRUE) %>% 
  footnote(general = c(champ2, N_acm, lect_axe3),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "6.3cm") 


### Tableau 3.2.9 - axe 4 ----
# tableaux (même principe)
axe4_1 <- res.ACM %>% 
  filter(type == "Active") %>% 
  filter(dim4_contrib > 2, dim4_coord < 0) %>% 
  select(modalites, n, dim4_coord, dim4_contrib, dim4_cos2, dim4_vtest) %>% 
  arrange(desc(dim4_contrib)) %>% 
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))

axe4_2 <- res.ACM %>% 
  filter(type == "Active") %>% 
  filter(dim4_contrib > 2, dim4_coord > 0) %>% 
  select(modalites, n, dim4_coord, dim4_contrib, dim4_cos2, dim4_vtest) %>% 
  arrange(desc(dim4_contrib)) %>% 
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))

axe4 <- bind_rows(axe4_1, axe4_2)

# lecture
lect_axe4 <- paste0("\\\\textsc{Lecture}: La modalité \\\\enquote{rédiger un article de parti ou de syndicat souvent} est la plus contributive de l'axe 4. Elle contribue à ", axe4[1,4], "\\\\% de son inertie, soit quasiment six fois la contribution moyenne. La contribution totale de ces variables à l'inertie de l'axe est ici de ", sum(axe4$Contribution), "\\\\%.")

# mise en forme 
axe4 %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrrrr", digits = 2,
        caption = "Modalités les plus contributives au quatrième axe", label = "axe4") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  row_spec(row = nrow(axe4_1), hline_after = TRUE) %>% 
  footnote(general = c(champ2, N_acm, lect_axe4),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "6cm") 


### Figure 3.2.1 - plan 1-2 ----
# paramètres du plan 
supp <- c("agecat", "anneelfi", "typemilitant") # variables sup à garder
seuil = 2.5 # seuil de contributions
act <- res.ACM$variables[res.ACM$type == "Active" & # variables actives 
                           (res.ACM$dim1_contrib > seuil | #sup au seuil sur l'axe 1
                              res.ACM$dim2_contrib > seuil)] # et sur l'axe 2

# projection du plan
res.ACM  %>% 
  filter(dim1_contrib > seuil | dim2_contrib > seuil |
           variables %in% supp) %>% # on part du tableau de résultat des modalités actives et on filtre uniquement celles dont la contribution dépasse le seuil pour l'un ou l'autre des deux axes (| est l'opérateur logique OU).
  
  ggplot(aes(x = dim1_coord, y = dim2_coord, # initialisation du graphique
             label = modalites, # les labels des points sont les modalités
             shape = type, # forme des points en fonction de sup/active
             colour = variables, # une couleur par variable
             alpha = dim1_cos2 + dim2_cos2, # transparence selon la représentation sur le plan
             size = n)) + # les formes des points dépendent des variables : à chaque variable son symbole
  
  coord_fixed() + # pour que les échelles des axes soient identiques
  scale_colour_manual(values = c(setNames(colorRampPalette(brewer.pal(12, "Paired"))(length(act)),act),
                                 setNames(rep("black",length(supp)), supp))) + # échelle en fonction du nombre de modalités
  
  geom_point() + # tracer les points
  geom_label_repel(size = 3.5, segment.alpha = 0.3, label.size = NA, label.padding	= 0, alpha = 1) + 
  # spécificité des labels
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne horizontale y = 0
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne verticale x = 0
  
  xlab(paste0("Aucune activité / activité à la FI (", round(var.ACM[1, 2], 1), " %)")) + # nom axe horizontal
  ylab(paste0("FI + parti et syndicat - / FI - parti et syndicat + (", round(var.ACM[2, 2], 1), " %)")) + # idem pour l'axe vertical
  
  guides(colour = FALSE, #légende (couleur, forme, taille, transparence)
         shape = guide_legend(title="Type", title.position = "top", ncol = 1, order = 1),
         size = guide_legend(title="N", title.position = "top", ncol = 2, order = 2),
         alpha = guide_legend(title = "Qualité de représentation", title.position = "top", ncol = 3, order = 3)) +
  
  theme_minimal() + # thème sans rien
  theme(legend.position="bottom", # en ajoutant la légende 
        legend.direction="vertical",
        text = element_text(family = "Times")) +
  
  geom_line(data = res.ACM[res.ACM$variables == "agecat",], orientation = "y", # segments ages
            mapping = aes(x = dim1_coord, y = dim2_coord), lwd = .6, color = "red", alpha = 0.2) +
  
  geom_line(data = res.ACM[res.ACM$variables == "anneelfi",][1:3,], # segments année d'arrivée à lfi 
            mapping = aes(x = dim1_coord, y = dim2_coord), lwd = .6, color = "green", alpha = 0.2) +
  
  geom_line(data = res.ACM[res.ACM$variables == "anneelfi",][3:4,],# idem mais sur un autre plan (sinon désordre)
            mapping = aes(x = dim1_coord, y = dim2_coord), lwd = .6, color = "green", alpha = 0.2) +
  
  geom_line(data = res.ACM[res.ACM$variables == "anneelfi",][4:6,], # idem
            mapping = aes(x = dim1_coord, y = dim2_coord), lwd = .6, color = "green", alpha = 0.2)


### Figure 3.2.1 - plan 3-4 ----
# paramètres du plan (meême principe)
supp <- c("anneelfi")
seuil = 2.5
act <- res.ACM$variables[res.ACM$type == "Active" &
                           (res.ACM$dim3_contrib > seuil |
                              res.ACM$dim4_contrib > seuil)]
# projection du plan (idem)
res.ACM  %>% 
  filter(dim3_contrib > seuil | dim4_contrib > seuil |
           variables %in% supp) %>% # on part du tableau de résultat des modalités actives et on filtre uniquement celles dont la contribution dépasse le seuil pour l'un ou l'autre des deux axes (| est l'opérateur logique OU).
  
  ggplot(aes(x = dim3_coord, y = dim4_coord, # initialisation du graphique
             label = modalites, # les labels des points sont les modalités
             shape = type,
             colour = variables,
             alpha = dim3_cos2 + dim4_cos2,
             size = n)) + # les formes des points dépendent des variables : à chaque variable son symbole
  
  coord_fixed() + # pour que les échelles des axes soient identiques
  scale_colour_manual(values = c(setNames(colorRampPalette(brewer.pal(12, "Paired"))(length(act)),act),
                                 setNames(rep("black",length(supp)), supp))) +
  
  geom_point() + # tracer les points
  geom_label_repel(size = 3.5, segment.alpha = 0.3, label.size = NA, label.padding	= 0, 
                   alpha = 1) +
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne horizontale y = 0
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne verticale x = 0
  
  xlab(paste0("Rédaction hors FI modérée / rédaction pour la FI intense (", round(var.ACM[3, 2], 1), " %)")) + 
  ylab(paste0("Rédaction hors FI intense / rédaction pour la FI modérée (", round(var.ACM[4, 2], 1), " %)")) + # idem pour l'axe vertical
  
  guides(colour = FALSE,
         shape = guide_legend(title="Type", title.position = "top", ncol = 1, order = 1),
         size = guide_legend(title="N", title.position = "top", ncol = 2, order = 2),
         alpha = guide_legend(title = "Qualité de représentation", title.position = "top", ncol = 3, order = 3)) +
  
  theme_minimal() +
  theme(legend.position="bottom",
        legend.direction="vertical",
        text = element_text(family = "Times"))  +
  
  geom_line(data = res.ACM[res.ACM$variables == "anneelfi",][4:6,],
            mapping = aes(x = dim3_coord, y = dim4_coord), lwd = .6, color = "black", alpha = 0.2)

### CAH ----
# recréation de la bdd (idem que plus haut, mais on ne garde que 7 axes)
data_acm2 <- quest_acm2 %>% 
  filter(lastpage > 4) %>% 
  select(freqmanif, freqtract_election, campagnelfi, campagne_reg, campagne_pres, participgroupefb,
         responsparti_rec, responssynd_rec, eluavant, elulfi, redac_tractfi, redac_tractparti, redac_articlefi,
         redac_articlega,redac_articlepartisynd, redac_articleperso, redac_aec, redac_atelierloi, comite,
         conventionfi, amfis, don, covid_resoc, covid_num, covid_aide, covid_coll, covid_manif, freqreu_rec,
         freqparticipereu_rec, engageasso, engagecoll,
         #supp : 
         genre, dipl_rec, classes_soc, ville, engage_par, agecat, typemilitant, retraite,sect, parti_conj,
         synd_conj, connaitelu, anneelfi, id, parti_trotsmao, parti_gaucherad, parti_gauche, parti_pcf,
         parti_autre) %>% 
  mutate_all(funs(replace(., is.na(.), paste(substitute(.), "NA", sep = ".")))) %>%
  mutate_if(is.character, as.factor)

acm_pratiques2 <- MCA(data_acm2, graph = FALSE, quali.sup = 32:50, ncp = 7, # 7 axes 
                      excl = c("freqmanif.NA", "freqtract_election.NA","freqtract_paselection.NA", 
                               "campagnelfi.NA", "campagne_reg.NA", "campagne_pres.NA", "participgroupefb.NA",
                               "eluavant.NA", "elulfi.NA", "redac_tractfi.NA", "redac_tractparti.NA",
                               "redac_articlefi.NA", "redac_articlega.NA", "redac_articlepartisynd.NA",
                               "redac_articleperso.NA","redac_aec.NA", "redac_atelierloi.NA", "comite.NA",
                               "conventionfi.NA", "amfis.NA", "covid_resoc.NA", "covid_num.NA", "covid_aide.NA",
                               "covid_coll.NA", "covid_manif.NA","freqreu_rec.NA", "freqparticipereu_rec.NA",
                               "don.NA",
                               # non concerné
                               "Pas dans un groupe1",
                               "Tract parti/syndicat : non concerné",
                               "Article parti/syndicat : non concerné"))

# classification
classes <- HCPC(acm_pratiques2, nb.clust = 4, graph = FALSE)


### Figure 3.2.3 - inertie CAH ----
classes$call$t$inert.gain[1:15] %>% # on prend les inerties dans la CAH
  as.data.frame %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname) + 1) %>% 
  ggplot(aes(x = rowname, y = `.`)) + 
  geom_bar(stat = "identity") + # ggplot pour faire un histogramme
  xlab("Nombre de classes") + # label x
  ylab("Gain d'inertie par rapport au\nnombre de classes précédent") + # label y
  theme_minimal() +
  theme(text = element_text(family = "Times", size = 10))

### Figure 3.2.4 - arbre hiérarchique ----
fviz_dend(classes, show_labels = FALSE, palette = "jco", ylab = "Dissimilarité intra-classe", main = "",
          ggtheme = theme_minimal(base_family = "Times",   base_line_size = 0))


### Figure 3.2.5 - nuage sur plan 1-2 ----
# BDD avec les points 
nuage <- classes$call$X %>% 
  mutate(clust = fct_recode(clust,
                            "Nouveaux militants" = "1",
                            "Militants d'agit-prop" = "2",
                            "Militants en voie de désengagement" = "3",
                            "Militants cadres" = "4"))

# mise en forme (même principe que pour l'ACM)
nuage %>% 
  ggplot(aes(x = `Dim 1`, y = `Dim 2`, colour = clust)) +
  
  geom_point(alpha = 0.6) + # points un peu transparents
  coord_fixed() +
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  
  xlab(paste0("Aucune activité / activité à la FI (", round(var.ACM[1, 2], 1), " %)")) + 
  ylab(paste0("FI + parti et syndicat - / FI - parti et syndicat + (", round(var.ACM[2, 2], 1), " %)")) +
  
  scale_color_manual(values = c("#0073C2FF","#868686FF","#EFC000FF","#CD534CFF")) + # couleurs ajoutées manuellement
  # pour matcher avec l'arbre hiérachique
  
  guides(colour = guide_legend(title="Carrière militante")) +
  
  theme_minimal() +
  theme(legend.direction="vertical",
        text = element_text(family = "Times", size = 10))


### Figure 3.2.6 - nuage sur plan 3-4 ----
# mise en forme (idem)
nuage %>% 
  ggplot(aes(x = `Dim 3`, y = `Dim 4`, colour = clust)) +
  
  geom_point(alpha = 0.6) +
  coord_fixed() +
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  
  xlab(paste0("Rédaction hors FI modérée / rédaction pour la FI intense (", round(var.ACM[3, 2], 1), " %)")) + 
  ylab(paste0("Rédaction hors FI intense / rédaction pour la FI modérée (", round(var.ACM[4, 2], 1), " %)")) +
  
  scale_color_manual(values = c("#0073C2FF","#868686FF","#EFC000FF","#CD534CFF")) +
  
  guides(colour = guide_legend(title="Carrière militante")) +
  
  theme_minimal() +
  theme(legend.direction="vertical",
        text = element_text(family = "Times", size = 10))
