# Nettoyage des réponses au questionnaire -------------------------------------

# Encodage du fichier : UTF-8


## Importation des packages -----
library(here) # pour chemin valable partout
library(tidyverse)
library(magrittr)
library(readr)
library(stringr)  # travail sur les chaines de caractères
library(sjlabelled) # labels avec la description des variables
library(expss) #export avec label + if_na


# Importation du questionnaire-----
quest <- read_csv(here("Donnees","quest_LFI.csv"), 
                      col_types = cols(statut = col_character()),
                  na = c("","NA","N/A"))

# Nettoyage----
## inexploitables + changements de forme----
quest$genre[quest$`genre[other]` %in% c("sexe et non «genre» mâle.",
                                        "Je suis reflechi et actif depuis plusieurs annees",
                                        "Helicoptere Apache",
                                        "A quel genre correspond autre ?",
                                        "cochon d'inde")] <- "Masculin"

quest %<>% mutate(genre = case_when(genre == "Masculin" ~ "Homme",
                                    genre == "Féminin"  ~ "Femme",
                                    genre == "Autre"    ~ "Autre"))

quest %<>% 
  filter(consent == "Oui", lastpage > 0, 
         !(lastpage == 1 & (is.na(genre) | (is.na(dept) & is.na(ville))))) %>% 
  mutate(retraite = `pcs[pcs10]`,
         pcs_sr = case_when(`pcs[pcs1]` == "Oui"  ~ "Agriculteurs exploitants",
                         `pcs[pcs2]` == "Oui"  ~ "Artisans, commerçants, chefs d'entreprise",
                         `pcs[pcs8]` == "Oui"  ~ "Employés",
                         `pcs[pcs3]` == "Oui"  ~ "Professions libérales",
                         `pcs[pcs4]` == "Oui"  ~ "Cadres",
                         `pcs[pcs5]` == "Oui"  ~ "Professeurs, professions scientifiques",
                         `pcs[pcs6]` == "Oui"  ~ "Professeurs des écoles, instituteurs et assimilé",
                         `pcs[pcs7]` == "Oui"  ~ "Autres professions intermédiaires",
                         `pcs[pcs9]` == "Oui"  ~ "Ouvriers",
                         `pcs[pcs11]` == "Oui" ~ "Élèves ou étudiants" ,
                         `pcs[pcs12]` == "Oui" ~ "Chômeurs",
                         `pcs[pcs13]` == "Oui" ~ "Autre inactifs",
                         TRUE                  ~ NA_character_),
         pcs = case_when(`pcs[pcs10]` == "Oui" ~ "Retraités",
                         `pcs[pcs1]` == "Oui"  ~ "Agriculteurs exploitants",
                         `pcs[pcs2]` == "Oui"  ~ "Artisans, commerçants, chefs d'entreprise",
                         `pcs[pcs8]` == "Oui"  ~ "Employés",
                         `pcs[pcs3]` == "Oui"  ~ "Professions libérales",
                         `pcs[pcs4]` == "Oui"  ~ "Cadres",
                         `pcs[pcs5]` == "Oui"  ~ "Professeurs, professions scientifiques",
                         `pcs[pcs6]` == "Oui"  ~ "Professeurs des écoles, instituteurs et assimilé",
                         `pcs[pcs7]` == "Oui"  ~ "Autres professions intermédiaires",
                         `pcs[pcs9]` == "Oui"  ~ "Ouvriers",
                         `pcs[pcs11]` == "Oui" ~ "Élèves ou étudiants",
                         `pcs[pcs12]` == "Oui" ~ "Chômeurs",
                         `pcs[pcs13]` == "Oui" ~ "Autres inactifs",
                            TRUE                  ~ NA_character_),
         age = 2021 - naiss) %>% 
  filter(!(`pcs[pcs8]` == "Oui" & `pcs[pcs3]` == "Oui"),
         !(`pcs[pcs1]` == "Oui" & `pcs[pcs2]` == "Oui"),
         !(`pcs[pcs4]` == "Oui" & `pcs[pcs5]` == "Oui"),
         !(`pcs[pcs3]` == "Oui" & `pcs[pcs7]` == "Oui"),
         !(naiss %in% c(1,4219))) %>% 
  filter(!(id %in% c(1039,1044))) %>% 
  select(-(`pcs[pcs1]`:`pcs[pcs13]`), -(`importanceodj[imp1]`:`importanceodj[imp6]`)) %>% 
  rename(genre_autre = `genre[other]`,
         etabens_prim = `etabens[ens1]`,etabens_second = `etabens[ens2]`,etabens_sup = `etabens[ens3]`,
         univ = `etabsup[univ]`,med = `etabsup[med]`, iut = `etabsup[iut]`, bts = `etabsup[bts]`,
         cpge = `etabsup[cpge]`, inge =`etabsup[inge]`, ens = `etabsup[ens]`,
         pcs_pere = `pcsfam[pcspere]`,pcs_mere = `pcsfam[pcsmere]`, pcs_par2 = `pcsfam[pcspar]`,
         pcs_conj = `pcsfam[pcsconj]`,
         dipl_pere = `diplfam[diplpere]`,dipl_mere = `diplfam[diplmere]`,dipl_par2 = `diplfam[diplp2]`,
         dipl_conj = `diplfam[diplconj]`,
         echpol = `echpol[echpol]`,echpol_pere = `echpol[echpolpere]`,echpol_mere = `echpol[echpolmere]`,
         echpol_par2 = `echpol[echpolpar]`,echpol_conj = `echpol[echpolconj]`,
         parti_pere = `engagefam[pere_parti]`,synd_pere = `engagefam[pere_synd]`,
         asso_pere = `engagefam[pere_asso]`,nsp_pere = `engagefam[pere_nsp]`,
         parti_mere = `engagefam[mere_parti]`,synd_mere = `engagefam[mere_synd]`,
         asso_mere = `engagefam[mere_asso]`,nsp_mere = `engagefam[mere_nsp]`,
         parti_par2 = `engagefam[par_parti]`,synd_par2 = `engagefam[par_synd]`,
         asso_par2 = `engagefam[par_asso]`,nsp_par2 = `engagefam[par_nsp]`,
         parti_conj = `engagefam[conj_parti]`,synd_conj = `engagefam[conj_synd]`,
         asso_conj = `engagefam[conj_asso]`,nsp_conj = `engagefam[conj_nsp]`,
         relig_pere = `religfam[religpere]`,relig_mere = `religfam[religmere1]`,
         relig_par2 = `religfam[religp2]`,
         amis_lfi = `amis[lfi]`, amis_gauche = `amis[parti1]`,amis_droite = `amis[parti2]`,
         amis_synd = `amis[synd]`,amis_abseng = `amis[abs]`,amis_autre = `amis[other]`,
         autresmili_comment = `autresmili[comment]`,
         sujetpol_autre = `sujetpol[other]`,sujetpol_evnmt = sujetpol1,sujetpol_ideo = sujetpol2,
         sujetpol_ideo_autre = `sujetpol2[other]`,
         engageparti = `engage[engageparti]`,engagesynd = `engage[engagesynd]`,engagecoll = `engage[engagecoll]`,
         engageasso = `engage[engageasso]`,
         responsparti_comment = `responsparti[comment]`,
         responssynd_comment = `responssynd[comment]`,
         accompmanif_amis = `accompmanif[ac1]`,accompmanif_fam = `accompmanif[ac2]`,
         accompmanif_parti = `accompmanif[ac3]`, accompmanif_synd = `accompmanif[ac4]`,
         accompmanif_coll = `accompmanif[ac5]`,accompmanif_lfi = `accompmanif[ac6]`,
         accompmanif_seul = `accompmanif[ac7]`,
         vote_presid = `vote[presid]`,vote_legis = `vote[legis]`,vote_region = `vote[region]`,
         vote_ue = `vote[ue]`, vote_munic = `vote[munic]`,
         engagelfi_jlm = `engagelfi[engagejlm]`, engagelfi_gauche = `engagelfi[engagegauche]`,
         engagelfi_election = `engagelfi[engageelection]`,engagelfi_droits = `engagelfi[engagedroits]`,
         engagelfi_carriere = `engagelfi[engagecarriere]`,engagelfi_fam = `engagelfi[engagefam]`,
         engagelfi_rencontre = `engagelfi[engagerencontre]`,engagelfi_souple = `engagelfi[engagesouple]`,
         engagelfi_prox = `engagelfi[engageprox]`,
         redac_tractfi = `redac[tractfi]`,redac_tractparti = `redac[tractparti]`,
         redac_communique = `redac[communique]`,redac_articlefi = `redac[articlefi]`,
         redac_articlega = `redac[articlega]`,redac_articlepartisynd = `redac[articlepartisynd]`,
         redac_articleperso = `redac[articleperso]`,redac_aec = `redac[aec]`,
         redac_atelierloi = `redac[atelierloi]`,
         freqtract_election = `freqtract[freqtractelection]`,
         freqtract_paselection = `freqtract[freqtractpaselection]`,
         campagne_reg = `campagnereg[region]`,campagne_pres = `campagnereg[president]`,
         covid_resoc = `covid[resoc]`,covid_num = `covid[num]`,covid_aide = `covid[aide]`,
         covid_coll = `covid[coll]`,covid_manif = `covid[manif]`,covid_autre = `covid[other]`,
         espace_pgrm = `espace[espace1]`,espace_parlm = `espace[espace2]`,espace_ue = `espace[espace3]`,
         espace_pol = `espace[espace4]`,espace_op = `espace[espace5]`,espace_elus = `espace[espace6]`,
         espace_autoorga = `espace[espace7]`,espace_elec = `espace[espace8]`, pas_espace = `espace[pasespace]`,
         actmili_actprincipale = `actmili[actprincipale]`,actmili_plusrespons = `actmili[plusrespons]`,
         actmili_plusutile = `actmili[plusutile]`,actmili_plussatis = `actmili[plussatis]`,
         difficultpro_placard = `typdifficultpro[placard]`,difficultpro_isole = `typdifficultpro[isole]`,
         difficultpro_harcele = `typdifficultpro[harcele]`,difficultpro_licenciement = `typdifficultpro[licenciement]`,
         difficultpro_pastrouve = `typdifficultpro[pastrouve]`,difficultpro_autre = `typdifficultpro[other]`,
         impquestion_lfi_serv = `impquestion[lfi1]`,impquestion_lfi_bac = `impquestion[lfi2]`,
         impquestion_lfi_cannab = `impquestion[lfi3]`,impquestion_lfi_vert = `impquestion[lfi4]`,
         impquestion_lfi_spap = `impquestion[lfi5]`,impquestion_lfi_nuc = `impquestion[lfi6]`,
         impquestion_npa_islam = `impquestion[npa1]`,impquestion_npa_licenci = `impquestion[npa2]`,
         impquestion_npa_spap = `impquestion[npa3]`,impquestion_npa_log = `impquestion[npa4]`,
         remarque_mili = remarque1, remarque_questio = remarque2)



quest %<>% 
  mutate(dipl = case_when(dipl == "BEP/CAP"           ~ "BEP/CAP ou équivalent",
                          dipl == "BEPC, brevet élémentaire, brevet des collèges, DNB" ~ "BEPC ou équivalent",
                          dipl == "Master, diplôme d'études approfondies, diplôme d'études supérieures spécialisées, diplôme d'ingénieur" ~ "Master et assimilé",
                          dipl == "Doctorat, habilitation à diriger des recherches" ~ "Doctorat, HDR",
                          T                           ~ dipl),
         dipl_pere = case_when(dipl_pere == "BEP/CAP" ~ "BEP/CAP ou équivalent",
                               dipl_pere == "BEPC, brevet élémentaire, brevet des collèges, DNB" ~ "BEPC ou équivalent",
                               dipl_pere == "Master, diplôme d'études approfondies, diplôme d'études supérieures spécialisées, diplôme d'ingénieur" ~ "Master et assimilé",
                               dipl_pere == "Doctorat, habilitation à diriger des recherches" ~ "Doctorat, HDR",
                               T                      ~ dipl_pere),
         dipl_mere = case_when(dipl_mere == "BEP/CAP" ~ "BEP/CAP ou équivalent",
                               dipl_mere == "BEPC, brevet élémentaire, brevet des collèges, DNB" ~ "BEPC ou équivalent",
                               dipl_mere == "Master, diplôme d'études approfondies, diplôme d'études supérieures spécialisées, diplôme d'ingénieur" ~ "Master et assimilé",
                               dipl_mere == "Doctorat, habilitation à diriger des recherches" ~ "Doctorat, HDR",
                               T                      ~ dipl_mere),
         dipl_conj = case_when(dipl_conj == "BEP/CAP" ~ "BEP/CAP ou équivalent",
                               dipl_conj == "BEPC, brevet élémentaire, brevet des collèges, DNB" ~ "BEPC ou équivalent",
                               dipl_conj == "Master, diplôme d'études approfondies, diplôme d'études supérieures spécialisées, diplôme d'ingénieur" ~ "Master et assimilé",
                               dipl_conj == "Doctorat, habilitation à diriger des recherches" ~ "Doctorat, HDR",
                               T                      ~ dipl_conj),
         dipl_par2 = case_when(dipl_par2 == "BEP/CAP" ~ "BEP/CAP ou équivalent",
                               dipl_par2 == "BEPC, brevet élémentaire, brevet des collèges, DNB" ~ "BEPC ou équivalent",
                               dipl_par2 == "Master, diplôme d'études approfondies, diplôme d'études supérieures spécialisées, diplôme d'ingénieur" ~ "Master et assimilé",
                               dipl_par2 == "Doctorat, habilitation à diriger des recherches" ~ "Doctorat, HDR",
                               T                      ~ dipl_par2),
         typemilitant = case_when(typemilitant == "Un-e militant-e actif-ve de la France Insoumise" ~ "Militant actif",
                                  typemilitant == "Un-e militant-e épisodique de la France Insoumise" ~ "Militant épisodique",
                                  typemilitant == "Un-e sympathisant-e de la France Insoumise" ~ "Sympathisant",
                                  typemilitant == "Un-e cadre de la France Insoumise" ~ "Militant cadre"),
         publifi = case_when(publifi == "Oui, régulièrement" ~ "Lit régulièrement",
                             publifi == "Oui, de temps en temps" ~ "Lit parfois",
                             publifi == "Non, jamais" ~ "Ne lit jamais"),
         actufirs = case_when(actufirs == "Oui, régulièrement" ~ "Lit régulièrement",
                              actufirs == "Oui, de temps en temps" ~ "Lit parfois",
                              actufirs == "Non, jamais" ~ "Ne lit jamais"),
         participgroupefb = case_when(participgroupefb == "Je propose des sujets de discussion ou des ressources au groupe." ~ "Propose",
                                      participgroupefb == "Je prends part aux débats quand il y en a." ~ "Débat",
                                      participgroupefb == "Je regarde les débats et lis les ressources qui m'intéressent sans participer." ~ "Participe pas"),
         maniflfi = case_when(maniflfi == "Oui, uniquement" ~ "Uniquement",
                              maniflfi == "Oui, mais aussi dans le cadre d'un autre type de militantisme" ~ "Pas uniquement",
                              maniflfi == "Non, seulement dans le cadre d'un autre type de militantisme" ~ "Jamais"),
         campagnelfi = case_when(campagnelfi == "Oui, à tous les scrutins depuis que je suis engagé à la FI" ~ "Tous les scrutins",
                                 campagnelfi == "Oui, mais pas à tous les scrutins" ~ "Pas tous les scrutins",
                                 campagnelfi == "Non" ~ "Jamais"),
         pcs_pere = 
           case_when(pcs_pere == "Agriculteur-rice"  ~ "Agriculteurs exploitants",
                     pcs_pere == "Artisan, commerçant-e, chef d’entreprise"  ~ 
                       "Artisans, commerçants, chefs d'entreprise",
                     pcs_pere == "Employé-e"  ~ "Employés",
                     pcs_pere == "Profession libérale"  ~ "Professions libérales",
                     pcs_pere == "Cadre"  ~ "Cadres",
                     pcs_pere == "Professeur-e, profession scientifique"  ~ 
                       "Professeurs, professions scientifiques",
                     pcs_pere == "Professeur-e des écoles, instituteur-rice et assimilé"  ~ 
                       "Professeurs des écoles, instituteurs et assimilé",
                     pcs_pere == "Ouvrier-ère"  ~ "Ouvriers",
                     pcs_pere == "Chômeur-euse" ~ "Chômeurs",
                     pcs_pere %in% c("Autre inactif-ve","Retraité-e") ~ "Autres inactifs",
                     TRUE                  ~ pcs_pere),
         pcs_mere = 
           case_when(pcs_mere == "Agriculteur-rice"  ~ "Agriculteurs exploitants",
                     pcs_mere == "Artisan, commerçant-e, chef d’entreprise"  ~ 
                       "Artisans, commerçants, chefs d'entreprise",
                     pcs_mere == "Employé-e"  ~ "Employés",
                     pcs_mere == "Profession libérale"  ~ "Professions libérales",
                     pcs_mere == "Cadre"  ~ "Cadres",
                     pcs_mere == "Professeur-e, profession scientifique"  ~ 
                       "Professeurs, professions scientifiques",
                     pcs_mere == "Professeur-e des écoles, instituteur-rice et assimilé"  ~ 
                       "Professeurs des écoles, instituteurs et assimilé",
                     pcs_mere == "Ouvrier-ère"  ~ "Ouvriers",
                     pcs_mere == "Chômeur-euse" ~ "Chômeurs",
                     pcs_mere %in% c("Autre inactif-ve", "Étudiant-e","Retraité-e") ~ "Autres inactifs",
                     TRUE                  ~ pcs_mere),
         pcs_conj = 
           case_when(pcs_conj == "Agriculteur-rice"  ~ "Agriculteurs exploitants",
                     pcs_conj == "Artisan, commerçant-e, chef d’entreprise"  ~ 
                       "Artisans, commerçants, chefs d'entreprise",
                     pcs_conj == "Employé-e"  ~ "Employés",
                     pcs_conj == "Profession libérale"  ~ "Professions libérales",
                     pcs_conj == "Cadre"  ~ "Cadres",
                     pcs_conj == "Professeur-e, profession scientifique"  ~ 
                       "Professeurs, professions scientifiques",
                     pcs_conj == "Professeur-e des écoles, instituteur-rice et assimilé"  ~ 
                       "Professeurs des écoles, instituteurs et assimilé",
                     pcs_conj == "Ouvrier-ère"  ~ "Ouvriers",
                     pcs_conj == "Chômeur-euse" ~ "Chômeurs",
                     pcs_conj %in% c("Autre inactif-ve", "Étudiant-e","Retraité-e") ~ "Autres inactifs",
                     TRUE                  ~ pcs_conj))

# changement de certaines modalités (précisé dans les questions ouvertes)
quest$pcs[337] <- "Employés" # aide à domicile
quest$dipl[371] <- "BEP/CAP ou équivalent" # certificat d'aptitude pédagogique
quest$dipl_pere[547] <- "BEP/CAP ou équivalent" # certificat d'études primaires
quest$dipl_mere[631] <- "BEP/CAP ou équivalent"# certificat d'études primaires
quest$dipl_mere[657] <- "BEP/CAP ou équivalent"# certificat d'études primaires
quest$pcs_mere[657] <- "Autres inactifs" # mère au foyer
quest$quelsynd[185] <- "auparavant à la cfdt" # s'est trompé de question
quest$anneelfi[quest$anneelfi == 2012] <- 2016 # 2012 impossible

quest %<>% 
  mutate(pcs_par2 = ifelse((is.na(pcs_pere) | is.na(pcs_mere)) & !is.na(pcs_par2),
                           pcs_par2, NA_character_),
         dipl_par2 = ifelse((is.na(pcs_pere) | is.na(pcs_mere)) & !is.na(pcs_par2),
                            dipl_par2, NA_character_),
         id = as.character(id), 
         echpol_par2 = as.character(echpol_par2))

# tout est vide :
quest$parti_par2 <- NULL
quest$synd_par2 <- NULL
quest$asso_par2 <- NULL
quest$nsp_par2 <- NULL
quest$ageresponsparti <- NULL
quest$ageresponssynd <- NULL
quest$regionales <- NULL
quest$dipl_par2 <- NULL

## traitement des filtres---- 
# engageparti => d'autres réponses
quest %<>% 
  mutate(statut = ifelse(pcs == "Élèves ou étudiants", "Non concerné", statut),
         redac_articlepartisynd = ifelse(engageparti == "Non" & engagesynd == "Non",
                                         "Non concerné", redac_articlepartisynd),
         redac_tractparti = ifelse(engageparti == "Non" & engagesynd == "Non",
                                   "Non concerné", redac_tractparti),
         ville = ifelse(dept %in% c("75 Paris","92 Hauts-de-Seine","93 Seine-St-Denis",
                                    "94 Val-de-Marne"),
                        "Dans une grande ville ou sa périphérie",ville),
         participgroupefb = ifelse(groupefb == "Non",
                                   "Non concerné",participgroupefb),
         accompmanif_amis = ifelse(agemanif == 0, "Non concerné", accompmanif_amis),
         accompmanif_fam = ifelse(agemanif == 0, "Non concerné", accompmanif_fam),
         accompmanif_parti = ifelse(agemanif == 0, "Non concerné", accompmanif_parti),
         accompmanif_synd = ifelse(agemanif == 0, "Non concerné", accompmanif_synd),
         accompmanif_coll = ifelse(agemanif == 0, "Non concerné", accompmanif_coll),
         accompmanif_lfi = ifelse(agemanif == 0, "Non concerné", accompmanif_lfi),
         accompmanif_seul = ifelse(agemanif == 0, "Non concerné", accompmanif_seul),
         nbamfis = ifelse(amfis == "Non", "Non concerné", nbamfis),
         intervamfis = ifelse(amfis == "Non", "Non concerné", intervamfis),
         debatconvention = ifelse(conventionfi == "Non",
                                  "Non concerné",debatconvention),
         freqmanif = ifelse(agemanif == 0, "Jamais", freqmanif),
         maniflfi = ifelse(freqmanif == "Jamais", "Non concerné", maniflfi),
         elulfi = ifelse(anneelfi - naiss < 17, "Non",elulfi),
         convaincre = ifelse(connaitelu == "Non", "Non",convaincre),
         espace_pgrm = ifelse(pas_espace == "Oui", "Non",espace_pgrm),
         espace_parlm = ifelse(pas_espace == "Oui", "Non",espace_parlm),
         espace_ue = ifelse(pas_espace == "Oui", "Non",espace_ue),
         espace_pol = ifelse(pas_espace == "Oui", "Non",espace_pol),
         espace_op = ifelse(pas_espace == "Oui", "Non",espace_op),
         espace_elus = ifelse(pas_espace == "Oui", "Non",espace_elus),
         espace_autoorga = ifelse(pas_espace == "Oui", "Non",espace_autoorga),
         espace_elec = ifelse(pas_espace == "Oui", "Non",espace_elec),
         actmili_actprincipale = ifelse(engageparti == "Non" & engagesynd == "Non" &
                                          engageasso == "Non",
                                        "Non concerné", actmili_actprincipale),
         actmili_plusrespons = ifelse(engageparti == "Non" & engagesynd == "Non" &
                                        engageasso == "Non",
                                      "Non concerné", actmili_plusrespons),
         actmili_plusutile = ifelse(engageparti == "Non" & engagesynd == "Non" &
                                      engageasso == "Non",
                                    "Non concerné", actmili_plusutile),
         actmili_plussatis = ifelse(engageparti == "Non" & engagesynd == "Non" &
                                      engageasso == "Non",
                                    "Non concerné", actmili_plussatis),
         difficultpro_placard = ifelse(difficultpro == "Non",
                                  "Non concerné",difficultpro_placard),
         difficultpro_isole = ifelse(difficultpro == "Non",
                                       "Non concerné",difficultpro_isole),
         difficultpro_harcele = ifelse(difficultpro == "Non",
                                       "Non concerné",difficultpro_harcele),
         difficultpro_licenciement = ifelse(difficultpro == "Non",
                                       "Non concerné",difficultpro_licenciement),
         difficultpro_pastrouve = ifelse(difficultpro == "Non",
                                       "Non concerné",difficultpro_pastrouve),
         difficultpro_autre = ifelse(difficultpro == "Non",
                                       "Non concerné",difficultpro_autre))


# Recodage matrice des partis
quest %<>% 
  mutate(parti_pere = as.character(
    case_when(!is.na(pcs_pere) ~ # vire les non concernés
                case_when(parti_pere == 1 & lastpage >= 2                    ~ "Oui",# page ouverte
                          nsp_pere == 1  & lastpage > 2                      ~ "NSP",# page validée
                          is.na(parti_pere) & is.na(nsp_pere) & lastpage > 2 ~ "Non"))),# page validée
    synd_pere = as.character(
      case_when(!is.na(pcs_pere) ~
                  case_when(synd_pere == 1 & lastpage >= 2                    ~ "Oui",
                            nsp_pere == 1  & lastpage > 2                     ~ "NSP",
                            is.na(synd_pere) & is.na(nsp_pere) & lastpage > 2 ~ "Non"))),
    asso_pere = as.character(
      case_when(!is.na(pcs_pere) ~
                  case_when(asso_pere == 1 & lastpage >= 2                    ~ "Oui",
                            nsp_pere == 1  & lastpage > 2                     ~ "NSP",
                            is.na(asso_pere) & is.na(nsp_pere) & lastpage > 2 ~ "Non"))),
    parti_mere = as.character(
      case_when(!is.na(pcs_mere) ~
                  case_when(parti_mere == 1 & lastpage >= 2                    ~ "Oui",
                            nsp_mere == 1  & lastpage > 2                      ~ "NSP",
                            is.na(parti_mere) & is.na(nsp_mere) & lastpage > 2 ~ "Non"))),
    synd_mere = as.character(
      case_when(!is.na(pcs_mere) ~
                  case_when(synd_mere == 1 & lastpage >= 2                    ~ "Oui",
                            nsp_mere == 1  & lastpage > 2                     ~ "NSP",
                            is.na(synd_mere) & is.na(nsp_mere) & lastpage > 2 ~ "Non"))),
    asso_mere = as.character(
      case_when(!is.na(pcs_mere) ~
                  case_when(asso_mere == 1 & lastpage >= 2                    ~ "Oui",
                            nsp_mere == 1  & lastpage > 2                     ~ "NSP",
                            is.na(asso_mere) & is.na(nsp_mere) & lastpage > 2 ~ "Non"))),
    parti_conj = as.character(
      case_when(!is.na(pcs_conj) ~
                  case_when(parti_conj == 1 & lastpage >= 2                    ~ "Oui",
                            nsp_conj == 1  & lastpage > 2                      ~ "NSP",
                            is.na(parti_conj) & is.na(nsp_conj) & lastpage > 2 ~ "Non"))),
    synd_conj = as.character(
      case_when(!is.na(pcs_conj) ~
                  case_when(synd_conj == 1 & lastpage >= 2                    ~ "Oui",
                            nsp_conj == 1  & lastpage > 2                     ~ "NSP",
                            is.na(synd_conj) & is.na(nsp_conj) & lastpage > 2 ~ "Non"))),
    asso_conj = as.character(
      case_when(!is.na(pcs_conj) ~
                  case_when(asso_conj == 1 & lastpage >= 2                    ~ "Oui",
                            nsp_conj == 1  & lastpage > 2                     ~ "NSP",
                            is.na(asso_conj) & is.na(nsp_conj) & lastpage > 2 ~ "Non")))) %>%
  select(-c(nsp_pere, nsp_mere, nsp_conj))

## questions ouvertes----
sigles <- function(var, texte) {
  return(str_detect(str_to_lower(str_replace_all(var, "\\.", "")), texte))
}
quest %<>%
  mutate(
    ### sujetpol----
    # sujetpol_autre 98
    # sujetpol_evnmt 42
    sujetpol = case_when(
      str_detect(sujetpol_autre, "68") | str_detect(sujetpol_autre, "Devaquet") |
        str_detect(sujetpol_autre, "Débré") | str_detect(sujetpol_autre, "manifs") |
        str_detect(sujetpol_autre, "mouvement social") | str_detect(sujetpol_autre, "ZAD") |
        str_detect(sujetpol_autre, "NDDL") | str_detect(sujetpol_evnmt, "Villepin") | 
        str_detect(sujetpol_evnmt, "sarkozy") | str_detect(sujetpol_evnmt, "Villepin") | 
        str_detect(sujetpol_evnmt, "1994") | str_detect(sujetpol_evnmt, "mariage") | 
        str_detect(sujetpol_evnmt, "1968") | str_detect(sujetpol_evnmt, "Devaquet") ~ "Un mouvement social",
                         #### sujetpol_evnmt ----
                         # "Contrat première embauche de De Villepin"
                         # "La question du mariage pour tous."
                         # "Retraite sous sarkozy"
                         # "1968"
                         # "Loi Devaquet"
                         # "recolte jeunesse 1967 1968 / Mort du Che"
                         # "Grande manifestation à Paris contre le projet de loi de financement des écoles publiques en 1994 (autrement toujours été avec mes parents des manif contre le nucléaire)"
      str_detect(sujetpol_autre, "présidentielles") | str_detect(sujetpol_autre, "Chirac") |
        str_detect(sujetpol_autre, "2002") | str_detect(sujetpol_autre, "mouvements") |
        str_detect(sujetpol_autre, "conseil municipal") | str_detect(sujetpol_autre, "TCE") |
        str_detect(str_to_lower(sujetpol_autre), "référendum") | 
        str_detect(sujetpol_autre, "traité") | sujetpol == "Une élection" | 
        str_detect(sujetpol_evnmt, "TCE") | str_detect(sujetpol_evnmt, "élection") | 
        str_detect(sujetpol_evnmt, "Election") ~ "Une élection ou référendum",
                         #### sujetpol_evnmt----
                         # "Référendum TCE"
                         # "l'élection de François Mittérand"
                         # "l'élection d'un membre du front national à la mairie en tant que conseiller municipal"
                         # "Election de Hollande"
      str_detect(sujetpol_autre, "Hollande") | str_detect(sujetpol_autre, "sociétal") |
        str_detect(sujetpol_autre, "Abolition") | str_detect(sujetpol_autre, "famille") |
        str_detect(sujetpol_autre, "écologie") | str_detect(sujetpol_autre, "bien de la France") |
        str_detect(sujetpol_autre, "1992") | str_detect(sujetpol_autre, "Discriminations") |
        str_detect(sujetpol_autre, "inégalités") | str_detect(sujetpol_autre, "Manifeste") |
        str_detect(sujetpol_autre, "internationnalisme") | str_detect(sujetpol_autre, "injustices") |
        str_detect(sujetpol_autre, "Dictatures") | str_detect(sujetpol_autre, "Pere") |
        str_detect(sujetpol_autre, "aberation") | str_detect(sujetpol_autre, "paysans") |
        str_detect(sujetpol_autre, "logements") | str_detect(sujetpol_autre, "organisation") |
        str_detect(sujetpol_autre, "féminisme") | str_detect(sujetpol_evnmt, "sociale") |
        str_detect(sujetpol_evnmt, "climats") | str_detect(sujetpol_evnmt, "planète") |
        str_detect(sujetpol_evnmt, "nazisme") ~ "Une question idéologique",
                         #### sujetpol_evnmt----
                         # "L’avenie De la planète"
                         # "l'injustice sociale"
                         # "Prise de conscience des enjeux climats/épuisement des ressources"
                         # "Je me demandais comment le peuple allemand avait pu accepter le nazisme"
      str_detect(str_to_lower(sujetpol_autre), "algérie") | str_detect(sujetpol_autre, "indochine") |
        str_detect(sujetpol_autre, "Solidarność") | str_detect(sujetpol_autre, "Irak") |
        str_detect(sujetpol_autre, "covid") | str_detect(sujetpol_autre, "Thatcher") |
        str_detect(sujetpol_autre, "Ukraine") | str_detect(sujetpol_autre, "sida") |
        str_detect(sujetpol_autre, "2008") | str_detect(sujetpol_autre, "Malik") |
        str_detect(sujetpol_autre, "foot") | str_detect(sujetpol_autre, "Orient") |
        str_detect(sujetpol_autre, "Ranucci") | str_detect(sujetpol_autre, "Mandela") |
        str_detect(sujetpol_ideo_autre, "Irak") ~ "Un autre événement marquant",
      str_detect(sujetpol_evnmt, "pulmonaire") | str_detect(sujetpol_evnmt, "absence") |
        str_detect(sujetpol_evnmt, "absence") | str_detect(sujetpol_evnmt, "zep") |
        str_detect(sujetpol_evnmt, "Mélenchon") | str_detect(sujetpol_evnmt, "médias") |
        str_detect(sujetpol_evnmt, "1905") ~ "Autre"),
                         #### sujetpol_evnmt----
                         # "Lorsque mon père est tombé malade (AVC, embolie pulmonaire) , il a d'abord été pris en charge par une clinique de soin privée. Rapidement, la gestion de sa maladie a été catastrophique. J'ai réalisé les méfaits d'un système de santé néolibérale et privé."
                         # "L'absence de structures dédiées à la jeunesse, dans le chef-lieu de canton où j'habitais. Je suis né et ai vécu à l'étranger jusqu'à l'âge de mes 21 ans. Dans un pays au régime non démocratique autoritaire, fascisant."
                         # "la loi de 1905"
                         # "L’impossibilité pour moi de poursuivre le latin, car en zep. Je ne me suis pas laissée faire, ai obtenu de poursuivre, et ai présenté les latin en option au bac."
                         # "un discours de Jean Luc Mélenchon en 2008 et ses conférences sur youtube"
                         # "Les débats politiques dans les médias."
                         
                         #### reste autre :----
                         # "Les cours d'Histoire dispensés au collège sur la XXème siècle"
                         # "Cours d'Histoire du XXème siècle dispensé au collège"
                         # "Débat en classe de 2nd"
                         # "L'arrivée en licence"
                         # "Entrée à l'université"
                         # "objection de conscience"
                         
                         # Mon adhésion à la CGT d'une grande entreprise (Framatome)           
                         # rencontre avec attac                                                
                         # de proche en proche dans le monde associatif                        
                         # Engagement syndical lyceen                                          
                         # Investissement associatif étudiant                                  
                         # présidence d'association                                            
                         # +. Mouvement de jeunesse                                            
                         # rencontre avec des militants communistes et de la jeunesse communis…
                         # Rencontres                                                          
                         # Mouvement de jeunesse
                         
                         # "La misère de mon département"
                         # "l'injustice salariale subie"
                         
                         # "Prise de parole de JLM en 2009"
                         # "la Fête de l'Huma"
                         # "Le hasard, je suis tombé sur une vidéo sur YouTube"
                         # "Participation au défilé FI du 18 mars 2017"
                         
                         # "differences avec mon pere"
                         # "Une façon de m'opposer... à mes parents"
                         # "Du temps libre (chômage) qui m'a permis de me renseigner"
                         # "mésentente avec mon père"
                         # "la vie telle qu'elle est proposée aux plus grand nombre de personnes"
                         # "un homme politique"
                         # "mon conjoint"
                         # "Amis"
                         # "CONJOINT"
                         # "Pif"
    ### sujetpol_ideo_autre 25----
    sujetpol_ideo = case_when(
      str_detect(sujetpol_ideo_autre, "écologiques") ~ "Les questions écologiques",
      str_detect(sujetpol_ideo_autre, "classes") ~ "Les questions d’inégalités économiques",
      str_detect(str_to_lower(sujetpol_ideo_autre), "tous") | str_detect(sujetpol_ideo_autre, "capitalisme") |
        str_detect(sujetpol_ideo_autre, "Questions sociales") | 
        str_detect(str_to_lower(sujetpol_ideo_autre), "tout") ~ "Pas de préférence (Recodage)",
      str_detect(sujetpol_ideo_autre, "monnaie") | str_detect(sujetpol_ideo_autre, "institution") |
        str_detect(sujetpol_ideo_autre, "République") | str_detect(sujetpol_ideo_autre, "Démocratie") ~
        "Les institutions (Recodage)"),
      #### reste autre :
      # "transformation de la société"
      # "judaïsme et marxisme"
      # "citoyens du monde"
      # "Découverte du communisme en histoire"
      # "Mon père était résistant ftp il m'a transmis les valeurs de la résistance"
      # "Questions de la paix"
      # "Les discriminations des homosexuel(le)s"
      # "suites de la seconde guerrel"
      # "féministes et scolaires"
      # "Palestine"
      # "L'écologie, rencontrée en Algérie où j'ai commencé à enseigner à  l'université en 1972 - On parlait de René Dumont - en 1968 le féminisme  avec l'avortement, la psychanalyse, ...  Inégalités sociales: je suis né dans  une famille nombreuse avec des conditions de vie difficiles et par  conséquence j'ai toujours été révolté par les questions sociales . [mais aussi] La guerre d'indépendance de l'Algèrie, du Vietnam, de la Palestine  Mais aussi les débats Chine/URSS, la résistance, ...   et surtout les \"injustices sociales\"."
    ### sujetpol_ideo_evnmt----
    sujetpol_evnmt_rec = case_when(
      str_detect(sujetpol_evnmt, "Charonne") | str_detect(sujetpol_evnmt, "déportation") |
        str_detect(sujetpol_evnmt, "assassinat") | str_detect(sujetpol_evnmt, "déportation") |
        str_detect(sujetpol_evnmt, "Sands") ~ "Répression",
      # "Le massacre de militants à la station de métro Charonne à Paris en 1962"
      # "Intervention policière au métro Charonne en 1962"
      # "La déportation en camp et la mort pour la France de mes deux grands-pères ; mes parents et moi-même devions poursuivre le combat..."# "La déportation en camp et la mort pour la France de mes deux grands-pères ; mes parents et moi-même devions poursuivre le combat..."
      # "L'assassinat des indépendentistes dans la grote d'Uvea en Nouvelle-Calédonie. Mais aussi le problème général que pose l'existence du Front National et de ses thèses."
      # "la grève de la faim de Bobby Sands, militants irlandais"
      # "Grève de la faim de Bobby Sands"
      str_detect(sujetpol_evnmt, "1978") | str_detect(sujetpol_evnmt, "pétrolier") |
        str_detect(sujetpol_evnmt, "financière") | str_detect(sujetpol_evnmt, "Pandémie") |
        str_detect(str_to_lower(sujetpol_evnmt), "apartheid") | str_detect(sujetpol_evnmt, "Vietnam") |
        str_detect(sujetpol_evnmt, "Berlin") | str_detect(sujetpol_evnmt, "Chili") | 
        str_detect(str_to_lower(str_replace(sujetpol_evnmt, "é","e")), "algerie") |
        str_detect(sujetpol_evnmt, "OAS") ~ 
        "Crises politiques et internationales"),
      # "La marée noire Amoco Cadiz 1978"
      # "Le choc pétrolier"
      # "crise financière 2008"
      # "Pandémie"
      # "apartheid"
      # "Apartheid en Afrique du Sud"
      # "Guerre du Vietnam"
      # "La chute du mur de Berlin"
      # "l11 septembre au chili"
      # "Golpe de estado en Chile 1973"
      # "La dictature au Chili"
      # "la fin de la guerre d'Algérie (par le biais de ma sœur plus agée engagée dans les jeunesses communistes)"
      # "L'arrivée du Général De Gaulle au pouvoir en 1958, la guerre d'algérie"
      # "guerre d\"Algerie"
      # "Attantat OAS"
    
    ### quelparti 347----
    # parti_fi = case_when(sigles(quelparti, "fi") & !str_detect(quelparti, "sfio") ~ "Oui")
    parti_trotsmao = case_when(
      sigles(quelparti, "oc- gop") | sigles(quelparti, "oct") | sigles(quelparti, "lo") | 
        sigles(quelparti, "lutte ouvrière") | sigles(quelparti, "oci") | sigles(quelparti, "pci") |
        sigles(quelparti, "poi") | sigles(quelparti, "tmi") | sigles(quelparti, "mppt") | 
        sigles(quelparti, "npa") | sigles(quelparti, "ligue trotskiste de france") |
        sigles(quelparti, "voix des travailleurs") | sigles(quelparti, "une organisation d'extreme gauche") |
        sigles(quelparti, "lcr") | sigles(quelparti, "jcr") ~ "Oui",
      !is.na(quelparti) ~ "Non"),
    parti_gaucherad = case_when(
      sigles(quelparti, "gu") | sigles(quelparti, "ensemble") | sigles(quelparti, "emsenble") | 
        sigles(quelparti, "alternatifs") | sigles(quelparti, "fase") | sigles(quelparti, "fasse") |
        sigles(quelparti, "fédération") | sigles(quelparti, "prcf") | sigles(quelparti, "fdg") | 
        sigles(quelparti, "front de gauche") | sigles(quelparti, "pour une écologie populaire et sociale") |
        sigles(quelparti, "parti de gauche") | sigles(quelparti, "pg") | sigles(quelparti, "partie de gauche") | 
        sigles(quelparti, "psu") | sigles(quelparti, "psa") | sigles(quelparti, "prs") |
        sigles(quelparti, "pour la république sociale") | sigles(quelparti, "parti de la décroissance") ~ "Oui",
      !is.na(quelparti) ~ "Non"),
    parti_pcf = case_when(
      sigles(quelparti, "pcf") | sigles(quelparti, "pc") | sigles(quelparti, "communiste") | 
        sigles(quelparti, "jc") | sigles(quelparti, "mjcf") | sigles(quelparti, "uec") |
        sigles(quelparti, "jeunesses communistes") | sigles(quelparti, "parti communiste français")  ~ "Oui",
      !is.na(quelparti) ~ "Non"),
    parti_gauche = case_when(
      sigles(quelparti, "eelv") | sigles(quelparti, "les verts") | sigles(quelparti, "génération.s") | 
        sigles(quelparti, "grs") | sigles(quelparti, "mdc") | sigles(quelparti, "mcr") |
        sigles(quelparti, "ps") | sigles(quelparti, "parti socialiste") | sigles(quelparti, "sfio") |
        sigles(quelparti, "mjs") | sigles(quelparti, "jeunes socialistes") | sigles(quelparti, "mrg") |
        sigles(quelparti, "gr") | sigles(quelparti, "parti radical") | sigles(quelparti, "nouvelle donne") | 
        sigles(quelparti, "parti socialitse") | sigles(quelparti, "labour") | sigles(quelparti, "psuv") ~ "Oui",
      !is.na(quelparti) ~ "Non"),
    parti_autre = case_when(
      sigles(quelparti,"cds") | sigles(quelparti,"modem") | sigles(quelparti,"ump") |
        sigles(quelparti,"front national") | sigles(quelparti,"front de la jeunesse") |
        sigles(quelparti,"pirate") | sigles(quelparti,"apra") | sigles(quelparti,"pcr") |
        sigles(quelparti,"rezistans") | sigles(quelparti,"espéranto") | sigles(quelparti,"dm25") |
        sigles(quelparti,"mouvement de gauche") | sigles(quelparti,"refondations") | sigles(quelparti,"fer") | 
        sigles(quelparti,"nantes en commun") | sigles(quelparti,"nous sommes montpellier") ~ "Oui",
      !is.na(quelparti)  ~ "Non"),
    
    ### quelsynd 332----
    synd_cgt = case_when(
      engagesynd == "Non" ~ "Non concerné",
      sigles(quelsynd, "cgt") | sigles(quelsynd, "ugict") ~ "Oui",
      !is.na(quelsynd) ~ "Non"),
    synd_cfdt = case_when(
      engagesynd == "Non" ~ "Non concerné",
      sigles(quelsynd, "cfdt") | sigles(quelsynd, "smi") ~ "Oui",
      !is.na(quelsynd) ~ "Non"),
    synd_fo = case_when(
      engagesynd == "Non" ~ "Non concerné",
      sigles(quelsynd, "fo") | sigles(quelsynd, "force ouvrière") | sigles(quelsynd, "snetaa") ~ "Oui",
      !is.na(quelsynd) ~ "Non"),
    synd_fsu = case_when(
      engagesynd == "Non" ~ "Non concerné",
      sigles(quelsynd, "fsu") | sigles(quelsynd, "snuipp") | sigles(quelsynd, "snes") | sigles(quelsynd, "snu") |
        sigles(quelsynd, "syndicat national des chercheurs scientifiques") | sigles(quelsynd, "snesup") ~ "Oui",
      !is.na(quelsynd) ~ "Non"),
    synd_jeunes = case_when(
      engagesynd == "Non" ~ "Non concerné",
      sigles(quelsynd, "force lyceenne") | sigles(quelsynd, "syndicat général des lycéens") |
        sigles(quelsynd, "unl") | sigles(quelsynd, "syndicat général des lycéens") | sigles(quelsynd, "fidl") |
        sigles(quelsynd, "sgl") | sigles(quelsynd, "syndicat étudiant") | sigles(quelsynd, "ubo") | 
        sigles(quelsynd, "sud solidaires") | sigles(quelsynd, "unef") | sigles(quelsynd, "echarde") | 
        sigles(quelsynd, "fage") | sigles(quelsynd, "fse") | sigles(quelsynd, "gaelis") ~ "Oui",
      !is.na(quelsynd) ~ "Non"),
    synd_autre = case_when(
      sigles(quelparti,"fen") | sigles(quelparti,"unsa") | sigles(quelparti,"cgc") | sigles(quelparti,"cnt") |
        sigles(quelparti,"confedération paysanne") | sigles(quelparti,"politique et professionnel") |
        sigles(quelparti,"snill") | sigles(quelparti,"convergence infirmière") |
        sigles(quelparti,"architecture") | sigles(quelparti,"snep") | sigles(quelparti,"snspp") | 
        sigles(quelparti,"synavi") | sigles(quelparti,"mg france") | sigles(quelparti,"sni") | 
        sigles(quelparti,"fer") | sigles(quelparti,"cste") | sigles(quelparti,"sn") ~ "Oui",
      engagesynd == "Non" ~ "Non concerné",
      synd_cgt == "Non" & synd_cfdt == "Non" & synd_fo == "Non" & synd_fsu == "Non" & synd_jeunes == "Non" ~ "Oui",
      !is.na(quelsynd) ~ "Non"))


quest %<>% # ici parce que y'a des gens qui répondent la FI seulement pour le parti
  mutate(engageparti = case_when(parti_trotsmao != "Oui" & parti_gaucherad != "Oui" & parti_pcf!= "Oui" & 
                                    parti_gauche != "Oui" & parti_autre != "Oui" ~ "Non", 
                                    T ~ engageparti),
         parti_trotsmao = ifelse(engageparti == "Non", "Non concerné", parti_trotsmao),
         parti_gaucherad = ifelse(engageparti == "Non", "Non concerné", parti_gaucherad),
         parti_pcf = ifelse(engageparti == "Non", "Non concerné", parti_pcf),
         parti_gauche = ifelse(engageparti == "Non", "Non concerné", parti_gauche),
         parti_autre = ifelse(engageparti == "Non", "Non concerné", parti_autre),
         responssynd_rec = 
           case_when(engagesynd == "Non" ~ "Jamais syndiqué",
                     responssynd_comment %in% c("President de Force Lycéenne\nPresident d'une AGE de l'Unef",
                                                "Trésorier syndical",
                                                "Trésorerie, élu cfvu",
                                                "Responsable locale puis nationale",
                                                "trésorière académique du SNES-FSU",
                                                "Trésorier CE",
                                                "Dans le syndicat infirmier:\n- trésorière \n- Représentante departementale",
                                                "Trésorier de Sud Education /université Poitiers 2005-2007\nà 58 ans",
                                                "Trésorier, membre d'UD",
                                                "responsable de S1 et trésorier départemental",
                                                "secrétaire de section\nsecrétaire nationale",
                                                "Secrétaire fédéral",
                                                "Secrétaire générale",
                                                "Membre elu du CNESER  un mandat",
                                                "conseil administratif national",
                                                "Membre du bureau national de l'UNEF",
                                                "Secrétaire national",
                                                "Responsable relation externe/CM",
                                                "Secrétaire du syndicat CGT dans l'administration Française.",
                                                "ancien secrétaire national du syndicat CGT des agents des Impôts",
                                                "CPH - juge prud'homal\nDS\nélu CE et DP \nAnimateur Commission Action Sociale départemental",
                                                "membre de la direction de l'UNEF de Caen, membre de la direction de la FSU de mon département et de la direction nationale de la FSU.",
                                                "ELUE PARITAIRE",
                                                "Elu commissaire paritaire national pour le SGEN-CFDT",
                                                "Elu au commission administrative paritaire de mon grade , mandaté Chsct , et dans le cadre des structures syndicales , élu à la commission exécutive départ de mon syndicat , représentant cadre au bureau départemental...",
                                                "Secrétaire académique\nCommissaire paritaire",
                                                "élu paritaire",
                                                "Commissions Techniques d,Etablissement et Commissions Paritaires",
                                                "Instance paritaire",
                                                "Commissaire paritaire ( représentant du personnel)",
                                                "commission paritaire sécurité sociale",
                                                "Animateur d'une commission sur la formation continue\nÉlu Cgt  au Conseil d'Administration National du Centre de Formation des Personnels Communaux (pdt  6ans)",
                                                "Président de GAELIS",
                                                "regionale et europe (copa cogeca)") ~ 
                       "Responsabilité syndicale nationale",
                     responssynd_comment %in% c("Présidente section locale",
                                                "secrétaire de section locale, membre du CA",
                                                "Secrétaire de S1 dans les premières années d'IPES puis d'enseignement",
                                                "Président-fondateur (Echarde ENSL)",
                                                "Président de l'Unef à Clermont Ferrand",
                                                "locale et nationale",
                                                "Secrétaire de section locale",
                                                "Secrétaire d'union locale",
                                                "Référente AESH dans l'académie de La Réunion",
                                                "animateur d'UL",
                                                "Conseil syndical, secrétaire d'UL",
                                                "secrétaire général union locale",
                                                "Bureau d'une fédération de la CGT, secrétariat général d'un syndicat de la CGT",
                                                "Vice président d'une union locale",
                                                "Secrétaire Général Union Départementale",
                                                "Membre de la Commission exécutive départementale, responsable cadre ,",
                                                "Secrétaire général d'une section syndicale, membre de la commission exécutive d'un syndicat national et d'Union Départementale.",
                                                "secrétaire départemental et trésorier régional",
                                                "Elue à l'union départementale",
                                                "Fondateur du syndicat au niveau national... puis secrétaire départemental du SNUDI FO dans la Sarthe. puis responsable du syndicat dans le Tarn.\n",
                                                "Responsable UD CGT, secrétaire national UNEF",
                                                "Secrétaire Départemental adjoint",
                                                "membre bureau départemental (93)",
                                                "membre du conseil syndical départemental",
                                                "Responsable départemental de l'Oise et délégué au Conseil académique de la vie lycéenne de Picardie",
                                                "conseil départemental, secrétaire de S1.",
                                                "Comité Exécutif Union Locale Alençon (61)",
                                                "Responsable locale puis nationale",
                                                "département",
                                                "Membre de la coordination de l'Union Académique",
                                                "Déléguée régionale Auvergne-Rhône-Alpes depuis 2019",
                                                "Membre, un court instant, de la direction collégiale de la structure. \nélu suppléant à la CVFU (Commission de Formation et de la Vie Universitaire) de l'UBO (Université de Bretagne Occidentale) en 2019.")  ~ 
                       "Responsabilité syndicale locale",
                     responssynd_comment %in% c("Président de l'antenne locale de mon université",
                                                "Echelon local et académique",
                                                "Le syndicat fonctionne de manière autogestionnaire, mais j'ai été élu au Conseil d'Administration de l'IEP pour le syndicat en 2017-2018",
                                                "Secrétaire de section de lycée pendant 7 ans\nSecrétaire départemental pendant 2 ans",
                                                "Candidat aux élections professionnelles :collège cadre",
                                                "Je suis responsable pour le SNES-FSU au niveau de mon établissement depuis 2020 ; je l'avais déjà été dans mon établissement précédent de 2015 à 2017.",
                                                "Membre du CE à la CGT retraités",
                                                "Conseillère du salarié",
                                                "Responsable d'une section d'établissement",
                                                "délégué",
                                                "mandat électifs secrétaire du CE délégué syndical et délégué syndical central",
                                                "Mandat en Instance CTE, Mandat en Commission excécutive",
                                                "Délégué CE, CAS, UFAS",
                                                "Élu",
                                                "Membre du bureau d'une sous section",
                                                "Membre de bureau",
                                                "Représentante CESER",
                                                "Délégué syndical, Secrétaire de Comité d’établissement, élu au Comité d’entreprise, Président de Commisions, élu au Conseil d’administration, Président d’un organisme paritaire",
                                                "Élu dans des instances consultatives officielles",
                                                "Représentant syndical dans mon entreprise puis licencié quelques semaines après",
                                                "DIRIGEANT SYNDICAT ENTREPRISE 5000 personnes\nSECRETAIRE DU COMITE CENTRAL D'ENTREPRISE\nADMINISTRATEUR REPRESENTANT LES SALARIES",
                                                "Secrétaire de section",
                                                "Responsable de section",
                                                "Coordinateur interne d'une section",
                                                "conseiller syndical",
                                                "Je suis secrétaire de bureau pour mon syndicat FO",
                                                "Secrétaire",
                                                "Secrétaire du syndicat",
                                                "Membre, un court instant, de la direction collégiale de la structure. \nélu suppléant à la CVFU (Commission de Formation et de la Vie Universitaire) de l'UBO (Université de Bretagne Occidentale) en 2019.",
                                                "secrétaire snudi-fo") |
                       responsparti_comment %in% c("secrétaire de section et membre du comité  fédéral, secrétaire d'un syndicat cgt, délégué du personnel, délégué syndical",
                                                   "Élu syndical, directeur de campagne à la france insoumise a plusieurs reprises") ~ 
                       "Responsabilités syndicales établissement",
           !is.na(engagesynd) ~ "Pas de responsabilité syndicale"),
         responsparti_rec = 
           case_when(engageparti == "Non" ~ "Jamais encarté",
                     responsparti_comment %in% c("Responsabilités départementales (co-secrétaire, trésorière) et nationales (Conseil national, commissions de travail internes)",
                                                 "Trésorière de parti\nCo coordinatrice\n",
                                                 "Conseiller National",
                                                 "Directeur de campagne aux législatives",
                                                 "Co-président de la commission \"Economie\" pendant un an",
                                                 "Commission thématique, animation de groupe, rencontre en tant que représentant du parti dans le cadre de relations unitaires",
                                                 "Membre du Comité National",
                                                 "ELUS\nCOMITE FEDERAL",
                                                 "Secrétaire jeunesse communiste",
                                                 "Responsable JEunes socialistes.",
                                                 "SECRETAIRE FEDERAL  PS",
                                                 "Responsable lyceen local a la JC\nResponsable national jeune au npa\nConseiller national a la GU\nCoordinateur départemental au FDG\nResponsable national a Ensemble",
                                                 "Secrétaire de cellule Pcf\nSecrétaire comité PG\nAnimateur commission logement PG\nAnimateur comité local PG\nAnimateur livret logement FI\nAnimateur groupé d'action FI\nCandidat législatives FI",
                                                 "Au PG, Co-secrétaire de groupe local,\nCo-secrétaire départemental et conseiller national.",
                                                 "Conseil national, co-secrétaire locale",
                                                 "Responsable aux affaires européennes",
                                                 "Conseil national du pg",
                                                 "co-présidente du Conseil Programmatique de Nouvelle Donne",
                                                 "Secrétaire d'organisation à l'APRA dans le contexte universitaire",
                                                 "Co-secrétaire départemental, Conseiller National",
                                                 "Responsable local, départemental et national",
                                                 "Locales et départementales sur le fond. Nationales et Internationales par nécessité. Secrétaire national UEC, conseil national MJCF, secrétaire départemental PCF",
                                                 "Responsable du fichier des adhérents\nQuelques tâches de développement informatique",
                                                 "Secrétaire de comité et membre du Conseil National",
                                                 "secretaire de section\n animateur national",
                                                 "Secrétaire de cercle des MJCF et secrétaire fédérale UEC",
                                                 "Secrétaire fédéral PS",
                                                 "Conseil federal",
                                                 "Responsable de cellule, et du comité nationale de la défense des femmes. \nCandidates aux législatives et élus conseillère municipale",
                                                 "élu municipal, secrétaire de cellule",
                                                 "Secrétaire départemental du Parti de Gauche Nord, Secrétaire national du PG, candidat lfi législatives 2017, européennes 2019, tête de liste aux municipales de Lille en 2020",
                                                 "membre du comité directeur du PRG22",
                                                 "secrétaire de section et membre du comité  fédéral, secrétaire d'un syndicat cgt, délégué du personnel, délégué syndical",
                                                 "Cosecrétaire de comité au Parti de Gauche pendant plusieurs années, plusieurs fois délégué au Congrès, délégué suppléant au Conseil National",
                                                 "membre de la direction politique Nationale et secrétaire de section",
                                                 "Membre du conseil national\nCandidat à de multiples reprises",
                                                 "Président de la commission nationale des conflits") ~
                       "Responsabilité partisane nationale",
                     responsparti_comment %in% c("Responsabilités locales",
                                                 "Departementale",
                                                 "département",
                                                 "Animateur MJS",
                                                 "Cadre Jeune / 75",
                                                 "Responsable de cellule...",
                                                 "responsable au niveau local",
                                                 "Co secrétaire de section.",
                                                 "Secrétaire de section PS\nco-secrétaire départemental PG",
                                                 "secrétaire dev section ps dans les années 1970",
                                                 "Secrétaire de section",
                                                 "Secrétaire de section au PCF. J'ai suivi l'école centrale d'un mois.",
                                                 "Responsable départemental.",
                                                 "Conseil national, secrétariat departemental",
                                                 "Parti Communiste Reunionnais :Responsable de section\n\nRE-974: vice présidente",
                                                 "responsable régional au PCI, responsabilité départementale au Parti de Gauche\n\n",
                                                 "Responsable local pcf",
                                                 "Secrétaire de cellule, de Section.",
                                                 "Locales, responsable de comité",
                                                 "MEMBRE DE LA DIRECTION DES JCR DE CAEN, membre de la direction du PCF du Calvados. Membre de la direction locale et nationale de l'UEC.",
                                                 "secrétaire de cellule, membre du comité départemental, rédacteur en chef de l'hebdo du PCF Gironde",
                                                 "Secrétaire du comité local",
                                                 "Secrétaire du comité local du Parti de Gauche. Président d'association.",
                                                 "Responsable de section locale, membre du bureau fédéral",
                                                 "tete de liste aux elections municipales\npuis élu d'opposition",
                                                 "Secrétariat départemental",
                                                 "secrétaire de section locale",
                                                 "secrettaire de section",
                                                 "Premier secrétaire d'une fédération départementale",
                                                 "Responsable local. + elu municipal",
                                                 "secrétaire de cellule.",
                                                 "Représentant à la Coordination Départementale (PG33)",
                                                 "secretaire section",
                                                 "Secrétaire départemental",
                                                 "Co-secrétaire départemental PG59",
                                                 "trésorier départemental",
                                                 "Secrétaire de cellule\nprésident d'une Maison de la Culture en région parisienne",
                                                 "animateur local",
                                                 "Durant 15 ans membres du conseil départemental au PCF",
                                                 "candidat, animateur de groupe",
                                                 "Porte parole départementale",
                                                 "Mandat interne au parti à l'échelle départemental",
                                                 "Responsabilités départementales (co-secrétaire, trésorière) et nationales (Conseil national, commissions de travail internes)",
                                                 "Responsable lyceen local a la JC\nResponsable national jeune au npa\nConseiller national a la GU\nCoordinateur départemental au FDG\nResponsable national a Ensemble",
                                                 "Secrétaire de cellule Pcf\nSecrétaire comité PG\nAnimateur commission logement PG\nAnimateur comité local PG\nAnimateur livret logement FI\nAnimateur groupé d'action FI\nCandidat législatives FI",
                                                 "Au PG, Co-secrétaire de groupe local,\nCo-secrétaire départemental et conseiller national.",
                                                 "Trésorière de cellule",
                                                 "Mandataire financier pour les élections municipales",
                                                 "Trésorier départemental  du PS puis du PG encore aujourd'hui",
                                                 "Conseil national, co-secrétaire locale",
                                                 "Membre du Secrétariat départemental durant 12 ans",
                                                 "Locales et départementales sur le fond. Nationales et Internationales par nécessité. Secrétaire national UEC, conseil national MJCF, secrétaire départemental PCF",
                                                 "bureau departemental pcf vaucluse .... elu  pcf dans la majorité conseil municipal avignon 77-_\" liste union de la gauche",
                                                 "Responsabilités départementales (co-secrétaire, trésorière) et nationales (Conseil national, commissions de travail internes)",
                                                 "Je suis membre du conseil fédéral du PCF 79, responsable de la communication.\nPar ailleurs, durant la campagne présidentielle de 2017, j’ai participé à l’Espace politique de la FI, qui regroupait autour de JLM, les partis ou mouvements amis.",
                                                 "Locales et départementales sur le fond. Nationales et Internationales par nécessité. Secrétaire national UEC, conseil national MJCF, secrétaire départemental PCF",
                                                 "Secrétaire de comité et membre du Conseil National",
                                                 "Co-secrétaire départementale.\nPourquoi votre question est-elle au passé ? Je suis toujours engagées au PG.",
                                                 "Co secrétaire de comité",
                                                 "Co-secrétaire départemental, Conseiller National",
                                                 "Responsable local, départemental et national",
                                                 "secretaire de section\n animateur national",
                                                 "Responsable de cellule, et du comité nationale de la défense des femmes. \nCandidates aux législatives et élus conseillère municipale",
                                                 "Secrétaire départemental du Parti de Gauche Nord, Secrétaire national du PG, candidat lfi législatives 2017, européennes 2019, tête de liste aux municipales de Lille en 2020",
                                                 "Responsable de section",
                                                 "J'ai été co-secrétaire départemental du PG45 (une petite centaine d'adhérents, environs 15/20 militants)",
                                                 "membre du comité directeur du PRG22",
                                                 "secrétaire de section et membre du comité  fédéral, secrétaire d'un syndicat cgt, délégué du personnel, délégué syndical",
                                                 "Cosecrétaire de comité au Parti de Gauche pendant plusieurs années, plusieurs fois délégué au Congrès, délégué suppléant au Conseil National",
                                                 "membre de la direction politique Nationale et secrétaire de section",
                                                 "Publication twitter",
                                                 "Trésorier. Organistion",
                                                 "Trèsorier",
                                                 "Trésorier",
                                                 "Directrice de campagne",
                                                 "Élu syndical, directeur de campagne à la france insoumise a plusieurs reprises",
                                                 "Directeur de campagne et directeur financier",
                                                 "mandataire financier de candidats",
                                                 "responsable des relations avec la presse",
                                                 "Secrétaire") ~ 
                       "Responsabilité partisane locale",
                     !is.na(engageparti) ~ "Pas de responsabilité partisane")
         )

quest %<>% 
  mutate(agecat = cut(quest$age, include.lowest = FALSE, right = FALSE,
                      dig.lab = 4, breaks = c(16, 25, 35, 45, 55, 63, 70, 87)) %>% 
           str_replace("\\[","") %>% 
           str_replace(",","-") %>% 
           str_replace("\\)"," ans"),
         agecat_rec = cut(quest$age, include.lowest = FALSE, right = FALSE,
                          dig.lab = 4, breaks = c(16, 30, 45, 63, 87)),
         anneelfi = ifelse(anneelfi == 1975, NA_integer_, anneelfi),
         dipl_rec = 
           case_when(dipl %in% c("Sans diplôme", "BEP/CAP ou équivalent", "BEPC ou équivalent", "Baccalauréat") ~ 
                       "Baccalauréat ou moins",
                     dipl %in% c("DEUG, BTS, DUT, DEUST", "Licence/licence professionnelle") ~ 
                       "Études supérieures courtes",
                     dipl %in% c("Maîtrise/Master 1", "Master et assimilé", "Doctorat, HDR") ~ 
                       "Études supérieures longues"),
         classes_soc_par = 
           case_when(pcs_pere %in% c("Cadres", "Professeurs, professions scientifiques","Professions libérales") | 
                       pcs_mere %in% c("Cadres", "Professeurs, professions scientifiques","Professions libérales") ~ 
                       "CS moyenne supérieure",
                     pcs_pere %in% c("Autres professions intermédiaires","Professeurs des écoles, instituteurs et assimilé") ~ 
                       "CS moyenne inférieure",
                     pcs_pere %in% c("Chômeurs", "Employés","Ouvriers","Autre inactifs", "Agriculteurs exploitants") ~ 
                       "CS populaires"),
         classes_soc = 
           case_when(pcs_sr == "Élèves ou étudiants" ~ classes_soc_par,
             pcs_sr %in% c("Cadres", "Professeurs, professions scientifiques","Professions libérales", 
                           "Artisans, commerçants, chefs d'entreprise") ~ "CS moyenne supérieure",
             pcs_sr %in% c("Autres professions intermédiaires","Professeurs des écoles, instituteurs et assimilé") ~ 
               "CS moyenne inférieure",
             pcs_sr %in% c("Agriculteurs exploitants","Chômeurs", "Employés","Ouvriers","Autre inactifs") ~ 
               "CS populaires"),
         dipl_par = 
           case_when(dipl_pere %in% c("Maîtrise/Master 1", "Master et assimilé", "Doctorat, HDR") |
                       dipl_pere %in% c("Maîtrise/Master 1", "Master et assimilé", "Doctorat, HDR") ~ 
                       "Études supérieures longues",
                     dipl_pere %in% c("DEUG, BTS, DUT, DEUST", "Licence/licence professionnelle") |
                       dipl_mere %in% c("DEUG, BTS, DUT, DEUST", "Licence/licence professionnelle") ~ 
                       "Études supérieures courtes",
                     dipl_pere %in% c("Baccalauréat") |
                       dipl_mere %in% c("Baccalauréat") ~ "Baccalauréat",
                     dipl_pere %in% c("BEP/CAP ou équivalent") | 
                       dipl_mere %in% c("BEP/CAP ou équivalent") ~ "BEP/CAP ou équivalent",
                     dipl_pere == "Sans diplôme" | dipl_mere == "Sans diplôme" ~ "Sans diplôme"),
         parti_par = if_else(parti_pere == "Oui" | parti_mere == "Oui", "Oui", "Non",NA_character_),
         synd_par = if_else(synd_pere == "Oui" | synd_mere == "Oui", "Oui", "Non",NA_character_),
         engage_par =  if_else(parti_par == "Oui" | synd_par == "Oui", "Oui", "Non",NA_character_),
         engage =  if_else(engageparti == "Oui" | engagesynd == "Oui", "Oui", "Non",NA_character_),
         engagelfi_jlm2 = case_when(engagelfi_jlm %in% c("Pas important du tout", "Peu important") ~ 
                                      "Pas important", 
                                    engagelfi_jlm %in% c("Assez important", "Très important") ~ "Important"),
         engagelfi_gauche2 = case_when(engagelfi_gauche %in% c("Pas important du tout", "Peu important") ~ 
                                         "Pas important", 
                                       engagelfi_gauche %in% c("Assez important", "Très important") ~ "Important"),
         engagelfi_election2 = case_when(engagelfi_election %in% c("Pas important du tout", "Peu important") ~ 
                                           "Pas important", 
                                         engagelfi_election %in% c("Assez important", "Très important") ~ "Important"),
         engagelfi_droits2 = case_when(engagelfi_droits %in% c("Pas important du tout", "Peu important") ~ 
                                         "Pas important", 
                                       engagelfi_droits %in% c("Assez important", "Très important") ~ "Important"),
         engagelfi_carriere2 = case_when(engagelfi_carriere %in% c("Pas important du tout", "Peu important") ~ 
                                           "Pas important", 
                                         engagelfi_carriere %in% c("Assez important", "Très important") ~ "Important"),
         engagelfi_rencontre2 = case_when(engagelfi_rencontre %in% c("Pas important du tout", "Peu important") ~ 
                                            "Pas important", 
                                          engagelfi_rencontre %in% c("Assez important", "Très important") ~ "Important"),
         engagelfi_souple2 = case_when(engagelfi_souple %in% c("Pas important du tout", "Peu important") ~ 
                                         "Pas important", 
                                       engagelfi_souple %in% c("Assez important", "Très important") ~ "Important"),
         engagelfi_prox2 = case_when(engagelfi_prox %in% c("Pas important du tout", "Peu important") ~ 
                                       "Pas important", 
                                     engagelfi_prox %in% c("Assez important", "Très important") ~ "Important"),
         engagelfi_fam2 = case_when(engagelfi_fam %in% c("Pas important du tout", "Peu important") ~ 
                                      "Pas important", 
                                    engagelfi_fam %in% c("Assez important", "Très important") ~ "Important"),
         covid_num = case_when(str_detect(covid_autre, "visio") | covid_num == "Oui"   ~ "Oui",
                               covid_num == "Non" ~ "Non"),
         covid_manif = case_when(str_detect(covid_autre, "manifs") | covid_manif == "Oui"   ~ "Oui",
                                 covid_manif == "Non" ~ "Non"),
         covid_coll = case_when(str_detect(covid_autre, "aide matérielle à personne en difficulté") |
                                  str_detect(covid_autre, "Secours populaire") |
                                  covid_coll == "Oui"   ~ "Oui",
                                covid_coll == "Non" ~ "Non"),
         freqreu = case_when(freqreu == "Je ne suis pas dans un groupe" ~ "Pas dans un groupe",
                              T ~ freqreu),
         freqreu_rec = case_when(freqreu == "Je ne suis pas dans un groupe" ~ "Pas dans un groupe",
                                 freqreu %in% c("Plus d'une fois par semaine", "Une fois par semaine") ~ 
                                   "Une fois par semaine ou plus",
                                 T ~ freqreu),
         freqparticipereu = case_when(freqreu == "Pas dans un groupe" ~ "Pas dans un groupe",
                                       T ~ freqparticipereu),
         freqparticipereu_rec = case_when(freqparticipereu %in% c("À peu de réunions", "À presque aucune réunion") ~
                                        "À peu voire aucune réunion",
                                        
                                      T ~ freqparticipereu))


# Dictionnaire----
questions <- read_csv(here("Doc","Questions.csv"),
                      na = c("","NA","N/A"))

questions <- c(names(questions),"Quelle est votre catégorie socio-professionnelle ? (Retraité)",
               "Quelle est votre catégorie socio-professionnelle ? (avant retraite)",
               "Quelle est votre catégorie socio-professionnelle ?","2021 - naiss",
               "Recodage évènement marquant",
               "quelparti = Extrême gauche","quelparti = autre gauche radicale","quelparti = PCF",
               "quelparti = gauche traditionnelle","quelparti = autre","quelsynd = CGT",
               "quelsynd = CFDT","quelsynd = FO","quelsynd = FSU","quelsynd = lycéen ou étudiant",
               "quelsynd = autre", "Responsabilité synd (recodage)", "Responsabilité parti (recodage)",
               "Catégorie d'âge", "Catégorie d'âge (quartile)", 
               "Diplôme (recodage)", 
               "Classe sociale des parents", "Classe sociale", "Diplôme des parents", 
               "Parents dans un parti", "Parents dans un syndicat",
               "Parents engagés dans un parti ou syndicat", "Engagé dans un parti ou syndicat",
               paste(names(questions)[98:106],"(recodage)"), "Réunion groupe (recodage)", "Réunions égo (recodage)")
questions <- as.data.frame(cbind(names(quest),questions))
names(questions)[1] <- "variables"
questions$type <- as.col_spec(quest) %>% 
  as.character() %>% 
  str_split("") %>% 
  unlist 

for (i in 1:nrow(questions)) {
  questions$X[i] <- quest[,i] %>% 
    as.matrix() %>% 
    as.factor() %>% 
    levels() %>% 
    glue_collapse(sep = "|")
}
rm(i)

questions$X <- ifelse(questions$type == "c", questions$X, "")
questions$type[questions$variables == "echpol_par2"] <- "c"

questions$X[questions$variables %in% c("id","submitdate","lastpage",
                                       "startdate","datestamp","genre_autre","amis_autre",
                                       "camarades","autresmili_comment","responsparti_comment",
                                       "sujetpol_autre","quelparti","quelsynd","sujetpol_evnmt_rec",
                                       "responssynd_comment","covid_autre","autoorga",
                                       "remarque_mili","remarque_questio","sujetpol_evnmt",
                                       "difficultpro_other","sujetpol_ideo_autre","age")] <- ""

questions$X[questions$variables %in% c("echpol","echpol_par2")] <- 
  questions$X[questions$variables == "echpol_pere"]

questions$X[questions$variables == "impquestion_npa_log"] <- 
  questions$X[questions$variables == "impquestion_npa_spap"]


questions$X[questions$variables %in% c("pcs_pere","pcs_mere","pcs_par2","pcs_conj","pcs")] <- 
  paste("Agriculteur-rice","Artisan, commerçant-e, chef d’entreprise","Employé-e",
        "Profession libérale","Cadre","Professeur-e, profession scientifique",
        "Professeur-e des écoles, instituteur-rice et assimilé",
        "Autres professions intermédiaires","Ouvrier-ère","Étudiant-e" ,"Chômeur-euse",
        "Autre inactif-ve", sep = "|")

questions$X[questions$variables %in% c("dipl_pere","dipl_mere","dipl_par2","dipl_conj")] <- 
  questions$X[questions$variables == "dipl"]

questions %<>%
  mutate(bloc = c(rep(1,6), rep(2,28), rep(3,29), rep(4,34), rep(5,69), rep(6,14), rep(7,2),
                  rep(1,4), rep(4,14), rep(1,6), rep(2,3), rep(3,1), rep(4,9), rep(5,2)))

for (n in 1:nrow(questions)) {
  questions$`pct_NA`[n] <- quest %>% 
    filter(lastpage >= questions$bloc[n]) %>% 
    select(questions$variables[n]) %>%
    summarise(pct_NA = 100 * sum(is.na(.)) / nrow(.)) %>% 
    unlist()
}

# on retranche les filtres
questions$pct_NA[questions$variables %in% c("sect","statut", "chgtentreprise")] <- 
  questions$pct_NA[questions$variables %in% c("sect","statut", "chgtentreprise")] - 
  prop.table(table(quest$pcs, useNA = "ifany"))["Élèves ou étudiants"] * 100

questions$pct_NA[questions$variables %in% c("univ","med","iut","bts","cpge","inge","ens")] <- 
  questions$pct_NA[questions$variables %in% c("univ","med","iut","bts","cpge","inge","ens")] - 
  prop.table(table(quest$etabens_sup, useNA = "ifany"))[4] * 100

questions$pct_NA[questions$variables %in% c("freqparticipereu")] <- 
  questions$pct_NA[questions$variables %in% c("freqparticipereu")] - 
  prop.table(table(quest$freqreu, useNA = "ifany"))["Pas dans un groupe"] * 100

write_csv(questions, here("Doc","Dictionnaire.csv"))

quest <- set_label(quest, label = questions$questions)

write_labelled_csv(quest, file = here("Donnees","quest_LFI2_labels.csv"))

write_csv(quest, here("Donnees","quest_LFI2.csv"))




# BDD pour ACM----
quest_acm <- quest %>% 
  mutate(etabens_prim = str_replace(etabens_prim,"Établissement","Établissement primaire"),
         etabens_second = str_replace(etabens_second,"Établissement","Établissement secondaire"),
         etabens_sup = str_replace(etabens_sup,"Établissement","Établissement supérieur"),
         univ = ifelse(univ == "Oui", "Université", "Pas université"),
         med = ifelse(med == "Oui", "Médecine", "Pas médecine"),
         iut = ifelse(iut == "Oui", "IUT", "Pas IUT"),
         bts = ifelse(bts == "Oui", "BTS", "Pas BTS"),
         cpge = ifelse(cpge == "Oui", "CPGE", "Pas CPGE"),
         inge = ifelse(inge == "Oui", "École d'ingénieur", "Pas école d'ingénieur"),
         ens = ifelse(ens == "Oui", "ENS", "Pas ENS"),
         sect = case_when(sect == "Le secteur privé" ~ "Secteur privé",
                          sect == "Le secteur public" ~ "Secteur public",
                          sect == "Le secteur associatif" ~ "Secteur associatif",
                          sect == "L’économie mixte (entreprise dans laquelle l’État et/ou les collectivités ont une participation)" ~ "Secteur mixte"),
         chgtentreprise = ifelse(chgtentreprise == "Oui", "A changé d'entreprise", 
                                 "N'a pas changé d'entreprise"),
         licencie = ifelse(licencie == "Oui", "Licencié", "Pas licencié"),
         classes_soc_par = ifelse(is.na(classes_soc_par), classes_soc_par, 
                           paste("Parents", str_to_lower(classes_soc_par), sep = " : ")),
         pcs_pere = ifelse(is.na(pcs_pere), pcs_pere, 
                           paste("Père", str_to_lower(pcs_pere), sep = " : ")),
         pcs_mere = ifelse(is.na(pcs_mere), pcs_mere, 
                           paste("Mère", str_to_lower(pcs_mere), sep = " : ")),
         pcs_par2 = ifelse(is.na(pcs_par2), pcs_par2, 
                           paste("Parent 2", str_to_lower(pcs_par2), sep = " : ")),
         pcs_conj = ifelse(is.na(pcs_conj), pcs_conj, 
                           paste("Conjoint", str_to_lower(pcs_conj), sep = " : ")),
         dipl_pere = ifelse(is.na(dipl_pere), dipl_pere, 
                            paste("Père", str_to_lower(dipl_pere), sep = " : ")),
         dipl_mere = ifelse(is.na(dipl_mere), dipl_mere, 
                            paste("Mère", str_to_lower(dipl_mere), sep = " : ")),
         dipl_conj = ifelse(is.na(dipl_conj), dipl_conj, 
                            paste("Conjoint", str_to_lower(dipl_conj), sep = " : ")),
         ville = case_when(ville == "Dans une zone rurale"                   ~ "Zone rurale",
                           ville == "Dans une ville moyenne ou petite ville" ~ "Ville petite ou moyenne",
                           ville == "Dans une grande ville ou sa périphérie" ~ "Grande ville ou sa périphérie"),
         parlpol = ifelse(is.na(parlpol), parlpol, 
                          paste("Enfant parlait politique", str_to_lower(parlpol), sep = " : ")),
         parolejeune = ifelse(parolejeune == "Oui", 
                              "À l'aise à l'oral à l'école","Pas à l'aise à l'oral à l'école"),
         parti_pere = ifelse(parti_pere == "Oui", "Père parti oui", "Père parti non"),
         synd_pere = ifelse(synd_pere == "Oui", "Père syndicat oui", "Père syndicat non"),
         asso_pere = ifelse(asso_pere == "Oui", "Père association oui", "Père association non"),
         parti_mere = ifelse(parti_mere == "Oui", "Mère parti oui", "Mère parti non"),
         synd_mere = ifelse(synd_mere == "Oui", "Mère syndicat oui", "Mère syndicat non"),
         asso_mere = ifelse(asso_mere == "Oui", "Mère association oui", "Mère association non"),
         parti_conj = ifelse(parti_conj == "Oui", "Conjoint parti oui", "Conjoint parti non"),
         synd_conj = ifelse(synd_conj == "Oui", "Conjoint syndicat oui", "Conjoint syndicat non"),
         asso_conj = ifelse(asso_conj == "Oui", "Conjoint association oui", "Conjoint association non"),
         messe = ifelse(is.na(messe), messe, 
                        paste("Activités religieuses", str_to_lower(messe), sep = " : ")),
         amis_lfi = ifelse(amis_lfi == "Oui", "Amis FI", "Pas d'amis FI"),
         amis_gauche = ifelse(amis_gauche == "Oui", "Amis GR", "Pas d'amis GR"),
         amis_droite = ifelse(amis_droite == "Oui", "Amis droite", "Pas d'amis droite"),
         amis_synd = ifelse(amis_synd == "Oui", "Amis syndiqués", "Pas d'amis syndiqués"),
         amis_abseng = ifelse(amis_abseng == "Oui", "Amis engagé", "Pas d'amis engagé"),
         autresmili = case_when(autresmili == "On ne se fréquente pas" ~ "Fréquente pas les autres militants",
                                autresmili == "On se fréquente occasionnellement aux rendez-vous militants (manifestations, tractages...) mais sans plus" ~ "Fréquente peu les autres militants",
                                autresmili == "On se fréquente souvent pour planifier des actions en commun" ~ "Fréquente souvent les autres militants"),
         sujetpol = ifelse(is.na(sujetpol), sujetpol, 
                           paste("Intérêt pour", str_to_lower(sujetpol), sep = " : ")),
         engageparti = ifelse(is.na(engageparti), engageparti, 
                              paste("Parti", str_to_lower(engageparti), sep = " : ")),
         engagesynd = ifelse(is.na(engagesynd), engagesynd, 
                             paste("Syndicat", str_to_lower(engagesynd), sep = " : ")),
         engagecoll = ifelse(is.na(engagecoll), engagecoll, 
                             paste("Collectif", str_to_lower(engagecoll), sep = " : ")),
         engageasso = ifelse(is.na(engageasso), engageasso, 
                             paste("Association", str_to_lower(engageasso), sep = " : ")),
         responsparti = ifelse(is.na(responsparti), responsparti, 
                               paste("Responsabilité parti", 
                                     str_to_lower(responsparti), sep = " : ")),
         responssynd = ifelse(is.na(responssynd), responssynd, 
                              paste("Responsabilité syndicat", 
                                    str_to_lower(responssynd), sep = " : ")),
         accompmanif_amis = ifelse(is.na(accompmanif_amis), accompmanif_amis,
                                   paste("1ere manifestations amis", 
                                         str_to_lower(accompmanif_amis), sep = " : ")),
         accompmanif_fam = ifelse(is.na(accompmanif_fam), accompmanif_fam,
                                  paste("1ere manifestations famille", 
                                        str_to_lower(accompmanif_fam), sep = " : ")),
         accompmanif_parti = ifelse(is.na(accompmanif_parti), accompmanif_parti,
                                    paste("1ere manifestations parti", 
                                          str_to_lower(accompmanif_parti), sep = " : ")),
         accompmanif_synd = ifelse(is.na(accompmanif_synd), accompmanif_synd,
                                   paste("1ere manifestations syndicat", 
                                         str_to_lower(accompmanif_synd), sep = " : ")),
         accompmanif_coll = ifelse(is.na(accompmanif_coll), accompmanif_coll,
                                   paste("1ere manifestations collectif", 
                                         str_to_lower(accompmanif_coll), sep = " : ")),
         accompmanif_lfi = ifelse(is.na(accompmanif_lfi), accompmanif_lfi,
                                  paste("1ere manifestations FI", 
                                        str_to_lower(accompmanif_lfi), sep = " : ")),
         accompmanif_seul = ifelse(is.na(accompmanif_seul), accompmanif_seul,
                                   paste("1ere manifestations seul", 
                                         str_to_lower(accompmanif_seul), sep = " : ")),
         vote_presid = ifelse(vote_presid == "Oui", "Vote présidentielle",
                              "Vote pas présidentielle"),
         vote_legis = ifelse(vote_legis == "Oui", "Vote législatives", "Vote pas législatives"),
         vote_region = ifelse(vote_region == "Oui", "Vote régionales", "Vote pas régionales"),
         vote_ue = ifelse(vote_ue == "Oui", "Vote européennes", "Vote pas européennes"),
         vote_munic = ifelse(vote_munic == "Oui", "Vote municipales", "Vote pas municipales"),
         eluavant = ifelse(eluavant == "Oui", "Élu avant FI", "Pas élu avant FI"),
         engagelfi_jlm = ifelse(is.na(engagelfi_jlm), engagelfi_jlm,
                                paste("JLM", str_to_lower(engagelfi_jlm), sep = " : ")),
         engagelfi_gauche = ifelse(is.na(engagelfi_gauche), engagelfi_gauche,
                                   paste("Gauche", str_to_lower(engagelfi_gauche), sep = " : ")),
         engagelfi_election = ifelse(is.na(engagelfi_election), engagelfi_election,
                                     paste("Victoire", str_to_lower(engagelfi_election), sep = " : ")),
         engagelfi_droits = ifelse(is.na(engagelfi_droits), engagelfi_droits,
                                   paste("Droits", str_to_lower(engagelfi_droits), sep = " : ")),
         engagelfi_carriere = ifelse(is.na(engagelfi_carriere), engagelfi_carriere,
                                     paste("Carrière politique", str_to_lower(engagelfi_carriere), sep = " : ")),
         engagelfi_fam = ifelse(is.na(engagelfi_fam), engagelfi_fam,
                                paste("Famille", str_to_lower(engagelfi_fam), sep = " : ")),
         engagelfi_rencontre = ifelse(is.na(engagelfi_rencontre), engagelfi_rencontre,
                                      paste("Rencontres", str_to_lower(engagelfi_rencontre), sep = " : ")),
         engagelfi_souple = ifelse(is.na(engagelfi_souple), engagelfi_souple,
                                   paste("Souplesse FI", str_to_lower(engagelfi_souple), sep = " : ")),
         engagelfi_prox = ifelse(is.na(engagelfi_prox), engagelfi_prox,
                                 paste("Proximité GA", str_to_lower(engagelfi_prox), sep = " : ")),
         freqreu = ifelse(is.na(freqreu) | freqreu == "Pas dans un groupe", 
                          freqreu, paste("Réunion GA", str_to_lower(freqreu), sep = " : ")),
         freqparticipereu = ifelse(is.na(freqparticipereu) | 
                                     freqparticipereu == "Pas dans un groupe", 
                                   freqparticipereu,
                                   paste("Assiste réunion", str_to_lower(freqparticipereu), sep = " : ")),
         freqreu_rec = ifelse(is.na(freqreu_rec) | freqreu_rec == "Pas dans un groupe", 
                          freqreu_rec, paste("Réunion GA", str_to_lower(freqreu_rec), sep = " : ")),
         freqparticipereu_rec = ifelse(is.na(freqparticipereu_rec) | 
                                     freqparticipereu_rec == "Pas dans un groupe", 
                                   freqparticipereu_rec,
                                   paste("Assiste réunion", str_to_lower(freqparticipereu_rec), sep = " : ")),
         redac_tractfi = ifelse(is.na(redac_tractfi), redac_tractfi,
                                paste("Tract FI", str_to_lower(redac_tractfi), sep = " : ")),
         redac_tractparti = ifelse(is.na(redac_tractparti), redac_tractparti,
                                   paste("Tract parti/syndicat", str_to_lower(redac_tractparti), sep = " : ")),
         redac_communique = ifelse(is.na(redac_communique), redac_communique,
                                   paste("Communiqués", str_to_lower(redac_communique), sep = " : ")),
         redac_articlefi = ifelse(is.na(redac_articlefi), redac_articlefi,
                                  paste("Article FI", str_to_lower(redac_articlefi), sep = " : ")),
         redac_articlega = ifelse(is.na(redac_articlega), redac_articlega,
                                  paste("Article GA", str_to_lower(redac_articlega), sep = " : ")),
         redac_articlepartisynd = ifelse(is.na(redac_articlepartisynd), redac_articlepartisynd,
                                         paste("Article parti/syndicat", str_to_lower(redac_articlepartisynd), sep = " : ")),
         redac_articleperso = ifelse(is.na(redac_articleperso), redac_articleperso,
                                     paste("Article personnel", str_to_lower(redac_articleperso), sep = " : ")),
         redac_aec = ifelse(is.na(redac_aec), redac_aec,
                            paste("Livrets AEC", str_to_lower(redac_aec), sep = " : ")),
         redac_atelierloi = ifelse(is.na(redac_atelierloi), redac_atelierloi,
                                   paste("Ateliers lois", str_to_lower(redac_atelierloi), sep = " : ")),
         publifi = ifelse(is.na(publifi), publifi,
                          paste("Publications FI", str_to_lower(publifi), sep = " : ")),
         actufirs = ifelse(is.na(actufirs), actufirs,
                           paste("Actualités FI", str_to_lower(actufirs), sep = " : ")),
         groupefb = ifelse(groupefb == "Oui","Groupe réseaux", "Pas groupe réseaux"),
         participgroupefb = ifelse(is.na(participgroupefb), participgroupefb,
                                   paste("Groupe réseaux", str_to_lower(participgroupefb), sep = " : ")),
         conventionfi = ifelse(conventionfi == "Oui", "Assemblée", "Pas assemblée"),
         debatconvention = ifelse(is.na(debatconvention), debatconvention,
                                  paste("Assemblée", str_to_lower(debatconvention), sep = " : ")),
         amfis = ifelse(amfis == "Oui", "AMFIS", "Pas AMFIS"),
         nbamfis = ifelse(is.na(nbamfis), nbamfis,
                          paste("AMFIS", str_to_lower(nbamfis), sep = " : ")),
         intervamfis = ifelse(is.na(intervamfis), intervamfis,
                              paste("Intervenant AMFIS", str_to_lower(intervamfis), sep = " : ")),
         freqtract_election = ifelse(is.na(freqtract_election), freqtract_election,
                                     paste("Tractage élections", str_to_lower(freqtract_election), sep = " : ")),
         freqtract_paselection = ifelse(is.na(freqtract_paselection), freqtract_paselection,
                                        paste("Tractage hors élections", str_to_lower(freqtract_paselection), sep = " : ")),
         freqmanif = ifelse(is.na(freqmanif), freqmanif,
                            paste("Manifestations", str_to_lower(freqmanif), sep = " : ")),
         maniflfi = ifelse(is.na(maniflfi), maniflfi,
                           paste("Manifestations avec FI", str_to_lower(maniflfi), sep = " : ")),
         campagnelfi = ifelse(is.na(campagnelfi), campagnelfi,
                              paste("Campagne FI", str_to_lower(campagnelfi), sep = " : ")),
         campagne_reg = ifelse(is.na(campagne_reg), campagne_reg,
                               paste("Campagne 2021", str_to_lower(campagne_reg), sep = " : ")),
         campagne_pres = ifelse(is.na(campagne_pres), campagne_pres,
                                paste("Campagne 2022", str_to_lower(campagne_pres), sep = " : ")),
         covid_resoc = ifelse(is.na(covid_resoc), covid_resoc,
                              paste("Réseaux sociaux covid", str_to_lower(covid_resoc), sep = " : ")),
         covid_num = ifelse(is.na(covid_num), covid_num,
                            paste("Réunions numériques covid", str_to_lower(covid_num), sep = " : ")),
         covid_aide = ifelse(is.na(covid_aide), covid_aide,
                             paste("Aides personnes covid", str_to_lower(covid_aide), sep = " : ")),
         covid_coll = ifelse(is.na(covid_coll), covid_coll,
                             paste("Collectes covid", str_to_lower(covid_coll), sep = " : ")),
         covid_manif = ifelse(is.na(covid_manif), covid_manif,
                              paste("Manifestations covid", str_to_lower(covid_manif), sep = " : ")),
         elulfi = ifelse(elulfi == "Oui", "Élu FI", "Pas élu FI"),
         connaitelu = ifelse(connaitelu == "Oui", "Connait élu", "Connait pas élu"),
         convaincre = ifelse(convaincre == "Oui", "Influe élu", "Influe pas élu"),
         comite = ifelse(comite == "Oui", "Comité", "Pas comité"),
         espace_pgrm = ifelse(is.na(espace_pgrm), espace_pgrm,
                              paste("Espace programme", str_to_lower(espace_pgrm), sep = " : ")),
         espace_parlm = ifelse(is.na(espace_parlm), espace_parlm,
                               paste("Espace parlementaire", str_to_lower(espace_parlm), sep = " : ")),
         espace_ue = ifelse(is.na(espace_ue), espace_ue,
                            paste("Espace parlement européen", str_to_lower(espace_ue), sep = " : ")),
         espace_pol = ifelse(is.na(espace_pol), espace_pol,
                             paste("Espace politique", str_to_lower(espace_pol), sep = " : ")),
         espace_op = ifelse(is.na(espace_op), espace_op,
                            paste("Espace opérationnel", str_to_lower(espace_op), sep = " : ")),
         espace_elus = ifelse(is.na(espace_elus), espace_elus,
                              paste("Espace élus", str_to_lower(espace_elus), sep = " : ")),
         espace_autoorga = ifelse(is.na(espace_autoorga), espace_autoorga,
                                  paste("Espace auto-organisation", str_to_lower(espace_autoorga), sep = " : ")),
         espace_elec = ifelse(is.na(espace_elec), espace_elec,
                              paste("Espace élections", str_to_lower(espace_elec), sep = " : ")),
         pas_espace = ifelse(pas_espace == "Oui", "Aucun espace", "Un espace"),
         actmili_actprincipale = ifelse(is.na(actmili_actprincipale), actmili_actprincipale,
                                        paste("Militantisme principal", 
                                              str_to_lower(actmili_actprincipale), sep = " : ")),
         actmili_plusrespons = ifelse(is.na(actmili_plusrespons), actmili_plusrespons,
                                      paste("Plus responsabilités", 
                                            str_to_lower(actmili_plusrespons), sep = " : ")),
         actmili_plusutile = ifelse(is.na(actmili_plusutile), actmili_plusutile,
                                    paste("Plus utile", 
                                          str_to_lower(actmili_plusutile), sep = " : ")),
         actmili_plussatis = ifelse(is.na(actmili_plussatis), actmili_plussatis,
                                    paste("Plus satisfaisant", 
                                          str_to_lower(actmili_plussatis), sep = " : ")),
         difficultpro = ifelse(difficultpro == "Oui", "Difficultés pro","Pas difficultés pro"),
         difficultpro_placard = ifelse(is.na(difficultpro_placard), difficultpro_placard,
                                       paste("Ralentissements carrière", 
                                             str_to_lower(difficultpro_placard), sep = " : ")),
         difficultpro_isole = ifelse(is.na(difficultpro_isole), difficultpro_isole,
                                     paste("Isolement", 
                                           str_to_lower(difficultpro_isole), sep = " : ")),
         difficultpro_harcele = ifelse(is.na(difficultpro_harcele), difficultpro_harcele,
                                       paste("Harcèlement", 
                                             str_to_lower(difficultpro_harcele), sep = " : ")),
         difficultpro_licenciement = ifelse(is.na(difficultpro_licenciement), difficultpro_licenciement,
                                            paste("Licenciement", 
                                                  str_to_lower(difficultpro_licenciement), sep = " : ")),
         difficultpro_pastrouve = ifelse(is.na(difficultpro_pastrouve), difficultpro_pastrouve,
                                         paste("Chômage", 
                                               str_to_lower(difficultpro_pastrouve), sep = " : ")),
         difficultfam = ifelse(is.na(difficultfam), difficultfam,
                               paste("Difficultés familiales", str_to_lower(difficultfam), sep = " : ")),
         don = ifelse(don == "Oui", "Don", "Pas de don"),
         opinionresp = ifelse(opinionresp == "Oui", "Toute opinion respectable", 
                              "Des opinions pas respectables"),
         ldc = ifelse(is.na(ldc), ldc,paste("Lutte des classes", str_to_lower(ldc), sep = " : ")),
         impquestion_lfi_serv = ifelse(is.na(impquestion_lfi_serv), impquestion_lfi_serv,
                                       paste("Service citoyen", 
                                             str_to_lower(impquestion_lfi_serv), sep = " : ")),
         impquestion_lfi_bac = ifelse(is.na(impquestion_lfi_bac), impquestion_lfi_bac,
                                      paste("Démantèlement BAC", 
                                            str_to_lower(impquestion_lfi_bac), sep = " : ")),
         impquestion_lfi_cannab = ifelse(is.na(impquestion_lfi_cannab), impquestion_lfi_cannab,
                                         paste("Cannabis", 
                                               str_to_lower(impquestion_lfi_cannab), sep = " : ")),
         impquestion_lfi_vert = ifelse(is.na(impquestion_lfi_vert), impquestion_lfi_vert,
                                       paste("Règle verte", 
                                             str_to_lower(impquestion_lfi_vert), sep = " : ")),
         impquestion_lfi_spap = ifelse(is.na(impquestion_lfi_spap), impquestion_lfi_spap,
                                       paste("Régularisation travailleurs SP", 
                                             str_to_lower(impquestion_lfi_spap), sep = " : ")),
         impquestion_lfi_nuc = ifelse(is.na(impquestion_lfi_nuc), impquestion_lfi_nuc,
                                      paste("Sortie nucléaire", 
                                            str_to_lower(impquestion_lfi_nuc), sep = " : ")),
         impquestion_npa_islam = ifelse(is.na(impquestion_npa_islam), impquestion_npa_islam,
                                        paste("Abrogation lois islamophobes", 
                                              str_to_lower(impquestion_npa_islam), sep = " : ")),
         impquestion_npa_licenci = ifelse(is.na(impquestion_npa_licenci), impquestion_npa_licenci,
                                          paste("Interdiction licenciements", 
                                                str_to_lower(impquestion_npa_licenci), sep = " : ")),
         impquestion_npa_spap = ifelse(is.na(impquestion_npa_spap), impquestion_npa_spap,
                                       paste("Régularisation tous SP", 
                                             str_to_lower(impquestion_npa_spap), sep = " : ")),
         impquestion_npa_log = ifelse(is.na(impquestion_npa_log), impquestion_npa_log,
                                      paste("Réquisition logements", 
                                            str_to_lower(impquestion_npa_log), sep = " : ")),
         parti_trotsmao = case_when(parti_trotsmao == "Oui" ~ "Extrême gauche",
                                    parti_trotsmao == "Non" ~ "Pas extrême gauche",
                                    parti_trotsmao == "Non concerné" ~ "Jamais encarté1"), 
         parti_gaucherad = case_when(parti_gaucherad == "Oui" ~ "Gauche de la gauche",
                                     parti_gaucherad == "Non" ~ "Pas gauche de la gauche",
                                     parti_gaucherad == "Non concerné" ~ "Jamais encarté2"), 
         parti_pcf = case_when(parti_pcf == "Oui" ~ "PCF",
                               parti_pcf == "Non" ~ "Pas PCF",
                               parti_pcf == "Non concerné" ~ "Jamais encarté3"), 
         parti_gauche = case_when(parti_gauche == "Oui" ~ "Gauche non communiste",
                                  parti_gauche == "Non" ~ "Gauche non communiste",
                                  parti_gauche == "Non concerné" ~ "Jamais encarté4"), 
         parti_autre = case_when(parti_autre == "Oui" ~ "Autres partis",
                                 parti_autre == "Non" ~ "Pas autres partis",
                                 parti_autre == "Non concerné" ~ "Jamais encarté5"), 
         synd_cgt = case_when(synd_cgt == "Oui" ~ "CGT",
                              synd_cgt == "Non" ~ "Pas CGT",
                              synd_cgt == "Non concerné" ~ "Jamais syndiqué1"), 
         synd_cfdt = case_when(synd_cfdt == "Oui" ~ "CFDT",
                               synd_cfdt == "Non" ~ "Pas CFDT",
                               synd_cfdt == "Non concerné" ~ "Jamais syndiqué2"), 
         synd_fo = case_when(synd_fo == "Oui" ~ "FO",
                             synd_fo == "Non" ~ "Pas FO",
                             synd_fo == "Non concerné" ~ "Jamais syndiqué3"), 
         synd_fsu = case_when(synd_fsu == "Oui" ~ "FSU",
                              synd_fsu == "Non" ~ "Pas FSU",
                              synd_fsu == "Non concerné" ~ "Jamais syndiqué4"), 
         synd_jeunes = case_when(synd_jeunes == "Oui" ~ "Syndicat lycéen/étudiant",
                                 synd_jeunes == "Non" ~ "Pas Syndicat lycéen/étudiant",
                                 synd_jeunes == "Non concerné" ~ "Jamais syndiqué5"), 
         synd_autre = case_when(synd_autre == "Oui" ~ "Autre syndicat",
                                synd_autre == "Non" ~ "Pas autre syndicat",
                                synd_autre == "Non concerné" ~ "Jamais syndiqué6"),
         freqparticipereu_rec = ifelse(freqparticipereu_rec == "Pas dans un groupe", "Pas dans un groupe1", 
                                       freqparticipereu_rec), 
         retraite = case_when(retraite == "Oui" ~ "Retraité",
                              retraite == "Non" ~ "Pas retraité"),
         engage_par = case_when(engage_par == "Oui" ~ "Parents engagés",
                                engage_par == "Non" ~ "Parents non-engagés"))

