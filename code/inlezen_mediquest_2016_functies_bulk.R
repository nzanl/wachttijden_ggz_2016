# inlezen en voorbereiden MediQuest gegevensbestand


voeg_samen_mq <- function(mq16inst, mq16vrij){

  # kolomnamen gelijk maken
  mq16vrij$KvK..uit.bestand.2015. <- NA
  names(mq16vrij)[names(mq16vrij) == "Opmerkingen.overig"] <- "Opmerkingen.MQ"
  names(mq16vrij)[names(mq16vrij) == "Naam.praktijk"] <- "Naam.zorgaanbieder"
  names(mq16vrij)[names(mq16vrij) == "Plaats.praktijk"] <- "Plaats.zorgaanbieder"

  mq16inst$Wachttijden.toelichting <- NA
  mq16inst$Website.vindbaar <- NA

  # check colnames gelijk
  colnames(mq16vrij)[!(colnames(mq16vrij) %in% colnames(mq16inst))]
  colnames(mq16inst)[!(colnames(mq16inst) %in% colnames(mq16vrij))]

  dim(mq16inst)
  dim(mq16vrij)
  # 34 cols

  # plak aan elkaar
  mq16 <- rbind(mq16inst, mq16vrij)

  # pas naam kolom aan
  names(mq16)[names(mq16) == "Toelichting.afwijkende.wachttijdenregeling.zorgverzekeraar.s."] <- "AFWIJKEND_ZV"
  return(mq16)
}

voeg_samen_mq_psyq <- function(mq16, psyq){
  colnames(psyq)[!(colnames(psyq) %in% colnames(mq16))]
  colnames(mq16)[!(colnames(mq16) %in% colnames(psyq))]

  mq16 <- rbind(mq16, psyq)
  return(mq16)
}

clean_velden <- function(mq16){
  ####################################################
  # Type.GGZ
  ####################################################

  #zet lege levels op NA, later meer op dit veld
  levels(mq16$Type.GGZ)[levels(mq16$Type.GGZ) == ""] <- NA

  ####################################################
  # Aanmeldtijd
  ####################################################
  levels(mq16$Aanmeldtijd)

  # <Stop, stop> : patientenstops
  # maak veld "PAT_STOP"
  mq16$PAT_STOP <- 0
  mq16[Aanmeldtijd %in% c("stop", "Stop", "Geen intakes", "geen intakes"), 
       PAT_STOP := 1]
  table(mq16$PAT_STOP)
  #0    1
  #2346   63

  levels(mq16$Aanmeldtijd)[levels(mq16$Aanmeldtijd) == "Stop"] <- NA
  levels(mq16$Aanmeldtijd)[levels(mq16$Aanmeldtijd) == "stop"] <- NA
  levels(mq16$Aanmeldtijd)[levels(mq16$Aanmeldtijd) == "Geen intakes"] <- NA
  levels(mq16$Aanmeldtijd)[levels(mq16$Aanmeldtijd) == "geen intakes"] <- NA

  # <n.v.t>
  # drop hele rij want ze doen geen verslavingszorg
  mq16<-mq16[ROW_ID != 2303,]

  levels(mq16$Aanmeldtijd)
  levels(mq16$Aanmeldtijd)[levels(mq16$Aanmeldtijd) == "n.v.t."] <- NA

  # <"verschillend zie toelichting">
  # aanbieder differentieert naar VZ, wachttijden worden apart genoemd in de toelichting
  
  # deze wachttijden zijn niet meegenomen, aanmeldtijd is op NA gezet
  levels(mq16$Aanmeldtijd)[levels(mq16$Aanmeldtijd) == "verschillend zie toelichting"] <- NA

  # "" en "-" allebei op NA zetten
  levels(mq16$Aanmeldtijd)[levels(mq16$Aanmeldtijd) %in% c("", "-")] <- NA

  # In overleg: als missing behandelen
  levels(mq16$Aanmeldtijd)[levels(mq16$Aanmeldtijd) %in% c("in overleg")] <- NA

  # aanmeldtijd naar numeric:
  mq16$Aanmeldtijd<-as.numeric(gsub(",", ".", gsub("\\.", "", as.character(mq16$Aanmeldtijd))))
  mq16[, HEEFT_AANMELD :=0]
  mq16[!is.na(Aanmeldtijd), HEEFT_AANMELD := 1]

  ####################################################
  # Behandelwachttijd
  ####################################################

  levels(mq16$Behandeltijd)
  # "" en "-" allebei op NA zetten, "geen behandeling"
  levels(mq16$Behandeltijd)[levels(mq16$Behandeltijd) %in%
                              c("", "-", " - ", "geen behandeling")] <- NA
  mq16[Behandeltijd == "n.v.t."]
  # is al gedropt doordat row is gedropt (Mentaal sterk verslavingszorg)
  levels(mq16$Behandeltijd)[levels(mq16$Behandeltijd) %in% c("n.v.t.")] <- NA

  # zet PAT_STOP op 1 als behandeltijd = stop
  mq16[Behandeltijd %in% c("stop", "Stop"), PAT_STOP := 1]
  table(mq16$PAT_STOP)
  #0    1
  #2344   64
  levels(mq16$Behandeltijd)[levels(mq16$Behandeltijd) == "Stop"] <- NA
  levels(mq16$Behandeltijd)[levels(mq16$Behandeltijd) == "stop"] <- NA
  # converteer naar numeric
  mq16$Behandeltijd<-as.numeric(gsub(",", ".", gsub("\\.", "", as.character(mq16$Behandeltijd))))
  mq16$HEEFT_BEHANDEL<-0
  mq16[!is.na(Behandeltijd), HEEFT_BEHANDEL := 1]

  ####################################################
  # Circuit
  ####################################################

  # circuit naar type ggz per type inst
  table(mq16$Circuit, mq16$Type.GGZ, useNA = "ifany")
  table(mq16$Circuit, mq16$INST, useNA = "ifany")

  # nav mailcontact MediQuest 11/10/16 moet zijn Adolescenten
  mq16[Typering=="klinisch, JEUGD", Circuit := "Adolescenten"]

  # drop alle jeugd records (vallen buiten scope NZa analyse)
  mq16<-mq16[Circuit != "Jeugd"]
  table(mq16$Circuit)

  # voeg vlag HEEFT_CIRCUIT 1/0 toe
  mq16$HEEFT_CIRCUIT <- 0
  mq16[Circuit %in% c("Adolescenten", "Jeugd", "Ouderen", 
                      "Verslavingszorg", "Volwassenen"), HEEFT_CIRCUIT := 1]

  table(mq16$Circuit, mq16$HEEFT_AANMELD, useNA = "ifany")
  # 710 zonder circuit
  # zet Aanmeldtijd zonder circuit op volwassenen --> AANNAME VOLWASSENEN ALS DEFAULT
  mq16[HEEFT_AANMELD == 1 & HEEFT_CIRCUIT == 0, Circuit := "Volwassenen"]

  # NIET GEBRUIKTE VELDEN: x, KvK..uit.bestand.2015, Hoort.bij
  mq16
}

maak_vlag_heeft_type_ggz<-function(mq16){

  table(mq16$Type.GGZ, useNA="ifany")

  # voeg vlag heeft type.ggz toe
  mq16$HEEFT_TYPEGGZ <- 0
  mq16[Type.GGZ %in% c("BGGZ", "BGGZ/SGGZ", "SGGZ"), HEEFT_TYPEGGZ := 1]

  return(mq16)
}

clean_datum <- function(mq16){
  # datum actualisatie
  levels(mq16$Datum.actualisatie)

  levels(mq16$Datum.actualisatie)[levels(mq16$Datum.actualisatie) %in% c("", "-")] <- NA

  selectie <- levels(mq16$Datum.actualisatie) == "sep-16"
  levels(mq16$Datum.actualisatie) [selectie] <- "15-9-2016"
  selectie <- levels(mq16$Datum.actualisatie) == "7-9-2017"
  levels(mq16$Datum.actualisatie) [selectie] <- "7-9-2016"
  # van factor naar datum veld
  mq16$Datum.actualisatie <- as.Date(mq16$Datum.actualisatie, "%d-%m-%Y")

  #oude wachttijden eruit filteren
  # selecteer op >= 1 juli 2016
  dim(mq16)
  dim(subset(mq16, Datum.actualisatie < as.Date("2016-07-01")))
  # "droppen" 59 records --> ok

  # maak filter ACTUEEL (INCLUSIEF AANNAME geen actualisatie datum = ACTUEEL)
  mq16$ACTUEEL <- 1
  mq16[Datum.actualisatie < as.Date("2016-07-01") , ACTUEEL := 0]
  return(mq16)
}

verwerk_handmatige_typering <- function(mq16){
  #################################################################
  # Typering

  # apart labellen:

  # crisis
  # klinisch
  # verslaving
  # forensisch
  # niet-zvw (wlz/arbeid en psyche/jeugd/wmo/pgb/dyslexie)
  # ouderen

  # kolom toegevoegd in meetbestand "Typering", handmatig aanvullende typering uitgevoerd
  # dit is op basis van informatie in vestigingslocatie en nadere typering (en heel soms aanbieder naam)

  # voeg samen alles met lvb/vg/lvg
  levels(mq16$Typering)[levels(mq16$Typering) %in% c("lg/lvg", "lvb", "vg/lvg")] <- "lg/lvg/lvb"
  levels(mq16$Typering)[levels(mq16$Typering) %in% c("diagnostiek", "Diagnostiek")] <- "diagnostiek"

  # Buiten reguliere analyse houden: crisis, diagnostiek, forensisch, niet-zvw, jeugd, online
  # Pas Type.ggz aan op basis van aanvullende typering
  mq16[Typering == "online", Type.GGZ := "Online"]
  mq16[Typering == "crisis", Type.GGZ := "Crisis"]
  mq16[Typering == "lg/lvg/lvb", Type.GGZ := "LVB/VG"]
  mq16[Typering == "doven", Type.GGZ := "Doven"]
  mq16[Typering == "forensisch", Type.GGZ := "Forensisch"]
  mq16[Typering == "klinisch", Type.GGZ := "Klinisch"]

  table(mq16$Typering)

  mq16[Typering == "Jeugd"]
  # neuroscan: biofeedback voor ADHD
  # doen ook 18+ blijkt uit declaraties
  levels(mq16$Typering)[levels(mq16$Typering) %in% c("Jeugd")] <- ""

  # circuit aanpassen voor ouderen en verslaving
  mq16[Typering == "ouderen", Circuit := "Ouderen"]
  mq16[Typering == "verslaving", Circuit := "Verslavingszorg"]
  mq16[Typering == "diagnostiek", Type.GGZ := "Diagnostiek"]
  mq16[Typering == "niet-zvw", Type.GGZ := "Niet-zvw"]

  # niet-zvw (Relatietherapie, Arbeidstraining en Maatschappelijke opvang (verslaving))
  levels(mq16$Typering)[levels(mq16$Typering) %in% c("verslaving")] <- ""
  levels(mq16$Typering)[levels(mq16$Typering) %in% c("ouderen")] <- ""
  levels(mq16$Typering)[levels(mq16$Typering) %in% c("zvw")] <- ""

  # diversen is de wachttijd bij Mentrum in brede zin
  # gewoon meenemen bij SGGZ dus
  levels(mq16$Typering)[levels(mq16$Typering) %in% c("diversen")] <- ""

  # ggz centraal: toch gewoon gespecialiseerde ggz (kortdurend)
  levels(mq16$Typering)[levels(mq16$Typering) %in% c("mogelijk GBGGZ")] <- ""

  # GGZ E: bleek om 18-23 te gaan, en is onder Type.GGZ = klinisch gezet
  levels(mq16$Typering)[levels(mq16$Typering) %in% c("klinisch, JEUGD")] <- ""
  return(mq16)
}

clean_velden_twee <- function(mq16){
  # vestigingslocatie
  table(mq16$Vestigingslocatie == "")
  levels(mq16$Vestigingslocatie)[levels(mq16$Vestigingslocatie) %in% c("")] <- "GEEN_LOCATIE_ONDERSCHEID"
  #FALSE  TRUE
  #1470   909

  # MAAK vestigingslocatie ID
  mq16[, LOCATIE_ID := as.integer(as.factor(
    paste(Naam.zorgaanbieder, Vestigingslocatie, sep="_")))]

  ###################################################################
  # stops / afwijkende wachttijden voor specifieke verzekeraars

  # veld Toelichting afwijkende wachttijdenregeling zorgverzekeraar(s)
  mq16[is.na(AFWIJKEND_ZV), AFWIJKEND_ZV := ""]

  # handmatig turven of er onderscheid wordt gemaakt, en zo ja door welke verzekeraar(s)
  write.csv2(levels(mq16$AFWIJKEND_ZV), 
             file = "work/toelichting_afwijkende_regeling_verzekeraar.csv")

  afwijkend_zv_ed <- read.csv2("input/toelichting_afwijkende_regeling_verzekeraar_ed.csv")

  # koppelen op level NR
  mq16[, AFWIJKEND_ZV_NR := as.integer(AFWIJKEND_ZV)]

  # koppel MAAK_ONDERSCHEID en VZ velden aan mq16 op basis van level nr
  afwijkend_zv_ed <- data.table(afwijkend_zv_ed[, c("CZ" , "VGZ", "Achmea.ZK",  "Menzis" , "DSW", "MAAKT_ONDERSCHEID", "NR")])
  afwijkend_zv_ed[is.na(afwijkend_zv_ed)] <- 0

  setnames(afwijkend_zv_ed, "NR", "AFWIJKEND_ZV_NR")
  setkey(afwijkend_zv_ed, AFWIJKEND_ZV_NR)
  setkey(mq16, AFWIJKEND_ZV_NR)

  mq16 <- mq16[afwijkend_zv_ed]

  ###############################
  # correctie op woonzorg van lelie groep voor PG (dementie)
  # dit zijn wachttijden voor plek in huis --> lopen niet mee in ggz analyse
  # slingedael wachttijden behouden, behandelfunctie voor bewoners

  droprows <- mq16[AGBCODE_INDIENER == 22227324 & Aanmeldtijd > 10]$ROW_ID
  mq16 <- mq16[!(ROW_ID %in% droprows),]
  return(mq16)
}

clean_velden_geo<-function(mq16){
  # Plaats.zorgaanbieder
  levels(mq16$Plaats.zorgaanbieder)[levels(mq16$Plaats.zorgaanbieder) == ""] <- NA
  levels(mq16$Plaats.zorgaanbieder)[levels(mq16$Plaats.zorgaanbieder) == "-"] <- NA
  return(mq16)
}

voeg_gemeente_soort_toe <- function(mq16){
  # leid af uit vlaggen
  mq16$GEM_SOORT <- "GEEN"
  mq16[G4 == 1, GEM_SOORT := "G4"]
  mq16[G32 == 1, GEM_SOORT := "G32"]
  mq16[G_OVG == 1, GEM_SOORT := "overig"]
  return(mq16)
}
