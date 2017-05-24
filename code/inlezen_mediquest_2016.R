# inlezen Wachttijden ggz 2016 MediQuest

lees_data <- function(){

  # functie om data in te lezen en bedrijfsvertrouwelijke Vektis data te droppen
  source("code/inlezen_mediquest_2016_ga_naar_opendata.R")

  # lees functies in die al het werk doen
  source("code/inlezen_mediquest_2016_functies_bulk.R")

  # functie voor toevoegen geo info
  source("code/inlezen_mediquest_2016_toevoegen_geoinfo.R")

  # functie voor handmatig typeren praktijken
  source("code/typeer_praktijken_zonder_type.ggz.R")

  # start data prep

  # documenteer pad van ruwe MediQuest en NZa data naar Open-data bestand
  #lees_in_en_filter_opendata()

  mq16 <- lees_in_open_data_set()
  dim(mq16)

  mq16 <- clean_velden(mq16)
  # drop 30 records buiten scope (o.a. jeugd)
  dim(mq16)

  mq16 <- maak_vlag_heeft_type_ggz(mq16)

  mq16 <- typeer_praktijken_zonder_type(mq16)

  mq16 <- clean_datum(mq16)

  mq16 <- verwerk_handmatige_typering(mq16)

  mq16 <- clean_velden_twee(mq16)
  # worden vier records gedropt
  dim(mq16)

  mq16 <- clean_velden_geo(mq16)

  #############
  # koppel elk record aan woonplaats - gemeente - G4/G32
  mq16 <- voeg_geo_info_toe(mq16)

  mq16 <- voeg_gemeente_soort_toe(mq16)

  dim(mq16)
  # 2544   60

  # schrijf weg als eindresultaat
  varnames <- names(mq16)
  blacklist <- c("TYPERING_HANDMATIG", "x", "AFWIJKEND_ZV_NR", "wp_naam_lo", 
                 "SGGZ_MEASURE_TYPE", "BGGZ_MEASURE_TYPE")
  
  dim(mq16[order(ROW_ID), !(varnames %in% blacklist), with = F])
  # 2544 x 54
  
  # schrijf open dataset weg, hiervoor is een metadata bestand opgesteld.
  # het metadata bestand is als csv en xlsx beschikbaar
  
  write.csv2(mq16[order(ROW_ID), !(varnames %in% blacklist), with = F], 
             "output/verrijkt_wachttijden_bestand_mediquest_2016_compleet.csv",
             row.names = FALSE)
  mq16
}
