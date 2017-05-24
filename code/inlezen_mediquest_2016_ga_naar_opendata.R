# lees ruwe data in

lees_in_en_filter_opendata <- function(){
  # deze functie laat zien hoe het ruwe open data bestand tot stand is gekomen,
  # maar kan niet gerund woren omdat hiervoor bedrijfsvertrouwelijke Vektis data nodig is
  
  # # Voeg drie csv's samen en verwijder bedrijfsvertrouwelijke Vektis velden
  mq16inst <- read.csv2("Input/MediQuest/161003_Wachttijden GGZ 2016_DEF_INST.csv")
  mq16inst$INST <- 1
  mq16vrij <- read.csv2("Input/MediQuest/161003_Wachttijden GGZ 2016_DEF_VRIJ.csv")
  mq16vrij$INST <- 0

  # Inlezen extra wachttijden PsyQ (Handmatig door NZa zelf verzameld)
  # missen een deel van de PsyQ locaties --> PsyQ locaties comleet gemaakt
  # peildatum 18 oktober 2016
  psyq <- read.csv2("Input/MediQuest/20161018 Wachttijden PsyQ.csv")
  names(psyq)[names(psyq) == "Toelichting.afwijkende.wachttijdenregeling.zorgverzekeraar.s."] <- "AFWIJKEND_ZV"
  psyq$INST <- 1
  psyq$Wachttijden.toelichting <- NA
  psyq$Website.vindbaar <- NA
  psyq$Typering <- ""

  mq16 <- voeg_samen_mq(mq16inst, mq16vrij)
  rm(mq16vrij, mq16inst)

  mq16 <- voeg_samen_mq_psyq(mq16, psyq)

  # converteer naar data.table
  mq16 <- data.table(mq16)

  # maak uniek ROW_ID aan
  mq16[, ROW_ID := 1:nrow(mq16)]

  # Vektis declaraties 2014 velden
  vek_cols <- c("PAT2014", "PAT2014_SUB18", "PAT2014_18PLUS", "PAT2014_BGGZ_18PLUS", "PAT2014_SGGZ_18PLUS")

  # maak veld INST_GROOT door te kijken naar PAT2014
  mq16[, grote_instelling := 0]
  mq16[PAT2014 > 4900, grote_instelling := 1]

  # drop Vektis cols ()
  # http://stackoverflow.com/questions/9202413/how-do-you-delete-a-column-by-name-in-data-table
  mq16[, (vek_cols) := NULL]
  saveRDS(mq16, file = "input/brondata_wachttijden_ggz_2016_ruw.rds")
  write.csv2(mq16, file = "input/brondata_wachttijden_ggz_2016_ruw.csv")
}

lees_in_open_data_set <- function() {
  mq16 <- readRDS("input/brondata_wachttijden_ggz_2016_ruw.rds")
  mq16
}
