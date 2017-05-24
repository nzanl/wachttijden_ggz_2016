
voeg_geo_info_toe <- function(mq16){

  # voeg geografische info per wachttijd toe

  # gebruik informatie uit vestigingslocatie en Plaats.zorgaanbieder om aanmeld wachttijd te koppelen aan woonplaats_code en verder.

  # stap 1 haal uit "vestigingslocatie" een plaatsnaam die we aan CBS gemeente kunnen koppelen
  source("code/inlezen_cbs_voor_geo.R")

  # creer een lijst met woonplaatsen cbs_woon die we op gaan zoeken in de data

  # gebruiken een lijst met bevolkingskernen CBS
  cbs_woon <- lees_in_cbs_bevolkingskernen_2011()

  # alles naar lowercase
  cbs_woon[, WOONPLAATS_LO := as.factor(tolower(WOONPLAATS))]

  # gebruik blacklist om woonplaatsen uit te sluiten die te veel matches op leveren

  # blacklist iteratief opgebouwd, zie onder
  blacklist <- c("ee", "een", "ees", "gees", "veen", "hee", "nederland", "ens", "lent",
               "est","andel", "echt", "handel", "rien", "son", "elburg")
  # drop woonplaatsen die op de blacklist staan
  cbs_woon <- subset(cbs_woon, !(WOONPLAATS_LO %in% blacklist))
  dim(cbs_woon)
  #[1] 2155    6
  ###############################################
  # MAAK APARTE "mq16_geo" subset van de data voor het overzicht
  # straks weer koppelen aan mq16 via ROW_ID

  mq16_geo <- subset(mq16, select = c(ROW_ID, Plaats.zorgaanbieder, 
                                      Vestigingslocatie, HEEFT_AANMELD))
  # alles lowercase maken
  mq16_geo$Vestigingslocatie <- factor(tolower(mq16_geo$Vestigingslocatie))
  mq16_geo$Plaats.zorgaanbieder <- factor(tolower(mq16_geo$Plaats.zorgaanbieder))

  # vervang nederland door _NL
  mq16_geo$Vestigingslocatie <- factor(gsub("nederland", "_NL", as.character(mq16_geo$Vestigingslocatie)))

  # NOTE TO SELF: . = any character * any length
  # () capture block; \\. = "." daarna spatie, daarna weer .*
  # daarna capture block selecteren

  # drop alles voor loc. (inclusief loc. )
  mq16_geo$Vestigingslocatie <- factor(gsub(".*(loc\\. .*)", "\\1", as.character(mq16_geo$Vestigingslocatie)))

  mq16_geo$Plaats.zorgaanbieder <- factor(tolower(mq16_geo$Plaats.zorgaanbieder))
  mq16_geo$PLAATS_LOCATIE <- 0
  mq16_geo$AANTAL_CHARS_NAAM <- 0
  mq16_geo$WOON_INDEX <- -1
  mq16_geo$WOONPLAATS <- "leeg"

  ##################################################################################
  # HOOFDALGORITME
  # ga de 2155 CBS woonplaatsen uit 2011 opzoeken in het veld "vestigingslocatie"

  # loop over deze wachttijd-records:
    # noteer de lengte van de woonplaats naam
    # als nieuwe lengte langer is, overschrijf met de nieuwe woonplaats naam
    # dus: bij dubbelingen kies de langste plaatsnaam


  for(i in 1:nrow(cbs_woon)){
    # tel aantal matches in data voor deze woonplaats
    aantal_matches <- length(grep(cbs_woon$WOONPLAATS_LO[i], mq16_geo$Vestigingslocatie))
    # hoog plaats_locatie op bij de gevonden records
    mq16_geo[grep(cbs_woon$WOONPLAATS_LO[i], 
                  mq16_geo$Vestigingslocatie)]$PLAATS_LOCATIE <- mq16_geo[grep(cbs_woon$WOONPLAATS_LO[i], 
                                                                               mq16_geo$Vestigingslocatie)]$PLAATS_LOCATIE + 1
    # tel aantal chars in woonplaats
    aantal_chars <- nchar(as.character(cbs_woon$WOONPLAATS_LO[i]))
    # als er matches zijn:
    if(aantal_matches > 0){
        for (j in 1:aantal_matches){
          if(aantal_chars > mq16_geo[grep(cbs_woon$WOONPLAATS_LO[i], mq16_geo$Vestigingslocatie)[j],]$AANTAL_CHARS_NAAM){
            mq16_geo[grep(cbs_woon$WOONPLAATS_LO[i], mq16_geo$Vestigingslocatie)[j],]$AANTAL_CHARS_NAAM <- aantal_chars
            mq16_geo[grep(cbs_woon$WOONPLAATS_LO[i], mq16_geo$Vestigingslocatie)[j],]$WOON_INDEX <- i
            mq16_geo[grep(cbs_woon$WOONPLAATS_LO[i], mq16_geo$Vestigingslocatie)[j],]$WOONPLAATS <- as.character(cbs_woon[i]$WOONPLAATS_LO)
          }
        }
      }
  }


  # zet alles zonder WOONPLAATS op plaats zorgaanbieder
  mq16_geo[WOON_INDEX == -1 & WOONPLAATS == "leeg", WOONPLAATS := Plaats.zorgaanbieder]

  # loop alles nu na en corrigeer handmatig
  write.csv2(mq16_geo, "work/mq16_geo.csv")

  # correcties inlezen
  mq16_geo_ed <- read.csv2("input/mq16_geo_ed.csv")
  mq16_geo_ed$X <- NULL
  mq16_geo_ed <- data.table(mq16_geo_ed)
  ###############################################################
  # fix plaatsnamen niet in CBS Lijst

  # plak URL eraan
  mq16_geo_ed <- merge(mq16_geo_ed,
                       subset(mq16, select = c(ROW_ID, URL.naar.wachttijden)), 
                       by = "ROW_ID", all.x = T)

  # zoek woonplaatsen uit mq16_geo_ed op in de CBS woonplaats - gemeente koppeling (cbs_wp16)
  cbs_wp16 <- lees_in_cbs_woonplaatsen_2016()

  tefixen <- unique(mq16_geo_ed[!(WOONPLAATS %in% cbs_wp16$wp_naam_lo) & WOONPLAATS != "NA" & HEEFT_AANMELD,]$WOONPLAATS)
  # gebruik dit als zoeker om te fixen
  # cbs_wp16[grep("eijerl", cbs_wp16$wp_naam_lo),]
  gefixed <- c("'s-hertogenbosch", "hoofddorp", "rijswijk", "'s-gravenhage", "capelle aan den ijssel",
             "krimpen aan den ijssel", "oud-beijerland", "oudenhoorn", "heerhugowaard", "bergen (nh)", "weert",
             "sittard", "oost west en middelbeers", "leidschendam", "west-terschelling", "capelle aan den ijssel",
             "hoogvliet rotterdam", "hengelo", "amsterdam" , "'s-gravenpolder" , "hoofddorp")


  mq16_geo_ed$WOONPLAATS <- as.character(mq16_geo_ed$WOONPLAATS)

  # fix niet koppelende woonplaatsen
  for(i in 1:length(tefixen)){
    # vind records met gebroken naam
    selectie <- (mq16_geo_ed$WOONPLAATS == tefixen[i])
    # vervang in selectie de namen met de fix
    mq16_geo_ed[selectie, ]$WOONPLAATS = gefixed[i]
  }

  #################################################################
  # koppel de cbs lijst aan het mq16_geo bestand

  # sluit van dubbele plaatsnamen de incorrecte plaats uit (obv google icm zorgaanbieder naam)
  zwartelijst <- c("WP2691", "WP2203", "WP2872", "WP1086", "WP2615", "WP2265",
                 "WP1869", "WP1737", "WP2107", "WP1709", "WP3198")
  mq16_geo_ed <- merge(mq16_geo_ed, 
                       subset(cbs_wp16, !(wp_code %in% zwartelijst)), 
                       by.x = "WOONPLAATS", by.y = "wp_naam_lo", all.x = T)
  # vind de duplicates
  # dit zijn identieke woonplaatsnamen (3x Beek)

  tefixen <- unique(mq16_geo_ed[duplicated(mq16_geo_ed$ROW_ID),]$WOONPLAATS)
  # deze moet nu leeg zijn, alle duplicates zijn via zwartelijst gecorrigeerd

  #################################################################
  # heeft elke aanmeld wachttijd nu een WP code?
  summary(mq16_geo_ed[HEEFT_AANMELD == 1]$wp_code)
  # JA! geen NA's

  ################################################################
  # PLAK WP_CODE AAN mq16 VIA ROW_ID
  mq16<-merge(mq16,subset(mq16_geo_ed, select = c(ROW_ID, wp_code)), 
              by = "ROW_ID", all.x = T)

  ################################################################
  # plak nu gemeente  info met aantal inwoners en adressendichtheid, en prov, GHOR, en LD

  # start met gm_code
  #cbs_wp16
  mq16 <- merge(mq16, cbs_wp16, by = "wp_code", all.x = T)
  
  ################################################################
  # Voeg G4 en G32 toe

  G4vec <- c("Utrecht", "'s-Gravenhage", "Rotterdam", "Amsterdam")

  # extract de gemeenten in mq16
  res <- unique(subset(mq16, !is.na(gm_naam), select = c(gm_naam, gm_code)))
  res <- data.table(res)

  res$G4 <- 0
  res[gm_naam %in% G4vec, G4 := 1]

   # bron: http://www.g32.nl/
  G32vec <- c("Alkmaar", "Almelo", "Almere",
         "Alphen aan den Rijn", "Amersfoort", "Apeldoorn", "Arnhem", "Breda",
         "Delft", "Deventer",
         "Dordrecht", "Ede", "Eindhoven", "Emmen", "Enschede", "Gouda", "Groningen",
         "Haarlem",
         "Haarlemmermeer", "Heerlen", "Helmond", "Hengelo", "\'s-Hertogenbosch",
         "Leeuwarden",
         "Leiden", "Lelystad", "Maastricht", "Nijmegen", "Oss", "Roosendaal",
         "Sittard-Geleen",
         "Schiedam", "Tilburg", "Venlo", "Zaanstad", "Zoetermeer", "Zwolle")
  res$G32 <- 0
  res[gm_naam %in% G32vec, G32 := 1]
  table(res$G32)
  res$G_OVG <- 0
  res[G32 == 0 & G4 == 0, G_OVG := 1]

  mq16<-merge(mq16, subset(res, select = c(gm_code, G4, G32, G_OVG)), 
              by = "gm_code", all.x = T)

  # toevoegen COROP regio
  cbs_gem_corop_2016 <- lees_in_cbs_corop_2016()

  mq16<-merge(mq16, subset(cbs_gem_corop_2016, select = c(gm_code, COROP)), 
              by = "gm_code", all.x = T)
  mq16
}
