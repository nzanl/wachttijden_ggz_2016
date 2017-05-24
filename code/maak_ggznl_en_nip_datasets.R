
maak_ggznl_dataset <- function(){
  # Inlezen GGZ NL wachttijd meting oktober 2016: N=24 grote instellingen:
  # 1,6 mld omzet
  # N=23
  # Bron: http://www.ggznederland.nl/uploads/assets/Analyse%20quick%20scan%20productie%202016.pdf

  # reken % terug naar aantal instellingen
  ggznl_2016_vo_aanmeld <- round(c(13, 35, 35, 9, 9) * 23 / 100, 0)
  ggznl_2016_vo_aanmeld_bin <- c("[1-2]", "[3-4]", "[5-6]", "[7-8]","[>8]")
  # N=24
  ggznl_2016_vo_behandel <- round(c(4, 33, 29, 17, 13, 4) * 24 / 100, 0)
  ggznl_2016_vo_behandel_bin <- c("[1-2]", "[3-4]", "[5-6]", "[7-8]","[9-10]", "[>10]")

  # N=17
  ggznl_2016_ou_aanmeld <- round(c(47, 29, 24) * 17 / 100, 0)
  ggznl_2016_ou_aanmeld_bin <- c("[1-2]", "[3-4]", "[5-6]")

  # N=17
  ggznl_2016_ou_behandel <- round(c(6, 47, 29, 6, 12) * 17 / 100, 0)
  ggznl_2016_ou_behandel_bin <- c("[1-2]", "[3-4]", "[5-6]", "[7-8]","[9-10]")

  #### samenvoegen
  ggznl_2016_vo_aanmeld <- data.frame(WT_N = ggznl_2016_vo_aanmeld, 
                                      WT_BIN = ggznl_2016_vo_aanmeld_bin)
  ggznl_2016_vo_behandel <- data.frame(WT_N = ggznl_2016_vo_behandel, 
                                       WT_BIN = ggznl_2016_vo_behandel_bin)
  ggznl_2016_vo_aanmeld$CIRCUIT <- "Volwassenen"
  ggznl_2016_vo_behandel$CIRCUIT <- "Volwassenen"

  ggznl_2016_vo_aanmeld$TYPE_WT <- "Aanmeld"
  ggznl_2016_vo_behandel$TYPE_WT <- "Behandel"

  ggznl_2016_ou_aanmeld <- data.frame(WT_N = ggznl_2016_ou_aanmeld, 
                                      WT_BIN = ggznl_2016_ou_aanmeld_bin)
  ggznl_2016_ou_behandel <- data.frame(WT_N = ggznl_2016_ou_behandel, 
                                       WT_BIN = ggznl_2016_ou_behandel_bin)
  ggznl_2016_ou_aanmeld$CIRCUIT <- "Ouderen"
  ggznl_2016_ou_behandel$CIRCUIT <- "Ouderen"
  ggznl_2016_ou_aanmeld$TYPE_WT <- "Aanmeld"
  ggznl_2016_ou_behandel$TYPE_WT <- "Behandel"
  
  ggznl_2016 <- rbind(ggznl_2016_vo_aanmeld,
                    ggznl_2016_vo_behandel,
                    ggznl_2016_ou_aanmeld,
                    ggznl_2016_ou_behandel
  )
  ggznl_2016$WEERGAVE_NR <- c(1:5, 1:6, 1:3, 1:5)
  ggznl_2016
}

maak_nip_2016_dataset <- function(){

  ###############################################################
  # NIP enquete 2016 werken als vrijgevestigde
  # https://www.psynip.nl/wp-content/uploads/2016/10/Onderzoeksrapport-2016-Werken-als-vrijgevestigde-Eindversie.pdf

  # Van de respondenten werkt 90 procent in de GBGGZ en 37 procent werkt (ook) in de GGGZ.
  # In juni en juli van 2016 konden vrijgevestigde zorgaanbieders via een online vragenlijst
  # hun mening over verschillende onderwerpen geven.

  # In totaal hebben 126 zorgaanbieders de vragen met betrekking tot het jeugdhulpstelsel ingevuld;
  # 209 zorgaanbieders hebben de vragen met betrekking tot het zorgstelsel ingevuld.
  # De meerderheid van de ondervraagden (83%) heeft een BIG-registratie.
  # De meeste vrijgevestigden werken meer dan 24 uur per week en halen hun primaire inkomen uit de eigen praktijk.

  # !! BELANGRIJK: NIP rapport is SUBSET MET WACHTLIJST.
  # 2016: 45% hanteert wachtlijst. 55% DUS NIET.

  # N=209 zvw
  nip_2016_vr_aanmeld <- round(c(16, 24, 40, 14, 6) * 209 * 0.45 / 100, 0)
  nip_2016_vr_aanmeld_bin <- c("[0-2]", "[3-4]", "[5-10]", "[11-20]","[>20]")
  # voeg respondenten zonder wachtlijst toe
  nip_2016_vr_aanmeld[1] <- nip_2016_vr_aanmeld[1] + round(0.55 * 209, 0)

  # N=209
  nip_2016_vr_behandel <- round(c(61, 14, 11, 10, 4) * 209 * 0.45 / 100, 0)
  nip_2016_vr_behandel_bin <- c("[0-2]", "[3-5]", "[6-10]", "[11-20]","[20+]")
  # voeg respondenten zonder wachtlijst toe
  nip_2016_vr_behandel[1] <- nip_2016_vr_behandel[1] + round(0.55 * 209, 0)

  # samenvoegen
  nip_2016_vr_aanmeld <- data.frame(WT_N = nip_2016_vr_aanmeld,
                                    WT_BIN = nip_2016_vr_aanmeld_bin)
  nip_2016_vr_behandel <- data.frame(WT_N = nip_2016_vr_behandel,
                                     WT_BIN = nip_2016_vr_behandel_bin)
  nip_2016_vr_aanmeld$CIRCUIT <- "Vrijgevestigd"
  nip_2016_vr_behandel$CIRCUIT <- "Vrijgevestigd"
  nip_2016_vr_aanmeld$TYPE_WT <- "Aanmeld"
  nip_2016_vr_behandel$TYPE_WT <- "Behandel"

  nip_2016 <- rbind(nip_2016_vr_aanmeld,
                   nip_2016_vr_behandel
  )
  nip_2016$WEERGAVE_NR <- c(1:5, 1:5)
  nip_2016
}
