
typeer_praktijken_zonder_type <- function(mq16) {
  # onderzoek typering praktijken zonder type.ggz
  table(subset(mq16, HEEFT_AANMELD == 1)$HEEFT_CIRCUIT,
        subset(mq16, HEEFT_AANMELD == 1)$HEEFT_TYPEGGZ, useNA = "ifany")

  dim(mq16[HEEFT_AANMELD == 1 & HEEFT_TYPEGGZ == 0,])
  # 60 wachttijden zonder typering

  # zet default (prior)
  mq16[HEEFT_AANMELD == 1 & HEEFT_TYPEGGZ == 0, Type.GGZ := "BGGZ/SGGZ"]

  # selecteer records met aanmeld wachttijd maar zonder ggz typering
  res <- mq16[HEEFT_AANMELD == 1 & HEEFT_TYPEGGZ == 0,
            .N, .(Naam.zorgaanbieder, Type.GGZ, URL.naar.wachttijden,
                  Datum.actualisatie)]
  dim(res)
  # N=50
  sum(res$N)
  # 60 records

  write.csv2(res, "work/praktijken_zonder_type_ggz_handmatige_typering.csv" )

  # na handmatige typering weer terug inlezen
  praktijken_handmatige_typering <- read.csv2("input/praktijken_zonder_type_ggz_handmatige_typering_ed.csv")
  praktijken_handmatige_typering <- data.table(praktijken_handmatige_typering)
  # overschrijf Vektis Type.ggz voor deze 50 praktijken

  # check dat we kunnen koppelen
  table(praktijken_handmatige_typering[, .(Naam.zorgaanbieder, TYPERING_HANDMATIG)]$Naam.zorgaanbieder 
        %in% mq16$Naam.zorgaanbieder)
  # ja 50x match
  dim(mq16)
  setkey(mq16, Naam.zorgaanbieder)
  setkey(praktijken_handmatige_typering, Naam.zorgaanbieder)
  mq16 <- praktijken_handmatige_typering[,.(Naam.zorgaanbieder, TYPERING_HANDMATIG)][mq16]
  dim(mq16)
  dim(mq16[!is.na(TYPERING_HANDMATIG),])
  # 1 extra record
  # kennelijk een aanbieder met zowel ongetypeerde als getypeerde records

  mq16[!is.na(TYPERING_HANDMATIG) & HEEFT_AANMELD == 0, 
       .(Naam.zorgaanbieder, Type.GGZ, HEEFT_AANMELD, Datum.actualisatie, TYPERING_HANDMATIG, ROW_ID)]
  # gaat om Psychotherapiecentrum Twente
  # is ok, 2e record heeft geen aanmeldtijd

  # Corrigeer type.ggz obv handmatige typering
  mq16[TYPERING_HANDMATIG %in% c("BGGZ", "SGGZ", "BGGZ/SGGZ"), Type.GGZ := TYPERING_HANDMATIG]

  table(mq16$Type.GGZ)
  mq16
}

