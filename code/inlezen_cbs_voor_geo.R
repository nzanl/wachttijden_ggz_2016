# inlezen CBS data voor regionale analyse

lees_in_cbs_bevolkingskernen_2011 <- function(){
  
  cbs_kern_inw_header <- read.csv2("input/cbs\\cbs_bevolkingskernen_2011\\Bevolkingskernen_201_041116162511.csv", 
                                   nrows = 5)

  cbs_kern_inw <- read.csv2("input/cbs\\cbs_bevolkingskernen_2011\\Bevolkingskernen_201_041116162511.csv", 
                            skip = 5, na.strings = c("-"))

  # aantal particuliere huishoudens in kern
  cbs_kern_inw <- subset(cbs_kern_inw, 
                         select = c(Bevolkingskernen, aantal.1, adressen.per.km2, klasse))
  cbs_kern_inw <- data.table(cbs_kern_inw)
  cbs_kern_inw <- cbs_kern_inw[Bevolkingskernen != "1 000 tot 2 000 inwoners"]

  summary(cbs_kern_inw$aantal.1)

  # verwijderen () en alles na /

  #()
  cbs_kern_inw$WOONPLAATS <- gsub("\\s*\\([^\\)]+\\)","", as.character(cbs_kern_inw$Bevolkingskernen))
  setnames(cbs_kern_inw, "aantal.1", "aant_part_hh")
  #/
  cbs_kern_inw$WOONPLAATS <- gsub("/.*", "", cbs_kern_inw$WOONPLAATS)
  # haal groot- eruit
  cbs_kern_inw$WOONPLAATS <- gsub("Groot - (.*)", "\\1", cbs_kern_inw$WOONPLAATS)
  # haal -Kern eruit (gebruik capture group ()
  cbs_kern_inw$WOONPLAATS <- gsub("(*.)-Kern", "\\1", cbs_kern_inw$WOONPLAATS)

  data.table(cbs_kern_inw)
}


# inlezen CBS woonplaatsen in 2016 met gem/prov codes

lees_in_cbs_woonplaatsen_2016 <- function(){
  cbs_wp16 <- read.csv2("input/cbs\\cbs_woonplaatsen_in_2016\\Woonplaatsen_in_Nede_081116084924.csv", 
                        skip = 5, na.strings = c("-"))
  colnames(cbs_wp16) <- c("wp_naam", "wp_code", "gm_naam", "gm_code", "pv_naam", "pv_code", "ld_naam", "ld_code")

  cbs_wp16$wp_naam_lo <- as.factor(tolower(as.character(cbs_wp16$wp_naam)))

  data.table(cbs_wp16)
}

# Inlezen CBS Gemeentenr - COROP nummer 1981-2016

lees_in_cbs_corop_2016 <- function(){
  cbs_gem_corop <- read.csv2("input/cbs\\cbs_gemeente-corop_koppeling\\gemeentenencoropvanaf1981.csv")

  cbs_gem_corop_2016 <- data.table(unique(subset(cbs_gem_corop, select=c(GM2016, COROP2016))))
  setnames(cbs_gem_corop_2016, "GM2016", "GM_CODE")
  setnames(cbs_gem_corop_2016, "COROP2016", "COROP")

  cbs_gem_corop_2016$gm_code <- sprintf("GM%04d", cbs_gem_corop_2016$GM_CODE)

  cbs_gem_corop_2016
}
