# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#Funkcija za uvoz csv splošno zadovoljstvo z življenjem po regijah

uvozi.zadovoljstvo.REGIJE <- function() {
  zadovoljstvo.REGIJE <- read_csv2("podatki/splosno_zadovoljstvo_z_zivljenjem_PO_REGIJAH.csv", skip=2, na=c("", "..."),
                                locale=locale(encoding="Windows-1250"))
  colnames(zadovoljstvo.REGIJE) <- c('REGIJA', 'SAMOOCENA',2012:2018)
  return(zadovoljstvo.REGIJE)
}


#Funkcija za uvoz csv izobrazba po regijah

uvozi.izobrazba.REGIJE <- function() {
  izobrazba.REGIJE <- read_csv2("podatki/izobrazba_PO_REGIJAH.csv", skip=2,
                                locale=locale(encoding='Windows-1250'))
  colnames(izobrazba.REGIJE) <- c('REGIJA','IZOBRAZBA','STATUS','SPOL', 2011:2018)
  izobrazba.REGIJE$STATUS <- NULL
  return(izobrazba.REGIJE)
}


#Funkcija za uvoz xls dostopnost dobrin po dohodnku

uvozi.dobrine.DOHODEK <- function() {
  dobrine.DOHODEK <- read_xlsx("podatki/dostopnost_izbranih_dobrin_DOHODEK.xlsx", skip=2)
  colnames(dobrine.DOHODEK) <- c('SPOL','KVINTIL','DOBRINA','MERITVE', 2014:2018)
  return(dobrine.DOHODEK)
}


#Funkcija za uvoz xls splošno zadovoljstvo z življenjem po dohodku

uvozi.zadovoljstvo.DOHODEK <- function() {
  zadovoljstvo.DOHODEK <- read_xlsx("podatki/splosno_zadovoljstvo_z_zivljenjem_DOHODEK.xlsx", skip=2)
  colnames(zadovoljstvo.DOHODEK) <- c('SAMOOCENA','REGIJA','SPOL','KVINTIL', 2012:2018)
  zadovoljstvo.DOHODEK$REGIJA <- NULL
  return(zadovoljstvo.DOHODEK)
}

#Funkcija za uvoz xls splošno zdravstveno stanje po dohodku

uvozi.zdravje.DOHODEK <- function() {
  zdravje.DOHODEK <- read_xlsx("podatki/splosno_zdravstveno_stanje_DOHODEK.xlsx", skip=2)
  colnames(zdravje.DOHODEK) <- c('KVINTIL','STANJE',2005:2018)
  return(zdravje.DOHODEK)
}

#Funkcija za uvoz html dostopnost dobrin po starosti

uvozi.dobrine.STAROST <- function() {
  link <- 'C:/Users/Ana/Documents/APPR-2019-20/podatki/dostopnost_izbranih_dobrin_STAROST.htm'
  stran <- read_html(link)
  dobrine.STAROST <- stran %>% html_nodes(xpath="//table") %>% html_table() %>% fill
  return(dobrine.STAROST)
}













