# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#Funkcija za uvoz csv splošno zadovoljstvo z življenjem po regijah

uvozi.zadovoljstvo.REGIJE <- function() {
  zadovoljstvo.REGIJE <- read_csv2("podatki/splosno_zadovoljstvo_z_zivljenjem_PO_REGIJAH.csv", skip=2, na=c("", "..."),
                                locale=locale(encoding="Windows-1250"))
  colnames(zadovoljstvo.REGIJE) <- c('REGIJA', 'SAMOOCENA',2012:2018)
  
  ocena <- c("4","6","8","10","neznano")
  imena <- c("0-4 (%)", "5-6 (%)", "7-8 (%)", "9-10 (%)", "Neznano (%)")
  tab2 <- data.frame(samoocena=ocena, ime=imena, stringsAsFactors = FALSE)
  
  zadovoljstvo.REGIJE <- zadovoljstvo.REGIJE %>% inner_join(tab2, c("SAMOOCENA"="ime")) %>%
    filter(samoocena == 8 | samoocena == 10) %>%
    select(REGIJA, '2013', '2014', '2015', '2016', '2017', '2018', samoocena)
  
  return(zadovoljstvo.REGIJE)
}

zadovoljstvo_regije <- uvozi.zadovoljstvo.REGIJE()


#Funkcija za uvoz csv izobrazba po regijah

uvozi.izobrazba.REGIJE <- function() {
  izobrazba.REGIJE <- read_csv2("podatki/izobrazba_PO_REGIJAH.csv", skip=2,
                                locale=locale(encoding='Windows-1250'))
  
  #imena stolpcev
  colnames(izobrazba.REGIJE) <- c('REGIJA','IZOBRAZBA','STATUS','SPOL', 2011:2018)
  
  #izberemo samo ljudi z višješolsko izobrazbo
  izobrazba.REGIJE <- izobrazba.REGIJE %>% filter(IZOBRAZBA == 'Višješolska, visokošolska - Skupaj') %>%
    select(REGIJA, SPOL, '2013', '2014', '2015', '2016', '2017', '2018')
  
  #izobrazba.REGIJE %>% group_by(SPOL, REGIJA, add = TRUE)
  return(izobrazba.REGIJE)
}

izobrazba_regije <- uvozi.izobrazba.REGIJE()

#Funkcija za uvoz CSV dostopnost dobrin po dohodnku

# uvozi.dobrine.DOHODEK <- function() {
#   dobrine.DOHODEK <- read_csv2("podatki/dostopnost_izbranih_dobrin_DOHODEK.csv", skip=2,
#                                     locale=locale(encoding='Windows-1250'))
#   colnames(dobrine.DOHODEK) <- c('SPOL','DOHODEK','DOBRINA','MERITVE', 2014:2018)
#   return(dobrine.DOHODEK)
# }
# 
# dobrine_dohodek <- uvozi.dobrine.DOHODEK()

#Funkcija za uvoz CSV splošno zadovoljstvo z življenjem po dohodku

uvozi.zdovoljstvo.DOHODEK <- function() {
  zadovoljstvo.DOHODEK <- read_csv2("podatki/splosno_zadovoljstvo_z_zivljenjem_DOHODEK.csv", skip=2,
                                locale=locale(encoding='Windows-1250'))
  colnames(zadovoljstvo.DOHODEK) <- c('REGIJA','SPOL','DOHODEK','SAMOOCENA', 2012:2018)
  
  ocena <- c("4","6","8","10","neznano")
  imena <- c("0-4 (%)", "5-6 (%)", "7-8 (%)", "9-10 (%)", "Neznano (%)")
  tab2 <- data.frame(samoocena=ocena, ime=imena, stringsAsFactors = FALSE)
  
  zadovoljstvo.DOHODEK <- zadovoljstvo.DOHODEK %>% inner_join(tab2, c("SAMOOCENA"="ime")) %>%
    filter(samoocena == 8 | samoocena == 10) %>%
    select(SPOL, DOHODEK, '2013', '2014', '2015', '2016', '2017', '2018', samoocena)
  
  return(zadovoljstvo.DOHODEK)
}

zadovoljstvo_dohodek <- uvozi.zdovoljstvo.DOHODEK()

#Funkcija za uvoz csv splošno zdravstveno stanje po dohodku

uvozi.zdravstvo.DOHODEK <- function() {
  zdravstvo.DOHODEK <- read_csv2("podatki/splosno_zdravstveno_stanje_DOHODEK.csv", skip=2,
                                  locale=locale(encoding='Windows-1250'))
  colnames(zdravstvo.DOHODEK) <- c('DOHODEK','ZDRAVSTVO', 2012:2018)
  
  zdravstvo.DOHODEK <- zdravstvo.DOHODEK %>% filter (ZDRAVSTVO == 'Zelo dobro') %>%
    select(DOHODEK, '2013', '2014', '2015', '2016', '2017', '2018')
  
  return(zdravstvo.DOHODEK)
}

zdravstvo_dohodek <- uvozi.zdravstvo.DOHODEK()

#Funkcija za uvoz CSV dostopnost dobrin po starosti

# uvozi.dobrine.STAROST <- function() {
#   dobrine.STAROST <- read_csv2("podatki/dostopnost_izbranih_dobrin_STAROST.csv", skip=2,
#                                     locale=locale(encoding='Windows-1250'))
#   colnames(dobrine.STAROST) <- c('DOHODEK','STANJE', 2012:2018)
#   return(dobrine.STAROST)
# }
# 
# dobrine_starost <- uvozi.dobrine.STAROST()

#Funkcija za uvoz CSV zadovoljstvo z življenjem po starosti

uvozi.zadovoljstvo.STAROST <- function() {
  zadovoljstvo.STAROST <- read_csv2("podatki/splosno_zadovoljstvo_z_zivljenjem_STAROST.csv", skip=2,
                               locale=locale(encoding='Windows-1250'))
  colnames(zadovoljstvo.STAROST) <- c('REGIJA','STAROST','SPOL','SAMOOCENA', 2012:2018)
  # zadovoljstvo.STAROST$REGIJA <- NULL
  
  ocena <- c("4","6","8","10","neznano")
  imena <- c("0-4 (%)", "5-6 (%)", "7-8 (%)", "9-10 (%)", "Neznano (%)")
  tab2 <- data.frame(samoocena=ocena, ime=imena, stringsAsFactors = FALSE)
  
  zadovoljstvo.STAROST <- zadovoljstvo.STAROST %>% inner_join(tab2, c("SAMOOCENA"="ime")) %>%
    filter(samoocena == 8 | samoocena == 10) %>%
    select(STAROST, SPOL, '2013', '2014', '2015', '2016', '2017', '2018', samoocena)
  
  return(zadovoljstvo.STAROST)
}

zadovoljstvo_starost <- uvozi.zadovoljstvo.STAROST()

#Funkcija za uvoz CSV zdravstveno stanje po starosti

uvozi.zdravstvo.STAROST <- function() {
  zdravstvo.STAROST <- read_csv2("podatki/splosno_zdravstveno_stanje_STAROST.csv", skip=2,
                                    locale=locale(encoding='Windows-1250'))
  colnames(zdravstvo.STAROST) <- c('DOHODEK','ZDRAVSTVO', 2011:2018)
  
  zdravstvo.STAROST <- zdravstvo.STAROST %>% filter (ZDRAVSTVO == 'Zelo dobro') %>%
    select(DOHODEK, '2013', '2014', '2015', '2016', '2017', '2018')
  
  return(zdravstvo.STAROST)
}

zdravstvo_starost <- uvozi.zdravstvo.STAROST()









