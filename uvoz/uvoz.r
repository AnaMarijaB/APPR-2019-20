# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#Funkcija za uvoz csv splošno zadovoljstvo z življenjem po regijah
# izvzela sem leto 2018, tiste ki so svoje zadovoljstvo ocenili z 10
#to tabelo bom rabila za zemljevid

uvozi.povprecno.zadovoljstvo <- function() {
  zadovoljstvo.REGIJE <- read_csv2("podatki/splosno_zadovoljstvo_z_zivljenjem_PO_REGIJAH.csv", skip=2, na=c("", "..."),
                                   locale=locale(encoding="Windows-1250"))
  colnames(zadovoljstvo.REGIJE) <- c('Regija', 'SAMOOCENA',2012:2018)
  
  ocena <- c("4","6","8","10","neznano")
  imena <- c("0-4 (%)", "5-6 (%)", "7-8 (%)", "9-10 (%)", "Neznano (%)")
  tab2 <- data.frame(samoocena=ocena, ime=imena, stringsAsFactors = FALSE)
  
  zadovoljstvo.REGIJE <- zadovoljstvo.REGIJE %>% inner_join(tab2, c("SAMOOCENA"="ime")) %>%
    filter(samoocena == 10) %>%
    select(Regija, '2013', '2014', '2015', '2016', '2017', '2018')
  
  zadovoljstvo.REGIJE <- zadovoljstvo.REGIJE %>% group_by(Regija)%>%summarise_all(funs(sum))
  zadovoljstvo.REGIJE <- gather(zadovoljstvo.REGIJE, key = "leto", value = "vrednost", -Regija)
  
  return(zadovoljstvo.REGIJE)
  
}

povprecno.zadovoljstvo <- uvozi.povprecno.zadovoljstvo()

#Funkcija za uvoz csv splošno zadovoljstvo z življenjem po regijah
# izvzela sem vsa leta, ljudi ki so zadovoljstvo ocenili z 8 ali več
#iz te tabele bom narisala graf

uvozi.zadovoljstvo.REGIJE <- function() {
  zadovoljstvo.REGIJE <- read_csv2("podatki/splosno_zadovoljstvo_z_zivljenjem_PO_REGIJAH.csv", skip=2, na=c("", "..."),
                                locale=locale(encoding="Windows-1250"))
  colnames(zadovoljstvo.REGIJE) <- c('Regija', 'SAMOOCENA',2012:2018)

  ocena <- c("4","6","8","10","neznano")
  imena <- c("0-4 (%)", "5-6 (%)", "7-8 (%)", "9-10 (%)", "Neznano (%)")
  tab2 <- data.frame(samoocena=ocena, ime=imena, stringsAsFactors = FALSE)

  zadovoljstvo.REGIJE <- zadovoljstvo.REGIJE %>% inner_join(tab2, c("SAMOOCENA"="ime")) %>%
    filter(samoocena == 8 | samoocena == 10) %>%
    select(Regija,'2013', '2014', '2015', '2016', '2017', '2018')
  
  zadovoljstvo.REGIJE <- zadovoljstvo.REGIJE %>% group_by(Regija)%>%summarise_all(funs(sum))
  zadovoljstvo.REGIJE <- gather(zadovoljstvo.REGIJE, key = "leto", value = "vrednost", -Regija)

  return(zadovoljstvo.REGIJE)
}


zadovoljstvo_regije <- uvozi.zadovoljstvo.REGIJE()

#Funkcija za uvoz csv izobrazba po regijah
#to tabelo bom rabila za zemljevid s procenti izobraženih ljudi. Primerjala bom razmerje med zadovoljstvom in izobrazbo po regijah

uvozi.izobrazba.REGIJE <- function() {
  izobrazba.REGIJE <- read_csv2("podatki/izobrazba_PO_REGIJAH.csv", skip=2,
                                locale=locale(encoding='Windows-1250'))
  
  #imena stolpcev
  colnames(izobrazba.REGIJE) <- c('Regija','IZOBRAZBA','STATUS','SPOL', 2013:2018)
  izobrazba.REGIJE$STATUS <- NULL
  izobrazba.REGIJE$SPOL <- NULL
  
  stevilo_vseh_ljudi <- izobrazba.REGIJE %>% select(-IZOBRAZBA) %>% group_by(Regija)%>% summarise_all(funs(sum))
  stevilo_vseh_ljudi <- gather(stevilo_vseh_ljudi, key = "leto", value = "vsi", -Regija)
  stevilo_vseh_ljudi <- stevilo_vseh_ljudi %>% filter(leto == 2018)
  stevilo_vseh_ljudi$leto <- NULL
  
  izobrazeni <- izobrazba.REGIJE %>% filter(IZOBRAZBA == 'Višješolska, visokošolska - Skupaj') %>%
    select(Regija,'2018')
  izobrazeni <- gather(izobrazeni, key = "leto", value = "IZOBRAZENI", -Regija)
  izobrazeni$leto <- NULL
  
  procenti <- merge(stevilo_vseh_ljudi, izobrazeni, by= 'Regija')
  procenti$povprecje <- ((procenti$IZOBRAZENI)*100)/(procenti$vsi)
  
  
  return(procenti)
}

izobrazba_regije <- uvozi.izobrazba.REGIJE()



#Funkcija za uvoz CSV splošno zadovoljstvo z življenjem po dohodku
#moški in ženske skupaj (vseh je 200). Izbrala sem tiste ki so zadovoljstvo ocenili z več kot 8
#rabim še povprečje
#narisala bom graf

uvozi.zdovoljstvo.DOHODEK <- function() {
  zadovoljstvo.DOHODEK <- read_csv2("podatki/splosno_zadovoljstvo_z_zivljenjem_DOHODEK.csv", skip=2,
                                locale=locale(encoding='Windows-1250'))
  colnames(zadovoljstvo.DOHODEK) <- c('REGIJA','SPOL','DOHODEK','SAMOOCENA', 2012:2018)
  
  ocena <- c("4","6","8","10","neznano")
  imena <- c("0-4 (%)", "5-6 (%)", "7-8 (%)", "9-10 (%)", "Neznano (%)")
  tab2 <- data.frame(samoocena=ocena, ime=imena, stringsAsFactors = FALSE)
  
  zadovoljstvo.DOHODEK <- zadovoljstvo.DOHODEK %>% inner_join(tab2, c("SAMOOCENA"="ime")) %>%
    filter(samoocena == 8 | samoocena == 10) %>%
    select(DOHODEK, '2013', '2014', '2015', '2016', '2017', '2018')
  
  zadovoljstvo.DOHODEK <- zadovoljstvo.DOHODEK %>% group_by(DOHODEK)%>%summarise_all(funs(sum))
  zadovoljstvo.DOHODEK <- gather(zadovoljstvo.DOHODEK, key = "leto", value = "vrednost", -DOHODEK)
  
  return(zadovoljstvo.DOHODEK)
}

zadovoljstvo_dohodek <- uvozi.zdovoljstvo.DOHODEK()

#Funkcija za uvoz csv splošno zdravstveno stanje po dohodku
#ZA ženske in moške skupaj, tisti ki so ga ocenili kot najboljše. Vseh je 100
#to bom rabila za graf. Primerjala bom razmerje med zadovoljstvom z življenjem, dohodkom in zdravstvenim stanjem

uvozi.zdravstvo.DOHODEK <- function() {
  zdravstvo.DOHODEK <- read_csv2("podatki/splosno_zdravstveno_stanje_DOHODEK.csv", skip=2,
                                  locale=locale(encoding='Windows-1250'))
  colnames(zdravstvo.DOHODEK) <- c('DOHODEK','ZDRAVSTVO', 2012:2018)
  
  zdravstvo.DOHODEK <- zdravstvo.DOHODEK %>% filter (ZDRAVSTVO == 'Zelo dobro' | ZDRAVSTVO == 'Dobro') %>%
    select(DOHODEK, '2013', '2014', '2015', '2016', '2017', '2018')
  
  zdravstvo.DOHODEK <- zdravstvo.DOHODEK %>% group_by(DOHODEK)%>%summarise_all(funs(sum))
  zdravstvo.DOHODEK <- gather(zdravstvo.DOHODEK, key = "leto", value = "vrednost", -DOHODEK)
  
  return(zdravstvo.DOHODEK)
}

zdravstvo_dohodek <- uvozi.zdravstvo.DOHODEK()


#Funkcija za uvoz CSV zadovoljstvo z življenjem po starosti
# za oba spola skupaj, tisti ki so zadovokstvo označili z 8 ali več. Vseh skupaj je 200
#dodati morem stolpec povprečja

uvozi.zadovoljstvo.STAROST <- function() {
  zadovoljstvo.STAROST <- read_csv2("podatki/splosno_zadovoljstvo_z_zivljenjem_STAROST.csv", skip=2,
                               locale=locale(encoding='Windows-1250'))
  colnames(zadovoljstvo.STAROST) <- c('REGIJA','STAROST','SPOL','SAMOOCENA', 2013:2018)
  
  ocena <- c("4","6","8","10","neznano")
  imena <- c("0-4 (%)", "5-6 (%)", "7-8 (%)", "9-10 (%)", "Neznano (%)")
  tab2 <- data.frame(samoocena=ocena, ime=imena, stringsAsFactors = FALSE)
  
  zadovoljstvo.STAROST <- zadovoljstvo.STAROST %>% inner_join(tab2, c("SAMOOCENA"="ime")) %>%
    filter(samoocena == 8 | samoocena == 10) %>%
    select(STAROST, '2013', '2014', '2015', '2016', '2017', '2018')
  
  zadovoljstvo.STAROST <- zadovoljstvo.STAROST %>% group_by(STAROST)%>%summarise_all(funs(sum))
  zadovoljstvo.STAROST <- gather(zadovoljstvo.STAROST, key = "leto", value = "vrednost", -STAROST)
  
  return(zadovoljstvo.STAROST)
}

zadovoljstvo_starost <- uvozi.zadovoljstvo.STAROST()

#Funkcija za uvoz CSV zdravstveno stanje po starosti

uvozi.zdravstvo.STAROST <- function() {
  zdravstvo.STAROST <- read_csv2("podatki/splosno_zdravstveno_stanje_STAROST.csv", skip=2,
                                    locale=locale(encoding='Windows-1250'))
  colnames(zdravstvo.STAROST) <- c('STAROST','ZDRAVSTVO', 2011:2018)
  
  zdravstvo.STAROST <- zdravstvo.STAROST %>% filter (ZDRAVSTVO == 'Zelo dobro') %>%
    select(STAROST, '2013', '2014', '2015', '2016', '2017', '2018')
  
  zdravstvo.STAROST <- gather(zdravstvo.STAROST, key = "leto", value = "vrednost", -STAROST)
  
  return(zdravstvo.STAROST)
}

zdravstvo_starost <- uvozi.zdravstvo.STAROST()









