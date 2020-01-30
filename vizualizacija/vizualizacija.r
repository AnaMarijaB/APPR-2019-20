# 3. faza: Vizualizacija podatkov

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source('lib/uvozi.zemljevid.r')
source('lib/libraries.r', encoding = 'UTF-8')

#UVOZIM ZEMLJEVID SLOVENIJE

Slovenija <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                             "SVN_adm1") %>% fortify()
colnames(Slovenija)[12] <- 'Regija'
Slovenija$Regija <- gsub('Notranjsko-kraška', 'Primorsko-notranjska', Slovenija$Regija)
Slovenija$Regija <- gsub('Spodnjeposavska', 'Posavska', Slovenija$Regija)

graf_slovenija <- ggplot(Slovenija, aes(x=long, y=lat, group=group, fill=Regija)) +
  geom_polygon() +
  labs(title="Slovenija") +
  theme_classic()

#tabela za zemljevid:
najsrecnejsi_2018 <- filter(zadovoljstvo_regije, leto == '2018')

#zemljevid: procent ljud, ki so zadovoljstvo v letu 2018 ocenili z 8 ali več
zemljevid.zadovoljstvo <- ggplot() +
  geom_polygon(data = right_join(najsrecnejsi_2018,Slovenija, by = c('Regija')),
               aes(x = long, y = lat, group = group, fill = vrednost))+
  xlab("") + ylab("") + ggtitle('število ljudi, ki so srečo ocenili več kot 8 v letu 2018') + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) + 
  scale_fill_gradient(low = '#25511C', high='#2BFF00', limits = c(50,80))
zemljevid.zadovoljstvo$labels$fill <- 'procent najsrečnejših'

#zemljevid: procent ljudi, ki imajo višješolsko zobrazbo ali več v letu 2018
zemljevid.izobrazba <- ggplot() +
  geom_polygon(data = right_join(izobrazba_regije,Slovenija, by = c('Regija')),
               aes(x = long, y = lat, group = group, fill = povprecje))+
  xlab("") + ylab("") + ggtitle('PROCENT LJUDI Z VIŠJEŠOLSKO IZOBRAZBO ALI VEČ V LETU 2018') + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) + 
  scale_fill_gradient(low = '#25511C', high='#2BFF00', limits = c(7,12))
zemljevid.izobrazba$labels$fill <- 'procent izobraženih'

#stolpični grafikon: procent ludi po kvantilih in letih, ki so svojo srečo označili več kot 8
prvi_kvintil1 <- zadovoljstvo_dohodek %>% filter(DOHODEK=='1.kvintil')
drugi_kvintil1 <- zadovoljstvo_dohodek %>% filter(DOHODEK=='2.kvintil')
tretji_kvintil1 <- zadovoljstvo_dohodek %>% filter(DOHODEK=='3.kvintil')
cetrti_kvintil1 <- zadovoljstvo_dohodek %>% filter(DOHODEK=='4.kvintil')
peti_kvintil1 <- zadovoljstvo_dohodek %>% filter(DOHODEK=='5.kvintil')

graf_sreca <- plot_ly(zadovoljstvo_dohodek, x = ~2013:2018, y = ~prvi_kvintil1$vrednost, type = 'bar', name = '1.kvintil', marker = list(color = 'rgb(225,52,139)')) %>%
  add_trace(y = ~drugi_kvintil1$vrednost, name = '2.kvintil', marker = list(color = 'rgb(176,58,117)')) %>%
  add_trace(y = ~tretji_kvintil1$vrednost, name = '3.kvintil', marker = list(color = 'rgb(131,46,88)')) %>%
  add_trace(y = ~cetrti_kvintil1$vrednost, name = '4.kvintil', marker = list(color = 'rgb(121,23,73)')) %>%
  add_trace(y = ~peti_kvintil1$vrednost, name = '5.kvintil', marker = list(color = 'rgb(102,0,51)')) %>%
  layout(title = 'PROCENT LJUDI PO DOHODKU, KI SO SVOJO SREČO OZNAČILI VEČ KOT 8',
         xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')

#stolpični grafikon: procent ludi po kvantilih in letih, ki so svoje zdravje ocenili z Dobro ali Zelo dobro
prvi_kvintil2 <- zdravstvo_dohodek %>% filter(DOHODEK=='1. kvintil')
drugi_kvintil2 <- zdravstvo_dohodek %>% filter(DOHODEK=='2. kvintil')
tretji_kvintil2 <- zdravstvo_dohodek %>% filter(DOHODEK=='3. kvintil')
cetrti_kvintil2 <- zdravstvo_dohodek %>% filter(DOHODEK=='4. kvintil')
peti_kvintil2 <- zdravstvo_dohodek %>% filter(DOHODEK=='5. kvintil')

graf_zdravje <- plot_ly(zdravstvo_dohodek, x = ~2013:2018, y = ~prvi_kvintil2$vrednost, type = 'bar', name = '1.kvintil', marker = list(color = 'rgb(225,52,139)')) %>%
  add_trace(y = ~drugi_kvintil2$vrednost, name = '2.kvintil', marker = list(color = 'rgb(176,58,117)')) %>%
  add_trace(y = ~tretji_kvintil2$vrednost, name = '3.kvintil', marker = list(color = 'rgb(131,46,88)')) %>%
  add_trace(y = ~cetrti_kvintil2$vrednost, name = '4.kvintil', marker = list(color = 'rgb(121,23,73)')) %>%
  add_trace(y = ~peti_kvintil2$vrednost, name = '5.kvintil', marker = list(color = 'rgb(102,0,51)')) %>%
  layout(title = 'PROCENT LJUDI PO DOHODKU, KI SO SVOJE ZDRAVJE OCENILI Z DOBRO ali ZELO DOBRO',
         xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')

#graf: zadovoljstvo z šivljenjem po starosti
graf_starost <- ggplot((data = zadovoljstvo_starost), aes(x= as.numeric(leto), y=vrednost, col=STAROST)) + geom_point() + geom_line() +
  scale_x_continuous('leto', breaks = seq(2013, 2018, 1), limits = c(2013,2018)) +
  ggtitle('PROCENT LJUDI PO STAROSTI, KI SO SVOJO SREČO OZNAČILI VEČ KOT 8')

#GRAF: ocena zdravstvenega stanja po starosti
graf_zdravstvo <- ggplot((data = zdravstvo_starost), aes(x= as.numeric(leto), y=vrednost, col=STAROST)) + geom_point() + geom_line() +
  scale_x_continuous('leto', breaks = seq(2013, 2018, 1), limits = c(2013,2018)) +
  ggtitle('PROCENT LJUDI PO STAROSTI, KI SO SVOJE ZDRAVJE OCENILI Z ZELO DOBRO')
