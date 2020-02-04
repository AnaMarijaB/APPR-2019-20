# 4. faza: Analiza podatkov

require(ggdendro)

fit <- lm(data=PORABA, vrednost ~ as.numeric(leto))
l <- data.frame(leto=seq(2000,2025))
predict(fit,l)
napoved <- l %>% mutate(vrednost=predict(fit, .))

lin1 <- ggplot(PORABA, aes(x=as.numeric(leto), y=vrednost/1e6*1000000))+
  geom_line()+
  geom_point(data=napoved, aes(x=as.numeric(leto), y=vrednost/1e6*1000000), color='blue', size=3)+
  geom_smooth(method='lm', se=FALSE)+
  xlab('leto')+ylab('€/družinski član')

#shiny

graf.dostopnost <- function(spol,starost,dobrina){
  if(spol=='moski'){
    podatki <- DOSTOPNOST %>% filter(SPOL=='Moški' &
                                     STAROST==starost & DOBRINA==dobrina)
  }
  if(spol=='zenski'){
    podatki <- DOSTOPNOST %>% filter(SPOL=='Ženske' &
                                     STAROST==starost & DOBRINA==dobrina)
  }
  don <- xts(x=podatki$VREDNOST, order.by=as.yearqtr.default(podatki$LETO))
  dygraph(don) %>%
    dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, 
              drawGrid = FALSE, colors="#f09ce0") %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, 
                hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1) %>%
    dySeries("V1", label = "Število")
}
