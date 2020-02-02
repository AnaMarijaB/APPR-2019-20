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

