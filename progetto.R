# Scarico librerie e importo dataset --------------------------------------
require(tidyverse)
require(magrittr)
library(lubridate)
library(dplyr)
library(tidyr)
require(ggplot2)
require(gridExtra)
remove(dati.env,ISTAT2020_red,RiceFarms)
#----------------------- -------------------------------------------------
is.na(SumHes)#verifichiamo se ci sono valori nulli
paesi<-unique(SumHes$country) #in questo modo vediamo tutti e 125 i paesi senza ripetizioni
summary(SumHes)#visione generale

# com  --------------------------------------------------------------------


#paesi_comunisti contiene i valori unici della colonna "country" solo per le righe in cui "com" è "yes", perciò vediamo solo quali sono i paesi comunisti.
paesi_comunisti <- SumHes %>%
  filter(com == "yes") %>%
  distinct(country)
view(paesi_comunisti)

#Anno, Paese e GDP dei paesi comunisti 
gdp_paese_com<- SumHes %>%
  filter(com == "yes") %>% 
  select(country,year,gdp)
view(gdp_paese_com)

#grafico pil comunisti 
grafico_pil_com<- ggplot(gdp_paese_com,aes(x = year, y = gdp, color=country))+
  geom_line()+
  geom_point()+ 
  geom_smooth(method = lm, se = T, linewidth = 0.5, linetype = 1, alpha = .5)+
  labs(title = "Pil paesi comunisti")

library(lme4)

# Esegui una regressione multilivello: la popolazione dipende dal comunismo?
multilevel_model <- lmer(pop ~ year + (com=="yes")+ (1|country), data = SumHes)

summary(multilevel_model)
#isolo i dati relativi ai paesi comunisti
pop_paese_com<- SumHes %>%
  filter(com == "yes") %>% 
  select(country,year,pop)
view(pop_paese_com)


# Crea un dataframe con i dati osservati e previsti
results <- data.frame(
  Observed = pop_paese_com$pop,                      # Dati osservati della popolazione
  Predicted = predict(multilevel_model)      # Dati previsti dal modello
)

# Crea un grafico a dispersione tra dati osservati e dati previsti
dati_oss_previsti<- ggplot(results, aes(x = Observed, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Popolazione Osservata", y = "Popolazione Prevista", title = "Grafico di Confronto tra Dati Osservati e Previsti")
#da come possiamo vedere, i dati previsti non corrispondono ai dati osservati. Ciò significa che il modello non si adatta bene ai dati 

# opec --------------------------------------------------------------------


#paesi che hanno esportato il petrolio 
paesi_exp_petrolio<-SumHes %>%
  filter(opec == "yes") %>%
  distinct(country)
view(paesi_exp_petrolio)


#Anno,Paese e GDP dei paesi esportatori di petrolio
gdp_petrolio<-SumHes %>%
  filter(opec == "yes") %>%
  select(year,country,gdp)
view(gdp_petrolio)
#Anno,Paese e SR dei paesi esportatori di petrolio
sr_petrolio<-SumHes %>%
  filter(opec == "yes") %>%
  select(year,country,sr)
view(sr_petrolio)
#sr e pil petrolio
sr_gdp_petrolio<-SumHes %>%
  filter(opec == "yes") %>%
  select(year,country,sr,gdp)

#relazione tra sr(dipendente) e gdp(indipendente)
regr_sr_gdp_opec<- lmer(sr ~ gdp+(1|country),data=sr_gdp_petrolio)
summary(regr_sr_gdp_opec)
#da qui vediamo che sr non dipende significamente dal pil

#grafico relazione sr e pil dei paesi opec (generale)
grafico_regr_sr_pil_opec<- ggplot(sr_gdp_petrolio, aes(x = gdp, y = sr, color= country)) +
  geom_point() +  # Punti di dispersione
  geom_smooth(method = "lm") +  # Linea di regressione
  labs(
    x = "PIL",
    y = "Tasso di Risparmio",
    title = "Regressione del Tasso di Risparmio sul PIL"
  )

#grafico pil esportatori petrolio
grafico_pil_opec<- ggplot(gdp_petrolio, aes(x = year, y = gdp, color=country))+
  geom_line()+
  geom_point()+
  geom_smooth(method = lm, se = T, size = 0.5, linetype = 1, alpha = .5)+
  labs(title = "Pil paesi esportatori di petrolio") #di default utilizza la formula = 'y ~ x'

#iran, iraq e saudi arabia crollo pil dal 79, perche?


#come il tasso di risparmio cambia in relazione al PIL nei paesi esportatori di petrolio prima della crisi/guerra
anni_precrisi<- c("1960","1961","1962","1963","1964","1965","1966","1967","1968","1969","1970","1971","1972","1973","1974","1975","1976","1977","1978","1979")
sr_pil_paesi_exp_precrisi<- SumHes %>%
  filter(opec == "yes") %>% 
  filter(year%in% anni_precrisi) %>% 
  select(year,country,sr,gdp)
view(sr_pil_paesi_exp_precrisi)

regr_sr_gdp_precrisi<- lm(sr ~ gdp,data=sr_pil_paesi_exp_precrisi)
summary(regr_sr_gdp_precrisi)

#grafico relazione sr e pil durante gli anni prima della crisi/guerra
grafico_regr_sr_pil_opec_precrisi <- ggplot(sr_pil_paesi_exp_precrisi, aes(x = gdp, y = sr, color=country)) +
  geom_point() +  
  geom_smooth(method = "lm") +  # Linea di regressione
  labs(
    x = "PIL",
    y = "Tasso di Risparmio",
    title = "Regressione del Tasso di Risparmio sul PIL"
  )



#sr e pil durante gli anni della crisi 
anni_crisi<- c("1979","1980","1981","1982","1983","1984","1985")
sr_pil_paesi_exp_crisi<- SumHes %>%
  filter(opec == "yes") %>% 
  filter(year%in% anni_crisi) %>% 
  select(year,country,sr,gdp)

regr_sr_gdp_crisi<- lm(sr ~ gdp,data=sr_pil_paesi_exp_crisi)
summary(regr_sr_gdp_crisi)
#come il tasso di risparmio cambia in relazione al PIL nei paesi esportatori di petrolio durante gli anni della crisi/guerra (relazione pil e sr durante anni della crisi/guerra)
grafico_regressione <- ggplot(sr_pil_paesi_exp_crisi, aes(x = sr, y = gdp, color = country)) +
  geom_point() +                 
  geom_smooth(method = "lm", se = T) + 
  labs(title = "Regressione tra sr e gdp nei paesi OPEC 1979-1985)",
       x = "Indice SR", y = "PIL (GDP)")


# Generale ----------------------------------------------------------------



#Regressione tasso di risparmio sul pil:il paese con un PIL medio più elevato tende a risparmiare di più?

install.packages("plotly")
library(plotly)


# Calcola la media del PIL per ciascun paese
media_pil_paese <- SumHes %>%
  group_by(country) %>%
  summarize(media_pil = mean(gdp, na.rm = TRUE))

# Trova il paese con il PIL medio più alto
paese_pil_piu_alto <- media_pil_paese %>%
  filter(media_pil == max(media_pil))

# Visualizza il paese con il PIL medio più alto
paese_pil_piu_alto#è l'USA

#analizziamo il pil e il sr dell'USA
pil_sr_USA<- SumHes %>%
  filter(country == "U.S.A.") %>% 
  select(country,year,gdp,sr)
view(pil_sr_USA)

regr_sr_gdp_usa<- lm(sr ~ gdp,data=pil_sr_USA)
summary(regr_sr_gdp_usa)

#come il tasso di risparmio cambia in relazione al PIL 
grafico_regr_sr_pil_usa <- ggplot(pil_sr_USA, aes(x = gdp, y = sr)) +
  geom_point() +  # Punti di dispersione
  geom_smooth(method = "lm") +  # Linea di regressione
  labs(
    x = "PIL",
    y = "Tasso di Risparmio",
    title = "Regressione del Tasso di Risparmio sul PIL"
  )

