library(dplyr) # process data
library(tidyr) # process data
library(broom) # tidy output of regression models
library(ggplot2) # graph
library(stringr) # text manipulation
library(purrr) # work with mulutiple objects
library(cluster) # for factor analysis
library(factoextra) # for the visualisation of factor analysis
library(janitor) # cross tabulation
library(forcats) # working with factor variables
library(did) # difference in differences estimation
library(CGPfunctions) # for slopegraphs
library(gridExtra) # for placing plots side by side
library(patchwork) # for placing plots side by side
# library(cowplot) # for placing plots side by side
library(viridis) # color blind color palette
# library(ggthemes) # includes color blind palette
library(RColorBrewer) # includes color blilnd palette
library(plm) # panel regression
library(lfe) # Linear Group Fixed Effects
library(dynpanel)
library(treemapify)
library(lubridate) # work with time series
# library(timetk) # seasonal decomposition of grouped data
library(seasonal)
library(strucchange)
library(changepoint)
library(ggfortify) # needed to plot time series
library(ggpmisc)
library(zoo) # work with time series
library(ggalt) # for dumbbel plots and encircling
library(superheat) # for time series heatmaps
library(htmlTable) # output tables
library(tangram) # grammar of tables  
library(tempdisagg) # temporal disaggregation - annual to quarterly
library(imputeTS) # imputation of missing data
# library(Amelia) # imputation of missing data
# library(vars) # vector autoregression models
# library(panelvar) # panel vector autoregression models
library(tseries) 
library(astsa)
library(timetk)
# library(forecast)
library(lmtest)
library(car)
library(orcutt) # Cochrane-Orcutt procedure to solve first order autocorrelation problems using an iterative method
library(prais) # Prais-Winsten estimator for models with strictly exogenous regressors and AR(1) serial correlation of the errors
library(urca)
library(dynlm) # dynamic models
library(ARDL) # autoregressive distributed lag models
library(knitr) # publishing data
library(glue) # for pasting strings
# library(modelr)
library(qqplotr) # Q-Q plots with ggplot
library(sf)
library(sp)
library(ggmap)
library(rgdal)
library(geojsonio)
library(corrr)

library(sjPlot) # за визуализация на регресионни таблици
library(sjmisc) # за визуализация на регресионни таблици
library(sjlabelled) # за визуализация на регресионни таблици
library(texreg)
library(stargazer)

# rm(list=ls())

# format(Sys.Date(), format = "%Y-%b-%d")

# sessionInfo()

# Sys.setlocale(locale = "C.UTF-8")

# Sys.setlocale(locale = "bg_BG.utf8")

# Ако е само времето

# Промяна на LC_TiME=C.UTF-8

# Sys.setlocale("LC_TIME", "bg_BG.utf8") # some Linux versions

# https://stackoverflow.com/questions/39340185/how-to-set-the-default-language-of-date-in-r

# slopegraphs
# https://cran.r-project.org/web/packages/CGPfunctions/vignettes/Using-newggslopegraph.html

# cluster analysis
# https://uc-r.github.io/kmeans_clustering

# difference in differences
# https://www.rdocumentation.org/packages/did/versions/1.2.2
# https://bookdown.org/ccolonescu/RPoE4/indvars.html#the-difference-in-differences-estimator
# https://dhicks.github.io/2018-10-10-did/
# https://www.princeton.edu/~otorres/DID101R.pdf

# Chow Test
# https://bookdown.org/ccolonescu/RPoE4/indvars.html#comparing-two-regressions-the-chow-test

# Тримесечни данни БВП - ПРОИЗВОДСТВЕН МЕТОД - НАЦИОНАЛНО НИВО
# gdpquarterly.csv

# НАСЕЛЕНИЕ ПОД, ВЪВ И НАД ТРУДОСПОБНА ВЪЗРАСТ ПО ПОЛ, ОБЛАСТИ, ОБЩИНИ И МЕСТОЖИВЕЕНЕ
# nasNUTS3trudosposobna.csv
# nasOBSHTINItrudosposobna.csv

# Многоотраслова статистика - ОСНОВНИ ПОКАЗАТЕЛИ, ХАРАКТЕРИЗИРАЩИ ДЕМОГРАФСКОТО, СОЦИАЛНОТО И ИКОНОМИЧЕСКО РАЗВИТИЕ НА ОБЛАСТИТЕ
# oblasti-annual.csv

# НАЕТИ ЛИЦА ПО ТРУДОВО И СЛУЖЕБНО ПРАВООТНОШЕНИЕ ПО ИКОНОМИЧЕСКИ ДЕЙНОСТИ (А21) И СТАТИСТИЧЕСКИ РАЙОНИ И ОБЛАСТИ(2008-2018Г.)НАЕТИ ЛИЦА ПО ТРУДОВО И СЛУЖЕБНО ПРАВООТНОШЕНИЕ ПО ИКОНОМИЧЕСКИ ДЕЙНОСТИ (А21) И СТАТИСТИЧЕСКИ РАЙОНИ И ОБЛАСТИ(2008-2018Г.)
# godishniNUTS3KID21naeti.csv

# СРЕДНА БРУТНА ЗАПЛАТА НА НАЕТИТЕ ЛИЦА ПО ТРУДОВО И СЛУЖЕБНО ПРАВООТНОШЕНИЕ ПО ИКОНОМИЧЕСКИ ДЕЙНОСТИ (А21) И СТАТИСТИЧЕСКИ РАЙОНИ И ОБЛАСТИ(2008-2018Г.)
# godishniNUTS3KID21zaplata.csv

# НАЕТИ ЛИЦА ПО ТРУДОВО И СЛУЖЕБНО ПРАВООТНОШЕНИЕ ПО ИКОНОМИЧЕСКИ ДЕЙНОСТИ (A38) И КЛАСОВЕ ПРОФЕСИИ(2008-2018Г.)
# godishniKID38iprofesiinaeti.csv

# СРЕДНА БРУТНА ЗАПЛАТА НА НАЕТИТЕ ЛИЦА ПО ТРУДОВО И СЛУЖЕБНО ПРАВООТНОШЕНИЕ ПО ИКОНОМИЧЕСКИ ДЕЙНОСТИ (A38) И КЛАСОВЕ ПРОФЕСИИ(2008-2018Г.)
# godishniKID38iprofesiizaplata.csv

# ТРИМЕСЕЧНИ ДАННИ - ЗАЕТИ ЛИЦА ПО ПОЛ, СТАТИСТИЧЕСКИ РАЙОНИ И ПО ОБЛАСТИ
# zaetiNUTS3quarterly.csv

# СПИСЪЧЕН БРОЙ В КРАЯ НА МЕСЕЦА НА НАЕТИТЕ ЛИЦА ПО ТРУДОВО И СЛУЖЕБНО ПРАВООТНОШЕНИЕ ПО ИКОНОМИЧЕСКИ ДЕЙНОСТИ (А21)(2008-2019Г.)
# mesechniKID21naeti.csv

# МЕСЕЧНИ ДАННИ - СРЕДНА БРУТНА МЕСЕЧНА ЗАПЛАТА НА НАЕТИТЕ ЛИЦА ПО ТРУДОВО И СЛУЖЕБНО ПРАВООТНОШЕНИЕ ПО ИКОНОМИЧЕСКИ ДЕЙНОСТИ (А21)(2008-2019Г.)
# mesechniKID21zaplata.csv

# НАЕТИ ЛИЦА СПОРЕД ПРОДЪЛЖИТЕЛНОСТТА НА РАБОТНОТО ВРЕМЕ (ПЪЛНО, НЕПЪЛНО)
# naetipylnonepylnoquarterly.csv

# НАЕТИ ЛИЦА ПО ВИД НА ДОГОВОРА С РАБОТОДАТЕЛЯ
# naetibezdogoworquarterly.csv

# НАЕТИ ЛИЦА СПОРЕД ВИДА НА РАБОТАТА (ПОСТОЯННА, ВРЕМЕННА)
# naetiwremennaquarterly.csv

# ОСНОВНИ ИКОНОМИЧЕСКИ ПОКАЗАТЕЛИ ПО СТРУКТУРНА БИЗНЕС СТАТИСТИКА, ПО СЕКТОРИ И ПО СТАТИСТИЧЕСКИ РАЙОНИ
# strukstatpredpr.csv

# РАЗПРЕДЕЛЕНИЕ НА ФАМИЛНИЯ БИЗНЕС ПО СТАТИСТИЧЕСКИ ОБЛАСТИ (NUTS 3) И ПО СТАТИСТИЧЕСКИ РАЙОНИ (NUTS 2)
# familnipredprregioni.csv

# РАБОТНИ МЕСТА И КОЕФИЦИЕНТ НА СВОБОДНИТЕ РАБОТНИ МЕСТА КЪМ КРАЯ НА ТРИМЕСЕЧИЕТО (2008-2019Г.)
# swrabmest.csv

# КОЕФИЦИЕНТИ НА БЕЗРАБОТИЦА ПО СТАТИСТИЧЕСКИ РАЙОНИ И ПОЛ
# koefbezr.csv


### Графики - монополист и монопсон

econplots.df <- data.frame(kolich = c(1:100),
                           cena = c(1:100))

gr_perfcomp <-
  econplots.df %>% 
  ggplot(aes(x=kolich,y=cena)) + xlim(-10, 100) + ylim(-10, 100) +
  geom_segment(x=0,y=0,xend=100,yend=0) + # xaxis
  geom_segment(x=0,y=0,xend=0,yend=100) + # yaxis
  geom_segment(x=10,y=90,xend=90,yend=10) + # VMP = MRP = D
  geom_segment(x=0,y=50,xend=100,yend=50) + # wages
  geom_segment(x=50,y=0,xend=50,yend=50, linetype = "dashed") +
  annotate("text", 
           x = c(90,-5,50,85,25,85), 
           y = c(5,50,-5,55,95,-5),
           label = c("D[L]", "w","L","MFC[L]==S[L]",
                     "VMP[L]==MRP[L]","служители"), parse = T) +
    annotate("text", x=-5,y=90, label="заплата", angle=90) +
  theme_bw() + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  ggtitle("А) Съвършен конкурент")


econplots.df %>% 
  mutate(vmp = 100 - kolich,
         mrp = 100 - 2*kolich, 
         wage = 50) %>% 
  filter(kolich %in% c(10,45,90))

gr_monopol <-
  econplots.df %>% 
  ggplot(aes(x=kolich,y=cena)) + xlim(-10, 100) + ylim(-10, 100) +
  geom_segment(x=0,y=0,xend=100,yend=0) + # xaxis
  geom_segment(x=0,y=0,xend=0,yend=100) + # yaxis
  geom_segment(x=10,y=90,xend=90,yend=10) + # VMP
  geom_segment(x=10,y=80,xend=45,yend=10) + # MRP
  geom_segment(x=0,y=50,xend=100,yend=50) + # wages
  geom_segment(x=25,y=0,xend=25,yend=75, linetype = "dashed") +
  geom_segment(x=25,y=75,xend=0,yend=75, linetype = "dashed") +
  geom_segment(x=50,y=0,xend=50,yend=50, linetype = "dashed") +
  annotate("text", 
           x = c(45,90,25,-5,-5,50,85,85), 
           y = c(5,5,-5,50,75,-5,55,-5),
           label = c("MRP[L]==D[L]","VMP[L]","L[0]",
                     "w[0]","w[1]","L[1]","MFC[L]==S[L]","служители"), parse = T) +
  annotate("text", x=-5,y=90, label="заплата", angle=90) +
  theme_bw() + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  ggtitle("Б) Монополист на прод. пазар")

### ГРАФИКА

(gr_perfcomp | gr_monopol)

ggsave("graphs/1_pc_i_monopol.png", type = "cairo-png", width = 8, height = 4)
ggsave("graphs/1_pc_i_monopol.pdf", device=cairo_pdf, width = 8, height = 4)
ggsave("graphs/1_pc_i_monopol.svg", device = svg, width=8, height=4)

econplots.df %>% 
  mutate(predlagane = kolich,
         mfcl = 2*kolich) %>% 
  filter(kolich %in% c(10,45,50))

gr_monopsony_basic <- 
  econplots.df %>% 
  ggplot(aes(x=kolich,y=cena)) + xlim(-10, 100) + ylim(-10, 100) +
  geom_segment(x=0,y=0,xend=100,yend=0) + # xaxis
  geom_segment(x=0,y=0,xend=0,yend=100) + # yaxis
  geom_segment(x=10,y=10,xend=90,yend=90) + # S_L
  geom_segment(x=10,y=20,xend=45,yend=90) + # MFC_L
  geom_segment(x=10,y=90,xend=90,yend=10) + # D_L
  geom_segment(x=100/3,y=0,xend=100/3,yend=200/3, linetype = "dashed") +
  geom_segment(x=100/3,y=200/3,xend=0,yend=200/3, linetype = "dashed") +
  geom_segment(x=100/3,y=100/3,xend=0,yend=100/3, linetype = "dashed") +
  annotate("text", x=-5,y=90, label="заплата", angle=90) +
  annotate("text", x=85,y=-5, label="служители") +
  annotate("text", 
           c(100/3, -5, -5, 90, 90, 45),
           c(-5, 100/3, 200/3, 5, 95, 95),
           label = c("L[0]","w[0]", "w*minute", "D==MRP[L]", "S[L]", "MFC[L]"),
           parse = T) +
    theme_bw() + 
    theme(axis.title.x=element_blank(), 
          axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank())

gr_monopsony_basic_eq <-
  gr_monopsony_basic +
  geom_segment(x=0,y=50,xend=50,yend=50, linetype = "dashed", color = "gray") +
  geom_segment(x=50,y=0,xend=50,yend=50, linetype = "dashed", color = "gray") +
  annotate("text", 
           c(50, -5),
           c(-5, 50),
           label = c("L[c]", "w[c]"),
           parse = T)

gr_monоpsony_mw_positive <- 
  gr_monopsony_basic_eq +
  geom_segment(x=40,y=0,xend=40,yend=60, linetype = "dotted") +
  geom_segment(x=0,xend=40,y=60,yend=60, linetype = "dotted") +
  annotate("text", 
           c(-5, 40),
           c(60, -5),
           label = c("w[1]", "L[1]"),
           parse = T) +
  ggtitle("А) Положителен ефект")

gr_monоpsony_mw_negative <- 
  gr_monopsony_basic_eq +
  geom_segment(x=25,y=0,xend=25,yend=75, linetype = "dotted") +
  geom_segment(x=0,xend=25,y=75,yend=75, linetype = "dotted") +
  annotate("text", 
           c(-5, 25),
           c(75, -5),
           label = c("w[1]", "L[1]"),
           parse = T) +
  ggtitle("Б) Отрицателен ефект")

### ГРАФИКА 
gr_monоpsony_mw_positive | gr_monоpsony_mw_negative

ggsave("graphs/2_monopson.png", type = "cairo-png", width = 8, height = 4)
ggsave("graphs/2_monopson.pdf", device=cairo_pdf, width = 8, height = 4)
ggsave("graphs/2_monopson.svg", device = svg, width=8, height=4)

gr_oligopsony_basic <-
  econplots.df %>% 
  mutate(diagonal = kolich) %>% 
  ggplot(aes(x=kolich,y=cena)) + 
  geom_segment(x=0,y=0,xend=100,yend=0) + # xaxis
  geom_segment(x=0,y=0,xend=0,yend=100) + # yaxis
  geom_segment(x=0,xend=100,y=0,yend=100, linetype = "dashed", color="gray") + # 45% линия
  geom_segment(x=40,xend=90,y=0,yend=100) +
  annotate("text", 
           c(-5, 40, 85),
           c(-5, -5, 100),
           label = c("0", "w[a0]", "R[a]"),
           parse = T) +
  theme_bw() + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  xlab("Заплата във фирма A") +
  ylab("Заплата във фирма B")

gr_oligopsony_identical <- 
  gr_oligopsony_basic +
  geom_segment(x=0,xend=100,y=40,yend=90) + # Rb
  geom_segment(x=80,xend=80,y=0,yend=80, linetype = "dotted") + #wa1
  geom_segment(x=0,xend=80,y=80,yend=80, linetype = "dotted") + #wb1
  geom_segment(x=0,xend=65,y=50,yend=50, linetype = "dotted") + #wb'
  geom_segment(x=65,xend=65,y=0,yend=72.5, linetype = "dotted") + #wa'
  geom_segment(x=0,xend=65,y=72.5,yend=72.5, linetype = "dotted") + #wa''
  annotate("text",
           c(100, -5, 80, -5, -5, 65, -5),
           c(85, 40, -5, 80, 50, -5, 72.5),
           label = c("R[b]", "w[b0]", "w[a1]", 
                     "w[b1]", "w[b]*minute", "w[a]*minute", "w[b]*second"),
           parse = T) +
  ggtitle("A) При идентична производителност")


gr_oligopsony_asymetrical <-
  gr_oligopsony_basic +
  geom_segment(x=0,xend=100,y=10,yend=60) +  # Rb
  geom_segment(x=60,xend=60,y=0,yend=40, linetype="dotted") + # wa1
  geom_segment(x=0,xend=60,y=40,yend=40, linetype="dotted") + # wb1
  geom_segment(x=55,xend=55,y=0,yend=55, linetype="dotted") + #wma
  geom_segment(x=0,xend=135/2,y=55,yend=55, linetype="dotted") + #wmb
  geom_segment(x=67.5,xend=67.5,y=0,yend=55, linetype="dotted") + #wa'
  geom_segment(x=0,xend=67.5,y=43.75,yend=43.75, linetype="dotted") +
  annotate("text",
           c(-5,-5,-5,-5),
           c(10,40,43.74,55),
           label=c("w[b0]","w[b1]","w[b]*minute","w[m]"),
           parse=T) +
  annotate("text",
           c(55,60,67.5),
           c(-5,-5,-5),
           label=c("w[m]","w[a1]","w[a]*minute"),
           parse=T) +
  annotate("text",
           c(40,30),
           c(10,20),
           label=c("R[a]","R[b]"),
           parse=T) +
  coord_cartesian(xlim = c(-5,70), ylim = c(-5,70)) +
  ggtitle("Б) При различна производителност")

gr_oligopsony_identical | gr_oligopsony_asymetrical

### ГРАФИКА
# функция на реакция на заплатите в условията на дуопсон

ggsave("graphs/3_duopsony_sym_i_asym.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/3_duopsony_sym_i_asym.png", width=8, height=4, type="cairo-png")
ggsave("graphs/3_duopsony_sym_i_asym.svg", width=8, height=4, device = svg)

econplots.df %>% 
  ggplot(aes(x=kolich,y=cena)) + xlim(-10, 100) + ylim(-10, 100) +
  geom_segment(x=0,y=0,xend=100,yend=0) + # xaxis
  geom_segment(x=0,y=0,xend=0,yend=100) + # yaxis
  geom_segment(x=0,xend=100,y=90,yend=20) + # w0 line
  geom_segment(x=0,xend=100,y=20,yend=90) + # w1 line
  geom_segment(x=0,xend=100,y=80,yend=10) + # w0' line
  geom_segment(x=50,xend=50,y=0,yend=55, linetype = "dotted") +
  # geom_segment(x=0,xend=50,y=55,yend=55, linetype = "dotted") +
  geom_segment(x=42.5,xend=42.5,y=0,yend=50, linetype = "dotted") +
  # geom_segment(x=0,xend=42.5,y=50,yend=50, linetype = "dotted") +
  annotate("text",
           x=c(42.5,-5,-5,-5,0,100),
           y=c(-5,90,80,20,-5,-5),
           label=c('x*minute',
                   "w[0]","w[0]*minute","w[1]","f[0]","f[1]"), parse = T) +
  annotate("text", c(50), c(-5), label = c("x*")) +
  theme_bw() + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  xlab("Местоположение") +
  ylab("Нетна заплата")

### ГРАФИКА

ggsave("graphs/4_wages_and_transport_costs.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/4_wages_and_transport_costs.png", width=8, height=4, type="cairo-png")
ggsave("graphs/4_wages_and_transport_costs.svg", width=8, height=4, device = svg)


# difference in difference plot

econplots.df %>% 
  ggplot(aes(x=kolich,y=cena)) + xlim(-10, 100) + ylim(-10, 100) +
  geom_segment(x=0,y=0,xend=100,yend=0) + # xaxis
  geom_segment(x=0,y=0,xend=0,yend=100) + # yaxis
  geom_segment(x=10,xend=90,y=30,yend=50, color = "darkgray") + # no treatment
  geom_segment(x=10,xend=50,y=50,yend=60) + # treatment, pre-intervention
  geom_segment(x=50,xend=90,y=60,yend=70, 
               linetype = "dotted") + # treatment, post-intervention counterfactual
  geom_segment(x=50,xend=90,y=60,yend=90) + # treatment, post-intervention
  geom_segment(x=50,xend=50,y=0,yend=90, 
               linetype = "dashed", color = "darkgray") + # intervention line +
  annotate("text",
           x=c(50,25,75,95,10,10),
           y=c(95,-5,-5,80,42.5,22.5),
           label=c("Интервенция","Преди","След",
                   "Ефект","третирана\nгрупа","контролна\nгрупа")) +
  annotate("text", x=-5, y=50, angle=-90, label="Резултат") +
  geom_segment(aes(x = 89, y = 72.5, xend = 89, yend = 87.5),
               color = "darkgray", size = 0.3, 
               arrow = arrow(type = "closed", length=unit(0.15,"cm"))) +
  geom_segment(aes(x = 89, y = 87.5, xend = 89, yend = 72.5),
               color = "darkgray", size = 0.3, 
               arrow = arrow(type = "closed", length=unit(0.15,"cm"))) +
  theme_bw() + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  xlab("") +
  ylab("")

### ГРАФИКА

ggsave("graphs/4_did.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/4_did.png", width=8, height=4, type="cairo-png")
ggsave("graphs/4_did.svg", width=8, height=4, device = svg)
  

# Добавяме БВП и растеж, които ще се ползват после за контрол
# Тримесечни данни по цени от 2015 г. и ръст спрямо предното
# тримесечие

bwptrim <- read.csv("data/bwp_trimesechni.csv", sep = ";", header = F)

levels(bwptrim$V2)

bwptrim <- 
  bwptrim %>% 
  filter(str_detect(V2, "2015")) %>% 
  select(periyod = V3, bwpc2015 = V5) %>% 
  mutate(periyod = as.yearqtr(periyod),
         bwpc2015 = as.numeric(as.character(bwpc2015))) %>% 
  mutate(bwprastev = 
           (bwpc2015 - dplyr::lag(bwpc2015))/dplyr::lag(bwpc2015)*100)

bwptrim

bwptrimizgl <- read.csv("data/bwp_i_rastev_trimesechni_izgladeni.csv", 
                        header = F, sep = ";")

bwptrimizgl <- 
  bwptrimizgl %>% 
  filter(str_detect(V2, "2015|предходното")) %>% 
  mutate(V2 = as.character(V2)) %>% 
  mutate(pokazatel = 
           case_when(str_detect(V2, "2015") ~ "bwpc2015izgl",
                     TRUE ~ "bwprastevizgl")) %>% 
  select(periyod = V3, pokazatel, stojnost = V5) %>% 
  mutate(stojnost = as.numeric(as.character(stojnost)),
         periyod = as.yearqtr(periyod)) %>% 
  spread(key = pokazatel, value = stojnost)

bwptrim <- 
  bwptrim %>% inner_join(bwptrimizgl)


### СЕКЦИЯ
###
### Годишни по дейности и области


naeti21nuts3.df <- read.csv("data/godishniNUTS3KID21naeti.csv", sep = ";", header = F)
zaplata21nuts3.df <- read.csv("data/godishniNUTS3KID21zaplata.csv", sep = ";", header = F)
naselenienuts3.df <- read.csv("data/nasNUTS3trudosposobna.csv", sep = ";", header = F)

head(naeti21nuts3.df)
head(zaplata21nuts3.df)
head(naselenienuts3.df)
str(naselenienuts3.df)

nastrsp.df <- # Население в трудоспособна възраст
  naselenienuts3.df %>% 
  select(oblast = V1, wyzrast = V2, mqsto = V4, pol = V5, godina = V6, broj = V7) %>% 
  filter(wyzrast == "В трудоспособна възраст", mqsto == "Общо", pol == "Общо") %>% 
  select(oblast, godina, broj)

nastrsp.df


naeti21nuts3.df <- 
  naeti21nuts3.df %>% 
  select(oblast = V1, sektor = V2, godina = V3, brojn = V5)

zaplata21nuts3.df <-
  zaplata21nuts3.df %>% 
  select(oblast = V1, sektor = V2, godina = V3, godzapl = V5)

nzn3.df <- 
  naeti21nuts3.df %>% 
  inner_join(zaplata21nuts3.df) %>% 
  inner_join(nastrsp.df)

# 1.. конфиденциални данни
# 2- няма случай

# Наети и заплата по дейности КИД 21 и 
# области с трудоспособно население, 
# без конфиденциални данни и нулеви бройки

nzn3w.df <- 
  nzn3.df %>% 
  mutate_at(vars(brojn, godzapl), na_if, "..") %>% 
  mutate_at(vars(brojn, godzapl), na_if, "-") %>% 
  mutate_at(vars(brojn, godzapl), as.character) %>% 
  mutate_at(vars(brojn,godzapl), as.numeric) %>% 
  na.omit %>% 
  mutate(mrz = case_when(godina == 2008 ~ 220,
                                    godina == 2009 ~ 240,
                                    godina == 2010 ~ 240,
                                    godina == 2011 ~ 270,
                                    godina == 2012 ~ 290,
                                    godina == 2013 ~ 310,
                                    godina == 2014 ~ 340,
                                    godina == 2015 ~ 360,
                                    godina == 2016 ~ 420,
                                    godina == 2017 ~ 460,
                                    godina == 2018 ~ 510
                                    )) %>% 
  mutate(meszapl = godzapl/12,
                   mrzproc = mrz/meszapl * 100,
                   mrzproc = round(mrzproc, 1),
         naetikoef = brojn/broj * 100,
         naetikoef = round(naetikoef, 2))

# Добавено на 2021-06-11
nzn3w.df <-
  nzn3w.df %>% 
  separate(obl_sek, c("oblast","sektor"), sep="_")

nzn3w.df

  nzn3w.df %>% tabyl(sektor, oblast)

# Наблюдения област-сектор-година с над 90% МРЗ спрямо заплатата
# групиране по сектор

nzn3w.df %>% filter(mrzproc > 90)
  
nzn3w.df %>% filter(mrzproc > 90) %>% 
  tabyl(sektor) %>% arrange(n)

# Наблюдения област-сектор-година с над 90% МРЗ спрямо заплатата
# групиране по области
nzn3w.df %>% filter(mrzproc > 90) %>% 
  tabyl(oblast) %>% arrange(n)

    # Наблюдения област-сектор-година с над 90% МРЗ спрямо заплатата
# групиране по години
nzn3w.df %>% filter(mrzproc > 90) %>% 
  tabyl(godina) %>% arrange(n)

nzn3w08i18.df <- nzn3w.df %>% 
  filter(godina %in% c(2008,2018),
         sektor == 0) %>% 
  mutate(godina = as.factor(godina))

# Графично представяне на изменението на заплатата като съотношение спрямо МРЗ
newggslopegraph(nzn3w08i18.df,godina,mrzproc,oblast)
# Във всички области има увеличение в края спрямо началото на периода
# с изключение на Благоевград, Бургас и Варна

nzn3w08i18.df %>% group_by(oblast) %>% 
  mutate(promqnakoefnaeti = diff(naetikoef),
         promqnamrz = diff(mrzproc)) %>% 
  select(oblast, promqnakoefnaeti, promqnamrz) %>% 
  unique %>% arrange(-promqnakoefnaeti)

# ГРАФИКА

nzn3w08i18.df %>% group_by(godina) %>% 
  mutate(poziciq = rank(mrzproc)) %>% 
  ggplot() + 
  geom_col(aes(x = oblast, y = mrzproc, fill = godina), position = "dodge") +
  geom_point(aes(x = oblast, y = naetikoef, 
                 shape = godina)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(begin = 0.3, end = 0.8, name = "МРЗ/СРЗ, %") +
  scale_shape_manual(name = "Коеф. наети", values = c(1,2)) + 
  xlab("") + ylab("проценти")

ggsave("graphs/6_mrzinaeti_2008_2018_oblasti.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/6_mrzinaeti_2008_2018_oblasti.png", width=8, height=4, type="cairo-png")
ggsave("graphs/6_mrzinaeti_2008_2018_oblasti.svg", width=8, height=4, device = svg)

nzn3forplot.df <-
  nzn3w08i18.df %>% filter(godina == 2008) %>% 
  mutate(rangnaeti2008 = rank(naetikoef),
         rangmrzproc2008 = rank(mrzproc)) %>% 
  select(oblast, rangnaeti2008, rangmrzproc2008) %>% 
  right_join(nzn3w08i18.df)

nzn3forplot.df

nzn3forplot.df %>% group_by(godina) %>% 
  mutate(poziciq = rank(naetikoef)) %>% 
  ggplot() + 
  geom_col(aes(x = reorder(oblast, as.numeric(rangmrzproc2008)), y = mrzproc, fill = godina), position = "dodge") +
  geom_point(aes(x = oblast, y = naetikoef, 
                 shape = godina)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(begin = 0.3, end = 0.8, name = "МРЗ/СРЗ, %") +
  scale_shape_manual(name = "Коеф. наети", values = c(1,2)) + 
  xlab("") + ylab("проценти")

ggsave("graphs/6_mrzinaeti_2008_2018_oblasti_ordered.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/6_mrzinaeti_2008_2018_oblasti_ordered.png", width=8, height=4, type="cairo-png")
ggsave("graphs/6_mrzinaeti_2008_2018_oblasti_ordered.svg", width=8, height=4, device = svg)

nzn3forplot.df %>% filter(godina == 2008) %>% 
  select(mrzproc, naetikoef) %>% correlate

nzn3forplot.df %>% filter(godina == 2018) %>% 
  select(mrzproc, naetikoef) %>% correlate

# temp %>% group_by(oblast) %>% 
#   mutate(mrzprprom = mrzproc - dplyr::lag(mrzproc,1)) %>% 
#   arrange(mrzprprom) %>% View

# Подреждаме областите по сектори според това колко голямо 
# е съотношението МРЗ/З
# Колко от секторите са в топ 3 за съответната област
# Секторите G, S, N, I водят с много пред другите и заемат над 90%

nzn3w.df %>% group_by(oblast,sektor) %>% 
  summarise(mzsredno = mean(mrzproc)) %>% 
  group_by(oblast) %>% top_n(3, mzsredno) %>% 
  tabyl(sektor) %>% arrange(n) %>% kable

# Същото, но с най-ниски съотношения МРЗ/З
# Това са J, B, O, D, K
nzn3w.df %>% group_by(oblast,sektor) %>% 
  summarise(mzsredno = mean(mrzproc)) %>% 
  group_by(oblast) %>% top_n(-3, mzsredno) %>% 
  tabyl(sektor) %>% arrange(n) %>% kable

nzn3w.df

# Графично представяне
# МРЗ/з по области

table(nzn3w.df$godina)

nzn3w.df %>% filter(sektor == 0) %>% 
ggplot(aes(x = oblast, y = mrzproc)) + geom_boxplot() +
  xlab("") + ylab("МРЗ/СРЗ, %") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("graphs/5_oblasti_whisker.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/5_oblasti_whisker.png", width=8, height=4, type="cairo-png")
ggsave("graphs/5_oblasti_whisker.svg", width=8, height=4, device = svg)

# ГРАФИКА 

# МРЗ/з по сектори

nzn3w.df %>% filter(sektor != 0) %>% 
  ggplot(aes(x = sektor, y = mrzproc)) + geom_boxplot() +
  xlab("") + ylab("МРЗ/СРЗ, %") + theme_bw()

ggsave("graphs/7_mrz_kid_21.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/7_mrz_kid_21.png", width=8, height=4, type="cairo-png")
ggsave("graphs/7_mrz_kid_21.svg", width=8, height=4, device = svg)

# Наети/тр.сп. нас. (коефициент наети по области)
nzn3w.df %>% filter(sektor == 0) %>% 
  ggplot(aes(x = oblast, y = naetikoef)) + geom_boxplot() +
  xlab("") + ylab("Коеф. наети, %") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Текстови етикети, показващи връзката между МРЗ/з и наетите
# по области (общо за всички сектори и за годините 2008 и 2018)
# София - град не е включена, защото стойностите се различават
# много от всички останали и се отразява на мащаба на графиката

nzn3w.df

#nzn3w.df %>% 
nzn3w08i18.df %>% 
#  filter(sektor ==0, godina %in% c(2008,2018), oblast != "SOF") %>% 
  filter(oblast != "SOF") %>% # да се махне после ако се сорсва файла
  mutate(godina = as.factor(godina)) %>% 
  ggplot(aes(x=mrzproc, y = naetikoef)) + 
  geom_point(aes(color = godina), size = 0.3) +
  geom_text(aes(label = oblast, color = godina), 
            size = 2, nudge_x = 0.5, nudge_y = 0.5) +
  geom_line(aes(group = oblast), color = "gray", linetype = "dotted") +
  theme_bw() + 
  scale_color_viridis_d(begin = 0.1, end = 0.7) +
  labs(color = "Година") +
  xlab("МРЗ/СРЗ, %") +
  ylab("Коефициент наети")

ggsave("graphs/6_mrz_naeti_oblast_text.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/6_mrz_naeti_oblast_text.png", width=8, height=4, type="cairo-png")
ggsave("graphs/6_mrz_naeti_oblast_text.svg", width=8, height=4, device = svg)

# Защо е толкова висок коефициентът на София?
# Подценява населението или има приходящи от други области, които работят?

# Изчисляване на процентно изменение в броя на наетите

# Процентна промяна при наетите,
# контролирайки за промяна в населението

nzn3w.df <- 
  nzn3w.df %>% 
  arrange(oblast,sektor,godina) %>% 
  group_by(oblast,sektor) %>% 
  mutate(naetiprprom = # без контрол за промяна в населението
           (brojn - dplyr::lag(brojn, 1))/dplyr::lag(brojn,1)*100) %>% 
  mutate(naetiprpromadj = # контролирайки за промяна в населението, т.е.
           (brojn - dplyr::lag(brojn, 1))/dplyr::lag(brojn,1)*100 - # % пром. наети
           (broj - dplyr::lag(broj, 1))/dplyr::lag(broj,1)*100) %>% # % пром. население
  mutate(mrzprprom = mrzproc - dplyr::lag(mrzproc,1))

nzn3w.df

# Графично представяне на процентните наети при промените
# за Видин и Варна за 2009, 2014 и 2018 г.

nzn3w.df %>% 
  filter(sektor !=0, godina %in% c(2009,2014,2018), oblast %in% c("VAR","VID")) %>% 
  mutate(godina = as.factor(godina)) %>% 
  ggplot(aes(x=mrzproc, y = naetiprpromadj, label = sektor, color = oblast)) + 
  geom_label() +
  theme_bw() + 
  scale_color_viridis_d(begin = 0.1, end = 0.7) +
  labs(color = "Област") +
  xlab("Отношение на МРЗ към СРЗ, %") +
  ylab("Промяна наети") +
  facet_wrap(godina ~ .)

# Има голяма волатилност при промените в броя на наетите в относително
# изражение за някои от отраслите, както е видно от графиката със сектора N
# в област Видин. 
# Разглеждаме тази волатилност

# Изчисляване на размаха

nzn3w.df %>% select(oblast, sektor, godina, 
                    naetiprprom, naetiprpromadj, brojn) %>% 
  group_by(sektor) %>% 
  summarise(minst = min(naetiprprom, na.rm = T),
            maxst = max(naetiprprom, na.rm = T)) %>% 
  mutate(razmah = maxst - minst) %>% 
  arrange(razmah)

nzn3w.df

nzn3w.df %>% select(oblast, sektor, godina, 
                    naetiprprom, naetiprpromadj, brojn) %>% 
  filter(brojn > 50) %>% 
  group_by(sektor) %>% 
  summarise(razmah = diff(range(naetiprprom, na.rm = T))) %>% 
  arrange(razmah)

# Разделяме на групи от по 5 перцентила.
# Колко са граничните стойности за всеки от тях?

nzn3w.df %>% 
  ungroup %>% 
  select(oblast, sektor, godina, 
         naetiprprom, naetiprpromadj, brojn) %>% 
  filter(godina > 2008, brojn > 50, sektor !=0) %>% 
  mutate(volatilnigrupi = ntile(naetiprpromadj, 20)) %>% 
  group_by(volatilnigrupi) %>% 
  arrange(volatilnigrupi) %>% 
  top_n(3, naetiprpromadj)

# Кои са средните стойности?

nzn3w.df %>% 
  ungroup %>% 
  select(oblast, sektor, godina, 
         naetiprprom, naetiprpromadj, brojn) %>% 
  filter(godina > 2008, brojn > 50, sektor !=0) %>% 
  mutate(volatilnigrupi = ntile(naetiprpromadj, 20)) %>% 
  group_by(volatilnigrupi) %>% 
  summarise(sredno = mean(naetiprpromadj)) %>% 
  arrange(sredno) %>% kable

# Разпределение на наблюденията по сектори и перцентилни групи

nzn3w.df %>% 
  ungroup %>% 
  select(oblast, sektor, godina, 
         naetiprprom, naetiprpromadj, brojn) %>% 
  filter(godina > 2008, brojn > 50, sektor !=0) %>% 
  mutate(volatilnigrupi = ntile(naetiprpromadj, 20)) %>% 
  group_by(volatilnigrupi) %>% 
  tabyl(volatilnigrupi, sektor) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>% View

nzn3w.df %>% 
  ungroup %>% 
  group_by(sektor) %>% 
  summarise(mrzsr = mean(mrzproc)) %>% 
  arrange(mrzsr)

# При строителството тази волатилност до голяма степен се дължи на 
# стопанския цикъл. Концентрацията в първата перцентилна група до 
# голяма степен се дължи на 2009 и особено на 2010 г.

nzn3w.df %>% 
  ungroup %>% 
  select(oblast, sektor, godina, 
         naetiprprom, naetiprpromadj, brojn) %>% 
  filter(godina > 2008, brojn > 50, sektor !=0) %>% 
  mutate(volatilnigrupi = ntile(naetiprpromadj, 20)) %>%
  mutate(volatilnigrupi = as.factor(volatilnigrupi)) %>% 
  # filter(sektor == "F") %>% 
  tabyl(godina, volatilnigrupi) %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 0)

# Създаване на нова таблица, която съдържа само секторите C, G, H, I
# Това намалява волатилността в промяната при наетите.

cghi.df <- 
  nzn3w.df %>% ungroup %>% 
  filter(sektor %in% c("C","G","H","I")) %>% 
  mutate(sektor = droplevels(sektor))


# Колко са наетите в тези сектори общо
cghi.df %>% 
  group_by(godina) %>% 
  summarise(obshtonaeti = sum(brojn)) 

# Колко са наетите в тези сектори за всеки конкретен сектор (средно за периода)
# Средна заплата по сектори (общо за целия период), лв. и МРЗ/з

cghi.df %>% 
  group_by(sektor,godina) %>% 
  mutate(naetiposektor = sum(brojn)) %>% 
  group_by(sektor) %>% 
  summarise(naetisektorsredno = mean(naetiposektor),
            srzapl = mean(meszapl), srwmrz = mean(mrzproc)) %>% 
  kable(digits = 0)

# Създаваме първата година с индекс 100

cghi.df <- 
  cghi.df %>% group_by(sektor,oblast) %>%
  arrange(oblast,sektor,godina) %>% 
  mutate(ind100meszapl = 100 * meszapl/first(meszapl),
         ind100mrzproc = 100 * mrzproc/first(mrzproc),
         ind100brojn = 100 * brojn/first(brojn))
  

# Средна заплата по сектори и по години 
graph1 <- 
  cghi.df %>% group_by(sektor,godina) %>% 
  summarise(srzapl = mean(ind100meszapl)) %>% 
  ggplot(aes(x=godina, y=srzapl, color = sektor)) + 
  geom_line(show.legend = FALSE) + 
  xlab("") + ylab("Ср. з. (лв.), 2008=100") +
  scale_x_continuous(breaks = seq(2008,2018,2)) +
  theme_bw() +
  scale_color_viridis_d() # използване на цветова гама, която се различава и при черно-бял печат
# scale_color_colorblind()
# scale_color_brewer(palette = "YlOrRd")

graph2 <- 
  cghi.df %>% group_by(sektor,godina) %>% 
  summarise(srwmrz = mean(mrzproc)) %>% 
  ggplot(aes(x=godina, y=srwmrz, color = sektor)) + 
  geom_line() + 
  xlab("") + ylab("МРЗ/СРЗ, %") +
  scale_x_continuous(breaks = seq(2008,2018,2)) + 
  labs(color = "Сектор") + theme_bw() +
# theme(legend.position=c(.85,.25)) +
  theme(legend.position = "none") +
  scale_color_viridis_d() # използване на цветова гама, която се различава и при черно-бял печат

# Как се променя броят на наетите по сектори през този период

graph3 <- 
  cghi.df %>% group_by(sektor,godina) %>% 
  summarise(naetipromposektor = mean(ind100brojn)) %>% 
  ggplot(aes(x=godina, y=naetipromposektor, color = sektor)) + 
  geom_line() + theme_bw() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(2008,2018,2)) +
  xlab("") + ylab("Брой наети, 2008=100") +
  theme(legend.position = "none")


graph4 <- 
  cghi.df %>% group_by(sektor,godina) %>% 
  summarise(naetipromposektor = mean(naetiprpromadj)) %>% 
  ggplot(aes(x=godina, y=naetipromposektor, color = sektor)) + 
  geom_line() + theme_bw() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(2008,2018,2)) +
  xlab("") + ylab("Наети, промяна (%)") +
  labs(color = "Сектор")

glimpse(cghi.df)

# Обща графика - как се променя заплатата в секторите
(graph1 | graph2) / (graph3 | graph4)

ggsave("graphs/7_cghi.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/7_cghi.png", width=8, height=4, type="cairo-png")
ggsave("graphs/7_cghi.svg", width=8, height=4, device = svg)

# Четирите сектора, представени в три различни години и три различни области
cghi.df %>% filter(oblast %in% c("VID","VAR","SOF"),
                  godina %in% c(2009, 2014, 2018)) %>% 
  ggplot(aes(x=mrzproc, y = naetiprpromadj, label = sektor, color = oblast)) + 
  geom_label() + facet_wrap(godina ~ .) + theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.8)

cghi.df %>% 
  filter(godina > 2008) %>% 
  ggplot() + geom_point(aes(x = mrzproc, y = naetiprpromadj, color = sektor)) + 
  facet_wrap(godina ~ ., ncol = 3) +
  theme_bw() + 
  scale_color_viridis_d() +
  theme(legend.position = c(0.65,0.05), legend.direction = "horizontal") +
  labs(color = "Сектор") +
  xlab("Отношение на МРЗ към СРЗ, %") + ylab("Наети, промяна (%)")

cghi.df %>% 
  filter(godina > 2008) %>% 
  ggplot() + geom_line(aes(x = godina, y = naetiprpromadj, color = sektor)) +
  facet_wrap(oblast ~ ., ncol = 4) +
  theme_bw() + 
  scale_color_viridis_d() 

glimpse(cghi.df)

# Създаваме квартилни групи (за всяка година и за всеки сектор)
# според това колко голямо е съотношението между МРЗ и средната заплата

cghi.df <- 
  cghi.df %>%
  group_by(sektor, godina) %>% 
  mutate(grupi = ntile(mrzproc, 4)) %>%
  mutate(grupi = as.factor(grupi)) %>% 
  ungroup

# Правим графика, за да видим дали има различия между 
# промените в заетостта според квартилната група МРЗ/заплата

ggr1 <- 
  cghi.df %>% group_by(grupi,godina) %>%  
  summarise(zaplsrizm = mean(mrzprprom),
            naetiindeks100 = mean(ind100brojn),
            grupasr = mean(mrzproc)) %>% 
  filter(godina > 2007) %>% 
  ggplot() + theme_bw() + scale_color_viridis_d() + xlab("") + labs(color = "Групи\nМРЗ/СРЗ, %")

cghi.df

# Ето я графиката
ghigraph1 <-
  ggr1 + geom_line(aes(x = godina, y = naetiindeks100, color = grupi)) + 
  ylab("Брой наети, 2008=100") +
  scale_x_continuous(breaks = seq(2008,2018,2)) +
  theme(legend.position = "none")

ghigraph2 <- 
  ggr1 + geom_line(aes(x = godina, y = grupasr, color = grupi)) +
  ylab("МРЗ/СРЗ, %") +
  scale_x_continuous(breaks = seq(2008,2018,2))

(ghigraph1 / ghigraph2)

ggsave("graphs/8_kwartili_oblasti_sektori.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/8_kwartili_oblasti_sektori.png", width=8, height=4, type="cairo-png")
ggsave("graphs/8_kwartili_oblasti_sektori.svg", width=8, height=4, device = svg)

# Как квартилните групи се разпределят по области
cghi.df %>% tabyl(oblast,grupi)

# Какви са средните стойности на всяка квартилна група
# относно процентнотно изменение в наетите

cghi.df %>% 
  group_by(oblast) %>% 
  mutate(nasprprom = (broj - dplyr::lag(broj, 1))/dplyr::lag(broj,1)*100) %>% 
  ungroup %>% 
  group_by(grupi) %>%
  filter(godina > 2008) %>% 
  summarise(naetisrprom = mean(naetiprprom),
            nasprprom = mean(nasprprom, na.rm = T), 
            naetisrpromadj = mean(naetiprpromadj),
            srzapl = mean(meszapl), 
            srwmrz = mean(mrzproc),
            srpromzapl = mean(mrzprprom)) %>% 
  kable(digits = 2)

# И като графика

cghi.df %>% filter(godina > 2008) %>% 
  ggplot() + geom_boxplot(aes(x = grupi, y = naetiprprom)) 

cghi.df %>% filter(godina > 2008) %>% 
  ggplot() + geom_boxplot(aes(x = grupi, y = naetiprpromadj))

cghi.df %>% filter(godina > 2008) %>% 
  ggplot() + geom_boxplot(aes(x = grupi, y = mrzprprom))

colnames(cghi.df)
cghi.df %>% group_by(godina, grupi) %>% 
  summarise(srnaetikoef = mean(naetikoef)) %>% 
  ungroup %>% 
  filter(godina %in% c(2008,2018)) %>% 
  mutate(grupi = as.factor(grupi),
         godina = paste0("g",godina)) %>% 
  spread(key = godina, value = srnaetikoef) %>% 
  ggplot(aes(y=grupi, x = g2008, xend = g2018 )) + geom_dumbbell()

# БЕЛЕЖКА!
# ПАНЕЛНА РЕГРЕСИЯ
# ANOVA

# ГОДИШНИ А38 И ПРОФЕСИИ
naeti38.df <- read.csv("data/godishniKID38iprofesiinaeti.csv", sep = ";", head = F)
zaplati38.df <- read.csv("data/godishniKID38iprofesiizaplata.csv", sep = ";", head = F)
kid38 <- read.csv("data/kidA38.csv", sep = "|")
profesii38 <- read.csv("data/profesiiA38.csv", sep = ";")

naeti38.df <- 
  naeti38.df %>% select(sektor = V1, prof = V2, godina = V3, naeti = V5)

zaplati38.df <- 
  zaplati38.df %>% select(sektor = V1, prof = V2, godina = V3, zaplata = V5)

nz38.df <-  
  naeti38.df %>% inner_join(zaplati38.df)

nz38.df <- 
  nz38.df %>% 
  mutate_at(vars(zaplata, naeti), as.character) %>% 
  mutate_at(vars(zaplata, naeti), na_if, "..") %>%
  mutate_at(vars(zaplata, naeti), na_if, "-") %>% 
  na.omit %>% 
  mutate_at(vars(zaplata, naeti), as.numeric) %>% 
  arrange(sektor,prof,godina) %>% 
  mutate(meszapl = zaplata/12,
         meszapl = round(meszapl, 0),
         mrz = case_when(godina == 2008 ~ 220,
                        godina == 2009 ~ 240,
                        godina == 2010 ~ 240,
                        godina == 2011 ~ 270,
                        godina == 2012 ~ 290,
                        godina == 2013 ~ 310,
                        godina == 2014 ~ 340,
                        godina == 2015 ~ 360,
                        godina == 2016 ~ 420,
                        godina == 2017 ~ 460,
                        godina == 2018 ~ 510),
         mrzproc = round(mrz/meszapl * 100, 0),
         nabl = paste(sektor, prof)) %>% 
  group_by(sektor,prof) %>% 
  mutate(meszaplprprom = (meszapl - dplyr::lag(meszapl))/dplyr::lag(meszapl)*100,
         naetiprprom = (naeti - dplyr::lag(naeti))/dplyr::lag(naeti)*100,
         mrzprprom = mrzproc - dplyr::lag(mrzproc)) %>% 
  ungroup

# treeмап плот. Не е особено информативен в случая

colnames(nz38.df)

nz38.df %>% filter(sektor !=0, prof !=0, godina == 2018, mrzproc > 80) %>% 
  summarise(sum(naeti))

nz38.df %>% filter(sektor != 0, prof != 0, godina == 2018) %>% 
  ggplot(aes(area = naeti, fill = mrzproc, subgroup = prof)) + 
  geom_treemap() + 
  geom_treemap_subgroup_border(colour = "black") +
  geom_treemap_text(aes(label = sektor), colour = "white") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = .2, colour =
                               "black", fontface = "italic", min.size = 0) +
  labs(fill = "МРЗ/СРЗ, %")

# display.brewer.all(colorblindFriendly = TRUE)
# YlOrRd   Greens   Blues

nz38.df %>% filter(sektor != 0, prof != 0, godina == 2018) %>% 
  ggplot(aes(area = naeti, fill = mrzproc)) + 
  geom_treemap() + geom_treemap_text(aes(label = nabl), colour = "white") +
  labs(fill = "МРЗ/СРЗ, %") # +
#scale_fill_continuous_tableau()
#scale_fill_viridis() 

ggsave("graphs/9_kid38_prof_naeti_2018.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/9_kid38_prof_naeti_2018.png", width=8, height=4, type="cairo-png")
ggsave("graphs/9_kid38_prof_naeti_2018.svg", width=8, height=4, device = svg)

display.brewer.all(colorblindFriendly = TRUE)

# Heatmap с МРЗ/З по икономически дейности А38 и професии
a38heatmap.df <- 
  nz38.df %>% filter(sektor !=0, prof != 0) %>% 
  group_by(sektor,prof) %>% 
  summarise(mrzprocsr = mean(mrzproc)) %>% 
  spread(key = prof, value = mrzprocsr)

a38heatmap.df <- as.data.frame(a38heatmap.df)
row.names(a38heatmap.df) <- a38heatmap.df$sektor
a38heatmap.df$sektor <- NULL
superheat(a38heatmap.df, 
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          heat.pal = rev(brewer.pal(5, "Greens")),
          order.rows = order(row.names(a38heatmap.df), decreasing = T)#,
         # title = "Заплащане по дейности и професии"
         )

a38profheatmapdqlnaeti.df <- 
  nz38.df %>% 
  filter(sektor !=0, prof != 0) %>% 
  select(-zaplata, -mrz, -mrzproc) %>% 
  group_by(godina) %>% 
  mutate(dqlnaeti = naeti/sum(naeti)) %>% 
  ungroup %>% 
  select(sektor, prof, dqlnaeti, godina) %>%
  group_by(sektor, prof) %>% 
  summarise(dqlnaetisr = mean(dqlnaeti)) %>% 
  mutate(dqlnaetisr = dqlnaetisr*100) %>% 
  spread(key = prof, value = dqlnaetisr)
  
a38profheatmapdqlnaeti.df <- as.data.frame(a38profheatmapdqlnaeti.df)  
row.names(a38profheatmapdqlnaeti.df) <- a38profheatmapdqlnaeti.df$sektor
a38profheatmapdqlnaeti.df$sektor <- NULL

superheat(a38profheatmapdqlnaeti.df,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          heat.pal = rev(brewer.pal(9, "Greens")),
          order.rows = order(row.names(a38heatmap.df), decreasing = T))

# bubblemap - Заплати и наети по сектори и професии

nz38.df %>% filter(sektor !=0, prof !=0) %>% 
  group_by(sektor,prof) %>% 
  summarise(srmrzproc = mean(mrzproc),
            srnaeti = mean(naeti)) %>% 
  ggplot(aes(x = sektor, y = as.factor(prof))) +
  geom_point(aes(size = srmrzproc, color = log(srnaeti))) +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_viridis_c()

colnames(nz38.df)

nz38.df %>% filter(sektor != 0, prof !=0, godina %in% c(2008,2018)) %>% 
  select(nabl, godina, naeti, mrzproc) %>% 
  group_by(nabl) %>% 
  mutate(ind100naeti = 100 * naeti/first(naeti)) %>% 
  ungroup() %>% 
  filter(naeti > 1000) %>% 
  group_by(godina) %>% 
  mutate(dqlnaeti = naeti/sum(naeti)*100) %>% 
  ggplot(aes(x = mrzproc, y = ind100naeti)) + 
  geom_label(aes(color = factor(godina), label = nabl, size = dqlnaeti)) +
  geom_line(aes(group = nabl), color = "gray", linetype = "dotted") +
  scale_color_viridis_d(begin = 0.2, end = 0.7, name = "Година") +
  scale_size(name = "Дял наети") +
  xlab("МРЗ/СРЗ, %") + ylab("Наети, 2008=100") +
  theme_bw()

# Показваме с числови стойности графиката.
# Например в сектор G5 наетите са средно 7,3%
# средно за периода като дял от всички наети
# (или приблизително 160 0000)

nz38.df %>% filter(sektor !=0, prof !=0) %>% 
  group_by(sektor,prof) %>% 
  summarise(srmrzproc = mean(mrzproc),
            srnaeti = mean(naeti)) %>% 
  ungroup %>% 
  mutate(naetidql = srnaeti/sum(srnaeti)*100) %>%
  arrange(-naetidql)

# Няма нито една двойка сектори-професии, при която 
# МРЗ да е повече от 100% от средната заплата. Има с над 80%,
# което включва класове професии 5,6 и 9 (най-често 6) в различни
# сектори, които включват N, I, CA, CB, CC, JB, JC, S
nz38.df %>% filter(sektor !=0, prof !=0) %>% 
  group_by(sektor,prof) %>% 
  summarise(srmrzproc = mean(mrzproc),
            srnaeti = mean(naeti)) %>% 
  ungroup %>% 
  mutate(naetidql = srnaeti/sum(srnaeti)*100) %>%
  filter(srmrzproc > 80) %>% tabyl(prof, sektor)

# МРЗ/з по професии за периода

nz38.df %>% filter(sektor == 0, prof != 0, godina == 2018)

nz38.df %>% filter(sektor == 0, prof != 0, godina == 2008)

colnames(nz38.df)

nz38.df %>% filter(sektor == 0, prof !=0, godina %in% c(2008,2018)) %>% 
  mutate(ind100naeti = 100 * naeti/first(naeti)) 

nz38.df %>% filter(sektor == 0, prof !=0, godina %in% c(2008,2018)) %>% 
  group_by(prof) %>% 
  mutate(ind100naeti = 100 * naeti/first(naeti)) %>% 
  ggplot() + 
  geom_col(aes(x = factor(prof), y = mrzproc, fill = factor(godina)), 
           position = "dodge") +
  theme_bw() +
  scale_fill_viridis_d(begin = 0.3, end = 0.8, name = "МРЗ/СРЗ, %") +
  xlab("") + ylab("проценти")

colnames(nz38.df)

nz38.df %>% filter(sektor == 0, prof !=0, godina %in% c(2008,2018)) %>% 
  select(prof, godina, naeti, mrzproc) %>% 
  group_by(prof) %>% 
  mutate(ind100naeti = 100 * naeti/first(naeti)) %>% 
  ungroup() %>% 
  group_by(godina) %>% 
  mutate(dqlnaeti = naeti/sum(naeti)*100) %>% 
  ggplot(aes(x = mrzproc, y = ind100naeti)) + 
  geom_label(aes(color = factor(godina), label = prof, size = dqlnaeti)) +
  geom_line(aes(group = prof), color = "gray", linetype = "dotted") +
  scale_color_viridis_d(begin = 0.2, end = 0.7, name = "Година") +
  scale_size(name = "Дял наети") +
  xlab("МРЗ/СРЗ, %") + ylab("Наети, 2008=100") +
  theme_bw()

ggsave("graphs/10_profesii_2008_2018.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/10_profesii_2008_2018.png", width=8, height=4, type="cairo-png")
ggsave("graphs/10_profesii_2008_2018.svg", width=8, height=4, device = svg)

# МРЗ/з по сектори за периода

nz38.df %>% filter(sektor !=0, prof == 0) %>% 
  ggplot(aes(x= sektor, y = mrzproc)) + geom_boxplot() +
  theme_bw() +
  xlab("Икономически дейности (А38)") + ylab("МРЗ/СРЗ, %") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0.2)) 

# Наети по сектори за периода

nz38.df %>% filter(sektor !=0, prof == 0) %>% 
  ggplot(aes(x= sektor, y = log(naeti))) + geom_boxplot()

# Наети - процентна промяна на годишна база
# от 2009 до 2018 г. и при условие, че се разглеждат двойки сектори-професии,
 
nz38.df %>% filter(sektor !=0, prof !=0, godina > 2008) %>% 
  arrange(naetiprprom)

# По принцип двойките сектор-професия са 324, които разбираме от

nz38.df %>% filter(sektor !=0, prof !=0, godina == 2018) %>%
  group_by(sektor,prof) %>% 
  summarise(mean(naeti))

# Премахваме двойките сектор-професия със стойности на размаха
# по отношение на промяната на наетите отвъд 95 перцентил

nzfilter <- 
  nz38.df %>% filter(sektor !=0, prof !=0, godina > 2008) %>% 
  group_by(sektor, prof) %>%
  summarise(maxpromnaeti = max(naetiprprom),
            minpromnaeti = min(naetiprprom),
            stotkl = sd(naetiprprom),
            srpromnaeti = mean(naetiprprom),
            srnaeti = mean(naeti),
            srmrzz = mean(mrzproc)) %>% 
  ungroup %>% 
  filter(srnaeti > 1000) %>% 
  mutate(razmah = maxpromnaeti - minpromnaeti,
         grupi = ntile(razmah,20))

nzfilter  %>% 
  filter(grupi == 20) %>% 
  arrange(-razmah) 

# Това са 11 двойки сектор-професия, като при тях размахът варира
# от 68 до 242%, стандартното отклонение - от 18,9 до 75,2%.
# Като изключим CL 9 (Производство на превозни средства, неквалифицирани работници),
# N (администрация) 7 (квалифицирани работници) и 8 (машинни оператори),
# QB 2 (медицински грижи - специалисти), всички останали имат МРЗ/заплата под 50%


# Така остават 220 двойки сектори-професии
nzfilter  %>% 
  filter(grupi != 20) %>% 
  arrange(-razmah) 

# Прилагаме този филтър по отношение на основната таблица с данни

nz38f.df <- 
  nzfilter %>% 
  filter(grupi != 20) %>% 
  select(sektor,prof) %>% 
  inner_join(nz38.df)

# Така наблюденията падат от 4065

nz38.df

# до 2420

2420/220

nz38f.df <- 
  nz38f.df %>% filter %>% 
  group_by(godina) %>% 
  mutate(kwintgrupi = ntile(mrzproc, 5),
         kwintgrupi = as.factor(kwintgrupi))

nz38f.df %>% filter(godina > 2008) %>% 
  mutate(plotgroups = case_when(kwintgrupi == 5 ~ "high", TRUE ~"low")) %>% 
  ggplot(aes(x = mrzprprom, y = naetiprprom)) + 
  geom_point(aes(color = plotgroups)) +
  facet_wrap(godina ~ ., ncol = 5) +
  scale_color_viridis_d()

nz38f.df %>% filter(godina > 2008) %>% 
  mutate(plotgroups = case_when(kwintgrupi == 5 ~ "високо", TRUE ~"ниско")) %>% 
  ggplot(aes(x = plotgroups, y = naetiprprom, color = plotgroups)) + geom_boxplot() +
  facet_wrap(godina ~ .) + scale_color_viridis_d(begin = 0.3, end = 0.8) + theme_bw()

colnames(nz38f.df)

nz38f.df %>% 
  group_by(kwintgrupi,godina) %>% 
  summarise(srpromnaeti = mean(naetiprprom)) %>% 
  filter(godina %in% c(2009,2018)) %>% 
  ungroup %>% 
  mutate(godina = paste0("g",godina),
         kwintgrupi = as.factor(kwintgrupi)) %>% 
  spread(key = godina, value = srpromnaeti) %>% 
  ggplot(aes(y = kwintgrupi, x = g2009, xend = g2018)) +
  geom_dumbbell(size = 1.2, size_x = 3, size_xend = 3,
                colour = "grey", colour_x = "blue", colour_xend = "red") +
  theme_bw() + 
  labs(title = "Наети, годишна промяна (%)",
       subtitle = "2009 и 2018 г.",
       x = "Процентна промяна при наетите",
       y = "Квинтилни групи (МРЗ/СРЗ, %)")

# Графика 10
# Процентна промяна на наетите по квинтилни групи на МРЗ/СРЗ
# А38 и професии

nz38f.df %>% filter(godina > 2008) %>% 
  ggplot(aes(x = kwintgrupi, y = naetiprprom, color = kwintgrupi)) + 
  geom_boxplot() +
  facet_wrap(godina ~ ., ncol = 5) + 
  scale_color_viridis_d() + 
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Квинтилни групи, МРЗ/СРЗ, %") +
  ylab('Наети, промяна ("%)')

# Да се запази тук

nz38f.df %>% 
  filter(godina %in% c(2008,2018)) %>% 
  group_by(nabl) %>% 
  mutate(ind100naeti = naeti/first(naeti)*100) %>% 
  select(godina, mrzproc, kwintgrupi, nabl, ind100naeti, naetiprprom) %>% 
  filter(godina == 2018) %>%
  ungroup %>% 
  group_by(kwintgrupi) %>% 
   summarise(srmrzproc = mean(mrzproc),
             srind100naeti = mean(ind100naeti))

nz38f.df %>% 
  filter(godina %in% c(2008, 2013, 2018)) %>% 
  group_by(nabl) %>% 
  mutate(ind100naeti = naeti/first(naeti)*100) %>% 
  select(godina, mrzproc, kwintgrupi, nabl, ind100naeti, naetiprprom) %>% 
  ungroup %>% 
  group_by(kwintgrupi, godina) %>% 
  summarise(srmrzproc = mean(mrzproc),
            srind100naeti = mean(ind100naeti)) %>% 
  ggplot(aes(x = srmrzproc, y = srind100naeti)) +
  geom_label(aes(color = factor(godina), label = kwintgrupi)) +
  geom_line(aes(group = kwintgrupi), color = "gray", linetype = "dotted") +
  scale_color_viridis_d(name = "Година") +
  xlab("МРЗ/СРЗ, %") + 
  ylab("Наети, 2008=100") +
  theme_bw()

ggsave("graphs/10_kwintgrupi_kid38_2008_2018.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/10_kwintgrupi_kid38_2008_2018.png", width=8, height=4, type="cairo-png")
ggsave("graphs/10_kwintgrupi_kid38_2008_2018.svg", width=8, height=4, device = svg)

nz38f.df %>% filter(kwintgrupi == 5) %>% 
  arrange(mrzproc)

nz38f.df %>% 
  group_by(nabl) %>% 
  mutate(ind100naeti = naeti/first(naeti)*100) %>% 
  select(godina, mrzproc, kwintgrupi, nabl, ind100naeti, naetiprprom) %>% 
  ungroup %>% 
  group_by(kwintgrupi, godina) %>% 
  summarise(srmrzproc = mean(mrzproc),
            srind100naeti = mean(ind100naeti)) %>% 
  ggplot(aes(x = godina, y = srind100naeti)) +
  geom_line(aes(color = factor(kwintgrupi))) +
  scale_x_continuous(breaks = seq(2008,2018,2)) +
  scale_color_viridis_d(name = "Квинтилна група") +
  xlab("") + ylab("Наети, 2008=100") +
  theme_bw()
  
nz38f.df %>% filter(godina > 2008) %>% 
  ggplot(aes(x = kwintgrupi, y = mrzproc, color = kwintgrupi)) + geom_boxplot() +
  facet_wrap(godina ~ .) + scale_color_viridis_d() + theme_bw()

g38 <- 
  nz38f.df %>% 
  filter(godina > 2008) %>% 
  group_by(nabl) %>% 
  mutate(ind100naeti = naeti / first(naeti)) %>% 
  ungroup %>% 
  mutate(plotgroups = case_when(kwintgrupi == 5 ~ "кв.гр. 5", TRUE ~"кв.гр. 1-4")) %>% 
  group_by(plotgroups, godina) %>% 
  summarise(srnaetiprom = mean(naetiprprom),
            srzprom = mean(mrzprprom),
            srind100naeti = mean(ind100naeti))  %>% 
  ggplot() + scale_color_viridis_d(begin = 0.1, end = 1) + theme_bw()
 
g38a <- g38 + 
  geom_line(aes(x = godina, y = srnaetiprom, color = plotgroups)) +
  xlab("") + ylab("Наети,\nпромяна (%)") +
  theme_bw() +
  theme(legend.position='none')

g38b <- g38 + 
  geom_line(aes(x = godina, y = srzprom, color = plotgroups)) +
  xlab("") + ylab("Заплата,\nпромяна (%)") +
  theme_bw() +
  labs(color = "МРЗ/СРЗ, %") +
  theme(legend.position='bottom', legend.direction = "horizontal")

(g38a / g38b)

# Графика 12

g38scatter1 <- 
  nz38f.df %>%  
  filter(godina > 2008) %>% 
  ggplot(aes(x = mrzproc, y = naetiprprom)) + 
  geom_point(aes(color = kwintgrupi), show.legend = FALSE) +
  geom_smooth(method="lm") +
  theme_bw() + scale_color_viridis_d() +
#  labs(color = "Квинт.\nгрупи") +
  xlab("МРЗ/СРЗ, %") + ylab("Наети за 2009-2018 г., год. промяна (%)")

g38scatter2 <- 
  nz38f.df %>%
  group_by(nabl) %>% 
  mutate(ind100naeti = naeti/first(naeti)*100) %>% 
  filter(godina == 2018) %>% 
  ggplot(aes(x = mrzproc, y = ind100naeti)) + 
  geom_point(aes(color = kwintgrupi)) +
  geom_smooth(method="lm") +
  theme_bw() + scale_color_viridis_d() +
  labs(color = "Квинт.\nгрупи") +
  xlab("МРЗ/СРЗ, %") + ylab("Наети през 2018 г., 2008=100")

g38scatter1 | g38scatter2

ggsave("graphs/10_kdi38_scatterplot.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/10_kdi38_scatterplot.png", width=8, height=4, type="cairo-png")
ggsave("graphs/10_kdi38_scatterplot.svg", width=8, height=4, device = svg)

nz38f.df %>%  filter(godina > 2008) %>% 
  # mutate(plotgroups = case_when(kwintgrupi == 5 ~ "high", TRUE ~"low")) %>% 
  ggplot(aes(x = mrzproc, y = naetiprprom, color = kwintgrupi)) + geom_point() +
  theme_bw() + scale_color_viridis_d() +
  facet_wrap(godina ~ ., ncol = 3)

###
# панелна регресия
# anova
###



# По професии

# МРЗ/за по професии за периода
# 5, 6, 9 имат средно за периода над 60%, 
# като 9 приближава 80%

# Наети по професии за периода

# МРЗ/за по професии за периода
# 5, 6, 9 имат средно за периода над 60%, 
# като 9 приближава 80%

gprof38boxplot1 <- 
  nz38.df %>%  filter(sektor ==0, prof !=0) %>% 
  ggplot(aes(x = as.factor(prof), y = mrzproc, color = as.factor(prof))) + 
  geom_boxplot(show.legend = FALSE) +
  xlab("Класове професии") + ylab("МРЗ/СРЗ, %") +
  scale_color_viridis_d() +
  theme_bw()

gprof38boxplot2 <- 
  nz38.df %>% filter(sektor ==0, prof != 0) %>% 
  ggplot(aes(x = as.factor(prof), y = naeti/1000, color = as.factor(prof))) + 
  geom_boxplot(show.legend = FALSE) +
  xlab("Класове професии") + ylab("Наети (хиляди)") +
  scale_color_viridis_d() +
  theme_bw()

gprof38boxplot1 | gprof38boxplot2

ggsave("graphs/10_profesii_boxplot.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/10_profesii_boxplot.pdf.png", width=8, height=4, type="cairo-png")
ggsave("graphs/10_profesii_boxplot.pdf.svg", width=8, height=4, device = svg)

nz38f.df %>% filter(godina == 2018) %>% 
  tabyl(prof, kwintgrupi)

###
# ГОДИШНИ ПО ОБЩИНИ
###

godobsht.df <- read.csv("data/nsi_po_obshtini.csv")

godobsht.df <- 
  godobsht.df %>% 
  mutate_at(vars(naeti,godzaplata), as.character) %>% 
  mutate_at(vars(naeti,godzaplata), as.numeric) %>% 
  mutate(meszapl = godzaplata/12,
         mrz = case_when(godina == 2008 ~ 220,
                         godina == 2009 ~ 240,
                         godina == 2010 ~ 240,
                         godina == 2011 ~ 270,
                         godina == 2012 ~ 290,
                         godina == 2013 ~ 310,
                         godina == 2014 ~ 340,
                         godina == 2015 ~ 360,
                         godina == 2016 ~ 420,
                         godina == 2017 ~ 460,
                         godina == 2018 ~ 510),
         mrzproc = round(mrz/meszapl * 100, 0)) %>% 
  filter(obshtina != "total")

nasobsht <- read.csv("data/naseleniepoobshtini.csv")

nzobshtgod.df <- godobsht.df %>% inner_join(nasobsht) 

nzobshtgod.df %>% tabyl(obshtkod) %>% arrange(-n)

# Премахваме някои общини, за които има по-малко наблюдения
# и премахваме дублирането при градовете Бяла, обл. Русе и Бяла, обл. Варна
# Добавяме нов променлива - коефициент на наети

nzobshtgod.df <- 
  nzobshtgod.df %>% 
  filter(obshtkod != "PAZ39") %>% 
  filter(obshtkod != "VRC19") %>% 
  filter(!(oblast == "Русе" & obshtkod == "VAR05")) %>% 
  filter(!(oblast == "Варна" & obshtkod == "RSE04")) %>% 
  mutate(obshtkod = droplevels(obshtkod),
         koefnaeti = round(naeti/nastrw * 100, 1))

# Видна е заивисмиостта между отношението МРЗ/з и коефициент наети
# Това обаче е статична снимка, като стойностите и при двете променливи
# могат да бъдат обяснение с различията в икономическата ситуация в
# съответените общини.

# Графика 13. МРЗ/СРЗ и коефициент наети
nzobshtgod.df %>% 
  filter(godina > 2011) %>% 
  ggplot(aes(x = mrzproc, y = koefnaeti)) + geom_point() +
  facet_wrap(godina ~ .) +
  xlab("МРЗ/СРЗ, %") +
  ylab("Коефициент наети") +
  theme_bw()

# Създаваме децилни групи според отношението МРЗ/з за всяка година
# и добавяме нови променливи с
# - годишната промяна в МРЗ/з (в %)
# - годишната промяна в броя на наетите (в %)
# - годишната промяна в населенеито в трудоспособна възраст (в %)

nzobshtgod.df <- 
  nzobshtgod.df %>% 
  arrange(obshtkod, godina) %>% 
  group_by(obshtkod) %>% 
  mutate(mrzprprom = mrzproc - dplyr::lag(mrzproc),
         nastrwprom = (nastrw - dplyr::lag(nastrw))/dplyr::lag(nastrw)*100,
         koefnaetiprom = (koefnaeti - dplyr::lag(koefnaeti))/dplyr::lag(koefnaeti)*100,
         naetiprprom = (naeti - dplyr::lag(naeti))/dplyr::lag(naeti)*100,
         meszaplprom = (meszapl - dplyr::lag(meszapl))/dplyr::lag(meszapl)*100,
         naetiprpromadj = naetiprprom - nastrwprom) %>% 
  ungroup %>% 
  group_by(godina) %>% 
  mutate(decilnigrupi = ntile(mrzproc,10)) %>% 
  ungroup

glimpse(nzobshtgod.df)

# Добавяме и индекс 100 за 2011 за брой наети и за население в
# трудоспособна възраст

nzobshtgod.df <- 
  nzobshtgod.df %>% 
  group_by(obshtkod) %>% 
  mutate(ind100naeti = naeti/first(naeti)*100,
         ind100nastrw = nastrw/first(nastrw)*100) %>% 
  ungroup

gobshtnas1 <- 
nzobshtgod.df %>%
  filter(godina == 2017) %>% 
  ggplot(aes(x = mrzproc, y = ind100naeti)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("МРЗ/СРЗ, %") +
  ylab("Брой наети, 2011=100") +
  theme_bw()

gobshtnas2 <-
nzobshtgod.df %>%
  filter(godina == 2017) %>% 
  ggplot(aes(x = mrzproc, y = ind100nastrw)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("МРЗ/СРЗ, %") +
  ylab("Нас. в трудосп. възраст, 2011=100") +
  theme_bw()

gobshtnas1 | gobshtnas2

ggsave("graphs/12_obshtini_nas_naeti_2017.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/12_obshtini_nas_naeti_2017.png", width=8, height=4, type="cairo-png")
ggsave("graphs/12_obshtini_nas_naeti_2017.svg", width=8, height=4, device = svg)

nzobshtgod.df %>%
  filter(godina == 2017) %>% 
  ggplot(aes(x = ind100naeti, y = ind100nastrw)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Наети, 2011=100") +
  ylab("Нас. в трудосп. възраст, 2011=100") +
  theme_bw()

nzobshtgod.df %>% 
  filter(godina == 2017) %>% 
  select(ind100naeti, ind100nastrw) %>% 
  correlate

# Контролна група
# Има ли връзка между големината на общината и равнището на заплатите?

glimpse(nzobshtgod.df)

nzobshtgod.df %>% 
  filter(godina == 2017) %>% 
  ggplot(aes(x=log(nastrw), y=log(meszapl))) + geom_point() +
  geom_smooth(method = "lm")

nzobshtgod.df %>% 
  group_by(godina) %>% 
  mutate(nasgrupi = ntile(nastrw, 5)) %>% 
  tabyl(nasgrupi, decilnigrupi)

library(ggmosaic)

nzobshtgod.df %>% 
  group_by(godina) %>% 
  mutate(nasgrupi = ntile(nastrw, 2),
         zaplgrupi = ntile(mrzproc, 2)) %>%
  mutate(nasgrupi = as.factor(nasgrupi),
         zaplgrupi = as.factor(zaplgrupi)) %>% 
  mutate(zapl = case_when(zaplgrupi == 1 ~ "1 ниски заплати",
                          zaplgrupi == 2 ~ "2 високи заплати"),
         nas = case_when(nasgrupi == 1 ~ "1 малко население",
                         nasgrupi == 2 ~ "2 голямо население")) %>% 
  ggplot() + 
  geom_mosaic(aes(x=product(nas, zapl), na.rm = TRUE, fill = nas))

# Карта

# Зареждане от шейп файл


# geoob <- readOGR("data/geo/municipalities-polygon.shp")

# Намираме къде са кодовете на общините
# geoob$nuts4

# И казваме да се запазят с region = "nuts4". Иначе ще сложи негова
# номерация
# geoob_fort <- fortify(geoob, region = "nuts4")

# tail(geoob_fort)

# Или с geojson

obshtgeo <- geojson_read("data/municipalities.geojson", what = "sp")

obshtzamerge <- nzobshtgod.df %>% 
  filter(godina == 2017) %>% 
  select(id = obshtkod, decilnigrupi, ind100naeti, mrzproc, koefnaeti, godina)

geoob <- fortify(obshtgeo, region = "nuts4")

tail(geoob)

# Поради някаква причина командата
# merge(geoob, obshtzamerge, by = "id") 
# нарушава целостта на картата


# Може ръчно да се добавят променливи с match
# geoob$koefnaeti <- obshtzamerge$koefnaeti[match(geoob$id, obshtzamerge$id)]
# стига в obshtzamerge да са използвани данни само за една година

geoobjoined <- left_join(geoob, obshtzamerge, by = "id")

geoobjoined

ggplot() + 
  geom_polygon(data = geoobjoined, aes(x = long, y = lat, group = group), 
               colour = "black", fill = "NA") +
  coord_map() +
  theme_void()

gkartaobshtini1 <-
  geoobjoined %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = koefnaeti), 
               colour = "gray") +
  coord_map() +
  theme_bw() +
  theme(axis.text=element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient(low="lightgreen", high="darkgreen", 
                      guide="colorbar", na.value="white") +
  labs(title = "Коефициент наети по общини", 
       x = element_blank(), y = element_blank(), 
       fill='Коеф.наети, %') +   theme(legend.position = "bottom") 

gkartaobshtini2 <-
  geoobjoined %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mrzproc), 
               colour = "gray") +
  coord_map() +
  theme_bw() +
  theme(axis.text=element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient(low="darkgreen", high="lightgreen", 
                      guide="colorbar", na.value="white") +
  labs(title = "Заплата по общини", 
       x = element_blank(), y = element_blank(), 
       fill='МРЗ/СРЗ, %') +   theme(legend.position = "bottom") 

gkartaobshtini2 | gkartaobshtini1

ggsave("graphs/12_kartograma_obhstini.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/12_kartograma_obhstini.png", width=8, height=4, type="cairo-png")
ggsave("graphs/12_kartograma_obhstini.svg", width=8, height=4, device = svg)

geoobjoined %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = ind100naeti), 
               colour = "gray") +
  coord_map() +
  theme_bw() +
  theme(axis.text=element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient(low="gray90", high="gray20", 
                      guide="colorbar", na.value="white") +
  labs(title = "Наети по общини, 2011=100", 
       x = element_blank(), y = element_blank(), 
       fill='Индекс наети') +   theme(legend.position = "bottom") 

# Има ли необичайно големи промени при наетите?

nzobshtgodfilter <- 
  nzobshtgod.df %>% na.omit %>% 
  group_by(obshtkod) %>% 
  summarise(maxpromnaeti = max(naetiprpromadj),
            minpromnaeti = min(naetiprpromadj),
            stotkl = sd(naetiprpromadj),
            srpromnaeti = round(mean(naetiprpromadj),1),
            srnaeti = mean(naeti),
            srmrzz = mean(mrzproc)) %>% 
  ungroup %>% 
  mutate(razmah = maxpromnaeti - minpromnaeti,
         grupi = ntile(razmah,20),
         grupi = as.factor(grupi)) %>% 
  arrange(-razmah)

# Размахът тук не е толкова голям. Отвъд 95 перцентил
# приема стойности от 48 до 95%. Въпреки това елиминираме
# общините с тези колебания

nzobshtgodfilter
  
nzobshtgod.df %>% group_by(decilnigrupi) %>% 
  summarise(naetisrpromqna = mean(naetiprpromadj, na.rm = T),
            mrzprocsrednozagr = mean(mrzproc)) 

nzobshtgodf.df <- 
  nzobshtgodfilter %>% filter(grupi != 20) %>% select(obshtkod) %>% 
  inner_join(nzobshtgod.df)

nzobshtgodf.df %>% 
  group_by(godina) %>% 
  summarize(n())

nzobshtgodf.df %>% 
  group_by(godina, decilnigrupi) %>% 
  summarize(n())

1733/249
263*0.95
249*7

nzobshtgod.df
nzobshtgodf.df
# Вместо 1938 наблюдения, след премахването на общините отвъд 95 перцентил
# има 1743 наблюдения

nzobshtgodf.df %>% 
  filter(godina %in% c(2011,2017), decilnigrupi %in% c(1,10)) %>% 
  group_by(godina, decilnigrupi) %>% 
  summarize(mean(ind100naeti))
  
nzobshtgodf.df %>% 
  filter(godina %in% c(2011,2017), decilnigrupi %in% c(10)) %>% 
  group_by(godina, decilnigrupi) %>% 
  arrange(-mrzproc)

# Процентанта промяна при наетите (претеглена) спрямо отношението МРЗ/заплата

nzobshtgodf.df %>% filter(godina == 2017) %>% 
  ggplot(aes(x=mrzproc, y=ind100naeti, color = as.factor(decilnigrupi))) + 
  geom_point() +
  scale_color_viridis_d() +
  theme_bw()

  nzobshtgodf.df %>% filter(godina > 2011) %>% 
  ggplot(aes(x=mrzproc, y=naetiprpromadj)) + 
  geom_point() +
  facet_wrap(decilnigrupi ~ .) +
  theme_bw()
  
# Процентната промяна при наетите (претеглена) спрямо процентната промяна в отношението МРЗ/заплата

nzobshtgodf.df %>% filter(godina == 2017) %>% 
  ggplot(aes(x=mrzprprom, y=naetiprpromadj, color = as.factor(decilnigrupi))) + 
  geom_point() + 
  scale_color_viridis_d() + theme_bw()

nzobshtgodf.df %>% filter(godina == 2017) %>% 
  ggplot(aes(x=mrzprprom, y=naetiprpromadj)) + geom_point() +
  stat_summary() +
  geom_smooth(method='lm')

# Графика 14. Промяна в МРЗ/СРЗ (%) и процентна промяна в броя наети по 
# децилни групи на МРЗ/СРЗ

nzobshtgodf.df %>% filter(godina > 2011) %>% 
  ggplot(aes(x=mrzprprom, y=naetiprpromadj)) + geom_point() +
  stat_summary() +
  geom_smooth(method='lm') +
  facet_wrap(decilnigrupi ~ ., ncol = 5) +
  theme_bw() +
  xlab("Промяна в МРЗ/СРЗ, %") +
  ylab("Наети, промяна (%)")

nzobshtgodf.df %>% 
  select(obshtkod, obshtina, godina, mrzproc, koefnaeti, decilnigrupi)

# Графика - dumbbell plot коефициент наети по общини 2011 и 2017 г.

nzobshtgodf.df %>% 
  group_by(godina, decilnigrupi) %>% 
  summarise(srkoefnaeti = mean(koefnaeti)) %>% 
  ungroup %>% 
  filter(godina %in% c(2011,2017)) %>%
  mutate(godina = paste0("g",godina),
         decilnigrupi = as.factor(decilnigrupi)) %>% 
  spread(key = godina, value = srkoefnaeti) %>% 
  ggplot(aes(y = decilnigrupi, x = g2011, xend = g2017)) +
  geom_dumbbell(size = 1.2, colour_x = "blue", colour_xend = "red") +
  theme_bw()

# Графика time series heatmap 2011-2017 коефициент наети по децилни групи (общини)

nzobshtheat.df <- 
  nzobshtgodf.df %>% 
  group_by(godina, decilnigrupi) %>% 
  summarise(srkoefnaeti = mean(koefnaeti)) %>% 
  ungroup %>% 
  mutate(decilnigrupi = paste("група",decilnigrupi)) %>% 
  spread(key = godina, value = srkoefnaeti) %>% 
  as.data.frame

row.names(nzobshtheat.df) <- nzobshtheat.df$decilnigrupi
nzobshtheat.df$decilnigrupi <- NULL

nzobshtheat.df

superheat(nzobshtheat.df,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          heat.pal = rev(brewer.pal(9, "Greens")),
          order.rows = order(row.names(nzobshtheat.df), decreasing = T))

# Има ли съществена разлика между изменението в средните стойности на процентната промяна
# при броя наети между най-високата децилна група и останалите децилни групи

nzobshtgodf.df %>% filter(godina > 2011) %>% 
  mutate(rzotnoshenie = 
           case_when(decilnigrupi == 10 ~ "дец. група 10", TRUE ~ "дец. група 1-9")) %>% 
  ggplot(aes(rzotnoshenie,naetiprpromadj)) + geom_boxplot() +
  theme_bw()

nzobshtgodf.df %>% filter(godina > 2011) %>% 
  mutate(rzotnoshenie = 
           case_when(decilnigrupi == 10 ~ "децилна група 10", TRUE ~ "децилни групи 1-9")) %>% 
  group_by(rzotnoshenie) %>% 
  summarise(srednonaetiprom = mean(naetiprpromadj), 
            minnaetiprom = min(naetiprpromadj),
            maxnaetiprom = max(naetiprpromadj))

nzobshtgodf.df %>% filter(godina > 2011) %>% 
  mutate(rzotnoshenie = 
           case_when(decilnigrupi == 10 ~ "дец. група 10", TRUE ~ "дец. група 1-9")) %>%  
  ggplot(aes(mrzprprom, naetiprpromadj)) + geom_point() +  stat_summary() +
  geom_smooth(method='lm') +
  facet_wrap(rzotnoshenie ~ .)

nzobshtgodf.df %>% 
  filter(godina %in% c(2011,2017)) %>% 
  group_by(decilnigrupi, godina) %>% 
  summarise(srmrzproc = mean(mrzproc),
            srind100naeti = mean(ind100naeti),
            srkoefnaeti = mean(koefnaeti)) %>% 
  ggplot(aes(x = srmrzproc, y = srind100naeti)) +
  geom_label(aes(color = factor(godina), label = decilnigrupi, size = srkoefnaeti)) +
  geom_line(aes(group = decilnigrupi), color = "gray", linetype = "dotted") +
  scale_color_viridis_d(begin = 0.2, end = 0.7, 
                        name = "Година") +
  theme_bw() +
  xlab("МРЗ/СРЗ, %") + ylab("Наети, 2011=100") +
  labs(size = "Коеф. наети, %")

ggsave("graphs/12_obshtini_decilni_2017.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/12_obshtini_decilni_2017.png", width=8, height=4, type="cairo-png")
ggsave("graphs/12_obshtini_decilni_2017.svg", width=8, height=4, device = svg)

nzobshtgodf.df %>% 
#  filter(decilnigrupi %in% c(1,10)) %>% 
  group_by(godina, decilnigrupi) %>% 
  summarise(srednopromqnanaeti = mean(naetiprpromadj)) %>% 
  spread(decilnigrupi, srednopromqnanaeti)

nzobshtgodf.df %>% 
  #  filter(decilnigrupi %in% c(1,10)) %>% 
  group_by(godina, decilnigrupi) %>% 
  summarise(srednopromqnanaeti = mean(naetiprprom)) %>% 
  spread(decilnigrupi, srednopromqnanaeti)


nzobshtgodf.df %>% 
  #  filter(decilnigrupi %in% c(1,10)) %>% 
  group_by(godina, decilnigrupi) %>% 
  summarise(srednopromqnanaeti = mean(ind100naeti)) %>% 
  spread(decilnigrupi, srednopromqnanaeti)

nzobshtgodf.df %>% 
  #  filter(decilnigrupi %in% c(1,10)) %>% 
  group_by(godina, decilnigrupi) %>% 
  summarise(srednopromqnanaeti = mean(ind100nastrw)) %>% 
  spread(decilnigrupi, srednopromqnanaeti)

nzobshtgodf.df %>% 
  #  filter(decilnigrupi %in% c(1,10)) %>% 
  mutate(ind100naetiadj = ind100naeti-ind100nastrw) %>% 
  group_by(godina, decilnigrupi) %>% 
  summarise(srednopromqnanaeti = mean(ind100naetiadj)) %>% 
  spread(decilnigrupi, srednopromqnanaeti)

nzobshtgodf.df

# панелна регресия
# anova

###
# ДАННИ ОТ АЗ - МЕСЕЧНИ
# по общини
###

# от януари 2010 до декември 2018

azn.df <- read.csv("data/az_bezrabotnipoobshtini.csv")

azn.df <- 
  azn.df %>% 
  rename(obshtkod = laucode) %>% 
  select(-ikonaktiwni)

# Използваме годишните данни от НСИ по общини, за да групираме общините по 
# такива с високи и ниски заплати. Правим това с осреднени данни за целия период.

razmerzaplatiobshtinigrupi <- 
  nzobshtgod.df %>% 
  group_by(obshtkod) %>% 
  summarise(srgodzaplata = mean(godzaplata)) %>% 
  mutate(zaplatigrupa = ntile(-srgodzaplata, 10)) %>% 
  ungroup %>% na.omit %>% 
  select(obshtkod, zaplatigrupa)

glimpse(razmerzaplatiobshtinigrupi)

azn.df <- 
  azn.df %>% 
  inner_join(razmerzaplatiobshtinigrupi)

# Какво е отношението МРЗ/з  в различните групи

nzobshtgod.df %>% inner_join(razmerzaplatiobshtinigrupi) %>% 
  group_by(zaplatigrupa) %>% 
  summarise(mean(mrzproc))

# В група 1 са ... , в група 10 са 37 ...

# Добавяме процентна промяна при безработните месец за месец.

glimpse(azn.df)

# Тъй като броят на икономически активното население от данните на АЗ
# не е актуализирано в продължение на периода 2011-2018, използваме данни
# от НСИ за населението в трудоспособна възраст


# nastrimesechie <- 
#   nasobsht %>% 
#   group_by(obshtkod) %>% 
#   mutate(razlika = c(NA, diff(nastrw)),
#          q1 = nastrw - razlika/4*3,
#          q2 = nastrw - razlika/4*2,
#          q3 = nastrw - razlika/4*1,
#          q4 = nastrw) %>% 
#   gather(nastrwtrim, key = trimesechie, q1, q2, q3, q4) %>% 
#   mutate(trimesechie = str_remove(trimesechie, "q"), 
#          trimesechie = as.numeric(trimesechie),
#          nastrwtrim = round(nastrwtrim, digits = 0))

nastrpomeseci <- 
  nasobsht %>% 
  group_by(obshtkod) %>% 
  mutate(razlika = c(NA, diff(nastrw)),
         m01 = nastrw - razlika/12*11,
         m02 = nastrw - razlika/12*10,
         m03 = nastrw - razlika/12*9,
         m04 = nastrw - razlika/12*8,
         m05 = nastrw - razlika/12*7,
         m06 = nastrw - razlika/12*6,
         m07 = nastrw - razlika/12*5,
         m08 = nastrw - razlika/12*4,
         m09 = nastrw - razlika/12*3,
         m10 = nastrw - razlika/12*2,
         m11 = nastrw - razlika/12*1,
         m12 = nastrw) %>% 
    gather(nastrwmesec, key = mesec, m01, m02, m03, m04, m05, m06, m07,
           m08, m09, m10, m11, m12) %>% 
  mutate(mesec = str_remove(mesec, "m"), 
    #     mesec = as.numeric(mesec),
         nastrwmesec = round(nastrwmesec, digits = 0))

# създаваме тримесечни категории
azn.df <- 
  azn.df %>% 
  separate(period, into = c('godina', 'mesec'), sep = 4) %>%
  mutate(godina = as.integer(godina),
         trimesechie = case_when(mesec == "01" ~ 1,
                   mesec == "02" ~ 1,
                   mesec == "03" ~ 1,
                   mesec == "04" ~ 2,
                   mesec == "05" ~ 2, 
                   mesec == "06" ~ 2,
                   mesec == "07" ~ 3,
                   mesec == "08" ~ 3,
                   mesec == "09" ~ 3,
                   mesec == "10" ~ 4,
                   mesec == "11" ~ 4,
                   mesec == "12" ~ 4)) %>% 
  # Обединяваме с населението по месеци вместо с nastrwtrim
  left_join(nastrpomeseci)

# nasobsht %>% 
#   group_by(obshtkod) %>% 
#   mutate(dataperiod = paste0(godina,"-01-01"),
#          dataperiod = ymd(dataperiod))

azn.df <-  
  azn.df %>% 
  mutate(periyod = paste0(godina,"-",mesec,"-01"),
         periyod = ymd(periyod))

# Създаването на времеви серии не работи с group_by
# Затова директно преминаваме към съзадаването на тренда
# с decompose(ts(object))$trend

glimpse(azn.df)

aznseasadj.df <- 
  azn.df %>% 
  select(obshtkod, regbezr, periyod, zaplatigrupa, nastrwmesec) %>% 
  group_by(obshtkod) %>% 
  mutate(tsregbezrseasonal = 
           decompose(ts(regbezr, start = 2010, frequency = 12))$seasonal,
           tsregbezradjusted = regbezr - tsregbezrseasonal)

aznseasadj.df %>% 
  filter(obshtkod %in% c("VID01","BGS01")) %>% 
  filter(periyod >= as.Date('2013-12-31') & periyod <= as.Date('2014-12-31')) %>%  
  ggplot(aes(x = periyod)) +
#  geom_line(aes(y = regbezr)) +
#  geom_line(aes(y = tsregbezradjusted, color = "red"))
  geom_line(aes(y = tsregbezrseasonal, color = obshtkod))
  
aznseasadj.df %>%
  mutate(monthvar = month(periyod)) %>% 
  group_by(monthvar, zaplatigrupa) %>%
  summarise(sezonnopogrupi = sum(tsregbezrseasonal)) %>% 
  ggplot(aes(x = monthvar)) +
  geom_line(aes(y = sezonnopogrupi, color = as.factor(zaplatigrupa)))

# Зареждаме таблица с месечната промяна на МРЗ

mrzmonthly <- read.csv("data/mrzmonthly.csv")

mrzmesechni <- 
  mrzmonthly %>% 
  filter(godina > 2009) %>% 
  mutate(mrzprom = 
           round(((mrz-dplyr::lag(mrz))/dplyr::lag(mrz)*100), digits=1)) %>% 
  mutate(periyod = ymd(paste0(godina,"-",mesec,"-01"))) %>% 
  select(periyod, mrz, mrzprom)
  
aznseasadj.df <- 
  aznseasadj.df %>% inner_join(mrzmesechni)

aznseasadj.df %>%
  group_by(obshtkod) %>% 
  mutate(ind100regbezrtrend = tsregbezradjusted/first(tsregbezradjusted)*100) %>% 
  ungroup() %>% 
  group_by(zaplatigrupa, periyod) %>%
  summarise(trendpogrupiind100 = mean(ind100regbezrtrend)) %>% 
  filter(zaplatigrupa %in% c(1,10)) %>% 
  ggplot(aes(x = periyod)) +
  geom_line(aes(y = trendpogrupiind100, color = as.factor(zaplatigrupa))) +
  scale_color_viridis_d(name = "МРЗ/СРЗ, %\nдец. групи") +
  theme_bw() +
  xlab("") + ylab("Регистрирани безработни (ян. 2010 = 100)") +
  geom_vline(aes(xintercept = periyod), 
             data=aznseasadj.df %>% filter(mrzprom > 0),
             linetype = "dashed", color = "gray") +
  geom_label(aes(x = periyod, y = mrzprom, label = paste(mrzprom, "%")), 
             data = aznseasadj.df %>% filter(mrzprom > 0))

ggsave("graphs/13_obshtini_decilni_dinamika.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/13_obshtini_decilni_dinamika.png", width=8, height=4, type="cairo-png")
ggsave("graphs/13_obshtini_decilni_dinamika.svg", width=8, height=4, device = svg)

aznseasadj.df

aznseasadj.df %>% 
  mutate(mkoefbezr = tsregbezradjusted/nastrwmesec*100) %>% 
  group_by(zaplatigrupa) %>% 
  summarise(mean(mkoefbezr))

# Представяме графично по години месец за месец
# как се променя разликата в коефициента на безработица
# между най-ниската и най-високата децилна група 


gobshtkbezr1 <- 
  aznseasadj.df %>% 
  mutate(mkoefbezr = tsregbezradjusted/nastrwmesec*100) %>% 
  group_by(zaplatigrupa, periyod) %>% 
  summarise(srgrmkoefbezr = mean(mkoefbezr)) %>% 
  filter(zaplatigrupa %in% c(1,10)) %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = srgrmkoefbezr, color = factor(zaplatigrupa))) +
  scale_color_viridis_d(name = "Децилни\nгрупи") +
  theme_bw() +
  xlab("") + ylab("Модиф. коеф. на безработица")

gobshtkbezr2 <-
  aznseasadj.df %>% 
  mutate(mkoefbezr = tsregbezradjusted/nastrwmesec*100) %>% 
  group_by(zaplatigrupa, periyod) %>% 
  summarise(srgrmkoefbezr = mean(mkoefbezr)) %>% 
  ungroup %>% 
  filter(zaplatigrupa %in% c(1,10)) %>% 
  mutate(zaplatigrupa = paste0("grupa", zaplatigrupa)) %>% 
  select(periyod, srgrmkoefbezr, zaplatigrupa) %>% 
  spread(key = zaplatigrupa, value = srgrmkoefbezr) %>% 
  mutate(razlika = grupa10-grupa1) %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = razlika)) +
  geom_vline(aes(xintercept = periyod), 
             data=aznseasadj.df %>% filter(mrzprom > 0),
             linetype = "dashed", color = "gray") +
  geom_label(aes(x = periyod, y = mrzprom/2-2.5, label = paste(mrzprom, "%")), 
             data = aznseasadj.df %>% filter(mrzprom > 0)) +
  theme_bw() +
  xlab("") + ylab("К. безр., разлика м/у дец. групи 10 и 1")

gobshtkbezr2 | gobshtkbezr1

ggsave("graphs/13_obshtini_decilni_modkoefbezr.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/13_obshtini_decilni_modkoefbezr.png", width=8, height=4, type="cairo-png")
ggsave("graphs/13_obshtini_decilni_modkoefbezr.svg", width=8, height=4, device = svg)

# did


# Има ли структурни промени в разликата между
# коефициентите на безработица от двете децилни групи

glimpse(aznseasadj.df)

azrazlika.df <-
  aznseasadj.df %>% 
    ungroup %>% 
  mutate(kbezr = tsregbezradjusted/nastrwtrim*100) %>% 
  group_by(zaplatigrupa, periyod) %>% 
  summarise(srkbezr = mean(kbezr)) %>% 
  filter(zaplatigrupa == 1)

azrazlika.df  

azrazlika.df$srkbezr %>% 
changepoint:: cpt.meanvar() %>%  # Identify change points
  autoplot() + theme_bw()

strucchange::breakpoints(azrazlika.df$srkbezr ~ 1) %>%
  autoplot() + theme_bw()

glimpse(azn.df)



# Разлика в разликите при наетите по общини
# увеличение от 1 септември 2011 (от 240 на 270)

azn.df %>% 
  filter(periyod == "2010-01-01") %>% 
  tabyl(zaplatigrupa)

did_azn_2011.df <-
  azn.df %>% 
  filter(zaplatigrupa %in% c(1,10),
         periyod > "2011-07-01",
         periyod < "2011-10-01") %>% 
  mutate(treatperiod = case_when(periyod == "2011-08-01" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(zaplatigrupa == 1 ~ 0,
                                TRUE ~ 1))

did_azn_2011.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednobezr = mean(regbezr)) %>% 
  spread(key = treatperiod, value = srednobezr) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)
  

summary(lm(log(regbezr) ~ treatperiod*treatgroup, data = did_azn_2011.df))

did_azn_2011_trim.df <-
  azn.df %>% 
  filter(zaplatigrupa %in% c(1,10),
         periyod > "2011-05-01",
         periyod < "2011-12-01") %>% 
  mutate(treatperiod = case_when(periyod < "2011-09-01" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(zaplatigrupa == 1 ~ 0,
                                TRUE ~ 1))

did_azn_2011_trim.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednobezr = mean(regbezr)) %>% 
  spread(key = treatperiod, value = srednobezr) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

did_azn_2011_trim.df %>% 
  group_by(obshtkod, treatperiod) %>% 
  mutate(bezrabotni = mean(regbezr)) %>% 
  select(obshtkod, regbezr, bezrabotni, treatperiod, treatgroup)

did_azn_2011_trim_aggr.df <-
  did_azn_2011_trim.df %>% 
  group_by(obshtkod, treatperiod, treatgroup) %>% 
  summarize(bezrabotni = mean(regbezr)) 

did_azn_2011_trim_aggr.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednobezr = mean(bezrabotni)) %>% 
  spread(key = treatperiod, value = srednobezr) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)


summary(lm(log(regbezr) ~ treatperiod*treatgroup, data = did_azn_2011_trim.df))
summary(lm(log(bezrabotni) ~ treatperiod*treatgroup, data = did_azn_2011_trim_aggr.df))


did_azn_2011_trim_dwojni.df <-
  azn.df %>% 
  filter(zaplatigrupa %in% c(1,2,9,10),
         periyod > "2011-05-01",
         periyod < "2011-12-01") %>% 
  mutate(treatperiod = case_when(periyod < "2011-09-01" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(zaplatigrupa == 1 ~ 0,
                                zaplatigrupa == 2 ~ 0,
                                TRUE ~ 1))

did_azn_2011_trim_dwojni.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednobezr = mean(regbezr)) %>% 
  spread(key = treatperiod, value = srednobezr) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(regbezr) ~ treatperiod*treatgroup, data = did_azn_2011_trim_dwojni.df))

# увеличение 1 май 2012 (от 270 на 290) 

did_azn_2012.df <-
  azn.df %>% 
  filter(zaplatigrupa %in% c(1,10),
         periyod > "2012-03-01",
         periyod < "2012-07-01") %>% 
  mutate(treatperiod = case_when(periyod == "2012-04-01" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(zaplatigrupa == 1 ~ 0,
                                TRUE ~ 1))

did_azn_2012.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednobezr = mean(regbezr)) %>% 
  spread(key = treatperiod, value = srednobezr) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(regbezr) ~ treatperiod*treatgroup, data = did_azn_2012.df))


did_azn_2012_trim.df <-
  azn.df %>% 
  filter(zaplatigrupa %in% c(1,10),
         periyod > "2012-01-01",
         periyod < "2012-09-01") %>% 
  mutate(treatperiod = case_when(periyod < "2012-05-01" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(zaplatigrupa == 1 ~ 0,
                                TRUE ~ 1))

did_azn_2012_trim.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednobezr = mean(regbezr)) %>% 
  spread(key = treatperiod, value = srednobezr) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(regbezr) ~ treatperiod*treatgroup, data = did_azn_2012_trim.df))

did_azn_2012_trim_dwojni.df <-
  azn.df %>% 
  filter(zaplatigrupa %in% c(1,2,9,10),
         periyod > "2012-01-01",
         periyod < "2012-09-01") %>% 
  mutate(treatperiod = case_when(periyod < "2012-05-01" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(zaplatigrupa == 1 ~ 0,
                                zaplatigrupa == 2 ~ 0,
                                TRUE ~ 1))

did_azn_2012_trim_dwojni.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednobezr = mean(regbezr)) %>% 
  spread(key = treatperiod, value = srednobezr) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(regbezr) ~ treatperiod*treatgroup, data = did_azn_2012_trim_dwojni.df))

# увеличение от 01.01.2016 г. от 380 на 420 лв.

did_azn_2016.df <-
  azn.df %>% 
  filter(zaplatigrupa %in% c(1,10),
         periyod > "2015-11-01",
         periyod < "2016-02-01") %>% 
  mutate(treatperiod = case_when(periyod == "2015-12-01" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(zaplatigrupa == 1 ~ 0,
                                TRUE ~ 1))

did_azn_2016.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednobezr = mean(regbezr)) %>% 
  spread(key = treatperiod, value = srednobezr) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(regbezr) ~ treatperiod*treatgroup, data = did_azn_2016.df))


did_azn_2016_trim.df <-
  azn.df %>% 
  filter(zaplatigrupa %in% c(1,10),
         periyod > "2015-09-01",
         periyod < "2016-04-01") %>% 
  mutate(treatperiod = case_when(periyod < "2016-01-01" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(zaplatigrupa == 1 ~ 0,
                                TRUE ~ 1))

did_azn_2016_trim.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednobezr = mean(regbezr)) %>% 
  spread(key = treatperiod, value = srednobezr) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(regbezr) ~ treatperiod*treatgroup, data = did_azn_2016_trim.df))

# увеличение от 01.01.2018 г. от 460 на 510 лв.

did_azn_2018.df <-
  azn.df %>% 
  filter(zaplatigrupa %in% c(1,10),
         periyod > "2017-11-01",
         periyod < "2018-02-01") %>% 
  mutate(treatperiod = case_when(periyod == "2017-12-01" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(zaplatigrupa == 1 ~ 0,
                                TRUE ~ 1))

did_azn_2018.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednobezr = mean(regbezr)) %>% 
  spread(key = treatperiod, value = srednobezr) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(regbezr) ~ treatperiod*treatgroup, data = did_azn_2016.df))


did_azn_2018_trim.df <-
  azn.df %>% 
  filter(zaplatigrupa %in% c(1,10),
         periyod > "2017-09-01",
         periyod < "2018-04-01") %>% 
  mutate(treatperiod = case_when(periyod < "2018-01-01" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(zaplatigrupa == 1 ~ 0,
                                TRUE ~ 1))

did_azn_2018_trim.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednobezr = mean(regbezr)) %>% 
  spread(key = treatperiod, value = srednobezr) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(regbezr) ~ treatperiod*treatgroup, data = did_azn_2016_trim.df))



###
# ДАННИ ОТ НОИ - ТРИМЕСЕЧНИ
###

noisektori.df <- read.csv("data/noi_sektori_trimesechni.csv")

noisektori.df <- 
  noisektori.df %>% 
  arrange(sektor,godina,quarter) %>% 
  mutate(period = as.yearqtr(paste0(godina,quarter),format = "%Y%q"))

noisektori.df

# МРЗ по тримесечия

mrzquarterly <- read.csv("data/mrzquarterly.csv")

read.csv("data/mrzquarterly.csv")

mrzquarterly <- 
  mrzquarterly %>% 
  mutate(period = str_remove(period,"Q"),
         period = as.yearqtr(period, format = ("%Y%q")),
         mzchanges = round((mz - dplyr::lag(mz))/dplyr::lag(mz)*100,1)) 

mrzquarterly2013 <- 
  mrzquarterly %>% filter(period >= 2013, period < 2019)


noisektori.df <- 
  inner_join(noisektori.df, mrzquarterly2013) %>% 
    mutate(mrzproc = round(mz/srzapl*100, 1)) %>% 
    group_by(sektor) %>% 
    mutate(naetiprprom = (naeti-dplyr::lag(naeti))/dplyr::lag(naeti)*100,
           mrzprocprom = mrzproc-dplyr::lag(mrzproc),
           naeti2013ind100 = naeti/first(naeti)*100)

noisektori.df %>% group_by(sektor) %>% 
  summarise(mean(naeti, na.rm = T))

# Премахваме секторите с прекалено голяма волатилност - 
# тези, които се намират отвъд 90 перцентил

noisektorinewolatilni <- 
  noisektori.df %>% 
  group_by(sektor) %>% 
  summarise(minsek = min(naetiprprom, na.rm = T),
            maxsek = max(naetiprprom, na.rm = T),
            srsek = mean(naetiprprom, na.rm = T),
            stotkl = sd(naetiprprom, na.rm = T),
            srnaeti = mean(naeti, na.rm = T),
            srzaplata = mean(srzapl)) %>% 
  mutate(razmah = maxsek - minsek) %>% 
    filter(srnaeti > 5000) %>% 
    arrange(-razmah) %>%  
    mutate(grupinaeti = ntile(maxsek, 10)) %>% 
    filter(grupinaeti < 10) %>% 
  select(sektor)

noisektori.df <- 
  noisektorinewolatilni %>% 
  inner_join(noisektori.df) %>%
  group_by(sektor) %>% 
  summarise(mrzprocsredno = mean(mrzproc)) %>% 
  mutate(kwintilnigrupi = ntile(mrzprocsredno,5)) %>% 
  ungroup %>% 
  inner_join(noisektori.df) %>% 
    arrange(sektor,period)
  
noisektori.df %>% 
  mutate(kwintilnigrupi = as.factor(kwintilnigrupi)) %>% 
  ggplot(aes(kwintilnigrupi,naetiprprom)) + geom_boxplot()

noisektori.df %>% 
  mutate(kwintilnigrupi = as.factor(kwintilnigrupi)) %>% 
  ggplot(aes(kwintilnigrupi,mrzproc)) + geom_boxplot()

gnoinaeti1 <-
  noisektori.df %>% 
  filter(godina == 2018,
         quarter == 1) %>% 
  ggplot(aes(x = mrzproc, y = naeti2013ind100)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = as.factor(kwintilnigrupi))) +
  scale_color_viridis_d() +
  theme_bw() +
  xlab("МРЗ/СРЗ, %") + ylab("Наети, 1 трим. 2018 г. (1 трим. 2013 = 100)") +
  labs(color = "Квинт.\nгрупи")

gnoinaeti2 <- 
  noisektori.df %>% 
  filter(godina %in% c(2013, 2018),
         quarter == 1) %>% 
  group_by(kwintilnigrupi, godina) %>% 
  summarise(srmrzsrzproc = mean(mrzproc),
            srnaetiind100 = mean(naeti2013ind100)) %>% 
  ggplot(aes(x = srmrzsrzproc, y = srnaetiind100)) +
  geom_label(aes(label = kwintilnigrupi, color = as.factor(godina))) +
  geom_line(aes(group = factor(kwintilnigrupi)), 
            color = "gray", linetype = "dotted") + 
  scale_color_viridis_d(begin = 0.2, end = 0.8) +
  theme_bw() +
  xlab("МРЗ/СРС, %") + ylab("Наети по квинт. гр., 1 трим. 2013 = 100") +
  labs(color = "Първо\nтримесечие")

gnoinaeti1 | gnoinaeti2

ggsave("graphs/14_noi_naeti.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/14_noi_naeti.png", width=8, height=4, type="cairo-png")
ggsave("graphs/14_noi_naeti.svg", width=8, height=4, device = svg)

# Правим сезонно изглаждане на данните
# на наетите и заплатите

glimpse(noisektori.df)

noisektoriadjusted.df <-
  noisektori.df %>% 
  group_by(sektor) %>% 
  mutate(tsnaetiseasonal = 
         decompose(ts(naeti, start = 2013, frequency = 4))$seasonal,
       tsnaetiadjusted = naeti - tsnaetiseasonal) %>% 
  mutate(tssrzaplseasonal = 
           decompose(ts(srzapl, start = 2013, frequency = 4))$seasonal,
         tssrzapladjusted = srzapl - tssrzaplseasonal) %>% 
  select(sektor, periyod = period, kwintilnigrupi, 
         tsnaetiadjusted, tssrzapladjusted,
         mz, mzchanges)

noisektoriadjusted.df <-
  noisektoriadjusted.df %>% 
  mutate(mrzprocadj = mz/tssrzapladjusted) %>% 
  group_by(sektor) %>% 
  mutate(ind100naeti = tsnaetiadjusted/first(tsnaetiadjusted)*100) %>% 
  ungroup

glimpse(noisektoriadjusted.df)

noisektorisumm.df <-
  noisektoriadjusted.df %>% 
  group_by(kwintilnigrupi, periyod) %>% 
  summarise(naetiwkwintgrupa = sum(tsnaetiadjusted),
            srmrzprocadj = mean(mrzprocadj),
            srind100naeti = mean(ind100naeti))
  
noisektorisumm.df %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = srind100naeti, 
                color = as.factor(kwintilnigrupi))) +
  geom_vline(aes(xintercept = periyod), 
             data=noisektoriadjusted.df %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  geom_label(aes(x = periyod, y = mzchanges + 105, label = paste(mzchanges, "%")), 
             data = noisektoriadjusted.df %>% filter(mzchanges > 0)) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(color = "Квинт.\nгрупи") +
  xlab("") + ylab("Наети, 1 трим. 2013 = 100")

ggsave("graphs/14_noi_naeti_kwintilni.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/14_noi_naeti_kwintilni.png", width=8, height=4, type="cairo-png")
ggsave("graphs/14_noi_naeti_kwintilni.svg", width=8, height=4, device = svg)

noisektorisumm.df %>% 
  filter(kwintilnigrupi %in% c(1,5)) %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = srind100naeti, 
                color = as.factor(kwintilnigrupi))) +
  geom_vline(aes(xintercept = periyod), 
             data=noisektoriadjusted.df %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  geom_vline(aes(xintercept = periyod), 
             data=noisektoriadjusted.df %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  geom_label(aes(x = periyod, y = mzchanges + 105, label = paste(mzchanges, "%")), 
             data = noisektoriadjusted.df %>% filter(mzchanges > 0)) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(color = "Квинт.\nгрупи") +
  xlab("") + ylab("Наети, 1 трим. 2013 = 100")

ggsave("graphs/14_noi_naeti_kwintilni_1_i_5.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/14_noi_naeti_kwintilni_1_i_5.png", width=8, height=4, type="cairo-png")
ggsave("graphs/14_noi_naeti_kwintilni_1_i_5.svg", width=8, height=4, device = svg)

noisektorisumm.df %>% filter(kwintilnigrupi == 5) %>% 
  arrange(-srmrzprocadj)

  gn1i10 <- 
  noisektori.df %>% 
  group_by(kwintilnigrupi,period) %>% 
  summarise(srnpromdecgr = mean(naetiprprom)) %>% 
  ungroup %>% 
  filter(kwintilnigrupi %in% c(1,5)) %>% 
  mutate(kwintilnigrupi = as.factor(kwintilnigrupi)) %>% 
  ggplot(aes(period,srnpromdecgr, color = kwintilnigrupi)) + geom_line() +
  scale_color_viridis_d() +
  theme_bw() +
  geom_vline(xintercept = mrzquarterlychangepoints$period[6:11], linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme(legend.position = c(0.92, 0.75),
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  xlab("") + ylab("Наети, промяна (%)") +
  labs(color = "Дец.група")

gnr1i10 <- 
  noisektori.df %>% 
  group_by(decilnigrupi,period) %>% 
  summarise(srnpromdecgr = mean(naetiprprom)) %>% 
  ungroup %>% 
  group_by(period) %>% 
  summarise(razlika = srnpromdecgr[decilnigrupi == 1] - srnpromdecgr[decilnigrupi == 10]) %>% 
  ggplot(aes(period,razlika)) + geom_line() +
  theme_bw() +
  geom_vline(xintercept = mrzquarterlychangepoints$period[6:11], linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") + ylab("Разлика")

# Графика 19

(gn1i10 / gnr1i10) + plot_layout(heights = c(2, 1))

# did

# ИКОНОМЕТРИЧНО ИЗСЛЕДВАНЕ
# НОИ тримесечни данни по сектори
# преомахнати са най-волатилните сектори (отвъд 90-я перцентил)
# и секторите с по-малко от 5 000 наети на година

gdpnacizgl.df <- read.csv("data/gdpquarterly_nsi_izgladeni.csv", sep=";", header = F)

gdpnacizgl.df <-
  gdpnacizgl.df %>% 
  select(periyod = V1, gdpceni2015izgl = V5)

gdpnac.df <- read.csv("data/gdpquarterly_nacionalno.csv", sep=";", header = F)

gdpnac.df <- 
  gdpnac.df %>% 
  select(periyod = V1, pokazatel = V3, stojnost = V5) %>% 
  spread(pokazatel, stojnost) %>% 
  rename(gdpceni2015 = 2, gdpcenipredhg = 3, gdptekushticeni = 4) %>% 
    inner_join(gdpnacizgl.df) %>% 
    mutate(periyod = str_remove(periyod,"Q"),
           periyod = as.yearqtr(periyod,format = "%Y%q"))

gdpnac.df

gdpnac.df <- 
  gdpnac.df %>% 
  mutate(gdpceni2015seasonal = 
  decompose(ts(gdpceni2015, start = 1993, frequency = 4))$seasonal,
  gdpceni2015izglR = gdpceni2015 - gdpceni2015seasonal) %>% 
  mutate(gdptekushticeniseasonal = 
           decompose(ts(gdptekushticeni, start = 1993, frequency = 4))$seasonal,
         gdptekushticeniizgl = gdptekushticeni - gdptekushticeniseasonal) %>% 
  select(-gdpceni2015seasonal, -gdptekushticeniseasonal)
  
glimpse(noisektori.df)

noisektori.df %>% tabyl(sektor, period)
noisektori.df %>% 
  group_by(sektor) %>% 
  summarise(srednomrzproc = mean(mrzproc),
            srednonaeti = mean(naeti)) %>% 
  arrange(-srednomrzproc)

nasnac1564only.df

forpanel_noisektori_trimesechni.df <- 
  noisektori.df %>% 
  rename(periyod = period) %>% 
  ungroup %>% 
  inner_join(gdpnac.df) %>% 
  inner_join(nasnac1564only.df) %>% 
 # select(sektor, period, mz, srzapl, mrzproc, naeti, mzchanges) %>% 
  arrange(sektor, periyod) %>% 
  group_by(sektor) %>% 
  mutate(lagged1naeti = dplyr::lead(naeti),
         lagged2naeti = dplyr::lead(naeti, n=2),
         laggedminus1naeti = dplyr::lag(naeti)) %>% 
  ungroup

noisektori_trimesechni.panel <- pdata.frame(forpanel_noisektori_trimesechni.df, index = c("sektor","periyod"))

# двупосочни ефекти - МРЗ - наети
# неизгладени тримесечни стойности за заплати и наети

summary(plm(log(naeti) ~ log(mrzproc), data = noisektori_trimesechni.panel, effect = "twoways"))
summary(plm(log(lagged1naeti) ~ log(mrzproc), data = noisektori_trimesechni.panel, effect = "twoways"))
summary(plm(log(naeti) ~ log(mrzproc) + log(brojnas1564), data = noisektori_trimesechni.panel, effect = "twoways"))

noisektori_trimesechni.panel

# индивидуални ефекти по сектори с контрол население и БВП

gdpnac.df

summary(plm(log(naeti) ~ log(mrzproc) + log(brojnas1564) + log(gdpceni2015), data = noisektori_trimesechni.panel, effect = "individual"))
summary(plm(log(lagged1naeti) ~ log(mrzproc) + log(brojnas1564) + log(gdpceni2015), data = noisektori_trimesechni.panel, effect = "individual"))
summary(plm(log(lagged2naeti) ~ log(mrzproc) + log(brojnas1564) + log(gdpceni2015), data = noisektori_trimesechni.panel, effect = "individual"))


# МРЗ и заплати

mod_zapl_sektori_noi_indfe <- plm(log(srzapl) ~ log(mz) + log(gdptekushticeni), 
                                  data = noisektori_trimesechni.panel, effect = "individual")
summary(mod_zapl_sektori_noi_indfe)
coef(mod_zapl_sektori_noi_indfe)

# Тест за серийна корелация
pwartest(mod_zapl_sektori_noi_indfe)
pbgtest(mod_zapl_sektori_noi_indfe)

#coeftest(mod_zapl_sektori_noi_indfe, vcovHC, df = Inf)
#coeftest(mod_zapl_sektori_noi_indfe, vcov = vcovHC, type = "HC1")
coeftest(mod_zapl_sektori_noi_indfe, 
         vcov = vcovHC, cluster = "group", 
         method = "arellano") # "arellano" corrects for both heteroskedasticity and autocorrelation.

se_modnoiz <-
  sqrt(diag(vcovHC(mod_zapl_sektori_noi_indfe, cluster = "group", 
       method = "arellano")))

summary(mod_zapl_sektori_noi_indfe)

summary(mod_zapl_sektori_noi_indfe, 
        vcov = vcovHC, cluster = "group", method = "arellano")

summary(lfe::felm(log(srzapl) ~ 
                    log(mz) + log(gdptekushticeni) | sektor | 0 | sektor, 
                  data = forpanel_noisektori_trimesechni.df))

modelpgmmzaplnoi <- 
  pgmm(log(srzapl) ~ log(mz) + log(laggedminus1naeti) + log(gdptekushticeni)  | 
         lag(log(srzapl), 2:24), data = noisektori_trimesechni.panel, 
                         effect = "individual", model = "twostep", transformation = "d")


summary(modelpgmmzaplnoi, robust = TRUE)

forpanel_noisektori_trimesechni.df %>% 
  filter(sektor == 2)

# изгладени тримесечни данни от НОИ

glimpse(noisektoriadjusted.df)

noisektoriadjusted.df %>% tail

forpanel_noisektoriadj_trimesechni.df <- 
  noisektoriadjusted.df %>% 
  ungroup %>% 
  inner_join(gdpnac.df) %>% 
  inner_join(nasnac1564only.df) %>% 
  arrange(sektor, periyod) %>% 
  group_by(sektor) %>% 
  mutate(lagged1naeti = dplyr::lead(tsnaetiadjusted),
         lagged2naeti = dplyr::lead(tsnaetiadjusted, n=2),
         laggedminus1naeti = dplyr::lead(tsnaetiadjusted)) %>% 
  ungroup

noisektori_trimesechni_adj.panel <- pdata.frame(forpanel_noisektoriadj_trimesechni.df, 
                                                index = c("sektor","periyod"))

noisektori_trimesechni_adj.panel

for_panel_mz_noi <- 
  forpanel_noisektoriadj_trimesechni.df %>% 
  select(sektor, periyod, tssrzapladjusted, mz, gdptekushticeniizgl)

glimpse(forpanel_noisektori_trimesechni.df)

for_panel_mz_noi_orig <- 
  forpanel_noisektori_trimesechni.df %>% 
  select(sektor, periyod, srzapl, mz, gdptekushticeni)

for_panel_mz_noi_orig
#write.csv(for_panel_mz_noi_orig, "data/temp_panel_mz_noi_orig.csv", row.names = F)

for_panel_mz_noi <- 
  forpanel_noisektori_trimesechni.df %>% 
  select(sektor, periyod, tssrzapladjusted, mz, gdptekushticeniizgl)
#write.csv(for_panel_mz_noi, "data/temp_panel_mz_noi_adj.csv", row.names = F)

for_panel_mz_noi

mz_noi.panel <- pdata.frame(for_panel_mz_noi, index = c('sektor', 'periyod'))

mz_noi.panel

summary(plm(log(tsnaetiadjusted) ~ log(mrzprocadj), data = noisektori_trimesechni_adj.panel, effect = "twoways"))
summary(plm(log(lagged1naeti) ~ log(mrzprocadj), data = noisektori_trimesechni_adj.panel, effect = "twoways"))
summary(plm(log(tsnaetiadjusted) ~ log(mrzprocadj) + log(brojnas1564), data = noisektori_trimesechni_adj.panel, effect = "twoways"))

mod_zapl_sektori_noi_indfe_adj <-
  plm(log(tssrzapladjusted) ~ log(mz) + log(gdptekushticeniizgl), 
    data = noisektori_trimesechni_adj.panel, effect = "individual")

summary(mod_zapl_sektori_noi_indfe_adj)

pbgtest(mod_zapl_sektori_noi_indfe_adj)

coeftest(mod_zapl_sektori_noi_indfe_adj, vcov = vcovHC, 
         cluster = "group", method = "arellano") # "arellano" corrects for both heteroskedasticity and autocorrelation.

se_modnoiz_adj <-
  sqrt(diag(vcovHC(mod_zapl_sektori_noi_indfe_adj, cluster = "group", 
                   method = "arellano")))

# Wald test to calculate F-values fo vcovHC
# temp <- 
  plm::pwaldtest(mod_zapl_sektori_noi_indfe_adj, 
               test = "F", vcov = function(x) vcovHC(x, cluster = "group", 
                                                     method = "arellano"))

summary(mod_zapl_sektori_noi_indfe)

# Заплати - иконометрични модели - срз ~ мрз, НОИ тримесечни с коригирани
# стандартни грешки, с ръчно добавени F стойности

stargazer(mod_zapl_sektori_noi_indfe, 
          mod_zapl_sektori_noi_indfe_adj, 
          se        = list(se_modnoiz, se_modnoiz_adj),
          omit.stat = "f",
          add.lines = list(c("F Statistic (df = 2; 51)", "483.61***", "461.22***")),
          digits=3,
          type = "html",
          out="star_tables.doc")
          
  

# summary                                                                                                       
summary(mod_zapl_sektori_noi_indfe_adj)
# summary with vcovHC and F-values

summary(mod_zapl_sektori_noi_indfe_adj, 
        vcov = vcovHC, cluster = "group", method = "arellano")

screenreg(mod_zapl_sektori_noi_indfe_adj)
screenreg(mod_zapl_sektori_noi_indfe_adj, 
          vcov = vcovHC, cluster = "group", method = "arellano")

plm::pwaldtest(mod_zapl_sektori_noi_indfe_adj, 
               test = "F", vcov = function(x) vcovHC(x, cluster = "group", 
                                                     method = "arellano"))

modelpgmmzaplnoiadj <- 
  pgmm(log(tssrzapladjusted) ~ log(mz) + log(laggedminus1naeti) + 
         log(gdptekushticeniizgl)  | 
         lag(log(tssrzapladjusted), 2:4), data = noisektori_trimesechni_adj.panel, 
       effect = "individual", model = "twostep", transformation = "d")

summary(modelpgmmzaplnoiadj)

screenreg(list(modelpgmmzaplnoi, modelpgmmzaplnoiadj))

summary(modelpgmmzaplnoiadj)

stargazer(modelpgmmzaplnoi, 
          modelpgmmzaplnoiadj, 
          digits=3,
          type = "html",
          out="star_tables.doc")

nzki

summary(modelpgmmzaplnoi)

# разлика в разликите НОИ

glimpse(noisektori.df)
noisektori.df %>% arrange(kwintilnigrupi)

  noisektori.df %>% 
  filter(kwintilnigrupi == 5,
         godina == 2013, 
         quarter == 1)

did_noisektori_2015.df <-
  noisektori.df %>% 
  filter(kwintilnigrupi %in% c(1,2,4,5),
         godina == 2015,
         quarter %in% c(2,3)) %>% 
  mutate(treatperiod = case_when(quarter == 2 ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(kwintilnigrupi == 1 ~ 0,
                                kwintilnigrupi == 2 ~ 0,
                                TRUE ~ 1))

did_noisektori_2015.df %>% filter(treatgroup == 1) %>% arrange(mrzprocsredno)

did_noisektori_2015_1.df <-
  noisektori.df %>% 
  filter(kwintilnigrupi %in% c(1,5),
         godina == 2015,
         quarter %in% c(2,3)) %>% 
  mutate(treatperiod = case_when(quarter == 2 ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(kwintilnigrupi == 1 ~ 0,
                                TRUE ~ 1))

did_noisektori_2015_1.df %>% filter(treatgroup == 0) %>% arrange(-mrzproc)

did_noisektori_2015.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednonaeti = mean(naeti)) %>% 
  spread(key = treatperiod, value = srednonaeti) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

did_noisektori_2015_1.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednonaeti = mean(naeti)) %>% 
  spread(key = treatperiod, value = srednonaeti) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(naeti) ~ treatperiod*treatgroup, data = did_noisektori_2015.df))

summary(lm(log(naeti) ~ treatperiod*treatgroup, data = did_noisektori_2015_1.df))

did_noisektori_2015.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednazaplata = mean(srzapl)) %>% 
  spread(key = treatperiod, value = srednazaplata) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

did_noisektori_2015_1.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednazaplata = mean(srzapl)) %>% 
  spread(key = treatperiod, value = srednazaplata) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(srzapl) ~ treatperiod*treatgroup, data = did_noisektori_2015.df))

summary(lm(log(srzapl) ~ treatperiod*treatgroup, data = did_noisektori_2015_1.df))


# Увеличение през януари 2016 г.

did_noisektori_2016.df <-
  noisektori.df %>% 
  filter(kwintilnigrupi %in% c(1,2,4,5),
         period > "2015 Q3",
         period < "2016 Q2") %>% 
  mutate(treatperiod = case_when(period == "2015 Q4" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(kwintilnigrupi == 1 ~ 0,
                                kwintilnigrupi == 2 ~ 0,
                                TRUE ~ 1))

did_noisektori_2016_1.df <-
  noisektori.df %>% 
  filter(kwintilnigrupi %in% c(1,5),
         period > "2015 Q3",
         period < "2016 Q2") %>% 
  mutate(treatperiod = case_when(period == "2015 Q4" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(kwintilnigrupi == 1 ~ 0,
                                TRUE ~ 1))


did_noisektori_2016.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednonaeti = mean(naeti)) %>% 
  spread(key = treatperiod, value = srednonaeti) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

did_noisektori_2016_1.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednonaeti = mean(naeti)) %>% 
  spread(key = treatperiod, value = srednonaeti) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(naeti) ~ treatperiod*treatgroup, data = did_noisektori_2016.df))
summary(lm(log(naeti) ~ treatperiod*treatgroup, data = did_noisektori_2016_1.df))

did_noisektori_2016.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednazaplata = mean(srzapl)) %>% 
  spread(key = treatperiod, value = srednazaplata) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

did_noisektori_2016_1.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednazaplata = mean(srzapl)) %>% 
  spread(key = treatperiod, value = srednazaplata) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(srzapl) ~ treatperiod*treatgroup, data = did_noisektori_2016.df))
summary(lm(log(srzapl) ~ treatperiod*treatgroup, data = did_noisektori_2016_1.df))

# Увеличение на МРЗ през януари 2018 г.

did_noisektori_2018.df <-
  noisektori.df %>% 
  filter(kwintilnigrupi %in% c(1,2,4,5),
         period > "2017 Q3",
         period < "2018 Q2") %>% 
  mutate(treatperiod = case_when(period == "2017 Q4" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(kwintilnigrupi == 1 ~ 0,
                                kwintilnigrupi == 2 ~ 0,
                                TRUE ~ 1))

did_noisektori_2018_1.df <-
  noisektori.df %>% 
  filter(kwintilnigrupi %in% c(1,5),
         period > "2017 Q3",
         period < "2018 Q2") %>% 
  mutate(treatperiod = case_when(period == "2017 Q4" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(kwintilnigrupi == 1 ~ 0,
                                  TRUE ~ 1))

did_noisektori_2018.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednonaeti = mean(naeti)) %>% 
  spread(key = treatperiod, value = srednonaeti) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

did_noisektori_2018_1.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednonaeti = mean(naeti)) %>% 
  spread(key = treatperiod, value = srednonaeti) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(naeti) ~ treatperiod*treatgroup, data = did_noisektori_2018.df))
summary(lm(log(naeti) ~ treatperiod*treatgroup, data = did_noisektori_2018_1.df))

did_noisektori_2018.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednazaplata = mean(srzapl)) %>% 
  spread(key = treatperiod, value = srednazaplata) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

did_noisektori_2018_1.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednazaplata = mean(srzapl)) %>% 
  spread(key = treatperiod, value = srednazaplata) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(srzapl) ~ treatperiod*treatgroup, data = did_noisektori_2018.df))
summary(lm(log(srzapl) ~ treatperiod*treatgroup, data = did_noisektori_2018_1.df))

  did_noisektori_2018_1.df <-
  noisektori.df %>% 
  filter(kwintilnigrupi %in% c(1,5),
         period > "2017 Q3",
         period < "2018 Q2") %>% 
  mutate(treatperiod = case_when(period == "2017 Q4" ~ 0,
                                 TRUE ~ 1),
         treatgroup = case_when(kwintilnigrupi == 1 ~ 0,
                                              TRUE ~ 1))


did_noisektori_2018_1.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednonaeti = mean(naeti)) %>% 
  spread(key = treatperiod, value = srednonaeti) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(naeti) ~ treatperiod*treatgroup, data = did_noisektori_2018_1.df))

did_noisektori_2018_1.df %>% 
  group_by(treatperiod, treatgroup) %>% 
  summarize(srednazaplata = mean(srzapl)) %>% 
  spread(key = treatperiod, value = srednazaplata) %>% 
  select(treatgroup, pre = 2, post = 3) %>% 
  mutate(razlika = post - pre,
         razlikaproc = razlika/pre*100)

summary(lm(log(srzapl) ~ treatperiod*treatgroup, data = did_noisektori_2018_1.df))

###
# Месечни данни по икономически дейности от НСИ
###

meszkid21.df <- read.csv("data/mesechniKID21zaplata.csv", sep = ";", header = F)
mesnkid21.df <- read.csv("data/mesechniKID21naeti.csv", sep = ";", header = F)

head(mesnkid21.df)
head(meszkid21.df)

mesnkid21.df <- 
  mesnkid21.df %>% 
  select(sektor = V1, periyod = V2, naeti = V3)

meszkid21.df <- 
  meszkid21.df %>% 
  select(sektor = V1, periyod = V2, meszapl = V4)

mesnzkid21.df <- 
  inner_join(mesnkid21.df, meszkid21.df)

mesnzkid21.df <- 
  mesnzkid21.df %>% 
  mutate(godina = substr(periyod,1,4),
         month = substr(periyod,5,8),
         month = utils:::.roman2numeric(month),
         periyod = paste0(godina,"-",month,"-","1"),
         periyod = as.Date(periyod, format = "%Y-%m-%d"))

glimpse(mesnzkid21.df)

mrzmonthly

mesnzkid21.df <- 
  mrzmonthly %>% 
    mutate(periyod = paste0(godina,"-",mesec,"-1"), 
    periyod = as.Date(periyod, format = "%Y-%m-%d")) %>% 
    select(periyod,mrz) %>% 
  inner_join(mesnzkid21.df) %>% 
  arrange(sektor,periyod) %>% 
  mutate(mrzproc = round(mrz/meszapl*100,1)) %>% 
  group_by(sektor) %>% 
  mutate(naetiprprom = (naeti - dplyr::lag(naeti))/dplyr::lag(naeti)*100,
         mrzprprom = mrzproc - dplyr::lag(mrzproc),
         ind100naeti2008 = naeti/first(naeti)*100,
         tsnaetiadjusted = 
           naeti - decompose(ts(naeti, start = 2008, frequency = 12))$seasonal,
         tsmeszapladjusted = 
           meszapl - decompose(ts(meszapl, start = 2008, frequency = 12))$seasonal,
         ind100naeti2008adjusted = tsnaetiadjusted/first(tsnaetiadjusted)*100,
         mrzprocadjusted = mrz/tsmeszapladjusted*100)

mesnzkid21.df %>% 
  ggplot(aes(sektor,mrzproc)) + geom_boxplot()

mesnzkid21.df %>% 
  ggplot(aes(sektor,naetiprprom)) + geom_boxplot()

mesnzkid21.df %>% 
  ggplot(aes(mrzproc,naetiprprom, color = sektor)) + 
  geom_point() + 
  scale_color_viridis_d()

mesnzkid21.df %>% 
  ggplot(aes(mrzprprom,naetiprprom, color = sektor)) + 
  geom_point() + 
  scale_color_viridis_d()

mesnzkid21.df %>% 
  filter(sektor %in% c("I","N","J","K")) %>% 
  ggplot(aes(periyod,naetiprprom)) +
  geom_line(aes(color = sektor), show.legend = F) +
  scale_color_viridis_d() +
  facet_wrap(sektor ~ .) +
  geom_vline(xintercept = mrzchanges$period, linetype = "dashed") +
  theme_bw()

glimpse(mesnzkid21.df)

gkid21naeti1 <-
  mesnzkid21.df %>% 
  filter(godina == 2018, month == 1, sektor != 0) %>% 
  ggplot(aes(x = mrzproc, y = ind100naeti2008)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("МРЗ/СРЗ") + ylab("Наети към ян. 2018, ян. 2008 = 100")

gkid21naeti2 <-
  mesnzkid21.df %>% 
  filter(godina %in% c(2008,2018),
         month == 1, sektor != 0) %>% 
  ggplot(aes(x = mrzproc, y = ind100naeti2008, color = godina)) +
  geom_label(aes(label = sektor)) +
  geom_line(aes(group = sektor), linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_color_viridis_d(begin = 0.2, end = 0.8) +
  labs(color = "Януари") +
  xlab("МРЗ/СРЗ, %") +
  ylab("Наети към ян. 2018, ян. 2008 = 100")

gkid21naeti1 | gkid21naeti2 

ggsave("graphs/15_naeti_kid21_2018.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/15_naeti_kid21_2018.png", width=8, height=4, type="cairo-png")
ggsave("graphs/15_naeti_kid21_2018.svg", width=8, height=4, device = svg)

mesnzkid21.df %>% 
  filter(godina %in% c(2008,2018),
         month == 1, sektor != 0) %>% 
  ggplot(aes(x = mrzproc, y = ind100naeti2008, color = godina)) +
  geom_label(aes(label = sektor)) +
  geom_line(aes(group = sektor), linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_color_viridis_d(begin = 0.2, end = 0.8) +
  labs(color = "Януари") +
  xlab("МРЗ/СРЗ, %") +
  ylab("Наети към ян. 2018, ян. 2008 = 100")

ggsave("graphs/15_naeti_kid21_2018_only.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/15_naeti_kid21_2018_only.png", width=8, height=4, type="cairo-png")
ggsave("graphs/15_naeti_kid21_2018_only.svg", width=8, height=4, device = svg)

mesnzkid21.df %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = ind100naeti2008adjusted)) +
  geom_label(aes(x = periyod, y = mrzprocadjusted, 
                 label = paste0(round(mrzprocadjusted, 0),"%"),
                 size = mrzprocadjusted), 
             data = mesnzkid21.df %>% filter(periyod == as.Date("2014-01-01")),
             alpha = 0.3) +
  facet_wrap(. ~ sektor) +
  theme_bw() +
  labs(size = "МРЗ/СРЗ, %\nкъм ян. 2014") +
  xlab("") + ylab("Наети, януари 2008 = 100") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y")

ggsave("graphs/15_kid21_2008_2018.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/15_kid21_2008_2018.png", width=8, height=4, type="cairo-png")
ggsave("graphs/15_kid21_2008_2018.svg", width=8, height=4, device = svg)

mesnzkid21.df <-
  mesnzkid21.df %>% 
  group_by(sektor) %>% 
  mutate(minzaplprom = (mrz - dplyr::lag(mrz))/mrz*100) %>% 
  ungroup

gmrznational1 <-
  mesnzkid21.df %>% 
  filter(sektor == 0) %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = mrz), color = "gray") +
  geom_line(aes(x = periyod, y = tsmeszapladjusted)) +
geom_text(aes(x = periyod, y = tsmeszapladjusted - 100, label = "СРЗ"),
          data = mesnzkid21.df %>% 
            filter(sektor == 0,
                   periyod == as.Date("2018-09-01", format("%Y-%m-%d")))) +
geom_text(aes(x = periyod, y = mrz - 100, label = "МРЗ"), color = "gray",
          data = mesnzkid21.df %>% 
            filter(sektor == 0,
                   periyod == as.Date("2018-09-01", format("%Y-%m-%d")))) +
  geom_vline(aes(xintercept = periyod),
             data=mesnzkid21.df %>% filter(minzaplprom > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  xlab("") + ylab("Мин. и средна заплата, лв.")

  gmrznational2 <-
  mesnzkid21.df %>% 
  filter(sektor == 0) %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = mrzprocadjusted)) +
  geom_vline(aes(xintercept = periyod),
             data=mesnzkid21.df %>% filter(minzaplprom > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  xlab("") + ylab("МРЗ/СРЗ, %")

(gmrznational1 / gmrznational2) + plot_layout(heights = c(2, 1))

ggsave("graphs/15_mrz_srz_national.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/15_mrz_srz_national.png", width=8, height=4, type="cairo-png")
ggsave("graphs/15_mrz_srz_national.svg", width=8, height=4, device = svg)

gmrznationalnaeti <- 
  mesnzkid21.df %>% 
  filter(sektor == 0) %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = tsnaetiadjusted/1000000)) +
  geom_vline(aes(xintercept = periyod),
             data=mesnzkid21.df %>% filter(minzaplprom > 0),
             linetype = "dashed", color = "gray") +
    theme_bw() +
  xlab("") + ylab("Наети, млн.")

(gmrznationalnaeti / gmrznational2) + plot_layout(heights = c(2, 1))

ggsave("graphs/15_mrz_naeti_national.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/15_mrz_naeti_national.png", width=8, height=4, type="cairo-png")
ggsave("graphs/15_mrz_naeti_national.svg", width=8, height=4, device = svg)

mesnzkid21.df %>% 
  filter(sektor == "C") %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = mrz), color = "gray") +
#  geom_line(aes(x = periyod, y = meszapl)) +
  geom_line(aes(x = periyod, y = tsmeszapladjusted), color = "blue") +
  geom_vline(aes(xintercept = periyod),
             data=mesnzkid21.df %>% filter(minzaplprom > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  xlab("") + ylab("Мин. и средна заплата, лв.")

mesnzkid21.df %>% 
  filter(sektor == "C") %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = mrzprocadjusted)) +
  geom_vline(aes(xintercept = periyod),
             data=mesnzkid21.df %>% filter(minzaplprom > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  xlab("") + ylab("МРЗ/СРЗ, %")

glimpse(mesnzkid21.df)

naetimeskid21 <- 
  seas(ts(mesnzkid21.df[mesnzkid21.df$sektor == 0, ]$naeti, 
   start = 2008, frequency = 12))

autoplot(naetimeskid21)

###
# Наети на непълно работно време
###
  
naetinepylno.df <- read.csv("data/naetipylnonepylnoquarterly.csv", sep = ";", header = F)

str(naetinepylno.df)

naetidqlnepylno.df <- 
  naetinepylno.df %>% 
  select(widnaeti = V1, period = V3, broj = V4) %>% 
  mutate(widnaeti = str_remove(widnaeti, "Наети на "),
         widnaeti = str_remove(widnaeti, " работно време")) %>% 
  group_by(period) %>% 
  summarise(dqlnepylno = (broj[widnaeti == "непълно"])/(broj[widnaeti == "Общо"])*100)

naetidqlnepylno.df <- 
  naetidqlnepylno.df %>%
  mutate(period = str_remove(period,"Q"),
         period = as.yearqtr(period, format = "%Y%q")) %>% 
  arrange(period) %>% 
    filter(period < "2019 Q1", period >= "2008 Q1")

mrzquarterly

mrzqchpoints <- 
  mrzquarterly %>% 
  filter(period < "2019 Q1", period >= "2008 Q1") %>% 
  select(period)


dqlnepylnots <- 
  ts(naetidqlnepylno.df$dqlnepylno, frequency = 4, start = 2008)

autoplot(dqlnepylnots)

dqlnepylnots %>% 
  changepoint:: cpt.meanvar() %>%  # Identify change points
  autoplot() + theme_bw()

strucchange::breakpoints(dqlnepylnots ~ 1) %>%
  autoplot() + theme_bw()

# НАЕТИ ЛИЦА ПО ВИД НА ДОГОВОРА С РАБОТОДАТЕЛЯ

naetibezdog.df <- read.csv("data/naetibezdogoworquarterly.csv", sep = ";", header = F)

glimpse(naetibezdog.df)

bezdogdql.df <- 
  naetibezdog.df %>% select(widnaeti = V1, period = V2, broj = V4) %>% 
  group_by(period) %>% 
  summarise(bezdogowordql = (broj[widnaeti == "Без договор"])/(broj[widnaeti == "Общо"])*100) %>% 
  mutate(period = str_remove(period, "Q"),
         period = as.yearqtr(period, format = "%Y%q")) %>% 
  filter(period < "2019 Q1", period >= "2008 Q1")

glimpse(bezdogdql.df)

bezdogowordqlts <- 
  ts(bezdogdql.df$bezdogowordql, frequency = 4, start = 2008)

autoplot(bezdogowordqlts)

bezdogowordqlts %>% 
  changepoint:: cpt.meanvar() %>%  # Identify change points
  autoplot() + theme_bw()

strucchange::breakpoints(bezdogowordqlts ~ 1) %>%
  autoplot() + theme_bw()


# НАЕТИ ЛИЦА СПОРЕД ВИДА НА РАБОТАТА (ПОСТОЯННА, ВРЕМЕННА)

wremennonaeti.df <- read.csv("data/naetiwremennaquarterly.csv", sep = ";", header = F)

glimpse(wremennonaeti.df)

wremennonaetidql.df <- 
  wremennonaeti.df %>% select(widnaeti = V1, period = V3, broj = V4) %>% 
  spread(key = widnaeti, value = broj) %>% 
  rename(postoqnna = 2, wremenna = 3, obshto = 4) %>% 
  mutate(wremennadql = round(wremenna/obshto*100, 1),
         period = str_remove(period, "Q"),
         period = as.yearqtr(period, format = "%Y%q")) %>% 
  select(period, wremennadql)

wremennonaetidql.df
bezdogdql.df
naetidqlnepylno.df

wrbezdognepylno.df <- 
  wremennonaetidql.df %>% 
  inner_join(bezdogdql.df) %>% 
  inner_join(naetidqlnepylno.df)

wrbezdognepylno.df %>% 
  select(2:4) %>% 
  ts(frequency=4, start=2008) %>% 
  autoplot

wrbezdognepylno.df <- 
  wrbezdognepylno.df %>% 
  gather(key = widnaeti, value = dql, 2:4) %>% 
  group_by(widnaeti) %>% 
  mutate(dqlizgladeni = dql - 
           decompose(ts(dql, start = 2008, frequency = 4))$seasonal)

# Наети без договор, наети на непълно работно време и временно наети
# за периода от 2011 до 2018 г. (като дял спрямо всички наети)

# Графика 20

mrzquarterly <-
  mrzquarterly %>% 
  rename(periyod = period)

wrbezdognepylno.df %>% 
  ggplot() + 
  geom_line(aes(x=period, y=dql)) +
  geom_line(aes(x=period, y=dqlizgladeni), color = "gray") +
  xlab("") + ylab("Дял от всички наети, %") +
  facet_wrap(widnaeti ~ ., ncol =1, scale = "free_y",
             labeller = labeller(widnaeti = c(bezdogowordql = "Наети без договор",
                                              dqlnepylno = "Наети на непълно работно време",
                                              wremennadql = "Временно наети"))) +
  scale_x_yearqtr(n = 11, format = "%Y") +
  geom_vline(aes(xintercept = periyod), 
             data = mrzquarterly %>%  
               filter(periyod > "2008 Q4", periyod < "2019 Q1", mzchanges >0), 
             linetype = "dashed") +
  theme_bw()

?scale_x_yearqtr

ggsave("graphs/16_dql_wremenno_naeti.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/16_dql_wremenno_naeti.png", width=8, height=4, type="cairo-png")
ggsave("graphs/16_dql_wremenno_naeti.svg", width=8, height=4, device = svg)

# Като абсолютни стойности

naetinepylno.df

naetinepylno.df <-
  naetinepylno.df %>% 
  select(widnaeti = V1, period = V3, broj = V4) %>% 
  filter(widnaeti %in% c("Общо", "Наети на непълно работно време")) %>% 
  mutate(widnaeti = str_replace(widnaeti, "Общо", "Общо наети")) %>% 
  select(period, widnaeti, broj) %>% 
  mutate(period = str_replace(period, "Q", "-"),
         period = as.yearqtr(period)) %>% 
  filter(period > "2007 Q4", period < "2019 Q1")


naetibezdog.df <- 
  naetibezdog.df %>% select(widnaeti = V1, period = V2, broj = V4) %>% 
  filter(widnaeti == "Без договор") %>% 
  mutate(widnaeti = str_replace(widnaeti, "Без договор", "Наети без договор")) %>% 
  select(period, widnaeti, broj) %>% 
  mutate(period = str_replace(period, "Q", "-"),
         period = as.yearqtr(period)) %>% 
  filter(period > "2007 Q4", period < "2019 Q1")

wremennonaeti.df <- 
  wremennonaeti.df %>% select(widnaeti = V1, period = V3, broj = V4) %>% 
  filter(widnaeti == "Наети на работа за определен период от време") %>% 
  mutate(widnaeti = str_replace(widnaeti, "Наети на работа за определен период от време", "Временно наети")) %>% 
  select(period, widnaeti, broj) %>% 
  mutate(period = str_replace(period, "Q", "-"),
         period = as.yearqtr(period)) %>% 
  filter(period > "2007 Q4", period < "2019 Q1")

naetibezdogineyplno.df <- rbind(naetinepylno.df, naetibezdog.df, wremennonaeti.df)


naetibezdogineyplno.df <- 
  naetibezdogineyplno.df %>% 
  arrange(widnaeti, period) %>% 
  group_by(widnaeti) %>% 
  mutate(brojizgladeni =
         broj - decompose(ts(broj, start = 2008, frequency = 4))$seasonal,
         brojtrend = decompose(ts(broj, start = 2008, frequency = 4))$trend,
         ind100broj = broj/first(broj)*100,
         ind100brojizgladeni = brojizgladeni/first(brojizgladeni)*100,
         seasizgladeni = final(seas(ts(broj, start = 2008, frequency = 4))),
         ind100seasizgladeni = seasizgladeni/first(seasizgladeni)*100)

glimpse(naetibezdogineyplno.df)

mrzquarterly2008_2018 <- 
  mrzquarterly %>% 
  filter(periyod > "2007 Q4", periyod < "2019 Q1") 

mrzquarterly2008_2018 

naetibezdogineyplno.df

mrzmonthly

naetibezdogineyplno.df

naetibezdogineyplno.df %>% 
  ggplot() +
  geom_line(aes(x = period, y = ind100seasizgladeni, color = widnaeti)) +
  geom_vline(aes(xintercept = periyod), 
             data=mrzquarterly2008_2018 %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_color_viridis_d() +
  labs(color = "") +
  xlab("") + ylab("Наети, януари 2008=100") +
  scale_x_yearqtr(n = 11, format = "%Y")

# ggsave("graphs/16_wremenno_naeti.pdf", width=8, height=4, device = cairo_pdf)
# ggsave("graphs/16_wremenno_naeti.png", width=8, height=4, type="cairo-png")
# ggsave("graphs/16_wremenno_naeti.svg", width=8, height=4, device = svg)

# 
# Наети на национално ниво
# 

# само в частния сектор
# със самонаети като контрол

nasnac1564.df <- read.csv("data/naselenie_i_rs_do64_po_obrazowanie_trimesechni.csv", sep = ";", header = F)

nasnac1564.df <-
  nasnac1564.df %>% 
  select(wyzrast = V1, obrazowanie = V2, periyod = V3, edinici = V4, brojnas = V5)

samonaeti.df <- read.csv("data/samonaeti_i_naeti_w_chastni.csv", sep = ";", header = F)

samonaeti.df <- 
  samonaeti.df %>% 
  select(widzaetost = V2, periyod = V3, edinici = V4, brojzaeti = V5)

levels(samonaeti.df$widzaetost)

samonaeti.df <-
  samonaeti.df %>% 
  mutate(periyod = str_replace(periyod, "Q", "-"),
         periyod = as.yearqtr(periyod)) %>%
  filter(periyod >= 2003, periyod < 2020)

mrzquarterly
  
mrztrimesechni <-
  mrzquarterly %>% 
  filter(periyod >= 2003)

zaplatitrimesechni.df <- read.csv("data/zaplati_nacionalni_trimesechni.csv", 
                                  header = F, sep = ";")

zaplatitrimesechni.df <-
  zaplatitrimesechni.df %>% 
  mutate(periyod = paste(V2, V3),
         periyod = as.yearqtr(periyod)) %>% 
  select(periyod, zaplatakyde = V1, zaplatalw = V5) %>% 
  filter(periyod < 2020, periyod >= 2003)

zapltrimchasten.df <- 
  zaplatitrimesechni.df %>% 
  filter(zaplatakyde == "Частен сектор") %>% 
  select(periyod, zaplatachasten = zaplatalw)

zapltrimchasten.df <-
  zapltrimchasten.df %>% 
  inner_join(mrztrimesechni) %>% 
  mutate(mrzprocchasten = mz/zaplatachasten*100)

nasnac1564only.df <- 
  nasnac1564.df %>% 
  filter(str_detect(wyzrast, "64"),
         obrazowanie == "Общо") %>% 
  mutate(periyod = str_replace(periyod, "Q","-"),
         periyod = as.yearqtr(periyod)) %>% 
  select(periyod, brojnas1564 = brojnas) %>% 
  filter(periyod < 2020, periyod >= 2003)

samonaeti.df <-
  samonaeti.df %>% 
  inner_join(zapltrimchasten.df) %>% 
  inner_join(nasnac1564only.df) %>% 
  select(-edinici) %>% 
  mutate(widzaetost = str_replace(widzaetost, "Самостоятелно заети лица", "samonaeti"),
         widzaetost = str_replace(widzaetost, "Наети лица в частни предприятия", "naetichastno"),
         widzaetost = str_replace(widzaetost, "Наети лица в обществени предприятия", "naetiobshtestwen"),
         widzaetost = str_replace(widzaetost, "Работодатели", "rabotodateli")) %>% 
  spread(key = widzaetost, value = brojzaeti) %>% 
  mutate(naetichastno = naetichastno*1000,
         samonaeti = samonaeti*1000,
         naetiobshtestwen = naetiobshtestwen*1000,
         rabotodateli = rabotodateli*1000,
         brojnas1564 = brojnas1564*1000,
         seasizglnaetichastno = 
           final(seas(ts(naetichastno, start = 2003, frequency = 4))),
         seasizglsamonaeti = 
           final(seas(ts(samonaeti, start = 2003, frequency = 4))),
         seasizglnaetiobshtestwen = 
           final(seas(ts(naetiobshtestwen, start = 2003, frequency = 4))),
         seasizgladenrabotodateli = 
           final(seas(ts(rabotodateli, start = 2003, frequency = 4))))

samonaeti.df %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = seasizglnaetiobshtestwen/1000)) +
  geom_vline(aes(xintercept = periyod), 
             data=samonaeti.df %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_x_yearqtr(n=10, format="%Y") +
  xlab("") + ylab("Наети в обществени пр. (хил.)")

samonaeti.df %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = seasizgladenrabotodateli/1000)) +
  geom_vline(aes(xintercept = periyod), 
             data=samonaeti.df %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_x_yearqtr(n=10, format="%Y") +
  xlab("") + ylab("Работодатели (хил.)")

gnaetichastno <-
  samonaeti.df %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = seasizglnaetichastno/1000)) +
  geom_vline(aes(xintercept = periyod), 
           data=samonaeti.df %>% filter(mzchanges > 0),
           linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_x_yearqtr(n=10, format="%Y") +
  xlab("") + ylab("Наети в частни\nпредпр. (хил.)")

gsamonaeti <-
  samonaeti.df %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = seasizglsamonaeti/1000)) +
  geom_vline(aes(xintercept = periyod), 
             data=samonaeti.df %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_x_yearqtr(n=10, format="%Y") +
  xlab("") + ylab("Самонаети\n(хил.)")

gmrzsrzchastno <-
  samonaeti.df %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = mrzprocchasten)) +
  geom_vline(aes(xintercept = periyod), 
             data=samonaeti.df %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_x_yearqtr(n=10, format="%Y") +
  xlab("") + ylab("МРЗ/СРЗ в\nчастния сектор, %")

(gnaetichastno / gsamonaeti  / gmrzsrzchastno) + plot_layout(heights = c(1, 1, 1))

# ggsave("graphs/17_mrz_chasten_sektor.pdf", width=8, height=4, device = cairo_pdf)
# ggsave("graphs/17_mrz_chasten_sektor.png", width=8, height=4, type="cairo-png")
# ggsave("graphs/17_mrz_chasten_sektor.svg", width=8, height=4, device = svg)

samonaeti.df %>% 
  mutate(ind100chasten = seasizglnaetichastno/first(seasizglnaetichastno)*100,
         ind100samonaeti = seasizglsamonaeti/first(seasizglsamonaeti)*100,
         ind100obshtestwen = seasizglnaetiobshtestwen/first(seasizglnaetiobshtestwen)*100,
         ind100rabotodateli = seasizgladenrabotodateli/first(seasizgladenrabotodateli)*100,
         ind100nas = brojnas1564/first(brojnas1564)*100) %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = ind100chasten,
                color = "в частни пр.")) +
  geom_line(aes(x = periyod, y = ind100samonaeti, color = "самонаети")) +
  geom_line(aes(x = periyod, y = ind100rabotodateli, color = "работодатели")) +
  geom_line(aes(x = periyod, y = ind100obshtestwen, color = "в обществени пр.")) +
  geom_line(aes(x = periyod, y = ind100nas, color = "население")) +
  geom_vline(aes(xintercept = periyod), 
             data=samonaeti.df %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_color_manual(name = "Брой хора", 
                     values = c("в частни пр." = "#404788ff", "в обществени пр." = "#fde725ff",
                                "самонаети" = "#238a8dff", "работодатели" = "#55c667ff",
                                "население" = "#39568cff")) +
  xlab("") + ylab("1 тримесечие на 2013 г. = 100") +
  scale_x_yearqtr(n = 10, format = "%Y")

# ggsave("graphs/17_zaeti_po_status.pdf", width=8, height=4, device = cairo_pdf)
# ggsave("graphs/17_zaeti_po_status.png", width=8, height=4, type="cairo-png")
# ggsave("graphs/17_zaeti_po_status.svg", width=8, height=4, device = svg)

obrazrs.df <- read.csv("data/rs_obrazowanie_trimesechni.csv", header = F, sep = ";")
obrazzaeti.df <- read.csv("data/zaeti_po_obrazowanie_trimesechni.csv", header = F, sep = ";")
obrazkz.df <- read.csv("data/kz_po_obrazowanie_trimesechni.csv", header = F, sep = ";")
obrazbezr.df <- read.csv("data/bezrabotni_po_obrazowanie_trimesechni.csv", header = F, sep = ";")
obrazkbezr.df <- read.csv("data/kb_po_obrazowanie_trimesechni.csv", header = F, sep = ";")

obrazrs.df
obrazzaeti.df
obrazkz.df
obrazbezr.df
obrazkbezr.df

bezrselect.fn <-
  function(danni) {
    danni %>% 
      select(wyzgrupa = V1, obrazowanie = V2, periyod = V3, broj = V5) %>% 
      filter(str_detect(wyzgrupa, "15"),
             obrazowanie != "Общо") %>%
      select(periyod, obrazowanie, broj) %>% 
      mutate(periyod = str_replace(periyod, "Q", "-"),
             periyod = as.yearqtr(periyod)) %>% 
      group_by(obrazowanie) %>% 
      mutate(brojizgladeni = broj - decompose(ts(broj, start = 2003, frequency = 4))$seasonal,
             ind100brojizgl = brojizgladeni/first(brojizgladeni)*100)
}


plotbezr.fn <-
  function(danni,promenliwa) {
  danni %>% 
  ggplot() +
  geom_line(aes_string(x="periyod", y=promenliwa, color = "obrazowanie")) +
  scale_color_viridis_d() + theme_bw() +
  xlab("") + labs(color = "Образование") +
  geom_vline(aes(xintercept = periyod), 
                 data=samonaeti.df %>% filter(mzchanges > 0),
                 linetype = "dashed", color = "gray")
}

# Алтернативно само с aes и с .data[[promenliwa]]

# plotbezr.fn <-
#   function(danni,promenliwa) {
#     danni %>% 
#       ggplot() +
#       geom_line(aes(x=periyod, y=.data[[promenliwa]], color = obrazowanie)) +
#       scale_color_viridis_d() + theme_bw() +
#       xlab("") + labs(color = "Образование")
# }


# аргументът на колоната е в кавички

gzaetiobraz <-
  plotbezr.fn(bezrselect.fn(obrazzaeti.df), "ind100brojizgl") +
  ylab("Заети, 1 тр. 2003=100") +
  guides(color = FALSE)

gzaetiobrazkoef <-
  plotbezr.fn(bezrselect.fn(obrazkz.df), "brojizgladeni") +
  ylab("Коеф. на заетост")

gzaetiobraz / gzaetiobrazkoef

# ggsave("graphs/18_zaeti_po_obrazowanie.pdf", width=8, height=4, device = cairo_pdf)
# ggsave("graphs/18_zaeti_po_obrazowanie.png", width=8, height=4, type="cairo-png")
# ggsave("graphs/18_zaeti_po_obrazowanie.svg", width=8, height=4, device = svg)

gbezrobraz <-
  plotbezr.fn(bezrselect.fn(obrazbezr.df), "ind100brojizgl") +
  ylab("Безр., 1 тр. 2003=100") +
  guides(color = FALSE)

gbezrobrazkoef <-
  plotbezr.fn(bezrselect.fn(obrazkbezr.df), "brojizgladeni") +
  ylab("Коеф. на безр.")

gbezrobraz / gbezrobrazkoef

# ggsave("graphs/18_bezrabotni_po_obrazowanie.pdf", width=8, height=4, device = cairo_pdf)
# ggsave("graphs/18_bezrabotni_po_obrazowanie.png", width=8, height=4, type="cairo-png")
# ggsave("graphs/18_bezrabotni_po_obrazowanie.svg", width=8, height=4, device = svg)

# безработни по възраст

bezrwyzrastkoef.df <- read.csv("data/kb_po_wyzrast_trimesechni.csv", sep = ";", header = F)
bezrwyzrast.df <- read.csv("data/bezrabotni_po_wyzrast_trimesechni.csv", sep = ";", header = F)


bezrwyzrast.fn <-
  function(danni) {
  danni %>% 
    filter(V1 == "Общо") %>% 
    select(periyod = V3, wyzrast = V2, broj = V5) %>% 
    mutate(periyod = str_replace(periyod, "Q", "-"),
           periyod = as.yearqtr(periyod),
           broj = as.numeric(as.character(broj))) %>% 
    filter(wyzrast %in% c("Общо","15 - 29"),
             periyod < 2020, periyod >= 2003) %>% 
    group_by(wyzrast) %>% 
    mutate(brojizgladeni = 
             final(seas(ts(broj, start = 2004, frequency = 4))),
           ind100brojizgladeni = brojizgladeni/first(brojizgladeni)*100)
}


gbezrwyzrastind100 <-
  bezrwyzrast.fn(bezrwyzrast.df) %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = ind100brojizgladeni, color = wyzrast)) +
  scale_color_viridis_d(begin = 0.2, end = 0.8) + theme_bw() +
  xlab("") + labs(color = "Възраст") +
  geom_vline(aes(xintercept = periyod), 
             data=samonaeti.df %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  ylab("Безр., 1 тр. 2003=100") +
  guides(color = FALSE)

gbezwyzrastkoef <-
  bezrwyzrast.fn(bezrwyzrastkoef.df) %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = brojizgladeni, color = wyzrast)) +
  scale_color_viridis_d(begin = 0.2, end = 0.8) + theme_bw() +
  xlab("") + labs(color = "Възраст") +
  geom_vline(aes(xintercept = periyod), 
             data=samonaeti.df %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  ylab("Коеф. на безр.")

gbezrwyzrastind100 / gbezwyzrastkoef

# ggsave("graphs/19_bezrabotni_po_wyzrast.pdf", width=8, height=4, device = cairo_pdf)
# ggsave("graphs/19_bezrabotni_po_wyzrast.png", width=8, height=4, type="cairo-png")
# ggsave("graphs/19_bezrabotni_po_wyzrast.svg", width=8, height=4, device = svg)

# и безработни по причини на напускане - съкращение

bezrprihcini.df <- read.csv("data/bzprichiniquarterly.csv", sep = ";", header = F)

str(bezrprihcini.df)

bezrprihcini.df <- 
  bezrprihcini.df %>% 
  filter(V1 == "Безработни лица с предишна заетост",
         V2 == "Съкращение, уволнение") %>% 
  select(periyod = V3, brojhilqdi = V6) 

# Във времевата серия от ИНФОСТАТ липсва 2018 Q4
# със стойности 43,5 хиляди
# и данните за него трябва да се добавят ръчно,
# като се взети от сайта на НСИ

bezrprihcini.df <- 
  rbind(bezrprihcini.df, 
      data.frame(periyod = c("2018Q4"), brojhilqdi = c(43.5)))

bezrprihcini.df <- 
  bezrprihcini.df %>% 
  mutate(periyod = as.character(periyod)) %>% 
  arrange(periyod) %>% 
  mutate(periyod = as.yearqtr(periyod)) %>% 
  filter(periyod >= 2003, periyod < 2020) %>% 
  mutate(procprom = 
           (brojhilqdi - dplyr::lag(brojhilqdi))/dplyr::lag(brojhilqdi)*100)

bezrprihcini.df

gsykrateni <- 
  bezrprihcini.df %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = brojhilqdi)) +
  geom_vline(aes(xintercept = periyod), 
             data = mrztrimesechni %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_x_yearqtr(n = 10, format = "%Y") +
  xlab("") + ylab("Съкратени и уволнени (хил.)")

gsykrateniprocprom <-
  bezrprihcini.df %>% 
  ggplot() +
  geom_line(aes(x = periyod, y = procprom)) +
  geom_point(aes(x = periyod, y = procprom), size = 0.5) +
  geom_vline(aes(xintercept = periyod), 
             data = mrztrimesechni %>% filter(mzchanges > 0),
             linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_x_yearqtr(n = 10, format = "%Y") +
  xlab("") + ylab("% промяна") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray")

gsykrateni / gsykrateniprocprom + plot_layout(heights = c(2, 1))

ggsave("graphs/19_sykrateni.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/19_sykrateni.png", width=8, height=4, type="cairo-png")
ggsave("graphs/19_sykrateni.svg", width=8, height=4, device = svg)

bezrprihcini.df %>% 
  mutate(trimesechie = format(periyod, "%q"),
         posokaprom = case_when(procprom > 0 ~ "pljus",
                                TRUE ~ "minus")) %>% 
  group_by(trimesechie) %>% 
  summarise(srednoprocprom = mean(procprom, na.rm = T)) %>% 
  kable

################
# Наети в Русе #
################ 

ruse.df <-
  read.csv("data/Ruse_minwage.csv")

ruse.df <-
  ruse.df %>% 
  rename(periyod = period) %>% 
  mutate(periyod = as.yearqtr(periyod))

# Попълваме лиспващите данни
# пробваме различни методи

temp <- ts(ruse.df$naeti)
temp[c(8,14)] <- NA
tempts <- ts(temp, start = c(2007, 3), frequency = 4)

# 72877 67817 
na.interpolation(tempts) #73668 66480.5
na.interpolation(tempts, option = "spline") #73939.65 65713.66
na.interpolation(tempts, option = "stine") #73794.36 66480.50
na.kalman(tempts) #73826.55 65988.67
na.kalman(tempts, model = "auto.arima") # 75335.63 67711.06
na.seadec(tempts) # 74472.35 65377.64

# Най-добри приближения дава методът na.interpolation
# Използваме него, като създаваме функция

imp.fn <- 
  function(x) {
na.interpolation(ts(ruse.df[[x]], start = c(2007, 3), 
                    frequency = 4))
}


ruse.df$naetiprivate <- round(imp.fn("naetiprivate"), 0)

as.vector(ruse.df$naeti - ruse.df$naetiprivate - ruse.df$naetipublic)

ruse.df$naetipublic[c(8,14)] = 
  ruse.df$naeti[c(8,14)] - ruse.df$naetiprivate[c(8,14)]

ruse.df$zaplsr <- round(imp.fn('zaplsr'), 0)
ruse.df$zaplsrprivate <- round(imp.fn("zaplsrprivate"),0)
ruse.df$zaplsrpublic <- round(imp.fn("zaplsrpublic"),0)
ruse.df$naetimrz <- round(imp.fn('naetimrz'),0)
ruse.df$naetipart <- round(imp.fn("naetipart"),0)

# Добавяме населението на област Русе

nasruse.df <-
  nastrsp.df %>% 
  filter(oblast == "RSE",
         godina > 2005, godina < 2019) %>% 
  mutate(periyod = paste0(godina, "-4"),
         periyod = as.yearqtr(periyod))

nasruse.df <-
  nasnac1564only.df %>% 
  filter(periyod > "2005 Q4", periyod < "2019 Q1") %>% 
  left_join(nasruse.df) %>% 
  select(periyod, brojnas1564, nasrusetrdsp = broj)

nasruse.df$nasrusetrdsp <-
  round(na.interpolation(ts(nasruse.df$nasrusetrdsp, 
                    start = 2006, frequency = 4),
                 option = "linear"), 0)

# има много голям спад от 2010 до 2011 г. - 12 хиляди души
# подобно е и с населението като цяло

ruse.df <-
  nasruse.df %>% 
  filter(periyod > "2007 Q2", periyod < "2017 Q4") %>% 
  select(periyod, nasrusetrdsp) %>% 
  inner_join(ruse.df)

ruse.df <- 
  ruse.df %>% 
  mutate(mrzprocchasten = mrz/zaplsrprivate * 100) %>% 
  inner_join(bwptrim)

# Добавяме заети, от които да извадим наетите и да получим самонети
# и работодатели

zaetiruse.df <- read.csv("data/zaetiNUTS3quarterly.csv", sep = ";", header = F, skip = 1)

zaetiruse.df <-
  zaetiruse.df %>% 
  filter(V1 == "Общо", V2 == "BG323") %>%
  select(periyod = V3, zaeti = V4) %>% 
  mutate(zaeti = zaeti * 1000,
         periyod = as.yearqtr(periyod))

ruse.df <- 
  ruse.df %>% inner_join(zaetiruse.df)

mzchangesruse <- 
  mrztrimesechni %>% 
  filter(mzchanges > 0, 
         periyod > "2007 Q2", periyod < "2017 Q4")

ruse.df %>% 
  ggplot(aes(x = periyod)) + 
#  geom_line(aes(y = naetiprivate)) +
  geom_line(aes(y = naetipublic)) +
  geom_line(aes(y = naetimrz), color = "blue") +
  geom_line(aes(y = naetipart), color = "green") +
  geom_vline(aes(xintercept = periyod), 
             data = mzchangesruse, 
             linetype = "dashed",
             color = "gray") + 
scale_x_yearqtr()

izglrs.fn <- 
  function(promenliwa) {
    final(seas(ts(promenliwa, start = c(2007, 3), frequency = 4)))
}

ind100.fn <-
  function(promenliwa) {
    promenliwa/first(promenliwa)*100
}

  
ruseizgl.df <- 
  ruse.df %>% 
  mutate(samonaetiirab = zaeti - naeti,
         naetiizgl = izglrs.fn(naeti),
         naetiprivateizgl = izglrs.fn(naetiprivate),
         naetipublicizgl = izglrs.fn(naetipublic),
         naetipartizgl = izglrs.fn(naetipart),
         naetimrzizgl = izglrs.fn(naetimrz),
         samonaetiirabizgl = izglrs.fn(samonaetiirab),
         zaplsrizgl = izglrs.fn(zaplsr),
         zaplsrprivateizgl = izglrs.fn(zaplsrprivate),
         zaplsrpublicizgl = izglrs.fn(zaplsrpublic),
         mrzprocchastenizgl = mrz/zaplsrprivateizgl*100,
         ind100naeti = ind100.fn(naeti),
         ind100naetiprivate = ind100.fn(naetiprivate),
         ind100naetipublic = ind100.fn(naetipublic),
         ind100naetimrz = ind100.fn(naetimrz),
         ind100naetipart = ind100.fn(naetipart),
         ind100samonaetiirab = ind100.fn(samonaetiirab),
         ind100nas = ind100.fn(nasrusetrdsp),
         ind100naetiizgl = ind100.fn(naetiizgl),
         ind100naetiprivateizgl = ind100.fn(naetiprivateizgl),
         ind100naetipublicizgl = ind100.fn(naetipublicizgl),
         ind100naetimrzizgl = ind100.fn(naetimrzizgl),
         ind100naetipartizgl = ind100.fn(naetipartizgl),
         ind100samonaetiirabizgl = ind100.fn(samonaetiirabizgl)
         )

gnasrusebase <- 
  ruseizgl.df %>% 
  ggplot(aes(x = periyod)) + 
  geom_vline(aes(xintercept = periyod), 
             data = mzchangesruse, 
             linetype = "dashed",
             color = "gray") +
  scale_x_yearqtr(format = "%Y") +
  theme_bw() +
  xlab("")

cwqt1 <- "#FDE725FF"
cwqt2 <- "#55C667FF"
cwqt3 <- "#238A8DFF"
cwqt4 <- "#39568CFF"
cwqt5 <- "#440154FF"

cwetowaskala <- 
scale_color_manual(name = "Брой хора", 
                     values = c("на МРЗ" = cwqt1, 
                                "в частни\nпредприятия" = cwqt2,
                                "в обществени\nпредприятия" = cwqt3, 
                                "самонаети и\nработодатели" = cwqt4,
                                "население" = cwqt5)) 
  
gwidowenaetirusebroj <- 
  gnasrusebase +
  geom_line(aes(y = naetimrzizgl, color = "на МРЗ")) +
  geom_line(aes(y = naetiprivateizgl, color = "в частни\nпредприятия")) +
  geom_line(aes(y = naetipublicizgl, color = "в обществени\nпредприятия" )) +
  geom_line(aes(y = samonaetiirabizgl, color = "самонаети и\nработодатели")) +
  # geom_line(aes(y = nasrusetrdsp, color = "население")) +
  ylab("Брой") +
  cwetowaskala +
  guides(color = FALSE)

gwidowenaetiruseindex <-
  gnasrusebase +
  geom_line(aes(y = ind100naetimrzizgl, color = "на МРЗ")) +
  geom_line(aes(y = ind100naetiprivateizgl, color = "в частни\nпредприятия")) +
  geom_line(aes(y = ind100naetipublicizgl, color = "в обществени\nпредприятия")) +
  geom_line(aes(y = ind100samonaetiirabizgl, color = "самонаети и\nработодатели")) +
  # geom_line(aes(y = ind100nas, color = "население")) +
  ylab("Трето трим. на 2007=100") +
  cwetowaskala

gwidowenaetirusebroj | gwidowenaetiruseindex

ggsave("graphs/20_zaeti_ruse_widowe.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/20_zaeti_ruse_widowe.png", width=8, height=4, type="cairo-png")
ggsave("graphs/20_zaeti_ruse_widowe.svg", width=8, height=4, device = svg)

gmrzonlyrs <- 
  gnasrusebase +
  geom_line(aes(y = mrzprocchasten)) + # няма смисъл от изглаждане на данните тук
  ylab("МРЗ/СРЗ, %")

gbwprskontrol <-
  gnasrusebase +
  geom_line(aes(y = bwpc2015izgl/1000)) +
  ylab("БВП, млрд.лв. (2015)")

gnaetimrzonlyrs <-
  gnasrusebase +
  geom_line(aes(y = naetimrzizgl)) +
  ylab("Наети на МРЗ")

gmrzinaetiruse <-
  ruseizgl.df %>% 
  ggplot(aes(x = mrzprocchastenizgl, y = naetimrzizgl)) +
  geom_point() +
  theme_bw() + xlab("МРЗ/СРЗ, % (частен сектор)") + ylab("Наети на МРЗ") +
  geom_smooth(method = "lm")

gnaetinamrztrendruse <- 
  gnasrusebase +
  geom_line(aes(y = ind100naetimrzizgl)) +
  geom_line(aes(y = ind100naetimrz), linetype = "dotted") +
  ylab("Наети на МРЗ, 2007=100")

(gmrzonlyrs / gbwprskontrol) | (gmrzinaetiruse / gnaetinamrztrendruse) 

ggsave("graphs/20_mrz_i_naeti_na_mrz_ruse.pdf", width=8, height=4, device = cairo_pdf)
ggsave("graphs/20_mrz_i_naeti_na_mrz_ruse.png", width=8, height=4, type="cairo-png")
ggsave("graphs/20_mrz_i_naeti_na_mrz_ruse.svg", width=8, height=4, device = svg)


gzaplchastensektor <-
  gnasrusebase +
  geom_line(aes(y = zaplsrprivate)) +
  ylab("СРЗ, лв.")


gnaetinamrztrendruse / gmrzonlyrs + plot_layout(heights = c(3, 1))

gnaetinamrztrendruse / gzaplchastensektor / gmrzonlyrs / gbwprskontrol

colnames(ruseizgl.df)
ruseizgl.df <-
  ruseizgl.df %>% 
  mutate(kaitzpriv = mrz/zaplsrprivateizgl)

mod1naetimrz <- lm(log(naetimrzizgl) ~ log(mrzprocchasten) + 
             log(bwpc2015izgl) + log(nasrusetrdsp),
           data = ruseizgl.df)

summary(mod1naetimrz)

mod1naetikaitz <- lm(log(naetimrzizgl) ~ log(kaitzpriv) + 
                     log(bwpc2015izgl) + log(nasrusetrdsp),
                   data = ruseizgl.df)

summary(mod1naetikaitz)

dwt(mod1naetikaitz)

summary(cochrane.orcutt(mod1naetimrz))
summary(prais_winsten(log(naetimrzizgl) ~ log(mrzprocchasten) + 
                log(bwpc2015izgl) + log(nasrusetrdsp),
              data = ruseizgl.df))
    
dwt(mod1naetimrz)
dwtest(mod1naetimrz)

# p-value is less than 0.05, we can reject the null hypothesis 
# and conclude that the residuals in this regression model are autocorrelated.

ruseregvarslevel <- list(ruseizgl.df$naetimrzizgl, ruseizgl.df$kaitzpriv,
                            ruseizgl.df$naetiprivateizgl, ruseizgl.df$nasrusetrdsp,
                         ruseizgl.df$zaplsrprivateizgl, ruseizgl.df$mrz,
                         ruseizgl.df$bwpc2015izgl)

names(ruseregvarslevel) <- c("naetimrz", "kaitzpriv", 
                   "naetipriv", "nasrstrdsp", "zaplpriv", "mrz", "bwpc2015")


ruseregvarslevel <- lapply(ruseregvarslevel, ts, start = c(2007, 3), frequency = 4)
ruseregvarslog <- lapply(ruseregvarslevel, log)
ruseregvarslogdiff <- lapply(ruseregvarslog, diff)


# Създаваме таблица с p.value от теста adf.test от пакета tseries
# който обаче работи с модела само с константа и тренд

unitrootsruse.df <- data.frame(promenliwi = names(ruseregvarslog))

unitrootsruse.df$adflogs <-
  round(
  sapply(lapply(ruseregvarslog, adf.test), function(x) x[["p.value"]]),
  2)

unitrootsruse.df$adflogsdiff <-
  round(
    sapply(lapply(ruseregvarslogdiff, adf.test), function(x) x[["p.value"]]),
    2)

# Добавяме и Филипс-Перон от същия пакет

unitrootsruse.df$pplogs <-
  round(
    sapply(lapply(ruseregvarslog, pp.test), function(x) x[["p.value"]]),
    2)

unitrootsruse.df$pplogsdiff <-
  round(
    sapply(lapply(ruseregvarslogdiff, pp.test), function(x) x[["p.value"]]),
    2)

# и теста KPSS, където нулевата хипотеза е за стационарност
# за разлика от предните два теста

unitrootsruse.df$kpsslogs <-
  round(
    sapply(lapply(ruseregvarslog, kpss.test), function(x) x[["p.value"]]),
    2)

unitrootsruse.df$kpsslogsdiff <-
  round(
    sapply(lapply(ruseregvarslogdiff, kpss.test), function(x) x[["p.value"]]),
    2)

# Ето и резултатите

unitrootsruse.df

unitroottests.df <- data.frame(promenliwi = names(ruseregvarslogdiff))

# Функция, която съпоставя получената тау статистика с критичните
# стойности в модела при 0.05. Приема като аргументи таблица с данни
# и вида на модела "none", "drift" и "trend" съответно без константа
# и тренд, с константа и с константа и тренд. Използва се критерия
# за селекция на Акаике за броя на лаговете, а максималният брой е
# ограничен до 4

urdfcoeff.fn <- 
  function (x,y) {
  tempur <- ur.df(x, lags = 4, selectlags = "AIC", type = y)
  ifelse(tempur@teststat[1,1] - tempur@cval[1,2] >= 0, "ед.к.", "стац.")
}


# За първите разлики създаваме нова таблица, където ще се съхраняват
# резултатите

unitroottestslogdiffs.df <- data.frame(promenliwi = names(ruseregvarslog))

unitroottestslogdiffs.df$dfnone <-
  sapply(ruseregvarslogdiff, function(x) urdfcoeff.fn(x,"none"))
unitroottestslogdiffs.df$dfdrift <-
  sapply(ruseregvarslogdiff, function(x) urdfcoeff.fn(x,"drift"))
unitroottestslogdiffs.df$dftrend <-
  sapply(ruseregvarslogdiff, function(x) urdfcoeff.fn(x,"trend"))

unitroottestslogdiffs.df

# Kwiatkowski-Phillips-Schmidt-Shin
# Ако test statistics > critical values
# отхвърляме нулевата хипотеза за стационарност

urkpsscoef.fn <-
  function(x,y) {
  tempurkpss <- ur.kpss(x, type = y, lags = "short")
  ifelse(tempurkpss@teststat - tempurkpss@cval[2] < 0, "стац.", "нестац.")
}

unitroottestslogdiffs.df$kpssrawn <- 
  sapply(ruseregvarslogdiff, function(x) urkpsscoef.fn(x, "mu"))
unitroottestslogdiffs.df$kpsstrend <-
  sapply(ruseregvarslogdiff, function(x) urkpsscoef.fn(x, "tau"))

plot(ruseregvarslog$naetimrz)
plot(ruseregvarslogdiff$naetimrz)

urzacoef.fn <-
  function (x, y) {
  urzastat <- ur.za(x, model = y, lag = 4)
  ifelse(urzastat@teststat - urzastat@cval[2] < 0, "стац.", "ед.к.")
}

unitroottestslogdiffs.df$zaintercept <- 
  sapply(ruseregvarslogdiff, function (x,y) urzacoef.fn(x, "intercept"))
unitroottestslogdiffs.df$zatrend <-
  sapply(ruseregvarslogdiff, function (x,y) urzacoef.fn(x, "trend"))
unitroottestslogdiffs.df$zaboth <-
  sapply(ruseregvarslogdiff, function (x,y) urzacoef.fn(x, "both"))


unitroottestsloglevels.df <- data.frame(promenliwi = names(ruseregvarslog))

unitroottestsloglevels.df$dfnone <- 
  sapply(ruseregvarslog, function(x) urdfcoeff.fn(x,"none"))
unitroottestsloglevels.df$dfdrift <-
  sapply(ruseregvarslog, function(x) urdfcoeff.fn(x,"drift"))
unitroottestsloglevels.df$dftrend <-
  sapply(ruseregvarslog, function(x) urdfcoeff.fn(x,"trend"))
unitroottestsloglevels.df$kpssrawn <-
  sapply(ruseregvarslog, function(x) urkpsscoef.fn(x, "mu"))
unitroottestsloglevels.df$kpsstrend <-
  sapply(ruseregvarslog, function(x) urkpsscoef.fn(x, "tau"))
unitroottestsloglevels.df$zaintercept <-
  sapply(ruseregvarslog, function(x) urzacoef.fn(x, "intercept"))
unitroottestsloglevels.df$zatrend <-
  sapply(ruseregvarslog, function(x) urzacoef.fn(x, "trend"))
unitroottestsloglevels.df$zaboth <-
  sapply(ruseregvarslog, function(x) urzacoef.fn(x, "both"))

urzabreaks.fn <-
  function (x, y) {
    urzastat <- ur.za(x, model = y, lag = 4)
    timetk::tk_index(ruseregvarslogdiff$zaplpriv)[urzastat@bpoint]
}

sapply(ruseregvarslogdiff, function (x) urzabreaks.fn(x,"intercept"))
sapply(ruseregvarslog, function (x) urzabreaks.fn(x,"both"))

plot(ruseregvarslog$naetimrz)
plot(ruseregvarslog$naetipriv)
plot(ruseregvarslog$zaplpriv)

plot(ruseregvarslogdiff$naetimrz)
plot(ruseregvarslogdiff$naetipriv)
plot(ruseregvarslogdiff$zaplpriv)

unitroottestsloglevels.df
unitroottestslogdiffs.df %>% 
  select(promenliwi, dfnone, dfdrift, kpssrawn, zaintercept) %>% 
  kable

    # линейна регресия на лог-трансформираните разлики

modeldifferenced <- lm(naetimrz ~ kaitzpriv + nasrstrdsp + bwpc2015, 
                       data = ruseregvarslogdiff)

# Моделът не излиза статистически значим

summary(modeldifferenced)

dwt(modeldifferenced)                        
plot(resid(modeldifferenced))
qqnorm(resid(modeldifferenced))
qqline(resid(modeldifferenced))
shapiro.test(resid(modeldifferenced)) # p < 0.05 и разпределението не е нормално
bptest(modeldifferenced) # p > 0.05 и разпределението на остатъците е хомоскедастично

ruseregvarslog.df <- data.frame(ruseregvarslog)

names(ruseregvarslog.df)

# Добавяме категорийна променлива с тримесечията,
# в които има промяна в МРЗ

mzdummyruse <- 
  mrzquarterly %>% 
  filter(periyod > "2007 Q2" & periyod < "2017 Q4") %>% 
  mutate(mzdummy = case_when(mzchanges > 0 ~ 1,
                             TRUE ~ 0)) %>% 
  select(mzdummy)

mzdummyruse$mzdummy

mzdummyruse <- ts(mzdummyruse$mzdummy, start = c(2007, 3), frequency = 4)

ruseregvarslog.df$mzdummy <- mzdummyruse

# Добавяме категорийна променлива
# с тримесечията, в които има спад в БВП

crisisdummyruse <- 
  bwptrim %>% 
  filter(periyod > "2007 Q2" & periyod < "2017 Q4") %>% 
  mutate(crisisdummy = case_when(bwprastevizgl > 0 ~ 0,
                                 TRUE ~ 1)) %>% 
  select(crisisdummy)

crisisdummyruse <- ts(crisisdummyruse$crisisdummy, start = c(2007,3), frequency = 4)

ruseregvarslog.df$crisisdummy <- crisisdummyruse

ruseregvarslog.df$crisisdummy

summary(dynlm(naetimrz ~ kaitzpriv + 
                L(naetimrz, 1) +
                L(naetimrz, 2) +
                L(kaitzpriv, 1), data = ruseregvarslog.df))

mods_ardl_naetimrz <- list()

# Model specifications
# whether the 'intercept' and/or the 'trend' are restricted 
# to participate in the long-run relationship or they are 
# unrestricted and so they participate in the short-run relationship. 

# Model without constant and without trend

mods_ardl_naetimrz$n <- 
  auto_ardl(naetimrz ~ kaitzpriv + bwpc2015 - 1, data = ruseregvarslog.df,
            max_order = 5, selection = "AIC")$best_model

# Model with constant

mods_ardl_naetimrz$c <- 
  auto_ardl(naetimrz ~ kaitzpriv + bwpc2015, data = ruseregvarslog.df,
            max_order = 5, selection = "AIC")$best_model

# Model with constant and trend

mods_ardl_naetimrz$ct <- 
  auto_ardl(naetimrz ~ kaitzpriv + bwpc2015 + trend(naetimrz), data = ruseregvarslog.df,
            max_order = 5, selection = "AIC")$best_model

lapply(mods_ardl_naetimrz, summary)
lapply(mods_ardl_naetimrz, function(x) summary(uecm(x)))

# These differ in terms of whether the 'intercept' and/or the 'trend' 
# are restricted to participate in the long-run relationship 
# or they are unrestricted and so they participate in 
# the short-run relationship.

# Case 1: No intercept and no trend.
# n

bounds_f_test(mods_ardl_naetimrz$n, case = "n", alpha = 0.05) # coint  
bounds_t_test(mods_ardl_naetimrz$n, case = "n", alpha = 0.05) # coint

# Case 2: Restricted intercept and no trend.  
# rc

bounds_f_test(mods_ardl_naetimrz$c, case = "rc", alpha = 0.05) # coint

# Case 3: Unrestricted intercept and no trend.
# uc

bounds_f_test(mods_ardl_naetimrz$c, case = "uc", alpha = 0.05) # coint
bounds_t_test(mods_ardl_naetimrz$c, case = "uc", alpha = 0.05) # coint

# Case 4: Unrestricted intercept and restricted trend.
# ucrt

bounds_f_test(mods_ardl_naetimrz$ct, case = "ucrt", alpha = 0.05) # coint

# Case 5: Unrestricted intercept and unrestricted trend.
# ucut  

bounds_f_test(mods_ardl_naetimrz$ct, case = "ucut", alpha = 0.05) # coint
bounds_t_test(mods_ardl_naetimrz$ct, case = "ucut", alpha = 0.05) # coint

coint_eq(mods_ardl_naetimrz$c, case = 3)
multipliers(uecm(mods_ardl_naetimrz$c), type = "lr")
multipliers(uecm(mods_ardl_naetimrz$c), type = "sr")
summary(mods_ardl_naetimrz$c)
summary(uecm(mods_ardl_naetimrz$c))

dwtest(mods_ardl_naetimrz$c)
bgtest(mods_ardl_naetimrz$c) 
shapiro.test(mods_ardl_naetimrz$c$residuals)
bptest(mods_ardl_naetimrz$c)

qqPlot(mods_ardl_naetimrz$c$residuals)
 
summary(recm(mods_ardl_naetimrz$ct, case = 4))
summary(recm(mods_ardl_naetimrz$ct, case = 5))

summary(uecm(mods_ardl_naetimrz$ct, case = 4))
summary(uecm(mods_ardl_naetimrz$ct, case = 5))

# Брой наети в частния сектор като независима променлива
# Модел с константа, но без тренд

plot(ruseregvarslog.df$naetipriv)
length(ruseregvarslog.df$naetipriv)


model_naetiprivruse <- 
  auto_ardl(naetipriv ~ kaitzpriv + bwpc2015 + nasrstrdsp, 
            data = ruseregvarslog.df,
            max_order = 5, selection = "AIC")$best_model

summary(model_naetiprivruse)

model_naetiprivruse_dummy <-  
  auto_ardl(naetipriv ~ kaitzpriv + nasrstrdsp | crisisdummy, 
            data = ruseregvarslog.df,
            max_order = 5, selection = "AIC")$best_model

summary(model_naetiprivruse_dummy)

dwtest(model_naetiprivruse) # H0 - no serial correlation
bgtest(model_naetiprivruse) # H0 - no serial correlation
shapiro.test(model_naetiprivruse$residuals) # H0 - normal distribution
bptest(model_naetiprivruse) #H0 - no heteroscedasticity
# В горния случай всички тестове минават успешно

tab_model(model_naetiprivruse) # sjPlot

bounds_f_test(model_naetiprivruse, case = "uc", alpha = 0.05) # coint
bounds_t_test(model_naetiprivruse, case = "uc", alpha = 0.05) # no coint

# При това положение не изчисляваме модел с коригирани грешки за коинтеграция

summary(uecm(model_naetiprivruse, case = "uc"))
multipliers(uecm(model_naetiprivruse, case = "uc"), type = "lr")

# Брой наети на минимална заплата като независима променлива
# Модел с константа, но без тренд

plot(ruseregvarslog.df$naetimrz)

ruseregvarslog.df

model_naetimrzruse <- 
  auto_ardl(naetimrz ~ kaitzpriv + bwpc2015 + nasrstrdsp, 
            data = ruseregvarslog.df,
            max_order = 5, selection = "AIC")$best_model

summary(model_naetimrzruse)

model_naetimrzruse_dummy <- 
  auto_ardl(naetimrz ~ kaitzpriv + nasrstrdsp | crisisdummy, 
            data = ruseregvarslog.df,
            max_order = 5, selection = "AIC")$best_model

summary(model_naetimrzruse_dummy)

ruseregvarslevel$nasrstrdsp

dwtest(model_naetimrzruse) # H0 - no serial correlation
bgtest(model_naetimrzruse) # H0 - no serial correlation
shapiro.test(model_naetimrzruse$residuals) # H0 - normal distribution
bptest(model_naetimrzruse) #H0 - no heteroscedasticity
# В горния случай всички тестове минават успешно

tab_model(model_naetimrzruse) # sjPlot

bounds_f_test(model_naetimrzruse, case = "uc", alpha = 0.05) # coint
bounds_t_test(model_naetimrzruse, case = "uc", alpha = 0.05) # no coint

# При това положение не изчисляваме модел с коригирани грешки за коинтеграция

summary(uecm(model_naetimrzruse, case = "uc"))
multipliers(uecm(model_naetimrzruse, case = "uc"), type = "lr")

# Заплата в частния сектор като независима променлива
# Модел с константа и тренд

plot(ruseregvarslog.df$zaplpriv)

ruseregvarslog.df

model_zaplataruse <- 
  auto_ardl(zaplpriv ~ kaitzpriv + bwpc2015 + trend(zaplpriv), 
            data = ruseregvarslog.df,
            max_order = 5, selection = "AIC")$best_model

summary(model_zaplataruse)



dwtest(model_zaplataruse) # H0 - no serial correlation
bgtest(model_zaplataruse) # H0 - no serial correlation
shapiro.test(model_zaplataruse$residuals) # H0 - normal distribution
bptest(model_zaplataruse) #H0 - no heteroscedasticity
# В горния случай всички тестове минават успешно без този за нормалност

tab_model(model_zaplataruse) # sjPlot



bounds_f_test(model_zaplataruse, case = "ucrt", alpha = 0.05) # coint
bounds_t_test(model_zaplataruse, case = "ucrt", alpha = 0.05) # no coint

tempmod <- uecm(model_zaplataruse, case = "ucrt")
summary(tempmod)
tempmod$coefficients[2]
recm(tempmod)

################################
# Крива на Бевъридж по региони #
################################

azobl.df <- read.csv("data/az_bezr_NUTS3_2013-2018.csv")

oblkodowe <- read.csv("data/oblastikodowe.csv")

naetioblmes.df <- read.csv("data/naeti_mesechni_NUTS3.csv", sep = ";", header = F)

naetioblmes.df <- 
  naetioblmes.df %>% 
  select(oblast = V1, periyod = V2, naeti = V3) %>% 
  separate(oblast, into = c("nutsoblkod", "oblime"), sep = " ")

head(naetioblmes.df)

naetioblmes.df <- 
  transform(naetioblmes.df, godina = substr(periyod, 1, 4), 
          mesec = substr(periyod, 5, 8))

naetioblmes.df$mesec <- 
  as.numeric(as.roman(as.character(naetioblmes.df$mesec)))

naetioblmes.df$periyod <- paste0(naetioblmes.df$godina,
                                 "-",naetioblmes.df$mesec,"-","01")

naetioblmes.df$periyod <- as.Date(naetioblmes.df$periyod, format="%Y-%m-%d")

head(naetioblmes.df)

azobl.df <- 
  azobl.df %>% 
  mutate(oblast = str_replace(oblast, "гр. София", "София (столица)"),
         oblast = str_replace(oblast, "Софийска", "София")) %>% 
  rename(oblime = oblast, periyod = period) %>% 
  mutate(periyod = paste0(periyod,01),
         periyod = as.Date(periyod, format = "%Y%m%d"))

azobl.df %>% filter(str_detect(oblime, "Соф")) %>% 
  tabyl(oblime)

azobl.df <-
  naetioblmes.df %>% select(-oblime) %>% 
  inner_join(oblkodowe) %>% 
  inner_join(azobl.df) 

azobl.df <- 
  azobl.df %>% 
  mutate(unemplrate = 100*regbezr/(regbezr + naeti),
         vacancyrate = 100*swobodnimesta/(swobodnimesta + naeti))
  
azoblmes.df <- 
  azobl.df %>% 
  arrange(ekatteoblkod, periyod) %>% 
  select(ekatteoblkod, periyod, nuts2ime,
         naeti, regbezr, swobodnimesta, postypilinarab)

azoblmes.df <- 
  azoblmes.df %>% 
  group_by(ekatteoblkod) %>% 
  mutate(izglnaeti = final(seas(ts(naeti, start = c(2013,9), frequency = 12))),
         izglregbezr = final(seas(ts(regbezr, start = c(2013,9), frequency = 12))),
         izglswmesta = final(seas(ts(swobodnimesta, start = c(2013,9), frequency = 12))),
         izglpostypili = final(seas(ts(postypilinarab, start = c(2013, 9), frequency = 12)))) 

azoblmes.df <-
  azoblmes.df %>% 
  mutate(izglvacancyrate = 100*izglswmesta/(izglswmesta + izglnaeti),
         izglunemplrate = 100*izglregbezr/(izglregbezr + izglnaeti))

azoblmes.df <- 
  azoblmes.df %>% 
  mutate(perxod = as.yearmon(periyod)) %>% 
  mutate(godina = year(periyod)) %>% 
  mutate(mesec = month(periyod))

azoblmes.df %>% 
  filter(ekatteoblkod == "RSE") %>% 
  ggplot(aes(x = izglunemplrate, y = izglvacancyrate)) + geom_line()

azoblmes.df %>% 
  filter(mesec %in% c(3,6,9,12)) %>% 
  ggplot(aes(x = izglunemplrate, y = izglvacancyrate)) +
  geom_line() + facet_wrap(ekatteoblkod ~ .) +
  xlab("Коеф. на безработица") +
  ylab("Коеф. на свободни раб. места")

names(azoblmes.df)

azoblmesforplot.df <- 
  azoblmes.df %>% 
  group_by(nuts2ime, periyod) %>% 
  summarise(regbezr = sum(izglregbezr), 
            swobmest = sum(izglswmesta),
            naeti = sum(izglnaeti)) %>% 
  mutate(unemplrate = regbezr/(regbezr+naeti)*100,
         vacancyrate = swobmest/(swobmest+naeti)*100,
         mesec = month(periyod)) %>% 
  filter(mesec %in% c(3,6,9,12)) %>% 
  mutate(perxod = as.yearmon(periyod)) 

azoblmesforplot.df$perxod  <- year(azoblmesforplot.df$perxod)

azoblmesforplot.df %>% 
  ggplot(aes(x = unemplrate, y = vacancyrate)) +
  geom_line() +
  geom_text(data = azoblmesforplot.df %>% filter(mesec == 9), 
            aes(label = perxod), size = 3, nudge_x = 0.2, nudge_y = 0.1,
            color = "gray", check_overlap = T) +
  geom_point(data = azoblmesforplot.df %>% filter(mesec == 9), 
            size = 0.5) +
  facet_wrap(nuts2ime ~ .) +
  xlab("Коеф. на безработица") +
  ylab("Коеф. на своб. раб. места") +
  theme_bw()

azoblmes.df %>% 
  group_by(nuts2ime, periyod) %>% 
  summarise(regbezr = sum(izglregbezr), 
            swobmest = sum(izglswmesta),
            naeti = sum(izglnaeti)) %>% 
  mutate(otnoshenie = swobmest/regbezr) %>% 
  ggplot(aes(x = periyod, y = otnoshenie)) +
  geom_line() + facet_wrap(. ~ nuts2ime) +
  xlab("") + ylab("Свободни места към брой безработни") +
  theme_bw()

azoblmes.df %>% 
  group_by(nuts2ime, periyod) %>% 
  summarise(regbezr = sum(izglregbezr), 
            swobmest = sum(izglswmesta),
            naeti = sum(izglnaeti)) %>% 
  mutate(otnoshenie = swobmest/regbezr,
         ind100otnoshenie = (otnoshenie - first(otnoshenie))*100) %>% 
  ggplot(aes(x = periyod, y = ind100otnoshenie, color = nuts2ime)) +
  geom_line() + 
  xlab("") + ylab("Свободни места към брой безработни") +
  theme_bw() +
  scale_color_viridis_d()

head(azobl.df)

azoblpanel.df <-
  azobl.df %>% 
  mutate(u29prc = regbezrdo29/regbezr*100,
         uLTprc = regbezrnad1g/regbezr*100) %>% 
  select(obl = ekatteoblkod, periyod, nuts2 = nuts2ime, M = postypilinarab,
         U = regbezr, V = swobodnimesta, u29prc, uLTprc, naeti) %>% 
  mutate(Ulag = dplyr::lag(U),
         Vlag = dplyr::lag(V),
         mesec = month(periyod),
         godina = year(periyod))

str(azoblpanel.df)

str(nastrsp.df)

head(azoblpanel.df)

obl.df <- read.csv("data/oblasti-annual.csv", sep = ";", skip = 1)

oblkontrol.df <- 
  obl.df %>% 
  select(obl=1,godina = 2, dma=3,wisshedql=13) %>% 
  inner_join(nastrsp.df %>% rename(obl = oblast)) %>% 
  rename(nastrsp = broj)

azoblpanel.df <- 
  azoblpanel.df %>% 
  left_join(oblkontrol.df) %>% 
  mutate(dmananaet = dma/naeti*1000) %>% 
  mutate(mesec = as.factor(as.character(mesec)))

azobpanel <- pdata.frame(azoblpanel.df, index = c("obl", "periyod"))

names(azoblpanel.df)
str(azoblpanel.df)
head(azobpanel)


zp <- plm(log(M) ~ log(Ulag) + log(Vlag), data = azobpanel, model = "pooling")
zi <- plm(log(M) ~ log(Ulag) + log(Vlag), data = azobpanel, model = "within", effect = "individual")
zt <- plm(log(M) ~ log(Ulag) + log(Vlag), data = azobpanel, model = "within", effect = "time")
zd<- plm(log(M) ~ log(Ulag) + log(Vlag), data = azobpanel, model = "within", effect = "twoway")

summary(zt)

# F Test For Individual And/Or Time Effects
# Test of individual and/or time effects based on
# the comparison of the within and the pooling model.

pFtest(zi, zp)
pFtest(zt, zp)
pFtest(zd, zp)

# pwtest
# Wooldridge's Test For Unobserved Effects In Panel Models
    
pwtest(zp, azobpanel, effect = "individual")
pwtest(zp, azobpanel, effect = "time")

# plmtest
# Lagrange FF Multiplier Tests For Panel Models
# Test of individual and/or time effects for panel models.

plmtest(zp, effect = "individual")
plmtest(zp, effect = "time")
plmtest(zp, effect = "twoways")

# pbgtest Breusch–Godfrey Test for Panel Models
# Test of serial correlation for 
# (the idiosyncratic component of) the errors in panel models.

pbgtest(zd)

# pdwtest Durbin–Watson Test for Panel Models
# Test of serial correlation for
# (the idiosyncratic component of) the errors in panel models.

pdwtest(zd)

coeftest(zd, vcov = vcovHC(zd, method = "arellano", type = "HC3"))

# pcdtest Tests of cross-section dependence for panel models
# Pesaran’s CD or Breusch–Pagan’s LM (local or global) tests 
# for cross sectional dependence in panel models

pcdtest(zd)

coeftest(zd, vcov = vcovSCC(zd, type = "HC3", cluster = "group"))

coef(zd)

coeftest(zt, vcovHC(zt, method = "arellano"))
coef(zt)

head(azoblpanel.df)


data.frame(fixef(zi) - min(fixef(zi))) %>% 
  mutate(obl = rownames(.)) %>% 
  rename(stojnost = 1) %>% 
  inner_join(azoblpanel.df %>% filter(periyod == "2013-09-01")) %>% 
  mutate(nuts2 = fct_relevel(nuts2, "Северозападен", "Северен централен",
                             "Североизточен", "Югоизточен", "Южен централен",
                             "Югозападен")) %>% 
  arrange(nuts2, stojnost) %>% 
  mutate(podredba = rownames(.)) %>% 
  ggplot(aes(x=reorder(obl, as.numeric(podredba)), y = stojnost, group = nuts2)) + 
  geom_col(aes(fill = nuts2)) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("") + ylab("")


data.frame(fixef(zi))

sz_panel <- pdata.frame(azoblpanel.df %>% filter(nuts2 == "Северозападен"), index = c("obl", "periyod"))

sz_reg <- plm(log(M) ~ log(Ulag) + log(Vlag), data = sz_panel, 
              model = "within", effect = "time")

summary(sz_reg)

sc_panel <- pdata.frame(azoblpanel.df %>% filter(nuts2 == "Северен централен"), index = c("obl", "periyod"))

sc_reg <- plm(log(M) ~ log(Ulag) + log(Vlag), data = sc_panel, 
              model = "within", effect = "time")

summary(sc_reg)

si_panel <- pdata.frame(azoblpanel.df %>% filter(nuts2 == "Североизточен"), index = c("obl", "periyod"))

si_reg <- plm(log(M) ~ log(Ulag) + log(Vlag), data = si_panel, 
              model = "within", effect = "time")

summary(si_reg)


yz_panel <- pdata.frame(azoblpanel.df %>% filter(nuts2 == "Югозападен"), 
                        index = c("obl", "periyod"))

yz_reg <- plm(log(M) ~ log(Ulag) + log(Vlag), data = yz_panel, 
              model = "within", effect = "time")

summary(yz_reg)
pdwtest(yz_reg)

azobpanel %>% 
  group_by(nuts2) %>% 
  nest() %>% 
  mutate(models = map(data, ~ plm(log(M) ~ log(Ulag) + log(Vlag), 
                                  model = "within", effect = "time", data = .))) %>% 
  mutate(summaries = map(models, glance)) %>% 
  unnest(summaries)

tmp <- with(azobpanel,
            by(azobpanel, nuts2,
               function(x) plm(log(M) ~ log(Ulag) + log(Vlag), 
                               model = "within", effect = "time", data = x)))
sapply(tmp, coef)

with(azobpanel,
by(azobpanel, nuts2, function(x) mean(x$U, na.rm = T)))
with(azobpanel,
     by(azobpanel, nuts2, function(x) mean(x$V, na.rm = T)))


lapply(split(azobpanel, azobpanel$nuts2), 
       function(x) summary(plm(log(M) ~ log(Ulag) + log(Vlag), 
                                                          model = "within", effect = "time", data = x)))


azoblpanel.df %>% 
  nest(-nuts2) %>% 
  mutate(fit = map(data, ~ t.test(.$V)),
         results = map(fit, glance)) %>% 
  unnest(results)
  
azoblpanel.df %>% 
  nest(-nuts2) %>% 
  mutate(fit = map(data, ~ 
                     plm(log(M) ~ log(Ulag) + log(Vlag), data = ., 
                               index = c("obl", "periyod"),
                               model = "within", effect = "time")),
         results = map(fit, glance)) %>% 
  unnest(results)

names(azoblpanel.df)

eqbase <- function(x) summary(plm(log(M) ~ log(Ulag) + log(Vlag),                            , 
                                  model = "within", effect = "time", data = x))

eqpetkov <- function(x) summary(plm(log(M) ~ log(Ulag) + log(Vlag) +
                                      u29prc + uLTprc + mesec, 
                                    model = "within", effect = "time", data = x))

eqkontrol <- function(x) summary(plm(log(M) ~ log(Ulag) + log(Vlag) +
                                      u29prc + uLTprc +
                                      log(nastrsp) + log(dmananaet) + wisshedql, 
                                    model = "within", effect = "time", data = x))

eqbase(azobpanel)
lapply(split(azobpanel, azobpanel$nuts2), eqbase)
fitbase <- lapply(split(azobpanel, azobpanel$nuts2), eqbase)
lapply(fitbase, coef)

eqpetkov(azobpanel)
lapply(split(azobpanel, azobpanel$nuts2), eqpetkov)
fitpetkov <- lapply(split(azobpanel, azobpanel$nuts2), eqpetkov)
lapply(fitpetkov, coef)
lapply(fitpetkov, pdwtest)

eqkontrol(azobpanel)
lapply(split(azobpanel, azobpanel$nuts2), eqkontrol)
fitkontrol <- lapply(split(azobpanel, azobpanel$nuts2), eqkontrol)
lapply(fitkontrol, coef)
lapply(fitkontrol, pdwtest)

fixef(zt)

    # azobl.df$bezrzamqsto
# lag(azobl.df$bezrzamqsto)

############
## Модели ##
############

azoblmes.df

azoblpanel.df

meszaplobl <- read.csv("data/mesechni_oblasti_zaplata_nsi.csv", 
                       header = F, sep = ";", skip = 1)

meszaplobl <- 
  meszaplobl %>% 
  select(nutsoblkod = V1, periyod = V2, zaplata = V3)

meszaplobl <- 
  transform(meszaplobl, godina = substr(periyod, 1, 4), 
            mesec = substr(periyod, 5, 8))

meszaplobl$mesec <- 
  as.numeric(as.roman(as.character(meszaplobl$mesec)))

meszaplobl$periyod <- paste0(meszaplobl$godina,
                                 "-", meszaplobl$mesec,"-","01")
  
meszaplobl$periyod <- as.Date(meszaplobl$periyod, format="%Y-%m-%d")

meszaplobl <- 
  oblkodowe %>% select(nutsoblkod, ekatteoblkod) %>% 
  inner_join(meszaplobl) %>% inner_join(mrzmesechni) %>% 
  select(-nutsoblkod) %>% 
  group_by(ekatteoblkod) %>% 
  mutate(izglzaplata = final(seas(ts(zaplata, start = c(2010,1), frequency = 12))),
         mrzproc = mrz/zaplata*100,
         izglmrzproc = mrz/izglzaplata*100)

meszaplobl.df <-
  meszaplobl %>% 
  select(-godina, -mesec) %>% 
  inner_join(azoblmes.df) %>% 
  ungroup

glimpse(meszaplobl.df)

meszaplobl.df <- 
  meszaplobl.df %>% 
  arrange(ekatteoblkod, periyod) %>%
  group_by(ekatteoblkod) %>% 
  mutate(lag1swmesta = dplyr::lag(swobodnimesta, 1),
         lag2swmesta = dplyr::lag(swobodnimesta, 2),
         lag3swmesta = dplyr::lag(swobodnimesta, 3),
         lag4swmesta = dplyr::lag(swobodnimesta, 4),
         lag1postypili = dplyr::lag(postypilinarab, 1),
         lag2postypili = dplyr::lag(postypilinarab, 2),
         lag3postypili = dplyr::lag(postypilinarab, 3),
         lag4postypili = dplyr::lag(postypilinarab, 4),
         lag1swmestaizgl = dplyr::lag(izglswmesta, 1),
         lag2swmestaizgl = dplyr::lag(izglswmesta, 2),
         lag3swmestaizgl = dplyr::lag(izglswmesta, 3),
         lag4swmestaizgl = dplyr::lag(izglswmesta, 4),
         lag1postypiliizgl = dplyr::lag(izglpostypili, 1),
         lag2postypiliizgl = dplyr::lag(izglpostypili, 2),
         lag3postypiliizgl = dplyr::lag(izglpostypili, 3),
         lag4postypiliizgl = dplyr::lag(izglpostypili, 4),
         lag1naeti = dplyr::lag(naeti, 1),
         lag2naeti = dplyr::lag(naeti, 2),
         lag3naeti = dplyr::lag(naeti, 3),
         lag4naeti = dplyr::lag(naeti, 4),
         lag1naetiizgl = dplyr::lag(izglnaeti, 1),
         lag2naetiizgl = dplyr::lag(izglnaeti, 2),
         lag3naetiizgl = dplyr::lag(izglnaeti, 3),
         lag4naetiizgl = dplyr::lag(izglnaeti, 4),
         ) %>% 
  ungroup

meszaplobl.panel <- pdata.frame(meszaplobl.df, index = c("ekatteoblkod", "periyod"))                                )

summary(plm(log(swobodnimesta) ~ log(mrzproc), 
            data = meszaplobl.panel,
            model="within", effect="twoways"))

summary(plm(log(lag4swmesta) ~ log(mrzproc), 
            data = meszaplobl.panel,
            model="within", effect="twoways"))

summary(plm(log(postypilinarab) ~ log(mrzproc), 
            data = meszaplobl.panel,
            model="within", effect="twoways"))         

summary(plm(log(lag4postypili) ~ log(mrzproc), 
            data = meszaplobl.panel,
            model="within", effect="twoways"))  


##
## Резултатите от панелни регресии върху месечни данни по области от АЗ
# не показват влияние на съотношението на МРЗ/СРЗ заплата и свободните работни
# места, както и постъпилите на работа.


# КИД21 по области, годишни, НСИ

# nzn3w.df <- 
#   nzn3w.df %>% 
#   filter(sektor !=0) %>% 
#   unite(obl_sek, c("oblast", "sektor")) %>% 
#   arrange(obl_sek, godina)

nzn3w.df

str(oblkodowe)

gdpnuts3.df <- read.csv("data/nama_10r_3gdp_1_Data.csv")

gdpnuts3.df <- 
  gdpnuts3.df %>% 
  mutate(Value = str_replace_all(Value, " ", ""),
         Value = as.numeric(Value)) %>% 
  select(nutsoblkod = GEO, gdpnuts3 = Value, godina = TIME) %>% 
  inner_join(oblkodowe) %>% 
  select(oblast = ekatteoblkod, godina, gdpnuts3) %>% 
  filter(godina < 2019) %>% 
  arrange(oblast, godina) 

forpanel_nzn21 <- 
  nzn3w.df %>% inner_join(gdpnuts3.df) %>% 
  mutate(sektor = as.factor(sektor),
         oblast = as.factor(oblast)) %>% 
    unite(obl_sek, c("oblast", "sektor")) %>% 
  mutate(bwpnanaet = gdpnuts3/brojn)

# write.csv(forpanel_nzn21, "data/temp_nzn21_obl.csv")

nzn.panel <- pdata.frame(forpanel_nzn21, index = c("obl_sek", "godina"))

summary(plm(log(brojn) ~ log(mrzproc), data = nzn.panel,
            model = "within", effect = "twoways"))

# сега и за заплатите по КИД21 и области 

mod_obl_kid21_zapl <- 
  plm(log(meszapl) ~ log(mrz), data = nzn.panel,
            model = "within", effect = "individual")

pbgtest(mod_obl_kid21_zapl)

se_obl_kid21_zapl <- 
  diag(sqrt(vcovHC(mod_obl_kid21_zapl, cluster = "group", method = "arellano")))

pwaldtest(mod_obl_kid21_zapl, test = "F", vcov = function(x) vcovHC(x, cluster = "group",
                                                                    method = "arellano"))

summary(pgmm(log(meszapl) ~ log(mrz) + log(bwpnanaet) + plm::lag(meszapl, 1) | 
               plm::lag(meszapl, 2:10), data = nzn.panel, effect = "individual", model = "twosteps"))

### cghi секторите

cghi.df %>% 
  group_by(sektor) %>% 
  summarise(mean(mrzproc))

cghi.df %>% 
  group_by(sektor, godina) %>% 
  summarise(sum(brojn))


names(cghi.df)

glimpse(forpanel_nzn21)

cghiforpanel <- 
  cghi.df %>% 
  inner_join(gdpnuts3.df) %>% 
  unite(obl_sek, c("oblast", "sektor")) %>% 
  mutate(bwpnanaet = gdpnuts3/brojn)
  
# write.csv(cghiforpanel, "data/temp_nzn21_nuts3_CGHI.csv", row.names = F)

cghi.panel <- pdata.frame(cghiforpanel, index = c("obl_sek", "godina"))

summary(plm(log(brojn) ~ log(mrzproc), data = cghi.panel,
            model = "within", effect = "twoways"))

# сега за заплатите

mod_obl_kid21_zapl_cghi <- 
  plm(log(meszapl) ~ log(mrz), data = cghi.panel,
      model = "within", effect = "individual")

pbgtest(mod_obl_kid21_zapl_cghi)

se_obl_kid21_zapl_cghi <- 
  diag(sqrt(vcovHC(mod_obl_kid21_zapl_cghi, cluster = "group", method = "arellano")))

pwaldtest(mod_obl_kid21_zapl_cghi, test = "F", vcov = function(x) vcovHC(x, cluster = "group",
                                                                    method = "arellano"))

summary(pgmm(log(meszapl) ~ log(mrz) + log(gdpnuts3) + plm::lag(meszapl, 1) | 
               plm::lag(meszapl, 2:3), data = cghi.panel, 
             effect = "individual", model = "twosteps"))


stargazer(mod_obl_kid21_zapl, 
          mod_obl_kid21_zapl_cghi, 
          se        = list(se_obl_kid21_zapl, se_obl_kid21_zapl_cghi),
          omit.stat = "f",
          add.lines = list(c("F Statistic (df = 1; 522) / (df = 1; 111)", "3181.3***", "2227.1***")),
          digits=3,
          type = "html",
          out="star_tables.doc")





###
# Годишни А38 и по професии
###

glimpse(nz38f.df) 

gdpannualnac.df <- read.csv("data/gdpannual_nacionalno.csv", sep = ";", header = F)

gdpannualnac.df <-
  gdpannualnac.df %>% 
  select(godina = V1, indikator = V3, stojnost = V5) %>% 
  spread(key = indikator, value = stojnost) %>% 
  rename(gdp2015ceni = 2, gdptekushticeni = 3)

nastrudspnacgod.df <- 
  read.csv("data/naselenie_trudosposobna_godishni_nacionalni.csv", sep = ";", header = F)

nastrudspnacgod.df <-
  nastrudspnacgod.df %>% 
  select(godina = V3, brojnas = V7)

gdpannualnac.df <-
  gdpannualnac.df %>% 
  inner_join(nastrudspnacgod.df)

glimpse(gpdannualnac.df)

nz38f.panel <- 
  nz38f.df %>% 
  inner_join(gdpannualnac.df) %>% 
  unite(sek_prof, c("sektor", "prof")) %>% 
  arrange(sek_prof, godina) %>% ungroup

nz38f.panel <-
  pdata.frame(nz38f.panel, index = c("sek_prof", "godina"))

nz38f.panel

# write.csv(nz38f.panel, "data/temp_nz38_zapl.csv", row.names = F)

summary(plm(log(naeti) ~ log(mrzproc), data = nz38f.panel,
            model = "within", effect = "twoways"))

summary(plm(log(naeti) ~ log(mrzproc) + log(brojnas), data = nz38f.panel,
            model = "within", effect = "individual"))

summary(plm(log(naeti) ~ log(mrzproc) + log(brojnas) + log(gdp2015ceni), 
            data = nz38f.panel,
            model = "within", effect = "individual"))

nz38f.panel

summary(plm(naeti ~ mrzproc, data = nz38f.panel,
            model = "within", effect = "twoways"))

summary(plm(meszapl ~ mrz, data = nz38f.panel,
            model = "within", effect = "individual" ))

# Влияе ли МРЗ на заплащането? КИД38 и професии

model_zapl_kid38prof <-
  plm(log(meszapl) ~ log(mrz) + log(gdptekushticeni), 
            data = nz38f.panel,
            model = "within", effect = "individual" )

pbgtest(model_zapl_kid38prof)

sqrt(diag(vcovHC(mod_zapl_sektori_noi_indfe, cluster = "group", 
                 method = "arellano")))

se_mod_zapl_kid38prof <-
  sqrt(diag(vcovHC(model_zapl_kid38prof, cluster = "group", 
                          method = "arellano")))

coeftest(model_zapl_kid38prof, 
         vcov = function(x) vcovHC(x, cluster = "group", 
                                   method = "arellano"))

se_mod_zapl_kid38prof

pwaldtest(model_zapl_kid38prof, test = "F", vcov = function(x) vcovHC(x, cluster = "group", 
                                                          method = "arellano"))

summary(model_zapl_kid38prof, vcov = function(x) vcovHC(x, cluster = "group", 
                                                                      method = "arellano"))
summary(model_zapl_kid38prof)

stargazer(model_zapl_kid38prof, 
          se = se_mod_zapl_kid38prof, 
          digits = 3, 
          type = "html",
          omit.stat = "f",
          add.lines = c("F Statistic (df = 2; 219)", "1908.9***"),
          out = "star_tables.doc")

nz38f.df %>% filter(sektor == "A", prof ==1)

summary(pgmm(log(meszapl) ~ log(mrz) + log(gdptekushticeni) + plm::lag(log(meszapl), 1) | 
               plm::lag(log(meszapl), 2:4), data = nz38f.panel, 
             effect = "individual", model = "twostep", transformation = "d", collapse = T))

modelpgmmzaplkid38prof_d <- 
  pgmm(log(meszapl) ~ log(mrz) + log(gdptekushticeni) | 
         plm::lag(log(meszapl), 2:11), data = nz38f.panel, 
       effect = "individual", model = "twostep", transformation = "d")

modelpgmmzaplkid38prof_ld <- 
  pgmm(log(meszapl) ~ log(mrz) + log(gdptekushticeni) | 
         plm::lag(log(meszapl), 2:11), data = nz38f.panel, 
       effect = "individual", model = "twostep", transformation = "ld")



screenreg(list(modelpgmmzaplkid38prof_d, modelpgmmzaplkid38prof_ld))

modelpgmmzaplkid38prof_withlag_d <- 
  pgmm(log(meszapl) ~ log(mrz) + log(gdptekushticeni) + plm::lag(log(meszapl), 1) | 
         plm::lag(log(meszapl), 2:11), data = nz38f.panel, 
       effect = "individual", model = "twosteps", transform = "d")

modelpgmmzaplkid38prof_withlag_ld <- 
  pgmm(log(meszapl) ~ log(mrz) + log(gdptekushticeni) + plm::lag(log(meszapl), 1) | 
         plm::lag(log(meszapl), 2:11), data = nz38f.panel, 
       effect = "individual", model = "twosteps", transform = "ld")

screenreg(list(modelpgmmzaplkid38prof_withlag_d, modelpgmmzaplkid38prof_withlag_ld))

screenreg(list(modelpgmmzaplkid38prof, modelpgmmzaplkid38prof_withlag))
stargazer(modelpgmmzaplkid38prof, modelpgmmzaplkid38prof_withlag, type = "text")

stargazer(modelpgmmzaplkid38prof_withlag_d, 
          modelpgmmzaplkid38prof_withlag_ld,
          #   se        = list(se_modnoiz, se_modnoiz_adj),
          #     omit.stat = "f",
          #    add.lines = list(c("F Statistic (df = 2; 51)", "483.61***", "461.22***")),
          digits=3,
          type = "html",
          out = "star_tables.doc")


  ?plm::lag

?plm::lag.plm

summary(modelpgmmzaplkid38prof)
      
screenreg(modelpgmmzaplkid38prof)
screenreg(modelpgmmzaplkid38prof_withlag)



stargazer(modelpgmmzaplkid38prof, 
          digits = 3, 
          type = "text")

stargazer(modelpgmmzaplkid38prof, 
          digits = 3, 
          type = "html",
          out = "star_tables.doc")

  ### USE QUANTILE REGRESSION with nz38f.panel

###
# годишни по общини НСИ
###

nzobshtgodfzapanel <-
  nzobshtgodf.df %>% 
  group_by(obshtkod) %>% 
  summarise(srednomrzproc = mean(mrzproc)) %>% 
  mutate(tilesmrzproc = ntile(srednomrzproc, n = 10)) %>% 
  inner_join(nzobshtgodf.df) %>% 
  inner_join(gdpannualnac.df)

nzobshtgodf.panel <- pdata.frame(nzobshtgodfzapanel, c("obshtkod", "godina"))

# write.csv(nzobshtgodf.panel, "data/temp_nzobshtgodf.csv", row.names = F)

summary(plm(log(naeti) ~ log(mrzproc) + log(gdptekushticeni), data = nzobshtgodf.panel,
            model = "within", effect = "twoways" ))

summary(plm(log(meszapl) ~ log(mrz) + log(gdptekushticeni), data = nzobshtgodf.panel,
            model = "within", effect = "individual" ))

# в общините с много ниски заплати може да няма големи промени в броя на наетите,
# защото в тях заетостта се осигурява главно от държавата и нейните структури


nzobsht10 <- nzobshtgodfzapanel %>% filter(tilesmrzproc == 10)

nzobsht10 %>% arrange(-srednomrzproc)
    
nzobsht10.panel <- pdata.frame(nzobsht10, index = c("obshtkod", "godina"))

nzobsht10.panel

nz38.df %>% 
  filter(sektor == 0, prof == 0, godina == 2017)

# влияние на МРЗ върху СРЗ по общини като цяло - годишни данни

mod_zapl_obsht_god_all <- 
  plm(log(meszapl) ~ log(mrz) + log(gdptekushticeni),
            data = nzobshtgodf.panel,
            model = "within", effect = "individual" )

summary(mod_zapl_obsht_god_all)

pbgtest(mod_zapl_obsht_god_all)

coeftest(mod_zapl_obsht_god_all)
coeftest(mod_zapl_obsht_god_all, vcov = vcovHC, cluster = "group", 
        method = "arellano")

se_obsht_zapl_god_all <- 
  sqrt(diag(vcovHC(mod_zapl_obsht_god_all, cluster = "group", 
                 method = "arellano")))

plm::pwaldtest(mod_zapl_obsht_god_all, 
               test = "F", vcov = function(x) vcovHC(x, cluster = "group", 
                                                     method = "arellano"))

summary(pgmm(log(meszapl) ~ log(mrz) + log(gdptekushticeni) + plm::lag(log(meszapl))  | 
       plm::lag(log(meszapl), 2:7), data = nzobshtgodf.panel, 
     effect = "individual", model = "twostep", transformation = "d"))

# влияние на МРЗ върху СРЗ по общини - годишни (перцентил с най-ниско заплащане)

# write.csv(nzobsht10.panel, "data/temp_nzobshtgod10.csv", row.names = F)

mod_zapl_obsht_god_lower10 <- 
  plm(log(meszapl) ~ log(mrz) + log(gdptekushticeni),
            data = nzobsht10.panel,
                        model = "within", effect = "individual" )

summary(mod_zapl_obsht_god_lower10)

pbgtest(mod_zapl_obsht_god_lower10)

coeftest(mod_zapl_obsht_god_lower10, vcov = vcovHC, cluster = "group", 
         method = "arellano")

se_obsht_zapl_god_10 <-
  sqrt(diag(vcovHC(mod_zapl_obsht_god_lower10, cluster = "group", 
                                           method = "arellano")))

plm::pwaldtest(mod_zapl_obsht_god_lower10, 
               test = "F", vcov = function(x) vcovHC(x, cluster = "group", 
                                                     method = "arellano"))

summary(pgmm(log(meszapl) ~ log(mrz) + log(gdptekushticeni) + plm::lag(log(meszapl))  | 
               plm::lag(log(meszapl), 2:7), data = nzobsht10.panel, 
             effect = "individual", model = "twostep", transformation = "d"))

screenreg(list(mod_zapl_obsht_god_all, mod_zapl_obsht_god_lower10))

stargazer(mod_zapl_obsht_god_all, 
          mod_zapl_obsht_god_lower10, 
          se        = list(se_obsht_zapl_god_10, se_obsht_zapl_god_all),
          omit.stat = "f",
          add.lines = list(c("F Statistic (df = 1; 248) / (df = 1; 23)", "3251***", "566.52***")),
          digits=3,
          type = "html",
          out="star_tables.doc")

###
# Тримесечни по сектори - НОИ
###

noisektori.df

noisektorinewolatilni

# Те са смесени

noisektoriadjusted.df %>% 
  mutate(lag1naetiadj = dplyr::lag(tsnaetiadjusted),
         lag2naetiadj = dplyr::lag(tsnaetiadjusted, 2),
         lag3naetiadj = dplyr::lag(tsnaetiadjusted, 3),
         lag4naetiadj = dplyr::lag(tsnaetiadjusted, 4))

noisektoriadj.panel <- pdata.frame(noisektoriadjusted.df, index = c("sektor", "periyod"))

summary(plm(log(tsnaetiadjusted) ~ log(tssrzapladjusted),
            data = noisektoriadj.panel,
            model = "within", effect = "twoways"))

###
# Месечни по общини АЗ
###

azn.df

aznforpanel <-
  nzobshtgodf.df %>% 
  group_by(obshtkod) %>% 
  summarise(srednomrzproc = mean(mrzproc)) %>% 
  mutate(tilesmrzproc = ntile(srednomrzproc, n = 10)) %>% 
  inner_join(azn.df)

# DID иначе не може, защото нямаме месечни заплати по общини
# трябва обаче децилните групи да се изчислят само за годината, в която
# се прави DID

nuts3kontrol.df <- read.csv("data/nuts3_kontrol.csv", sep=";", head = F)

str(nuts3kontrol.df)

nuts3dqlpredpr.df <- 
  nuts3kontrol.df %>% 
  select(oblast = V1, godina = V2, pokazatel = V3, stojnost = V5) %>% 
  filter(str_detect(pokazatel, "предприятията")) %>% 
  mutate(pokazatel = str_remove_all(pokazatel, "Относителен дял на предприятията с "),
         pokazatel = str_remove_all(pokazatel, " заети лица в общия брой предприятия за областта")) %>% 
  spread(key = "pokazatel", value = "stojnost") %>% 
  rename(ot10do49 = 3, ot50do249 = 4, do9 = 5, nad250 = 6) %>% 
  select(oblast, godina, do9, ot10do49, ot50do249, nad250)

nuts3dqlpredpr.df <- 
  nzn3w.df %>% 
  select(oblast, godina, mrz) %>% 
  inner_join(nzn3.df) %>% 
  filter(sektor == 0) %>% 
  mutate(godzapl = as.numeric(as.character(godzapl)),
         brojn = as.numeric(as.character(brojn)),
         meszapl = godzapl/12,
         mrzproc = mrz/meszapl) %>% 
  select(oblast, godina, mrz, meszapl, mrzproc, brojn) %>% 
  mutate(oblast = as.factor(oblast)) %>% 
  inner_join(nuts3dqlpredpr.df)

nuts3dqlpredpr.df %>% 
  ggplot(aes(x = do9, y = mrzproc)) + geom_point()

nuts3dqlpredpr.df %>% 
  ggplot(aes(x = ot10do49, y = mrzproc)) + geom_point()

nuts3dqlpredpr.df %>% 
  ggplot(aes(x = ot50do249, y = mrzproc)) + geom_point()

enterprises.df <- read.csv("data/bd_enace2_r3_1_Data.csv")


enterprises.df <- 
  oblkodowe %>% select(GEO = 1, oblast = 2) %>% 
  inner_join(enterprises.df)

glimpse(enterprises.df)

enterprises.df <- 
  enterprises.df %>% 
  filter(TIME > 2010) %>% 
  select(oblast, godina = TIME, indikator= INDIC_SB, sektor = NACE_R2, Value) %>% 
  mutate(Value = str_replace_all(Value, " ","")) %>% 
  mutate(godina = as.factor(godina),
         Value = as.numeric(Value))

nuts3predpr.df <-
  enterprises.df %>% 
  filter(sektor == "B-S_X_K642") %>% 
  select(oblast, godina, indikator, Value) %>% 
  spread(key = "indikator", value = "Value") %>% 
  mutate(srnaetiwpredpr = V16911/V11910) %>% 
  select(oblast, godina, srnaetiwpredpr) %>% 
  mutate(godina = as.integer(as.character(godina))) %>% 
  inner_join(nuts3dqlpredpr.df)

nuts3predpr.df %>% 
  ggplot(aes(x = srnaetiwpredpr, y = mrzproc)) + geom_point()
