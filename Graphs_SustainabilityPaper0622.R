library(readxl)
library(Rcpp)
library(dplyr)
library(tidyverse)
library(FSA)
library(psych)
library(GGally)
library(PerformanceAnalytics)
library(dendextend)
library(plotly)
library(foreign)
library(writexl)
library(ggalt)
library(ggforce)
library(gghighlight)
library(gridExtra)
library(Hmisc)
library(corrplot)
library(xtable)

#Land Cover Graphs
#Import data
LaoSDG_ALL_LC <- read_excel("~/SDG paper/Graphs/IndicatorPSN_1221.xlsx")

LaoSDG_ALL_LC$Natpop<-LaoSDG_ALL_LC$Natpop15-LaoSDG_ALL_LC$Natpop05
#LaoSDG_ALL_LC$LNatpop<-LaoSDG_ALL_LC$LNatpop15-LaoSDG_ALL_LC$LNatpop05
LaoSDG_ALL_LC$LNatpop<-log(LaoSDG_ALL_LC$Natpop15)-log(LaoSDG_ALL_LC$Natpop05)

LaoSDG_ALL_LC$Croppop<-LaoSDG_ALL_LC$Croppop15-LaoSDG_ALL_LC$Croppop05
LaoSDG_ALL_LC$LCroppop<-LaoSDG_ALL_LC$LCroppop15-LaoSDG_ALL_LC$LCroppop05

LaoSDG_ALL_LC$Orchpop<-LaoSDG_ALL_LC$Orchpop15-LaoSDG_ALL_LC$Orchpop05
LaoSDG_ALL_LC$LOrchpop<-LaoSDG_ALL_LC$LOrchpop15-LaoSDG_ALL_LC$LOrchpop05

LaoSDG_ALL_LC$Grasspop<-LaoSDG_ALL_LC$Grasspop15-LaoSDG_ALL_LC$Grasspop05
LaoSDG_ALL_LC$LGrasspop<-LaoSDG_ALL_LC$LGrasspop15-LaoSDG_ALL_LC$LGrasspop05

LaoSDG_ALL_LC$Prov<-round(LaoSDG_ALL_LC$Dcode/100)
LaoSDG_ALL_LC$tot_pop<-LaoSDG_ALL_LC$tot_pop15-LaoSDG_ALL_LC$tot_pop05
LaoSDG_ALL_LC$tot_popgr<-(LaoSDG_ALL_LC$tot_pop15-LaoSDG_ALL_LC$tot_pop05)/LaoSDG_ALL_LC$tot_pop05

LaoSDG_ALL_LC$Agric05<-LaoSDG_ALL_LC$cropland05+LaoSDG_ALL_LC$orchard05 +LaoSDG_ALL_LC$grasslands05
LaoSDG_ALL_LC$Agric15<-LaoSDG_ALL_LC$cropland15+LaoSDG_ALL_LC$orchard15 +LaoSDG_ALL_LC$grasslands15
LaoSDG_ALL_LC$Agric<-LaoSDG_ALL_LC$Agric15-LaoSDG_ALL_LC$Agric05
LaoSDG_ALL_LC$AgricPop05<-LaoSDG_ALL_LC$Agric05/LaoSDG_ALL_LC$tot_pop05
LaoSDG_ALL_LC$LAgricPop05<-log(LaoSDG_ALL_LC$Agric05/LaoSDG_ALL_LC$tot_pop05)
LaoSDG_ALL_LC$AgricPop15<-LaoSDG_ALL_LC$Agric15/LaoSDG_ALL_LC$tot_pop15
LaoSDG_ALL_LC$LAgricPop15<-log(LaoSDG_ALL_LC$Agric15/LaoSDG_ALL_LC$tot_pop15)
LaoSDG_ALL_LC$AgricPop<-LaoSDG_ALL_LC$AgricPop15-LaoSDG_ALL_LC$AgricPop05
LaoSDG_ALL_LC$LAgricPop<-LaoSDG_ALL_LC$LAgricPop15-LaoSDG_ALL_LC$LAgricPop05

LaoSDG_ALL_LC$LAgricPop2<-(log(LaoSDG_ALL_LC$Agric15)/LaoSDG_ALL_LC$tot_pop15)-(log(LaoSDG_ALL_LC$Agric05)/LaoSDG_ALL_LC$tot_pop05)
LaoSDG_ALL_LC$LCroppop2<-(log(LaoSDG_ALL_LC$cropland15)/LaoSDG_ALL_LC$tot_pop15)-(log(LaoSDG_ALL_LC$cropland05)/LaoSDG_ALL_LC$tot_pop05)
LaoSDG_ALL_LC$LNatpop2<-(log(LaoSDG_ALL_LC$PerNatural15)/LaoSDG_ALL_LC$tot_pop15)-(log(LaoSDG_ALL_LC$PerNatural05)/LaoSDG_ALL_LC$tot_pop05)

LaoSDG_ALL_LC$NatArea<-(LaoSDG_ALL_LC$Natpop15*LaoSDG_ALL_LC$tot_pop15)-(LaoSDG_ALL_LC$Natpop05*LaoSDG_ALL_LC$tot_pop05)

#write_xlsx(LaoSDG_ALL_LC,"~/SDG paper/Graphs/IndicatorAll_0721.xlsx")

#Correlation Graphs (700x500)

Correlation3Lpc_15<-select(LaoSDG_ALL_LC,Pov15,Stunt15,LNatpop15) #Final one

ggpairs(Correlation3Lpc_15,lower=list(continuous=wrap("smooth_loess",size=0.1)),
        title = "B", 
        columnLabels = c("Poverty Headcount", "Stunting Rate", "Natural Area p.c. (log)"))

Correlation3Lpc_05<-select(LaoSDG_ALL_LC,Pov05,Stunt05,LNatpop05) #Final one

ggpairs(Correlation3Lpc_05,lower=list(continuous=wrap("smooth_loess",size=0.1)),
        title = "A", 
        columnLabels = c("Poverty Headcount", "Stunting Rate", "Natural Area p.c. (log)"))

Correlation3Lpc_CH<-select(LaoSDG_ALL_LC,PovChange,StuntChange,LNatpop) #Final one

ggpairs(Correlation3Lpc_CH,lower=list(continuous=wrap("smooth_loess",size=0.1)),
        title = "B. Correlation SDGs Change", 
        columnLabels = c("Poverty Headcount", "Stunting Rate", "Natural Area p.c."))

# res15 <- rcorr(as.matrix(Correlation3Lpc_15))
# corrplot(res15$r, type = "upper", order = "hclust", 
#          p.mat = res15$P, sig.level = 0.01, insig = "blank",tl.col = "black", tl.srt = 45)
# 
# res05 <- rcorr(as.matrix(Correlation3Lpc_05))
# corrplot(res05$r, type = "upper", order = "hclust", 
#          p.mat = res05$P, sig.level = 0.01, insig = "blank",tl.col = "black", tl.srt = 45)

# CorrelationLpc_All<-select(LaoSDG_ALL_LC,
#                            Pov05,Stunt05,LNatpop05,LAgricPop05,Pov15,Stunt15,LNatpop15,LAgricPop15,PovChange,StuntChange,LNatpop,LAgricPop,NatArea,Agric) #Final one

#CorrelationLpc_All<-select(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],
#                            Pov05,Stunt05,LNatpop05,LAgricPop05,Pov15,Stunt15,LNatpop15,LAgricPop15,PovChange,StuntChange,LNatpop,LAgricPop,NatArea,Agric) #Final one

#CorrelationLpc_All<-select(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),],
#                           Pov05,Stunt05,LNatpop05,LAgricPop05,Pov15,Stunt15,LNatpop15,LAgricPop15,PovChange,StuntChange,LNatpop,LAgricPop,NatArea,Agric) #Final one

CorrelationLpc_All<-select(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2 & LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],
                            Pov05,Stunt05,LNatpop05,LAgricPop05,Pov15,Stunt15,LNatpop15,LAgricPop15,PovChange,StuntChange,LNatpop,LAgricPop,NatArea,Agric) #Final one

#res<-cor(CorrelationLpc_All)
resAll <- rcorr(as.matrix(CorrelationLpc_All))
colnames(resAll$r) <- c("Poverty Rate 2005","Stunting Rate 2005","Natural Area p.c 2005 (log)","Agricultural area p.c. 2005 (log)",
                      "Poverty Rate 2015","Stunting Rate 2015","Natural Area p.c 2015 (log)","Agricultural area p.c. 2015 (log)",
                      "\u0394 Poverty Rate","\u0394 Stunting Rate",
                      "\u0394 Natural area p.c (log)","\u0394 Agricultural area p.c. (log)",
                      "\u0394 Natural area","\u0394 Agricultural area")
rownames(resAll$r) <- c("Poverty Rate 2005","Stunting Rate 2005","Natural Area p.c 2005 (log)","Agricultural area p.c. 2005 (log)",
                        "Poverty Rate 2015","Stunting Rate 2015","Natural Area p.c 2015 (log)","Agricultural area p.c. 2015 (log)",
                        "\u0394 Poverty Rate","\u0394 Stunting Rate",
                        "\u0394 Natural area p.c (log)","\u0394 Agricultural area p.c. (log)",
                        "\u0394 Natural area","\u0394 Agricultural area")
corrplot(resAll$r, type = "upper", order = "original", p.mat = resAll$P,
          sig.level = 0.05, insig = "blank",tl.col = "black",tl.srt = 40)

#corstars(CorrelationLpc_All) #Check function at the end

#Initial Values (1200x400)

fitP<-lm(PovChange~Pov05, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1),])
fitS<-lm(StuntChange~Stunt05, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),])
fitN<-lm((-NatChange)~PerNatural05, LaoSDG_ALL_LC)
fitLN<-lm((-LNatpop)~LNatpop05, LaoSDG_ALL_LC)


p1<-ggplot(LaoSDG_ALL_LC, aes(x=Pov05, y=PovChange))+ 
  geom_point(aes(colour=factor(`PovChange 1T 10%`)),size=2)+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty 2005") + ylab("Poverty Change (p.p.)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("A. Slope =",signif(fitP$coef[[2]], 2),
                     " P value =",signif(summary(fitP)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

p2<-ggplot(LaoSDG_ALL_LC, aes(x=Stunt05, y=StuntChange))+ 
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)),size=2)+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),] ,method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting 2005") + ylab("Stunting Change (p.p.)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("B. Slope =",signif(fitS$coef[[2]], 2),
                     " P value =",signif(summary(fitS)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

p3<-ggplot(LaoSDG_ALL_LC, aes(x=PerNatural05, y=-NatChange))+ #Final
  geom_point(colour="turquoise3",size=2)+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Natural Area 2005") + ylab("Natural Area Loss p.c. (log)")+
  labs(title = paste("C. Slope =",signif(fitN$coef[[2]], 2),
                     " P value =",signif(summary(fitN)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

# ggplot(LaoSDG_ALL_LC, aes(x=LNatpop05, y=-LNatpop))+ 
#   geom_point(colour="turquoise3",size=2)+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Ln Natural Area p.c. 2005") + ylab("Ln Natural Degradation p.c.")+
#   labs(title = paste(" Slope =",signif(fitLN$coef[[2]], 4),
#                      " P value =",signif(summary(fitLN)$coef[2,4], 4)))

grid.arrange(p1,p2,p3,nrow=1)

#Change (1200x400)

fitP_S<-lm(StuntChange~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),])
fitP_LN<-lm(-LNatpop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1),])
fitS_LN<-lm(-LNatpop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),])

p1<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=StuntChange))+ #Pov vs Stunt
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`),shape=factor(`StuntChangeSE 1T 10%`)),size=2)+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Stunting Change (p.p.)")+
  scale_colour_discrete(name="Signif. Pov.",labels=c("No","Yes"))+
  scale_shape_discrete(name="Signif. Stunt.", labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("C. Slope =",signif(fitP_S$coef[[2]], 2),
                     " P value =",signif(summary(fitP_S)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

p2<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=-LNatpop))+ #Pov vs Nat
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Natural Area Loss p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("B. Slope =",signif(fitP_LN$coef[[2]], 2),
                     " P value =",signif(summary(fitP_LN)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

p3<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=-LNatpop))+ #Stunt vs Nat
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Natural Area Loss p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("A. Slope =",signif(fitS_LN$coef[[2]], 2),
                     " P value =",signif(summary(fitS_LN)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

#When checking population effect use ,size=(tot_popgr-min(tot_popgr)) on aes()
grid.arrange(p3,p2,p1,nrow=1)

#Change with correlation (1200x400) FINAL

Correlation3Lpc_CH<-select(LaoSDG_ALL_LC,PovChange,`PovChange 1T 10%`,StuntChange,`StuntChangeSE 1T 10%`,LNatpop) #Final one
Corr_SP<-rcorr(as.matrix(Correlation3Lpc_CH[(Correlation3Lpc_CH$`PovChange 1T 10%`==1 & Correlation3Lpc_CH$`StuntChangeSE 1T 10%`==1),c(1,3)] ))
Corr_SN<-rcorr(as.matrix(Correlation3Lpc_CH[( Correlation3Lpc_CH$`StuntChangeSE 1T 10%`==1),c(3,5)] ))
Corr_PN<-rcorr(as.matrix(Correlation3Lpc_CH[(Correlation3Lpc_CH$`PovChange 1T 10%`==1),c(1,5)] ))

Correlation3ALpc_CH<-select(LaoSDG_ALL_LC,PovChange,`PovChange 1T 10%`,StuntChange,`StuntChangeSE 1T 10%`,LAgricPop) #Final one
Corr_PA<-rcorr(as.matrix(Correlation3ALpc_CH[(Correlation3ALpc_CH$`PovChange 1T 10%`==1),c(1,5)] ))
Corr_SA<-rcorr(as.matrix(Correlation3ALpc_CH[(Correlation3ALpc_CH$`StuntChangeSE 1T 10%`==1),c(3,5)] ))

p1<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=StuntChange))+ #Pov vs Stunt
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`),shape=factor(`StuntChangeSE 1T 10%`)),size=2)+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Stunting Change (p.p.)")+
  scale_colour_discrete(name="Signif. Pov.",labels=c("No","Yes"))+
  scale_shape_discrete(name="Signif. Stunt.", labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("C. Corr =",signif(Corr_SP$r[1,2], 2),
                     " P value =",signif(Corr_SP$P[1,2], 2)))+ 
  theme(plot.title = element_text(size=12))

p2<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=-LNatpop))+ #Pov vs Nat
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Natural Area Loss p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("B. Corr =",signif(Corr_PN$r[1,2], 2),
                     " P value =",signif(Corr_PN$P[1,2], 2)))+ 
  theme(plot.title = element_text(size=12))

p3<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=-LNatpop))+ #Stunt vs Nat
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Natural Area Loss p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("A. Corr =",signif(Corr_SN$r[1,2], 2),
                     " P value =",signif(Corr_SN$P[1,2], 2)))+ 
  theme(plot.title = element_text(size=12))

#When checking population effect use ,size=(tot_popgr-min(tot_popgr)) on aes()
grid.arrange(p3,p2,p1,nrow=1)

# Table 1 (using previous correlation output and eliminating outliers Agro only *Nat outliers do not change corr*)

Correlation4ALpc_05<-LaoSDG_ALL_LC |>
  select(Pov05, Stunt05, LNatpop05, LNatpop15, LAgricPop05,LAgricPop15) |>
  mutate(LAgricPop05=ifelse((LAgricPop05 < -2 | LAgricPop15 < -2)  ,NA, LAgricPop05))|>
  #mutate(LNatpop05=ifelse((LNatpop05 < -2 | LNatpop15 < -2)  ,NA, LNatpop05))|>
  select(-LAgricPop15,-LNatpop15)

Correlation4ALpc_15<-LaoSDG_ALL_LC |>
  select(Pov15, Stunt15,LNatpop05, LNatpop15,LAgricPop05, LAgricPop15) |>
  mutate(LAgricPop15=ifelse((LAgricPop05 < -2 | LAgricPop15 < -2) ,NA, LAgricPop15))|>
  #mutate(LNatpop15=ifelse((LNatpop05 < -2 | LNatpop15 < -2)  ,NA, LNatpop15))|>
  select(-LAgricPop05,-LNatpop05)

Corr_05<-rcorr(as.matrix(Correlation4ALpc_05))
Corr_15<-rcorr(as.matrix(Correlation4ALpc_15))

Corr_tab<-matrix(0, 5, 3)
colnames(Corr_tab) = c('2005','2015', '2015-2005')
rownames(Corr_tab) <- c('Stunting Rate – Poverty Headcount',
                        'Stunting Rate – Natural land per capita (log)',
                        'Poverty Headcount – Natural land per capita (log)',
                        'Stunting Rate – Agricultural land per capita (log)',
                        'Poverty Headcount – Agricultural land per capita (log)')

Corr_tab[c(1:2,4),1]<-Corr_05$r[c(1,3:4),2]
Corr_tab[c(3,5),1]<-Corr_05$r[c(3:4),1]
Corr_tab[c(1:2,4),2]<-Corr_15$r[c(1,3:4),2]
Corr_tab[c(3,5),2]<-Corr_15$r[c(3:4),1]
Corr_tab[1,3]<-Corr_SP$r[1,2]
Corr_tab[2,3]<-Corr_SN$r[1,2]
Corr_tab[3,3]<-Corr_PN$r[1,2]
Corr_tab[4,3]<-Corr_SA$r[1,2]
Corr_tab[5,3]<-Corr_PA$r[1,2]

#Land Cover (800x500)
# pop<-data.frame(LaoSDG_ALL_LC$Dcode, LaoSDG_ALL_LC$Province, LaoSDG_ALL_LC$tot_pop, LaoSDG_ALL_LC$tot_pop05, LaoSDG_ALL_LC$tot_pop15, LaoSDG_ALL_LC$Agric, LaoSDG_ALL_LC$cropland)
# sample<-LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),]
# eliminated<-pop[(abs(LaoSDG_ALL_LC$Agric)<129 | LaoSDG_ALL_LC$tot_pop>15000),]
# pop$ratpop<-(pop$LaoSDG_ALL_LC.tot_pop15-pop$LaoSDG_ALL_LC.tot_pop05)/pop$LaoSDG_ALL_LC.tot_pop05

# LaoSDG_ALL_LC$rate_pop<-(LaoSDG_ALL_LC$tot_pop15-LaoSDG_ALL_LC$tot_pop05)/LaoSDG_ALL_LC$tot_pop05
# sample2<-LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$rate_pop<0.3),]
# eliminated2<-pop[(abs(LaoSDG_ALL_LC$Agric)<129 | LaoSDG_ALL_LC$rate_pop>0.3),]

#Change with population abs (1200x400)

fitS_pop<-lm(StuntChange~tot_pop, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$tot_pop<30000),])
fitP_pop<-lm(PovChange~tot_pop, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$tot_pop<30000),])
fitN_pop<-lm(LNatpop~tot_pop, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<30000),])

p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<30000),], aes(x=tot_pop, y=StuntChange))+ #tot_pop vs Stunt
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)),size=2)+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$tot_pop<30000),],method='lm', formula= y~x, se=TRUE)+
  xlab("Population Change (abs)") + ylab("Stunting Change (p.p.)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("C. Slope =",signif(fitS_pop$coef[[2]], 2),
                     " P value =",signif(summary(fitS_pop)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<30000),], aes(x=tot_pop, y=PovChange))+ #Pov vs Nat
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$tot_pop<30000),],method='lm', formula= y~x, se=TRUE)+
  xlab("Population Change (abs)") + ylab("Poverty Change (p.p.)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("B. Slope =",signif(fitP_pop$coef[[2]], 2),
                     " P value =",signif(summary(fitP_pop)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<30000),], aes(x=tot_pop, y=-LNatpop2))+ #Stunt vs Nat
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(colour="turquoise3",size=2)+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<30000),],method='lm', formula= y~x, se=TRUE)+
  xlab("Population Change (abs)") + ylab("Natural Area Loss p.c. (log)")+
  #scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("A. Slope =",signif(fitN_pop$coef[[2]], 2),
                     " P value =",signif(summary(fitN_pop)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

#When checking population effect use ,size=(tot_popgr-min(tot_popgr)) on aes()
grid.arrange(p3,p2,p1,nrow=1)

#Change with population grow (1200x400) FINAL

fitS_pop<-lm(StuntChange~tot_popgr, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),])
fitP_pop<-lm(PovChange~tot_popgr, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),])
fitN_pop<-lm(NatChange~tot_popgr, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),])

p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),], aes(x=tot_popgr, y=StuntChange))+ #tot_popgr vs Stunt
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)),size=2)+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),],method='lm', formula= y~x, se=TRUE)+
  xlab("Population Growth") + ylab("Stunting Change (p.p.)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("C. Slope =",signif(fitS_pop$coef[[2]], 2),
                     " P value =",signif(summary(fitS_pop)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),], aes(x=tot_popgr, y=PovChange))+ #Pov vs Nat
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)),size=2)+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),],method='lm', formula= y~x, se=TRUE)+
  xlab("Population Growth") + ylab("Poverty Change (p.p.)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("B. Slope =",signif(fitP_pop$coef[[2]], 2),
                     " P value =",signif(summary(fitP_pop)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),], aes(x=tot_popgr, y=NatChange))+ #Stunt vs Nat
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(LandUseType)),size=2)+ #aes(colour=factor(LandUseType)) or colour="turquoise3"
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),],method='lm', formula= y~x, se=TRUE)+
  xlab("Population Growth") + ylab("Natural Area Change (% land)")+
  scale_colour_manual(name="Increase in agriculture",values=c("turquoise3","red", "blue"), labels=c("Normal","Low","High"))+ #name="Land Use Type",
  theme(legend.position = "bottom")+
  labs(title = paste("A. Slope =",signif(fitN_pop$coef[[2]], 2),
                     " P value =",signif(summary(fitN_pop)$coef[2,4], 2)))+ 
  theme(plot.title = element_text(size=12))

#When checking population effect use ,size=(tot_popgr-min(tot_popgr)) on aes()
grid.arrange(p3,p2,p1,nrow=1)


#Graphs with Ln Agro per capita 
fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC)
fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC)
fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC)
fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC)

p1<-ggplot(LaoSDG_ALL_LC,aes(x=LAgricPop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC,aes(x=LAgricPop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitApc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC,aes(x=LAgricPop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC,aes(x=LAgricPop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)


#Graphs with Ln Agro per capita (w/o Vientiane Cap)
fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])

p1<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LAgricPop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LAgricPop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitApc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LAgricPop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LAgricPop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#For legend

ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LAgricPop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)),color='Province')

#Graphs with Ln Agro per capita (High than -2) FINAL Fig E.1
sample<-LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),]
eliminated<-LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05< -2 | LaoSDG_ALL_LC$LAgricPop15< -2),]

fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),])
fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),])
fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),])
fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),])

p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),],aes(x=LAgricPop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste("A. Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),],aes(x=LAgricPop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste("B. Slope =",signif(fitApc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),],aes(x=LAgricPop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste("C. Slope =",signif(fitApc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),],aes(x=LAgricPop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste("D. Slope =",signif(fitApc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#For legend

ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),],aes(x=LAgricPop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)),color='Province')

#Graphs with Ln Agro per capita (w/o Vientiane Cap or high -2)
fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital' & LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),])
fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital'& LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),])
fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital' & LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),])
fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital'& LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),])

p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital' & LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),],aes(x=LAgricPop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital'& LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),],aes(x=LAgricPop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitApc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital' & LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),],aes(x=LAgricPop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital' & LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),],aes(x=LAgricPop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#Graphs with Ln Agro per capita (w/o low Agri change and low increase of population)
fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])

p1<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitApc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#legend
ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)),color='Province')

#Graphs with Ln Agro per capita (w low increase of population)
fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),])
fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),])
fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),])
fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),])

p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitApc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitApc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[( LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)


#Graphs with Ln Crop per capita 
fitCpc_stunt05L<-lm(Stunt05~LCroppop05,LaoSDG_ALL_LC)
fitCpc_stunt15L<-lm(Stunt15~LCroppop15,LaoSDG_ALL_LC)
fitCpc_pov05L<-lm(Pov05~LCroppop05,LaoSDG_ALL_LC)
fitCpc_pov15L<-lm(Pov15~LCroppop15,LaoSDG_ALL_LC)

p1<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste("A. Slope =",signif(fitCpc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste("B. Slope =",signif(fitCpc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste("C. Slope =",signif(fitCpc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste("D. Slope =",signif(fitCpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#For legend
ggplot(LaoSDG_ALL_LC,aes(x=LCroppop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitCpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_pov15L)$coef[2,4], 2)),color='Province')

#Graphs with Ln Crop per capita (w/o Vietiane Cap)
fitCpc_stunt05L<-lm(Stunt05~LCroppop05,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
fitCpc_stunt15L<-lm(Stunt15~LCroppop15,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
fitCpc_pov05L<-lm(Pov05~LCroppop05,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
fitCpc_pov15L<-lm(Pov15~LCroppop15,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])

p1<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LCroppop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitCpc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LCroppop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitCpc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LCroppop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitCpc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LCroppop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitCpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#For legend
ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LCroppop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitCpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_pov15L)$coef[2,4], 2)),color='Province')

#Graphs with Ln Crop per capita (w/o High -2)
fitCpc_stunt05L<-lm(Stunt05~LCroppop05,LaoSDG_ALL_LC)
fitCpc_stunt15L<-lm(Stunt15~LCroppop15,LaoSDG_ALL_LC)
fitCpc_pov05L<-lm(Pov05~LCroppop05,LaoSDG_ALL_LC)
fitCpc_pov15L<-lm(Pov15~LCroppop15,LaoSDG_ALL_LC)

p1<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitCpc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitCpc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitCpc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitCpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#For legend
ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LCroppop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitCpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_pov15L)$coef[2,4], 2)),color='Province')

#Graphs with Ln Crop per capita (with low agro and low increase pop)
fitCpc_stunt05L<-lm(Stunt05~LCroppop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitCpc_stunt15L<-lm(Stunt15~LCroppop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitCpc_pov05L<-lm(Pov05~LCroppop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitCpc_pov15L<-lm(Pov15~LCroppop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])

p1<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LCroppop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitCpc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LCroppop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitCpc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LCroppop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitCpc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LCroppop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitCpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitCpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#Graphs with Ln Orch per capita 
fitOpc_stunt05L<-lm(Stunt05~LOrchpop05,LaoSDG_ALL_LC)
fitOpc_stunt15L<-lm(Stunt15~LOrchpop15,LaoSDG_ALL_LC)
fitOpc_pov05L<-lm(Pov05~LOrchpop05,LaoSDG_ALL_LC)
fitOpc_pov15L<-lm(Pov15~LOrchpop15,LaoSDG_ALL_LC)

p1<-ggplot(LaoSDG_ALL_LC,aes(x=LOrchpop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitOpc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC,aes(x=LOrchpop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitOpc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC,aes(x=LOrchpop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitOpc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC,aes(x=LOrchpop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitOpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#Graphs with Ln Orch per capita (w high -5)
sample2<-LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),]
eliminated2<-LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05< -5 | LaoSDG_ALL_LC$LOrchpop15< -5),]

fitOpc_stunt05L<-lm(Stunt05~LOrchpop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),])
fitOpc_stunt15L<-lm(Stunt15~LOrchpop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),])
fitOpc_pov05L<-lm(Pov05~LOrchpop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),])
fitOpc_pov15L<-lm(Pov15~LOrchpop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),])

p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),],aes(x=LOrchpop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste("A. Slope =",signif(fitOpc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),],aes(x=LOrchpop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste("B. Slope =",signif(fitOpc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),],aes(x=LOrchpop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste("C. Slope =",signif(fitOpc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),],aes(x=LOrchpop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste("D. Slope =",signif(fitOpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#Graphs with Ln Orch per capita (with low agro and low increase pop)
fitOpc_stunt05L<-lm(Stunt05~LOrchpop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitOpc_stunt15L<-lm(Stunt15~LOrchpop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitOpc_pov05L<-lm(Pov05~LOrchpop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitOpc_pov15L<-lm(Pov15~LOrchpop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])

p1<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LOrchpop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchard/Plantations p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitOpc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LOrchpop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchard/Plantations  p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitOpc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LOrchpop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchard/Plantations  p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitOpc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LOrchpop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchard/Plantations  p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitOpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitOpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#Graphs with Ln Grass per capita 
fitGpc_stunt05L<-lm(Stunt05~LGrasspop05,LaoSDG_ALL_LC)
fitGpc_stunt15L<-lm(Stunt15~LGrasspop15,LaoSDG_ALL_LC)
fitGpc_pov05L<-lm(Pov05~LGrasspop05,LaoSDG_ALL_LC)
fitGpc_pov15L<-lm(Pov15~LGrasspop15,LaoSDG_ALL_LC)

p1<-ggplot(LaoSDG_ALL_LC,aes(x=LGrasspop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste("A. Slope =",signif(fitGpc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC,aes(x=LGrasspop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste("B. Slope =",signif(fitGpc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC,aes(x=LGrasspop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste("C. Slope =",signif(fitGpc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC,aes(x=LGrasspop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste("D. Slope =",signif(fitGpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#Graphs with Ln Grass per capita (No zeros) 
fitGpc_stunt05L<-lm(Stunt05~LGrasspop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),])
fitGpc_stunt15L<-lm(Stunt15~LGrasspop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),])
fitGpc_pov05L<-lm(Pov05~LGrasspop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),])
fitGpc_pov15L<-lm(Pov15~LGrasspop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),])

p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),],aes(x=LGrasspop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitGpc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),],aes(x=LGrasspop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitGpc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),],aes(x=LGrasspop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitGpc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),],aes(x=LGrasspop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitGpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#Graphs with Ln Grass per capita (with low agro and low increase pop)
fitGpc_stunt05L<-lm(Stunt05~LGrasspop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitGpc_stunt15L<-lm(Stunt15~LGrasspop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitGpc_pov05L<-lm(Pov05~LGrasspop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
fitGpc_pov15L<-lm(Pov15~LGrasspop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])

p1<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LGrasspop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste(" Slope =",signif(fitGpc_stunt05L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LGrasspop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste(" Slope =",signif(fitGpc_stunt15L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LGrasspop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste(" Slope =",signif(fitGpc_pov05L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LGrasspop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste(" Slope =",signif(fitGpc_pov15L$coef[[2]], 2),
                     " P value =",signif(summary(fitGpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#Change Agricultural (800x500)

fitP_Ap<-lm(AgricPop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),])
fitS_Ap<-lm(AgricPop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1  ),])

p1<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=AgricPop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1  ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("A. Slope =",signif(fitP_Ap$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Ap)$coef[2,4], 4)))

p2<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=AgricPop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1  ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("B. Slope =",signif(fitS_Ap$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Ap)$coef[2,4], 4)))

#Change Cropland
fitP_Cp<-lm(Croppop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),])
fitS_Cp<-lm(Croppop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 ),])

p3<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=Croppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("C. Slope =",signif(fitP_Cp$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Cp)$coef[2,4], 4)))

p4<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=Croppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("D. Slope =",signif(fitS_Cp$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Cp)$coef[2,4], 4)))

grid.arrange(p1,p2,p3,p4,nrow=2)


#Change Agricultural (800x500)

sampleChange<-LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & abs(LaoSDG_ALL_LC$LCroppop)<0.5 ),]
fitP_Ap<-lm(AgricPop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & abs(LaoSDG_ALL_LC$LCroppop)<0.5),])
fitS_Ap<-lm(AgricPop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),])

p1<-ggplot(LaoSDG_ALL_LC[abs(LaoSDG_ALL_LC$LCroppop)<0.5,], aes(x=PovChange, y=AgricPop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitP_Ap$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Ap)$coef[2,4], 4)))

p2<-ggplot(LaoSDG_ALL_LC[abs(LaoSDG_ALL_LC$LCroppop)<0.5,], aes(x=StuntChange, y=AgricPop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitS_Ap$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Ap)$coef[2,4], 4)))

#Change Cropland
fitP_Cp<-lm(Croppop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),])
fitS_Cp<-lm(Croppop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),])

p3<-ggplot(LaoSDG_ALL_LC[abs(LaoSDG_ALL_LC$LCroppop)<0.5,], aes(x=PovChange, y=Croppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitP_Cp$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Cp)$coef[2,4], 4)))

p4<-ggplot(LaoSDG_ALL_LC[abs(LaoSDG_ALL_LC$LCroppop)<0.5,], aes(x=StuntChange, y=Croppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitS_Cp$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Cp)$coef[2,4], 4)))

grid.arrange(p1,p2,p3,p4,nrow=2)


#Change Agricultural (800x500)
fitP_Ap<-lm(LAgricPop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),])
fitS_Ap<-lm(LAgricPop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),])

p1<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),], aes(x=PovChange, y=LAgricPop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitP_Ap$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Ap)$coef[2,4], 4)))

p2<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),], aes(x=StuntChange, y=LAgricPop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitS_Ap$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Ap)$coef[2,4], 4)))

#Change Cropland
fitP_Cp<-lm(LCroppop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),])
fitS_Cp<-lm(LCroppop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),])

p3<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),], aes(x=PovChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitP_Cp$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Cp)$coef[2,4], 4)))

p4<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),], aes(x=StuntChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitS_Cp$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Cp)$coef[2,4], 4)))

grid.arrange(p1,p2,p3,p4,nrow=2)

#Legends (800x500)

p1<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=PovChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  labs(title = paste(" Slope =",signif(fitP_Lp$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Lp)$coef[2,4], 4)))

p2<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=StuntChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  labs(title = paste(" Slope =",signif(fitS_Lp$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Lp)$coef[2,4], 4)))

grid.arrange(p1,p2,nrow=2)

#Change Agricultural (800x500)
fitP_Ap<-lm(LAgricPop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),])
fitS_Ap<-lm(LAgricPop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),])

p1<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=PovChange, y=LAgricPop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitP_Ap$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Ap)$coef[2,4], 4)))

p2<-ggplot(LaoSDG_ALL_LC[ LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=StuntChange, y=LAgricPop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitS_Ap$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Ap)$coef[2,4], 4)))

#Change Cropland
fitP_Cp<-lm(LCroppop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),])
fitS_Cp<-lm(LCroppop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),])

p3<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=PovChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitP_Cp$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Cp)$coef[2,4], 4)))

p4<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=StuntChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitS_Cp$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Cp)$coef[2,4], 4)))

grid.arrange(p1,p2,p3,p4,nrow=2)

#Log Change Agriculture (all)
fitP_Ap<-lm(LAgricPop2~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),])
fitS_Ap<-lm(LAgricPop2~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),])

p1<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=LAgricPop2))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitP_Ap$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Ap)$coef[2,4], 4)))

p2<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=LAgricPop2))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitS_Ap$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Ap)$coef[2,4], 4)))

#Change Cropland
fitP_Cp<-lm(LCroppop2~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),])
fitS_Cp<-lm(LCroppop2~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 ),])

p3<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=LCroppop2))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitP_Cp$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Cp)$coef[2,4], 4)))

p4<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=LCroppop2))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste(" Slope =",signif(fitS_Cp$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Cp)$coef[2,4], 4)))

grid.arrange(p1,p2,p3,p4,nrow=2)

#Legends (800x500)

p1<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=PovChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  labs(title = paste(" Slope =",signif(fitP_Lp$coef[[2]], 4),
                     " P value =",signif(summary(fitP_Lp)$coef[2,4], 4)))

p2<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=StuntChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  labs(title = paste(" Slope =",signif(fitS_Lp$coef[[2]], 4),
                     " P value =",signif(summary(fitS_Lp)$coef[2,4], 4)))

grid.arrange(p1,p2,nrow=2)

#Last Map Graphs (500x400)

Xaysetha <- data.frame(
  name=c("Poverty","Stunting","Natural"),
  value=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1701,c(9,15,19)])*100,2)),
  perc=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1701,c(9,15,19)])*100,2))
)

Xaysetha[1,3]<-Xaysetha[1,3]*.2
Xaysetha[2,3]<-Xaysetha[2,3]*.2
Xaysetha[3,3]<--Xaysetha[3,3]
Xaysetha[3,2]<--Xaysetha[3,2]


ggplot(Xaysetha, aes(x=name,y=perc, fill=name))+
  geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=c("#73ae80", "#9972af","#c8b35a"))+
  geom_text(aes(label=value), nudge_y=c(7,7,1), color="black", size=4.5)+
  xlab("") + theme_minimal(base_size = 14)+
  labs(title="Xaysetha District")+
  theme(legend.position='none',axis.text.x = element_text(face="bold", size=18),plot.title = element_text(face="bold",size=25))+
  geom_hline(yintercept = 0)+ 
  scale_y_continuous(name="Natural Degradation (p.p.)",limits = c(-7, 7),
                     sec.axis = sec_axis(~.*5, name=paste("Poverty and Stunting Change (p.p.)")))


Xaybuly <- data.frame(
  name=c("Poverty","Stunting","Natural"),
  value=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1311,c(9,15,19)])*100,2)),
  perc=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1311,c(9,15,19)])*100,2))
)

Xaybuly[1,3]<-Xaybuly[1,3]*.2
Xaybuly[2,3]<-Xaybuly[2,3]*.2
Xaybuly[3,3]<--Xaybuly[3,3]
Xaybuly[3,2]<--Xaybuly[3,2]


ggplot(Xaybuly, aes(x=name,y=perc, fill=name))+
  geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=c("#73ae80", "#9972af","#c8b35a"))+
  geom_text(aes(label=value), nudge_y=c(4.5,4,0.8), color="black", size=4.5)+
  xlab("") + theme_minimal(base_size = 14)+
  labs(title="Xaybuly District")+
  theme(legend.position='none',axis.text.x = element_text(face="bold", size=18),plot.title = element_text(face="bold",size=25))+
  geom_hline(yintercept = 0)+ 
  scale_y_continuous(name="Natural Degradation (p.p.)",limits = c(-7, 7),
                     sec.axis = sec_axis(~.*5, name=paste("Poverty and Stunting Change (p.p.)")))


Phalanxay <- data.frame(
  name=c("Poverty","Stunting","Natural"),
  value=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1315,c(9,15,19)])*100,2)),
  perc=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1315,c(9,15,19)])*100,2))
)

Phalanxay[1,3]<-Phalanxay[1,3]*.2
Phalanxay[2,3]<-Phalanxay[2,3]*.2
Phalanxay[3,3]<--Phalanxay[3,3]
Phalanxay[3,2]<--Phalanxay[3,2]


ggplot(Phalanxay, aes(x=name,y=perc, fill=name))+
  geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=c("#73ae80", "#9972af","#c8b35a"))+
  geom_text(aes(label=paste(value,"%")), nudge_y=c(4,3.1,1), color="black", size=4.5)+
  xlab("") + theme_minimal(base_size = 14)+
  labs(title="Phalanxay District")+
  theme(legend.position='none',axis.text.x = element_text(face="bold", size=18),plot.title = element_text(face="bold",size=25))+
  geom_hline(yintercept = 0)+ 
  scale_y_continuous(name="Natural Degradation (p.p.)",limits = c(-7, 7),
                     sec.axis = sec_axis(~.*5, name=paste("Poverty and Stunting Change (p.p.)")))


Xaysathan <- data.frame(
  name=c("Poverty","Stunting","Natural"),
  value=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==811,c(9,15,19)])*100,2)),
  perc=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==811,c(9,15,19)])*100,2))
)

Xaysathan[1,3]<-Xaysathan[1,3]*.2
Xaysathan[2,3]<-Xaysathan[2,3]*.2
Xaysathan[3,3]<--Xaysathan[3,3]
Xaysathan[3,2]<--Xaysathan[3,2]


ggplot(Xaysathan, aes(x=name,y=perc, fill=name))+
  geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=c("#73ae80", "#9972af","#c8b35a"))+
  geom_text(aes(label=value), nudge_y=c(6,5,0.5), color="black", size=4.5)+
  xlab("") + theme_minimal(base_size = 14)+
  labs(title="Xaysathan District")+
  theme(legend.position='none',axis.text.x = element_text(face="bold", size=18),plot.title = element_text(face="bold",size=25))+
  geom_hline(yintercept = 0)+ 
  scale_y_continuous(name="Natural Degradation (p.p.)",limits = c(-7, 7),
                     sec.axis = sec_axis(~.*5, name=paste("Poverty and Stunting Change (p.p.)")))

Sanxay <- data.frame(
  name=c("Poverty","Stunting","Natural"),
  value=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1704,c(9,15,19)])*100,2)),
  perc=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1704,c(9,15,19)])*100,2))
)

Sanxay[1,3]<-Sanxay[1,3]*.2
Sanxay[2,3]<-Sanxay[2,3]*.2
Sanxay[3,3]<--Sanxay[3,3]
Sanxay[3,2]<--Sanxay[3,2]


ggplot(Sanxay, aes(x=name,y=perc, fill=name))+
  geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=c("#73ae80", "#9972af","#c8b35a"))+
  geom_text(aes(label=value), nudge_y=c(9,5,1), color="black", size=4.5)+
  xlab("") + theme_minimal(base_size = 14)+
  labs(title="Sanxay District")+
  theme(legend.position='none',axis.text.x = element_text(face="bold", size=18),plot.title = element_text(face="bold",size=25))+
  geom_hline(yintercept = 0)+ 
  scale_y_continuous(name="Natural Degradation (p.p.)",limits = c(-10, 10),
                     sec.axis = sec_axis(~.*5, name=paste("Poverty and Stunting Change (p.p.)")))

Sing <- data.frame(
  name=c("Poverty","Stunting","Natural"),
  value=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==302,c(9,15,19)])*100,2)),
  perc=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==302,c(9,15,19)])*100,2))
)

Sing[1,3]<-Sing[1,3]*.2
Sing[2,3]<-Sing[2,3]*.2
Sing[3,3]<--Sing[3,3]
Sing[3,2]<--Sing[3,2]


ggplot(Sing, aes(x=name,y=perc, fill=name))+
  geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=c("#73ae80", "#9972af","#c8b35a"))+
  geom_text(aes(label=paste(value,"%")), nudge_y=c(3.5,3.3,1), color="black", size=4.5)+
  xlab("") + theme_minimal(base_size = 14)+
  labs(title="Sing District")+
  theme(legend.position='none',axis.text.x = element_text(face="bold", size=18),plot.title = element_text(face="bold",size=25))+
  geom_hline(yintercept = 0)+ 
  scale_y_continuous(name="Natural Degradation (p.p.)",limits = c(-7, 7),
                     sec.axis = sec_axis(~.*5, name=paste("Poverty and Stunting Change (p.p.)")))

Sone <- data.frame(
  name=c("Poverty","Stunting","Natural"),
  value=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==710,c(9,15,19)])*100,2)),
  perc=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==710,c(9,15,19)])*100,2))
)

Sone[1,3]<-Sone[1,3]*.2
Sone[2,3]<-Sone[2,3]*.2
Sone[3,3]<--Sone[3,3]
Sone[3,2]<--Sone[3,2]


ggplot(Sone, aes(x=name,y=perc, fill=name))+
  geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=c("#73ae80", "#9972af","#c8b35a"))+
  geom_text(aes(label=paste(value,"%")), nudge_y=c(3,3,1), color="black", size=4.5)+
  xlab("") + theme_minimal(base_size = 14)+
  labs(title="Sone District")+
  theme(legend.position='none',axis.text.x = element_text(face="bold", size=18),plot.title = element_text(face="bold",size=25))+
  geom_hline(yintercept = 0)+ 
  scale_y_continuous(name="Natural Degradation (p.p.)",limits = c(-7, 7),
                     sec.axis = sec_axis(~.*5, name=paste("Poverty and Stunting Change (p.p.)")))

###Country analysis
stunting <- read_excel("SDG paper/World_Country indicators/stunting_countries.xlsx", 
                       sheet = "Goal2")

poverty <- read_excel("SDG paper/World_Country indicators/poverty_countries.xlsx", 
                      sheet = "Goal1")

poverty2000 <- read_excel("SDG paper/World_Country indicators/poverty2.xlsx", 
                          sheet = "Goal1")

forest <- read_excel("SDG paper/World_Country indicators/forest_countries.xlsx", 
                     sheet = "Goal15")

stunting$SDG2<-as.numeric(stunting$Value)
poverty$SDG1<-as.numeric(poverty$Value)
poverty2000$SDG1<-as.numeric(poverty2000$Value)
forest$SDG15<-as.numeric(forest$Value)

Source_Stunt<-"Source: Joint Child Malnutrition Estimates (2021 Edition), United Nations Children's Fund (UNICEF), World Health Organisation (WHO) and the World Bank Group."
Source_Forest="FAO, Global Forest Resources Assessment and FAOSTAT"

#Bar graphs-Years

ggplot(stunting[(stunting$SeriesCode=="SH_STA_STNT"),],aes(x=TimePeriod))+
  geom_bar()+ggtitle("Countries with Stunting Rates")+xlab("Year")

ggplot(stunting[(stunting$SeriesCode=="SH_STA_STNTN" & stunting$Source==Source_Stunt),],aes(x=TimePeriod))+
  geom_bar()+ggtitle("Countries with Stunted Number (thousands)")+xlab("Year")

ggplot(poverty[poverty$Location=="ALLAREA",],aes(x=TimePeriod))+
  geom_bar()+ggtitle("Countries with Poverty Headcount")+xlab("Year")

ggplot(poverty2000[poverty2000$SeriesDescription=="Proportion of population below international poverty line (%)",],aes(x=TimePeriod))+
  geom_bar()+ggtitle("Countries with Poverty Headcount")+xlab("Year")


ggplot(forest[(forest$Source==Source_Forest & forest$SeriesCode=="AG_LND_FRST"),],aes(x=TimePeriod))+
  geom_bar()+ggtitle("Countries with Forest Area")+xlab("Year")

#Comparison
poverty2000$GeoAreaCode<-as.numeric(poverty2000$GeoAreaCode)

PovStunt_2000<-merge(stunting[(stunting$SeriesCode=="SH_STA_STNT" & stunting$TimePeriod==2000),c(6,7,8,15,23)], 
                     poverty2000[poverty2000$SeriesDescription=="Proportion of population below international poverty line (%)",c(6,7,8,15,25)], 
                     by="GeoAreaCode")

PovStunt_2000<-PovStunt_2000 %>%
  rename("Country SDG2"=GeoAreaName.x,
         "Country SDG1"=GeoAreaName.y,
         "Year SDG2"=TimePeriod.x ,
         "Year SDG1"=TimePeriod.y)

All_2000<-merge(PovStunt_2000,forest[(forest$TimePeriod==2000 & forest$Source==Source_Forest & forest$SeriesCode=="AG_LND_FRST"),c(6,7,8,21)], by="GeoAreaCode")


Comparison00<-select(All_2000,SDG1,SDG2,SDG15)

ggpairs(Comparison00,lower=list(continuous=wrap("smooth_loess",size=0.1)),
        title = "Correlation SDGs 2000", 
        columnLabels = c("Poverty Headcount", "Prevalence of Stunting)", "Forest Area"))


#Our World Data

stunting_OWD<- read_excel("SDG paper/World_Country indicators/OurWorldData/stunting.xlsx")
poverty_OWD<-read_excel("SDG paper/World_Country indicators/OurWorldData/poverty.xlsx")
poverty320_OWD<-read_excel("SDG paper/World_Country indicators/OurWorldData/poverty320.xlsx")
forest_OWD<- read_excel("SDG paper/World_Country indicators/OurWorldData/forest.xlsx")

stunting2_OWD<- read_excel("SDG paper/World_Country indicators/OurWorldData/stunting2.xlsx")

#Number of countries

ggplot(stunting_OWD,aes(x=Year))+
  geom_bar()+ggtitle("Countries with Stunting Rates")+xlab("Year")

ggplot(poverty_OWD,aes(x=Year))+
  geom_bar()+ggtitle("Countries with Poverty Rates")+xlab("Year")

ggplot(forest_OWD[forest_OWD$Year>2000,],aes(x=Year))+
  geom_bar()+ggtitle("Countries with Forest Rates")+xlab("Year")


ggplot(stunting2_OWD,aes(x=Year))+
  geom_bar()+ggtitle("Countries with Stunting Rates")+xlab("Year")

ggplot(poverty320_OWD,aes(x=Year))+
  geom_bar()+ggtitle("Countries with Poverty Rates")+xlab("Year")

#Comparison

PovStunt_2000_OWD<-merge(stunting2_OWD[stunting2_OWD$Year==2000,c(2,4)], 
                         poverty_OWD[(poverty_OWD$Year==2000),c(2,4)], 
                         by="Code")

All_2000_OWD<-merge(PovStunt_2000_OWD,forest_OWD[(forest_OWD$Year==2000),c(2,4)], by="Code")
All_2000_OWD$LogForest<-log(All_2000_OWD$`Forest cover`)


ggpairs(All_2000_OWD[,c(2,3,5)],lower=list(continuous=wrap("smooth_loess",size=0.1)),
        title = "Cross-country Correlation SDGs 2000", 
        columnLabels = c("Prevalence Stunting","Poverty Headcount", "Forest Area (Log)"))

PovStunt_2000_OWD<-merge(stunting2_OWD[stunting2_OWD$Year==2000,c(2,4)], 
                         poverty320_OWD[(poverty320_OWD$Year==2000),c(2,4)], 
                         by="Code")

All_2000_OWD<-merge(PovStunt_2000_OWD,forest_OWD[(forest_OWD$Year==2000),c(2,4)], by="Code")
All_2000_OWD$LogForest<-log(All_2000_OWD$`Forest cover`)


ggpairs(All_2000_OWD[,c(2,3,5)],lower=list(continuous=wrap("smooth_loess",size=0.1)),
        title = "Cross-country Correlation SDGs 2000", 
        columnLabels = c("Prevalence Stunting","Poverty Headcount", "Forest Area (Log)"))

PovStunt_2016_OWD<-merge(stunting2_OWD[stunting2_OWD$Year==2016,c(2,4)], 
                         poverty_OWD[(poverty_OWD$Year==2016),c(2,4)], 
                         by="Code")

All_2016_OWD<-merge(PovStunt_2016_OWD,forest_OWD[(forest_OWD$Year==2016),c(2,4)], by="Code")
All_2016_OWD$LogForest<-log(All_2016_OWD$`Forest cover`)


ggpairs(All_2016_OWD[,c(2,3,5)],lower=list(continuous=wrap("smooth_loess",size=0.1)),
        title = "Cross-country Correlation SDGs 2016", 
        columnLabels = c("Prevalence Stunting","Poverty Headcount", "Forest Area (Log)"))


##Box plots

#Stack databases 2000

All_2000_OWD$poverty<- All_2000_OWD$poverty/100
All_2000_OWD$`Prevalence of stunting, height for age (% of children under 5)` <- All_2000_OWD$`Prevalence of stunting, height for age (% of children under 5)` /100

CountryData00<-All_2000_OWD[,c(-5)]
CountryData00<-CountryData00 %>%
  rename("Location"=Code,
         "SDG2"="Prevalence of stunting, height for age (% of children under 5)",
         "SDG1"=poverty ,
         "SDG15"="Forest cover")
CountryData00$Code<-"Cross Country"

DistrictData<-select(LaoSDG_ALL_LC,Dcode,Pov15,Stunt15,PerNatural15)
DistrictData<-DistrictData %>%
  rename("Location"=Dcode,
         "SDG2"=Stunt15,
         "SDG1"=Pov15 ,
         "SDG15"=PerNatural15)

DistrictData$SDG15<-DistrictData$SDG15*100
DistrictData$Code<-"Subnational Laos"

AllData00<-rbind(CountryData00,DistrictData)

ggplot(AllData00, aes(x=Code, y=SDG1)) + 
  geom_boxplot()

ggplot(AllData00, aes(x=Code, y=SDG2)) + 
  geom_boxplot()

ggplot(AllData00, aes(x=Code, y=SDG15)) + 
  geom_boxplot()

#Stack databases 2016

All_2016_OWD$poverty<- All_2016_OWD$poverty/100
All_2016_OWD$`Prevalence of stunting, height for age (% of children under 5)` <- All_2016_OWD$`Prevalence of stunting, height for age (% of children under 5)` /100

CountryData16<-All_2016_OWD[,c(-5)]
CountryData16<-CountryData16 %>%
  rename("Location"=Code,
         "SDG2"="Prevalence of stunting, height for age (% of children under 5)",
         "SDG1"=poverty ,
         "SDG15"="Forest cover")
CountryData16$Code<-"Cross Country"

AllData16<-rbind(CountryData16,DistrictData)

ggplot(AllData16, aes(x=Code, y=SDG1)) + 
  geom_boxplot()

ggplot(AllData16, aes(x=Code, y=SDG2)) + 
  geom_boxplot()

ggplot(AllData16, aes(x=Code, y=SDG15)) + 
  geom_boxplot()

#For each SDG

DistrictData<-select(LaoSDG_ALL_LC,Dcode,Pov15,Stunt15,PerNatural15)
DistrictData<-DistrictData %>%
  rename("Location"=Dcode,
         "SDG2"=Stunt15,
         "SDG1"=Pov15 ,
         "SDG15"=PerNatural15)

DistrictData$SDG15<-DistrictData$SDG15*100
DistrictData$Code<-"Subnational Laos"

Stunting16<-stunting2_OWD[stunting2_OWD$Year==2016,c(2,4)]
Stunting16$`Prevalence of stunting, height for age (% of children under 5)` <- Stunting16$`Prevalence of stunting, height for age (% of children under 5)` /100
Stunting16<- Stunting16 %>%
  rename("Location"=Code,
         "SDG2"="Prevalence of stunting, height for age (% of children under 5)")
Stunting16$Code<-"Cross Country"

AllStunt16<-rbind(Stunting16,DistrictData[,c(1,3,5)])

ggplot(AllStunt16, aes(x=Code, y=SDG2, fill=Code)) + 
  geom_boxplot()+ geom_jitter(color="black", size=0.2, alpha=0.5) +
  labs(x="Geographical Level", y="Stunting Rates" )+
  theme(legend.position="none")

ggplot(AllStunt16, aes(x=Code, y=SDG2, fill=Code)) + 
  geom_violin()

# Poverty16<-poverty_OWD[(poverty_OWD$Year==2016),c(2,4)]
# Poverty16$poverty<- Poverty16$poverty/100
# Poverty16<- Poverty16 %>%
#   rename("Location"=Code,
#          "SDG1"=poverty)
# Poverty16$Code<-"Cross Country"
# 
# AllPov16<-rbind(Poverty16,DistrictData[,c(1,2,5)])
# 
# ggplot(AllPov16, aes(x=Code, y=SDG1, fill=Code)) + 
#   geom_boxplot()
#   
# ggplot(AllPov16, aes(x=Code, y=SDG1, fill=Code)) + 
#     geom_violin()

Poverty16<-poverty320_OWD[(poverty320_OWD$Year==2016),c(2,4)]
Poverty16$poverty<- Poverty16$poverty/100
Poverty16<- Poverty16 %>%
  rename("Location"=Code,
         "SDG1"=poverty)
Poverty16$Code<-"Cross Country"

AllPov16<-rbind(Poverty16,DistrictData[,c(1,2,5)])

ggplot(AllPov16, aes(x=Code, y=SDG1, fill=Code)) + 
  geom_boxplot()+ geom_jitter(color="black", size=0.2, alpha=0.5)+
  labs(x="Geographical Level", y="Poverty Rates" )+
  theme(legend.position="none")

ggplot(AllPov16, aes(x=Code, y=SDG1, fill=Code)) + 
  geom_violin()

Forest16<-forest_OWD[(forest_OWD$Year==2016),c(2,4)]
Forest16<- Forest16 %>%
  rename("Location"=Code,
         "SDG15"="Forest cover")
Forest16$Code<-"Cross Country"

AllFor16<-rbind(Forest16,DistrictData[,c(1,4,5)])

ggplot(AllFor16, aes(x=Code, y=SDG15, fill=Code)) + 
  geom_boxplot()+ geom_jitter(color="black", size=0.2, alpha=0.5)+
  labs(x="Geographical Level", y="Forest/Natural Area" )+
  theme(legend.position="none")

ggplot(AllFor16, aes(x=Code, y=SDG15, fill=Code)) + 
  geom_violin()

#Functions:
# corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
#                     result=c("none", "html", "latex")){
#   #Compute correlation matrix
#   require(Hmisc)
#   x <- as.matrix(x)
#   correlation_matrix<-rcorr(x, type=method[1])
#   R <- correlation_matrix$r # Matrix of correlation coeficients
#   p <- correlation_matrix$P # Matrix of p-value 
#   
#   ## Define notions for significance levels; spacing is important.
#   mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
#   
#   ## trunctuate the correlation matrix to two decimal
#   R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
#   
#   ## build a new matrix that includes the correlations with their apropriate stars
#   Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
#   diag(Rnew) <- paste(diag(R), " ", sep="")
#   rownames(Rnew) <- colnames(x)
#   colnames(Rnew) <- paste(colnames(x), "", sep="")
#   
#   ## remove upper triangle of correlation matrix
#   if(removeTriangle[1]=="upper"){
#     Rnew <- as.matrix(Rnew)
#     Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
#     Rnew <- as.data.frame(Rnew)
#   }
#   
#   ## remove lower triangle of correlation matrix
#   else if(removeTriangle[1]=="lower"){
#     Rnew <- as.matrix(Rnew)
#     Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
#     Rnew <- as.data.frame(Rnew)
#   }
#   
#   ## remove last column and return the correlation matrix
#   Rnew <- cbind(Rnew[1:length(Rnew)-1])
#   if (result[1]=="none") return(Rnew)
#   else{
#     if(result[1]=="html") print(xtable(Rnew), type="html")
#     else print(xtable(Rnew), type="latex") 
#   }
# } 