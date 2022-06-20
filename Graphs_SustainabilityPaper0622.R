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
library(networkD3)

### Graphs and Tables for SDG Paper
### Author: Diana Garcia

### Import data 
LaoSDG_ALL_LC <- read_excel("~/SDG paper/Graphs/IndicatorPSN_1221.xlsx")

### Transform data
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


### Appendix B.

#Initial Values (1200x400)

Corr_In<-select(LaoSDG_ALL_LC,PovChange,`PovChange 1T 10%`,StuntChange, Pov05, `StuntChangeSE 1T 10%`, Stunt05, PerNatural05, LNatpop05) #Final one
Corr_S<-rcorr(as.matrix(Corr_In[(Corr_In$`StuntChangeSE 1T 10%`==1),c(3,6)] ))
Corr_P<-rcorr(as.matrix(Corr_In[(Corr_In$`PovChange 1T 10%`==1),c(1,4)] ))
Corr_N<-rcorr(as.matrix(Corr_In[,c(7,8)]))


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
  labs(title = paste("A. Corr =",signif(Corr_P$r[1,2], 2),
                     " P value =",signif(Corr_P$P[1,2], 2)))+ 
  theme(plot.title = element_text(size=12))

p2<-ggplot(LaoSDG_ALL_LC, aes(x=Stunt05, y=StuntChange))+ 
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)),size=2)+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),] ,method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting 2005") + ylab("Stunting Change (p.p.)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("A. Corr =",signif(Corr_S$r[1,2], 2),
                     " P value =",signif(Corr_S$P[1,2], 2)))+
  theme(plot.title = element_text(size=12))

p3<-ggplot(LaoSDG_ALL_LC, aes(x=PerNatural05, y=-NatChange))+ #Final
  geom_point(colour="turquoise3",size=2)+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Natural Area 2005") + ylab("Natural Area Change (p.p.)")+
  labs(title = paste("A. Corr =",signif(Corr_N$r[1,2], 2),
                     " P value =",signif(Corr_N$P[1,2], 2)))+
  theme(plot.title = element_text(size=12))

# ggplot(LaoSDG_ALL_LC, aes(x=LNatpop05, y=-LNatpop))+
#   geom_point(colour="turquoise3",size=2)+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Ln Natural Area p.c. 2005") + ylab("Ln Natural Degradation p.c.")+
#   labs(title = paste(" Slope =",signif(fitLN$coef[[2]], 4),
#                      " P value =",signif(summary(fitLN)$coef[2,4], 4)))

grid.arrange(p1,p2,p3,nrow=1)

rm(fitLN, fitN, fitP, fitS)
rm(p1,p2,p3)
rm(Corr_In, Corr_N, Corr_P, Corr_S)

### Figure 2

#Change (1200x400)

# fitP_S<-lm(StuntChange~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),])
# fitP_LN<-lm(-LNatpop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1),])
# fitS_LN<-lm(-LNatpop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),])
# 
# p1<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=StuntChange))+ #Pov vs Stunt
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`),shape=factor(`StuntChangeSE 1T 10%`)),size=2)+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Stunting Change (p.p.)")+
#   scale_colour_discrete(name="Signif. Pov.",labels=c("No","Yes"))+
#   scale_shape_discrete(name="Signif. Stunt.", labels=c("No","Yes"))+
#   theme(legend.position = "bottom")+
#   labs(title = paste("C. Slope =",signif(fitP_S$coef[[2]], 2),
#                      " P value =",signif(summary(fitP_S)$coef[2,4], 2)))+ 
#   theme(plot.title = element_text(size=12))
# 
# p2<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=-LNatpop))+ #Pov vs Nat
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Natural Area Loss p.c. (log)")+
#   scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
#   theme(legend.position = "bottom")+
#   labs(title = paste("B. Slope =",signif(fitP_LN$coef[[2]], 2),
#                      " P value =",signif(summary(fitP_LN)$coef[2,4], 2)))+ 
#   theme(plot.title = element_text(size=12))
# 
# p3<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=-LNatpop))+ #Stunt vs Nat
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Stunting Change (p.p.)") + ylab("Natural Area Loss p.c. (log)")+
#   scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
#   theme(legend.position = "bottom")+
#   labs(title = paste("A. Slope =",signif(fitS_LN$coef[[2]], 2),
#                      " P value =",signif(summary(fitS_LN)$coef[2,4], 2)))+ 
#   theme(plot.title = element_text(size=12))
# 
# #When checking population effect use ,size=(tot_popgr-min(tot_popgr)) on aes()
# grid.arrange(p3,p2,p1,nrow=1)

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

### Table 1 

# Note: using previous correlation output from Fig 2 and eliminating outliers on Agro indicator only 
# We dont eliminate outliers on Nat indicator because correlations do not change significantly

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

View(Corr_tab)

rm(list = ls(pattern = "^Corr"))
rm(list = ls(pattern = "^p"))

### Appendix C.
LaoSDG_LC_change <- as.data.frame(read_excel("~/SDG paper/Graphs/ChangeLC0515_FV2.xlsx", sheet = "Final4"))

nodes <- data.frame( name=c(LaoSDG_LC_change$Source, 
                            LaoSDG_LC_change$Target) %>% unique() )

LaoSDG_LC_change$IDsource=match(LaoSDG_LC_change$Source, nodes$name)-1 
LaoSDG_LC_change$IDtarget=match(LaoSDG_LC_change$Target, nodes$name)-1

ColourScal ='d3.scaleOrdinal() .range(["#35B779FF","#FDE725FF","#B4DE2CFF",
"#FFB833","#5F33FF","#FF61B7","#35B779FF", "#482878FF","#440154FF","#26828EFF","#31688EFF","#3E4A89FF", "#61FFFE" ])'

sankeyNetwork(Links = LaoSDG_LC_change, Nodes = nodes,
              Source = "IDtarget", Target = "IDsource",
              Value = "Area [degree^2]", NodeID = "name",
              fontSize= 14,  width = 800, height= 500,
              colourScale=ColourScal,sinksRight=FALSE)

rm(LaoSDG_LC_change, nodes, ColourScal)

### Appendix D. 

# Correlation Graphs (700x500) FINAL

Correlation3Lpc_15<-select(LaoSDG_ALL_LC,Pov15,Stunt15,LNatpop15) #Final one

ggpairs(Correlation3Lpc_15,lower=list(continuous=wrap("smooth_loess",size=0.1)),
        title = "B. Indicators 2015", 
        columnLabels = c("Poverty Headcount", "Stunting Rate", "Natural Area p.c. (log)"))

Correlation3Lpc_05<-select(LaoSDG_ALL_LC,Pov05,Stunt05,LNatpop05) #Final one

ggpairs(Correlation3Lpc_05,lower=list(continuous=wrap("smooth_loess",size=0.1)),
        title = "A. Indicators 2005", 
        columnLabels = c("Poverty Headcount", "Stunting Rate", "Natural Area p.c. (log)"))

rm(Correlation3Lpc_05, Correlation3Lpc_15)

# Other Correlations:

# Correlation3Lpc_CH<-select(LaoSDG_ALL_LC,PovChange,StuntChange,LNatpop) 
# 
# ggpairs(Correlation3Lpc_CH,lower=list(continuous=wrap("smooth_loess",size=0.1)),
#         title = "B. Correlation SDGs Change",
#         columnLabels = c("Poverty Headcount", "Stunting Rate", "Natural Area p.c."))

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

# CorrelationLpc_All<-select(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2 & LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],
#                             Pov05,Stunt05,LNatpop05,LAgricPop05,Pov15,Stunt15,LNatpop15,LAgricPop15,PovChange,StuntChange,LNatpop,LAgricPop,NatArea,Agric) #Final one
# 
# res<-cor(CorrelationLpc_All)
# resAll <- rcorr(as.matrix(CorrelationLpc_All))
# colnames(resAll$r) <- c("Poverty Rate 2005","Stunting Rate 2005","Natural Area p.c 2005 (log)","Agricultural area p.c. 2005 (log)",
#                       "Poverty Rate 2015","Stunting Rate 2015","Natural Area p.c 2015 (log)","Agricultural area p.c. 2015 (log)",
#                       "\u0394 Poverty Rate","\u0394 Stunting Rate",
#                       "\u0394 Natural area p.c (log)","\u0394 Agricultural area p.c. (log)",
#                       "\u0394 Natural area","\u0394 Agricultural area")
# rownames(resAll$r) <- c("Poverty Rate 2005","Stunting Rate 2005","Natural Area p.c 2005 (log)","Agricultural area p.c. 2005 (log)",
#                         "Poverty Rate 2015","Stunting Rate 2015","Natural Area p.c 2015 (log)","Agricultural area p.c. 2015 (log)",
#                         "\u0394 Poverty Rate","\u0394 Stunting Rate",
#                         "\u0394 Natural area p.c (log)","\u0394 Agricultural area p.c. (log)",
#                         "\u0394 Natural area","\u0394 Agricultural area")
# corrplot(resAll$r, type = "upper", order = "original", p.mat = resAll$P,
#           sig.level = 0.05, insig = "blank",tl.col = "black",tl.srt = 40)
# 
#corstars(CorrelationLpc_All) #Check function at the end


## Tests for Agricultural land area 

## Graphs with Ln Agro per capita 
# fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC)
# fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC)
# fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC)
# fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC)
# 
# p1<-ggplot(LaoSDG_ALL_LC,aes(x=LAgricPop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p2<-ggplot(LaoSDG_ALL_LC,aes(x=LAgricPop15, y=Stunt15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
#   labs(title = paste(" Slope =",signif(fitApc_stunt15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p3<-ggplot(LaoSDG_ALL_LC,aes(x=LAgricPop05, y=Pov05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
#   labs(title = paste(" Slope =",signif(fitApc_pov05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p4<-ggplot(LaoSDG_ALL_LC,aes(x=LAgricPop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)
# 

# #Graphs with Ln Agro per capita (w/o Vientiane Cap)
# fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
# fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
# fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
# fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
# 
# p1<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LAgricPop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p2<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LAgricPop15, y=Stunt15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
#   labs(title = paste(" Slope =",signif(fitApc_stunt15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p3<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LAgricPop05, y=Pov05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
#   labs(title = paste(" Slope =",signif(fitApc_pov05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p4<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LAgricPop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)
# 
# #For legend
# 
# ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LAgricPop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)),color='Province')
#
# #Graphs with Ln Agro per capita (w/o Vientiane Cap or high -2)
# fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital' & LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),])
# fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital'& LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),])
# fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital' & LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),])
# fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital'& LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),])
# 
# p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital' & LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),],aes(x=LAgricPop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital'& LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),],aes(x=LAgricPop15, y=Stunt15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
#   labs(title = paste(" Slope =",signif(fitApc_stunt15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital' & LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),],aes(x=LAgricPop05, y=Pov05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
#   labs(title = paste(" Slope =",signif(fitApc_pov05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p4<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Province!='Vientiane Capital' & LaoSDG_ALL_LC$LAgricPop05>=-2 & LaoSDG_ALL_LC$LAgricPop15>=-2),],aes(x=LAgricPop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)
# 
# #Graphs with Ln Agro per capita (w/o low Agri change and low increase of population)
# fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# 
# p1<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p2<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop15, y=Stunt15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
#   labs(title = paste(" Slope =",signif(fitApc_stunt15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p3<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop05, y=Pov05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
#   labs(title = paste(" Slope =",signif(fitApc_pov05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p4<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)
# 
# #legend
# ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)),color='Province')
# 
# #Graphs with Ln Agro per capita (w low increase of population)
# fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),])
# fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),])
# fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),])
# fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),])
# 
# p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitApc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop15, y=Stunt15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
#   labs(title = paste(" Slope =",signif(fitApc_stunt15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop05, y=Pov05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
#   labs(title = paste(" Slope =",signif(fitApc_pov05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p4<-ggplot(LaoSDG_ALL_LC[( LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LAgricPop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(Province,Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitApc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitApc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)

### Appendix E.1

#Ln Agro per capita (High than -2) FINAL (800x500)
sample<-LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),]
eliminated<-LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05< -2 | LaoSDG_ALL_LC$LAgricPop15< -2),]

# fitApc_stunt05L<-lm(Stunt05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),])
# fitApc_stunt15L<-lm(Stunt15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),])
# fitApc_pov05L<-lm(Pov05~LAgricPop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),])
# fitApc_pov15L<-lm(Pov15~LAgricPop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),])

Corr_agro<-select(LaoSDG_ALL_LC,Stunt05,Stunt15,Pov05,Pov15,LAgricPop05,LAgricPop15) 
Corr_Sa05<-rcorr(as.matrix(Corr_agro[(Corr_agro$LAgricPop05>-2 & Corr_agro$LAgricPop15>-2),c(1,5)] ))
Corr_Sa15<-rcorr(as.matrix(Corr_agro[(Corr_agro$LAgricPop05>-2 & Corr_agro$LAgricPop15>-2),c(2,6)] ))
Corr_Pa05<-rcorr(as.matrix(Corr_agro[(Corr_agro$LAgricPop05>-2 & Corr_agro$LAgricPop15>-2),c(3,5)] ))
Corr_Pa15<-rcorr(as.matrix(Corr_agro[(Corr_agro$LAgricPop05>-2 & Corr_agro$LAgricPop15>-2),c(4,6)] ))

p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),],aes(x=LAgricPop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste("A. Corr =",signif(Corr_Sa05$r[1,2], 2),
                     " P value =",signif(Corr_Sa05$P[1,2], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),],aes(x=LAgricPop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste("B. Corr =",signif(Corr_Sa15$r[1,2], 2),
                     " P value =",signif(Corr_Sa15$P[1,2], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),],aes(x=LAgricPop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste("C. Corr =",signif(Corr_Pa05$r[1,2], 2),
                     " P value =",signif(Corr_Pa05$P[1,2], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),],aes(x=LAgricPop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste("D. Corr =",signif(Corr_Pa15$r[1,2], 2),
                     " P value =",signif(Corr_Pa15$P[1,2], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#For side legend (ordered provinces from north to south)

ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LAgricPop05>-2 & LaoSDG_ALL_LC$LAgricPop15>-2),],aes(x=LAgricPop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Agricultural land p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(color='Province')

rm(sample, eliminated)
rm(list = ls(pattern = "^fit"))
rm(list = ls(pattern = "^Corr"))
rm(list = ls(pattern = "^p"))

## Tests for Cropland 
# 
# #Graphs with Ln Crop per capita (w/o Vietiane Cap)
# fitCpc_stunt05L<-lm(Stunt05~LCroppop05,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
# fitCpc_stunt15L<-lm(Stunt15~LCroppop15,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
# fitCpc_pov05L<-lm(Pov05~LCroppop05,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
# fitCpc_pov15L<-lm(Pov15~LCroppop15,LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',])
# 
# p1<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LCroppop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Cropland p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitCpc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitCpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p2<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LCroppop15, y=Stunt15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Cropland p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
#   labs(title = paste(" Slope =",signif(fitCpc_stunt15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitCpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p3<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LCroppop05, y=Pov05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Cropland p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
#   labs(title = paste(" Slope =",signif(fitCpc_pov05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitCpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p4<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LCroppop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitCpc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitCpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)
# 
# #For legend
# ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',],aes(x=LCroppop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitCpc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitCpc_pov15L)$coef[2,4], 2)),color='Province')
#
# #Graphs with Ln Crop per capita (with low agro and low increase pop)
# fitCpc_stunt05L<-lm(Stunt05~LCroppop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitCpc_stunt15L<-lm(Stunt15~LCroppop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitCpc_pov05L<-lm(Pov05~LCroppop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitCpc_pov15L<-lm(Pov15~LCroppop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# 
# p1<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LCroppop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Cropland p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitCpc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitCpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p2<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LCroppop15, y=Stunt15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Cropland p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
#   labs(title = paste(" Slope =",signif(fitCpc_stunt15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitCpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p3<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LCroppop05, y=Pov05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Cropland p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
#   labs(title = paste(" Slope =",signif(fitCpc_pov05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitCpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p4<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LCroppop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitCpc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitCpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)

### Appendix E.2

#Graphs with Ln Crop per capita (800x500)

# fitCpc_stunt05L<-lm(Stunt05~LCroppop05,LaoSDG_ALL_LC)
# fitCpc_stunt15L<-lm(Stunt15~LCroppop15,LaoSDG_ALL_LC)
# fitCpc_pov05L<-lm(Pov05~LCroppop05,LaoSDG_ALL_LC)
# fitCpc_pov15L<-lm(Pov15~LCroppop15,LaoSDG_ALL_LC)

Corr_crop<-select(LaoSDG_ALL_LC,Stunt05,Stunt15,Pov05,Pov15,LCroppop05,LCroppop15) 
Corr_Sc05<-rcorr(as.matrix(Corr_crop[,c(1,5)]))
Corr_Sc15<-rcorr(as.matrix(Corr_crop[,c(2,6)]))
Corr_Pc05<-rcorr(as.matrix(Corr_crop[,c(3,5)]))
Corr_Pc15<-rcorr(as.matrix(Corr_crop[,c(4,6)]))


p1<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(`Province`,`Position`)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste("A. Corr =",signif(Corr_Sc05$r[1,2], 2),
                     " P value =",signif(Corr_Sc05$P[1,2], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(`Province`,`Position`)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste("B. Corr =",signif(Corr_Sc15$r[1,2], 2),
                     " P value =",signif(Corr_Sc15$P[1,2], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(`Province`,`Position`)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste("C. Corr =",signif(Corr_Pc05$r[1,2], 2),
                     " P value =",signif(Corr_Pc05$P[1,2], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC,aes(x=LCroppop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(`Province`,`Position`)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste("D. Corr =",signif(Corr_Pc15$r[1,2], 2),
                     " P value =",signif(Corr_Pc15$P[1,2], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#For side legend (ordered provinces from north to south)
ggplot(LaoSDG_ALL_LC,aes(x=LCroppop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(`Province`,`Position`)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Cropland p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(color='Province')

rm(list = ls(pattern = "^fit"))
rm(list = ls(pattern = "^Corr"))
rm(list = ls(pattern = "^p"))


##Test for orchard
# #Graphs with Ln Orch per capita 
# fitOpc_stunt05L<-lm(Stunt05~LOrchpop05,LaoSDG_ALL_LC)
# fitOpc_stunt15L<-lm(Stunt15~LOrchpop15,LaoSDG_ALL_LC)
# fitOpc_pov05L<-lm(Pov05~LOrchpop05,LaoSDG_ALL_LC)
# fitOpc_pov15L<-lm(Pov15~LOrchpop15,LaoSDG_ALL_LC)
# 
# p1<-ggplot(LaoSDG_ALL_LC,aes(x=LOrchpop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Orchards p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitOpc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitOpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p2<-ggplot(LaoSDG_ALL_LC,aes(x=LOrchpop15, y=Stunt15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Orchards p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
#   labs(title = paste(" Slope =",signif(fitOpc_stunt15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitOpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p3<-ggplot(LaoSDG_ALL_LC,aes(x=LOrchpop05, y=Pov05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Orchards p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
#   labs(title = paste(" Slope =",signif(fitOpc_pov05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitOpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p4<-ggplot(LaoSDG_ALL_LC,aes(x=LOrchpop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Orchards p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitOpc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitOpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)
#
# #Graphs with Ln Orch per capita (with low agro and low increase pop)
# fitOpc_stunt05L<-lm(Stunt05~LOrchpop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitOpc_stunt15L<-lm(Stunt15~LOrchpop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitOpc_pov05L<-lm(Pov05~LOrchpop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitOpc_pov15L<-lm(Pov15~LOrchpop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# 
# p1<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LOrchpop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Orchard/Plantations p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitOpc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitOpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p2<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LOrchpop15, y=Stunt15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Orchard/Plantations  p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
#   labs(title = paste(" Slope =",signif(fitOpc_stunt15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitOpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p3<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LOrchpop05, y=Pov05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Orchard/Plantations  p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
#   labs(title = paste(" Slope =",signif(fitOpc_pov05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitOpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p4<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LOrchpop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Orchard/Plantations  p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitOpc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitOpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)

### Appendix E.3

# Graphs with Ln Orch per capita (800x500)
# Outliers: with high -5

sample<-LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),]
eliminated<-LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05< -5 | LaoSDG_ALL_LC$LOrchpop15< -5),]

# fitOpc_stunt05L<-lm(Stunt05~LOrchpop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),])
# fitOpc_stunt15L<-lm(Stunt15~LOrchpop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),])
# fitOpc_pov05L<-lm(Pov05~LOrchpop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),])
# fitOpc_pov15L<-lm(Pov15~LOrchpop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),])

Corr_crop<-select(LaoSDG_ALL_LC,Stunt05,Stunt15,Pov05,Pov15,LOrchpop05,LOrchpop15) 
Corr_So05<-rcorr(as.matrix(Corr_crop[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),c(1,5)]))
Corr_So15<-rcorr(as.matrix(Corr_crop[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),c(2,6)]))
Corr_Po05<-rcorr(as.matrix(Corr_crop[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),c(3,5)]))
Corr_Po15<-rcorr(as.matrix(Corr_crop[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),c(4,6)]))

p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),],aes(x=LOrchpop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste("A. Corr =",signif(Corr_So05$r[1,2], 2),
                     " P value =",signif(Corr_So05$P[1,2], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),],aes(x=LOrchpop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste("B. Corr =",signif(Corr_So15$r[1,2], 2),
                     " P value =",signif(Corr_So15$P[1,2], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),],aes(x=LOrchpop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste("C. Corr =",signif(Corr_Po05$r[1,2], 2),
                     " P value =",signif(Corr_Po05$P[1,2], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),],aes(x=LOrchpop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste("D. Corr =",signif(Corr_Po15$r[1,2], 2),
                     " P value =",signif(Corr_Po15$P[1,2], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#For side legend (ordered provinces from north to south)
ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$LOrchpop05>-5 & LaoSDG_ALL_LC$LOrchpop15>-5),],aes(x=LOrchpop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Orchards p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(color='Province')

rm(sample, eliminated)
rm(list = ls(pattern = "^fit"))
rm(list = ls(pattern = "^Corr"))
rm(list = ls(pattern = "^p"))

## Appendix E. 4 (WHERE I STOPPED)

#Graphs with Ln Grass per capita 

Corr_grass<-select(LaoSDG_ALL_LC,Stunt05,Stunt15,Pov05,Pov15,LGrasspop05,LGrasspop15) 
Corr_Sg05<-rcorr(as.matrix(Corr_grass[,c(1,5)]))
Corr_Sg15<-rcorr(as.matrix(Corr_grass[,c(2,6)]))
Corr_Pg05<-rcorr(as.matrix(Corr_grass[,c(3,5)]))
Corr_Pg15<-rcorr(as.matrix(Corr_grass[,c(4,6)]))

# fitGpc_stunt05L<-lm(Stunt05~LGrasspop05,LaoSDG_ALL_LC)
# fitGpc_stunt15L<-lm(Stunt15~LGrasspop15,LaoSDG_ALL_LC)
# fitGpc_pov05L<-lm(Pov05~LGrasspop05,LaoSDG_ALL_LC)
# fitGpc_pov15L<-lm(Pov15~LGrasspop15,LaoSDG_ALL_LC)

p1<-ggplot(LaoSDG_ALL_LC,aes(x=LGrasspop05, y=Stunt05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
  labs(title = paste("A. Corr =",signif(Corr_Sg05$r[1,2], 2),
                     " P value =",signif(Corr_Sg05$P[1,2], 2)))+theme(legend.position = "none")

p2<-ggplot(LaoSDG_ALL_LC,aes(x=LGrasspop15, y=Stunt15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
  labs(title = paste("B. Corr =",signif(Corr_Sg15$r[1,2], 2),
                     " P value =",signif(Corr_Sg15$P[1,2], 2)))+theme(legend.position = "none")

p3<-ggplot(LaoSDG_ALL_LC,aes(x=LGrasspop05, y=Pov05))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
  labs(title = paste("C. Corr =",signif(Corr_Pg05$r[1,2], 2),
                     " P value =",signif(Corr_Pg05$P[1,2], 2)))+theme(legend.position = "none")

p4<-ggplot(LaoSDG_ALL_LC,aes(x=LGrasspop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(title = paste("D. Corr =",signif(Corr_Pg15$r[1,2], 2),
                     " P value =",signif(Corr_Pg15$P[1,2], 2)))+theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,nrow=2)

# For the side legend 
ggplot(LaoSDG_ALL_LC,aes(x=LGrasspop15, y=Pov15))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
  geom_smooth(method='lm', formula= y~x, se=TRUE)+
  xlab("Grasslands p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
  labs(color='Province')

rm(list = ls(pattern = "^fit"))
rm(list = ls(pattern = "^Corr"))
rm(list = ls(pattern = "^p"))


#Tests for Grassland

# #Graphs with Ln Grass per capita (No zeros) 
# fitGpc_stunt05L<-lm(Stunt05~LGrasspop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),])
# fitGpc_stunt15L<-lm(Stunt15~LGrasspop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),])
# fitGpc_pov05L<-lm(Pov05~LGrasspop05,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),])
# fitGpc_pov15L<-lm(Pov15~LGrasspop15,LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),])
# 
# p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),],aes(x=LGrasspop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Grasslands p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitGpc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitGpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),],aes(x=LGrasspop15, y=Stunt15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Grasslands p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
#   labs(title = paste(" Slope =",signif(fitGpc_stunt15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitGpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),],aes(x=LGrasspop05, y=Pov05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Grasslands p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
#   labs(title = paste(" Slope =",signif(fitGpc_pov05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitGpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p4<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$Grasspop05!=0 & LaoSDG_ALL_LC$Grasspop15!=0),],aes(x=LGrasspop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Grasslands p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitGpc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitGpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)
# 
# #Graphs with Ln Grass per capita (with low agro and low increase pop)
# fitGpc_stunt05L<-lm(Stunt05~LGrasspop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitGpc_stunt15L<-lm(Stunt15~LGrasspop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitGpc_pov05L<-lm(Pov05~LGrasspop05,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitGpc_pov15L<-lm(Pov15~LGrasspop15,LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),])
# 
# p1<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LGrasspop05, y=Stunt05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Grasslands p.c. 2005 (Log)") + ylab("Stunting rate 2005")+
#   labs(title = paste(" Slope =",signif(fitGpc_stunt05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitGpc_stunt05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p2<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LGrasspop15, y=Stunt15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Grasslands p.c. 2015 (Log)") + ylab("Stunting rate 2015")+
#   labs(title = paste(" Slope =",signif(fitGpc_stunt15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitGpc_stunt15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p3<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LGrasspop05, y=Pov05))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Grasslands p.c. 2005 (Log)") + ylab("Poverty rate 2005")+
#   labs(title = paste(" Slope =",signif(fitGpc_pov05L$coef[[2]], 2),
#                      " P value =",signif(summary(fitGpc_pov05L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# p4<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>100 & LaoSDG_ALL_LC$tot_pop<15000),],aes(x=LGrasspop15, y=Pov15))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`Province`,levels = levels(reorder(LaoSDG_ALL_LC$Province,LaoSDG_ALL_LC$Position)))))+
#   geom_smooth(method='lm', formula= y~x, se=TRUE)+
#   xlab("Grasslands p.c. 2015 (Log)") + ylab("Poverty rate 2015")+
#   labs(title = paste(" Slope =",signif(fitGpc_pov15L$coef[[2]], 2),
#                      " P value =",signif(summary(fitGpc_pov15L)$coef[2,4], 2)))+theme(legend.position = "none")
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)

## Appendix E.5

# Change Agricultural or Cropland (800x500)

# fitP_Ap<-lm(LAgricPop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),])
# fitS_Ap<-lm(LAgricPop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1  ),])

Corr_ac<-select(LaoSDG_ALL_LC,StuntChange,PovChange,`StuntChangeSE 1T 10%`,`PovChange 1T 10%`,LAgricPop,LCroppop) 
Corr_PA<-rcorr(as.matrix(Corr_ac[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1),c(2,5)]))
Corr_SA<-rcorr(as.matrix(Corr_ac[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),c(1,5)]))
Corr_PC<-rcorr(as.matrix(Corr_ac[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1),c(2,6)]))
Corr_SC<-rcorr(as.matrix(Corr_ac[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),c(1,6)]))

p1<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=LAgricPop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1  ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("A. Corr =",signif(Corr_PA$r[1,2], 4),
                     " P value =",signif(Corr_PA$P[1,2], 4)))

p2<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=LAgricPop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("B. Corr =",signif(Corr_SA$r[1,2], 4),
                     " P value =",signif(Corr_SA$P[1,2], 4)))


# fitP_Cp<-lm(Croppop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),])
# fitS_Cp<-lm(Croppop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 ),])

p3<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("C. Corr =",signif(Corr_PC$r[1,2], 4),
                     " P value =",signif(Corr_PC$P[1,2], 4)))

p4<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("D. Corr =",signif(Corr_SC$r[1,2], 4),
                     " P value =",signif(Corr_SC$P[1,2], 4)))

grid.arrange(p1,p2,p3,p4,nrow=2)

# For side legend

p3<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))

p4<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=LCroppop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))

grid.arrange(p3,p4,nrow=2)

rm(list = ls(pattern = "^fit"))
rm(list = ls(pattern = "^Corr"))
rm(list = ls(pattern = "^p"))

## Appendix E.6

# Change Orchard or Grassland (800x500)

Corr_og<-select(LaoSDG_ALL_LC,StuntChange,PovChange,`StuntChangeSE 1T 10%`,`PovChange 1T 10%`,LOrchpop,LGrasspop) 
Corr_PO<-rcorr(as.matrix(Corr_og[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1),c(2,5)]))
Corr_SO<-rcorr(as.matrix(Corr_og[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),c(1,5)]))
Corr_PG<-rcorr(as.matrix(Corr_og[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1),c(2,6)]))
Corr_SG<-rcorr(as.matrix(Corr_og[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),c(1,6)]))

p1<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=LOrchpop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1  ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Orchard Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("A. Corr =",signif(Corr_PO$r[1,2], 4),
                     " P value =",signif(Corr_PO$P[1,2], 4)))

p2<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=LOrchpop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Orchard Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("B. Corr =",signif(Corr_SO$r[1,2], 4),
                     " P value =",signif(Corr_SO$P[1,2], 4)))

p3<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=LGrasspop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Grassland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("C. Corr =",signif(Corr_PG$r[1,2], 4),
                     " P value =",signif(Corr_PG$P[1,2], 4)))

p4<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=LGrasspop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Grassland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "none")+
  labs(title = paste("D. Corr =",signif(Corr_SG$r[1,2], 4),
                     " P value =",signif(Corr_SG$P[1,2], 4)))

grid.arrange(p1,p2,p3,p4,nrow=2)

# For side legend

p3<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=LGrasspop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),],method='lm', formula= y~x, se=TRUE)+
  xlab("Poverty Change (p.p.)") + ylab("Grassland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))

p4<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=LGrasspop))+ 
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
  geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
  xlab("Stunting Change (p.p.)") + ylab("Grassland Change p.c. (log)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))

grid.arrange(p3,p4,nrow=2)

rm(list = ls(pattern = "^fit"))
rm(list = ls(pattern = "^Corr"))
rm(list = ls(pattern = "^p"))


## Test for change agricultural land covers
# #Change Agricultural (800x500)
# 
# sampleChange<-LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & abs(LaoSDG_ALL_LC$LCroppop)<0.5 ),]
# fitP_Ap<-lm(AgricPop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & abs(LaoSDG_ALL_LC$LCroppop)<0.5),])
# fitS_Ap<-lm(AgricPop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),])
# 
# p1<-ggplot(LaoSDG_ALL_LC[abs(LaoSDG_ALL_LC$LCroppop)<0.5,], aes(x=PovChange, y=AgricPop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitP_Ap$coef[[2]], 4),
#                      " P value =",signif(summary(fitP_Ap)$coef[2,4], 4)))
# 
# p2<-ggplot(LaoSDG_ALL_LC[abs(LaoSDG_ALL_LC$LCroppop)<0.5,], aes(x=StuntChange, y=AgricPop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Stunting Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitS_Ap$coef[[2]], 4),
#                      " P value =",signif(summary(fitS_Ap)$coef[2,4], 4)))
# 
# #Change Cropland
# fitP_Cp<-lm(Croppop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),])
# fitS_Cp<-lm(Croppop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),])
# 
# p3<-ggplot(LaoSDG_ALL_LC[abs(LaoSDG_ALL_LC$LCroppop)<0.5,], aes(x=PovChange, y=Croppop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitP_Cp$coef[[2]], 4),
#                      " P value =",signif(summary(fitP_Cp)$coef[2,4], 4)))
# 
# p4<-ggplot(LaoSDG_ALL_LC[abs(LaoSDG_ALL_LC$LCroppop)<0.5,], aes(x=StuntChange, y=Croppop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[( LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1  & abs(LaoSDG_ALL_LC$LCroppop)<0.5),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitS_Cp$coef[[2]], 4),
#                      " P value =",signif(summary(fitS_Cp)$coef[2,4], 4)))
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)
# 
# 
# #Change Agricultural (800x500)
# fitP_Ap<-lm(LAgricPop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitS_Ap<-lm(LAgricPop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),])
# 
# p1<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),], aes(x=PovChange, y=LAgricPop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitP_Ap$coef[[2]], 4),
#                      " P value =",signif(summary(fitP_Ap)$coef[2,4], 4)))
# 
# p2<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),], aes(x=StuntChange, y=LAgricPop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Stunting Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitS_Ap$coef[[2]], 4),
#                      " P value =",signif(summary(fitS_Ap)$coef[2,4], 4)))
# 
# #Change Cropland
# fitP_Cp<-lm(LCroppop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),])
# fitS_Cp<-lm(LCroppop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),])
# 
# p3<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),], aes(x=PovChange, y=LCroppop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitP_Cp$coef[[2]], 4),
#                      " P value =",signif(summary(fitP_Cp)$coef[2,4], 4)))
# 
# p4<-ggplot(LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000),], aes(x=StuntChange, y=LCroppop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(abs(LaoSDG_ALL_LC$Agric)>129 & LaoSDG_ALL_LC$tot_pop<15000 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitS_Cp$coef[[2]], 4),
#                      " P value =",signif(summary(fitS_Cp)$coef[2,4], 4)))
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)
# 
# #Legends (800x500)
# 
# p1<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=PovChange, y=LCroppop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
#   labs(title = paste(" Slope =",signif(fitP_Lp$coef[[2]], 4),
#                      " P value =",signif(summary(fitP_Lp)$coef[2,4], 4)))
# 
# p2<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=StuntChange, y=LCroppop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
#   labs(title = paste(" Slope =",signif(fitS_Lp$coef[[2]], 4),
#                      " P value =",signif(summary(fitS_Lp)$coef[2,4], 4)))
# 
# grid.arrange(p1,p2,nrow=2)
# 
# #Change Agricultural (800x500)
# fitP_Ap<-lm(LAgricPop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),])
# fitS_Ap<-lm(LAgricPop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),])
# 
# p1<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=PovChange, y=LAgricPop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitP_Ap$coef[[2]], 4),
#                      " P value =",signif(summary(fitP_Ap)$coef[2,4], 4)))
# 
# p2<-ggplot(LaoSDG_ALL_LC[ LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=StuntChange, y=LAgricPop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Stunting Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitS_Ap$coef[[2]], 4),
#                      " P value =",signif(summary(fitS_Ap)$coef[2,4], 4)))
# 
# #Change Cropland
# fitP_Cp<-lm(LCroppop~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),])
# fitS_Cp<-lm(LCroppop~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),])
# 
# p3<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=PovChange, y=LCroppop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitP_Cp$coef[[2]], 4),
#                      " P value =",signif(summary(fitP_Cp)$coef[2,4], 4)))
# 
# p4<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=StuntChange, y=LCroppop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitS_Cp$coef[[2]], 4),
#                      " P value =",signif(summary(fitS_Cp)$coef[2,4], 4)))
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)
# 
# #Log Change Agriculture (all)
# fitP_Ap<-lm(LAgricPop2~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),])
# fitS_Ap<-lm(LAgricPop2~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),])
# 
# p1<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=LAgricPop2))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitP_Ap$coef[[2]], 4),
#                      " P value =",signif(summary(fitP_Ap)$coef[2,4], 4)))
# 
# p2<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=LAgricPop2))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 ),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Stunting Change (p.p.)") + ylab("Agricultural Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitS_Ap$coef[[2]], 4),
#                      " P value =",signif(summary(fitS_Ap)$coef[2,4], 4)))
# 
# #Change Cropland
# fitP_Cp<-lm(LCroppop2~PovChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 ),])
# fitS_Cp<-lm(LCroppop2~StuntChange, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 ),])
# 
# p3<-ggplot(LaoSDG_ALL_LC, aes(x=PovChange, y=LCroppop2))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 ),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitP_Cp$coef[[2]], 4),
#                      " P value =",signif(summary(fitP_Cp)$coef[2,4], 4)))
# 
# p4<-ggplot(LaoSDG_ALL_LC, aes(x=StuntChange, y=LCroppop2))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 ),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
#   theme(legend.position = "none")+
#   labs(title = paste(" Slope =",signif(fitS_Cp$coef[[2]], 4),
#                      " P value =",signif(summary(fitS_Cp)$coef[2,4], 4)))
# 
# grid.arrange(p1,p2,p3,p4,nrow=2)
# 
# #Legends (800x500)
# 
# p1<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=PovChange, y=LCroppop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`PovChange 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Poverty Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
#   labs(title = paste(" Slope =",signif(fitP_Lp$coef[[2]], 4),
#                      " P value =",signif(summary(fitP_Lp)$coef[2,4], 4)))
# 
# p2<-ggplot(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Province!='Vientiane Capital',], aes(x=StuntChange, y=LCroppop))+ 
#   geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
#   geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)))+
#   geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$Province!='Vientiane Capital'),],method='lm', formula= y~x, se=TRUE)+
#   xlab("Stunting Change (p.p.)") + ylab("Cropland Change p.c. (log)")+
#   scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
#   labs(title = paste(" Slope =",signif(fitS_Lp$coef[[2]], 4),
#                      " P value =",signif(summary(fitS_Lp)$coef[2,4], 4)))
# 
# grid.arrange(p1,p2,nrow=2)


## Map Figure 3
#Last Map Graphs (500x400)

Xaysetha <- data.frame(
  name=c("Poverty","Stunting","Natural"),
  value=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1701,c("PovChange","StuntChange","NatChange")])*100,2)),
  perc=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1701,c("PovChange","StuntChange","NatChange")])*100,2))
)

Xaysetha[1,3]<-Xaysetha[1,3]/4.5
Xaysetha[2,3]<-Xaysetha[2,3]/4.5
# Xaysetha[3,3]<--Xaysetha[3,3]
# Xaysetha[3,2]<--Xaysetha[3,2]


ggplot(Xaysetha, aes(x=name,y=perc, fill=name))+
  geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=c("#73ae80", "#9972af","#c8b35a"))+
  geom_text(aes(label=value), nudge_y=c(8,8,5.2), color="black", size=4.5)+
  xlab("") + theme_minimal(base_size = 14)+
  labs(title="Xaysetha District")+
  theme(legend.position='none',axis.text.x = element_text(face="bold", size=18),plot.title = element_text(face="bold",size=25))+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 1.5)+
  scale_y_continuous(name="Natural Area Change (p.p.)",limits = c(-10, 10),
                     sec.axis = sec_axis(~.*4.5, name=paste("Poverty and Stunting Change (p.p.)")))


Xaybuly <- data.frame(
  name=c("Poverty","Stunting","Natural"),
  value=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1311,c("PovChange","StuntChange","NatChange")])*100,2)),
  perc=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1311,c("PovChange","StuntChange","NatChange")])*100,2))
)

Xaybuly[1,3]<-Xaybuly[1,3]/4.5
Xaybuly[2,3]<-Xaybuly[2,3]/4.5
# Xaybuly[3,3]<--Xaybuly[3,3]
# Xaybuly[3,2]<--Xaybuly[3,2]


ggplot(Xaybuly, aes(x=name,y=perc, fill=name))+
  geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=c("#73ae80", "#9972af","#c8b35a"))+
  geom_text(aes(label=value), nudge_y=c(4.7,4.2,7.5), color="black", size=4.5)+
  xlab("") + theme_minimal(base_size = 14)+
  labs(title="Xaybuly District")+
  theme(legend.position='none',axis.text.x = element_text(face="bold", size=18),plot.title = element_text(face="bold",size=25))+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 1.5)+
  scale_y_continuous(name="Natural Area Change (p.p.)",limits = c(-10, 10),
                     sec.axis = sec_axis(~.*4.5, name=paste("Poverty and Stunting Change (p.p.)")))


Xaysathan <- data.frame(
  name=c("Poverty","Stunting","Natural"),
  value=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==811,c(9,15,19)])*100,2)),
  perc=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==811,c(9,15,19)])*100,2))
)

Xaysathan[1,3]<-Xaysathan[1,3]/4.5
Xaysathan[2,3]<-Xaysathan[2,3]/4.5
# Xaysathan[3,3]<--Xaysathan[3,3]
# Xaysathan[3,2]<--Xaysathan[3,2]


ggplot(Xaysathan, aes(x=name,y=perc, fill=name))+
  geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=c("#73ae80", "#9972af","#c8b35a"))+
  geom_text(aes(label=value), nudge_y=c(7.3,6,2.5), color="black", size=4.5)+
  xlab("") + theme_minimal(base_size = 14)+
  labs(title="Xaysathan District")+
  theme(legend.position='none',axis.text.x = element_text(face="bold", size=18),plot.title = element_text(face="bold",size=25))+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 1.5)+
  scale_y_continuous(name="Natural Area Change (p.p.)",limits = c(-10, 10),
                     sec.axis = sec_axis(~.*4.5, name=paste("Poverty and Stunting Change (p.p.)")))

Sanxay <- data.frame(
  name=c("Poverty","Stunting","Natural"),
  value=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1704,c(9,15,19)])*100,2)),
  perc=as.matrix(round(t(LaoSDG_ALL_LC[LaoSDG_ALL_LC$Dcode==1704,c(9,15,19)])*100,2))
)

Sanxay[1,3]<-Sanxay[1,3]/4.5
Sanxay[2,3]<-Sanxay[2,3]/4.5
# Sanxay[3,3]<--Sanxay[3,3]
# Sanxay[3,2]<--Sanxay[3,2]


ggplot(Sanxay, aes(x=name,y=perc, fill=name))+
  geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=c("#73ae80", "#9972af","#c8b35a"))+
  geom_text(aes(label=value), nudge_y=c(10.5,6,2), color="black", size=4.5)+
  xlab("") + theme_minimal(base_size = 14)+
  labs(title="Sanxay District")+
  theme(legend.position='none',axis.text.x = element_text(face="bold", size=18),plot.title = element_text(face="bold",size=25))+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 1.5)+
  scale_y_continuous(name="Natural Area Change (p.p.)",limits = c(-10, 10),
                     sec.axis = sec_axis(~.*4.5, name=paste("Poverty and Stunting Change (p.p.)")))

#-Other relevant districts

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
  scale_y_continuous(name="Natural Area Change (p.p.)",limits = c(-7, 7),
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


### Country analysis

## Appendix G.1

# #Indicators from SDG Global Database
# 
# stunting <- read_excel("~/SDG paper/World_Country indicators/stunting_countries.xlsx", 
#                        sheet = "Goal2")
# 
# poverty <- read_excel("~/SDG paper/World_Country indicators/poverty_countries.xlsx", 
#                       sheet = "Goal1")
# 
# poverty2000 <- read_excel("~/SDG paper/World_Country indicators/poverty2.xlsx", 
#                           sheet = "Goal1")
# 
# forest <- read_excel("~/SDG paper/World_Country indicators/forest_countries.xlsx", 
#                      sheet = "Goal15")
# 
# stunting$SDG2<-as.numeric(stunting$Value)
# poverty$SDG1<-as.numeric(poverty$Value)
# poverty2000$SDG1<-as.numeric(poverty2000$Value)
# forest$SDG15<-as.numeric(forest$Value)
# 
# Source_Stunt<-"Source: Joint Child Malnutrition Estimates (2021 Edition), United Nations Children's Fund (UNICEF), World Health Organisation (WHO) and the World Bank Group."
# Source_Forest="FAO, Global Forest Resources Assessment and FAOSTAT"
# 
# ## Bar graphs-Years
# 
# #Stunting Rates
# ggplot(stunting[(stunting$SeriesCode=="SH_STA_STNT"),],aes(x=TimePeriod))+
#   geom_bar()+ggtitle("Countries with Stunting Rates")+xlab("Year")
# 
# #Stunting numbers
# ggplot(stunting[(stunting$SeriesCode=="SH_STA_STNTN" & stunting$Source==Source_Stunt),],aes(x=TimePeriod))+
#   geom_bar()+ggtitle("Countries with Stunted Number (thousands)")+xlab("Year")
# 
# #Poverty
# ggplot(poverty[poverty$Location=="ALLAREA",],aes(x=TimePeriod))+
#   geom_bar()+ggtitle("Countries with Poverty Headcount")+xlab("Year")
# 
# # ggplot(poverty2000[poverty2000$SeriesDescription=="Proportion of population below international poverty line (%)",],aes(x=TimePeriod))+
# #   geom_bar()+ggtitle("Countries with Poverty Headcount")+xlab("Year")
# 
# #Forest
# ggplot(forest[(forest$Source==Source_Forest & forest$SeriesCode=="AG_LND_FRST"),],aes(x=TimePeriod))+
#   geom_bar()+ggtitle("Countries with Forest Area")+xlab("Year")
# 
# # Distributions for 2000
# 
# poverty2000$GeoAreaCode<-as.numeric(poverty2000$GeoAreaCode)
# 
# PovStunt_2000<-merge(stunting[(stunting$SeriesCode=="SH_STA_STNT" & stunting$TimePeriod==2000),c(6,7,8,15,23)], 
#                      poverty2000[poverty2000$SeriesDescription=="Proportion of population below international poverty line (%)",c(6,7,8,15,25)], 
#                      by="GeoAreaCode")
# 
# PovStunt_2000<-PovStunt_2000 %>%
#   rename("Country SDG2"=GeoAreaName.x,
#          "Country SDG1"=GeoAreaName.y,
#          "Year SDG2"=TimePeriod.x ,
#          "Year SDG1"=TimePeriod.y)
# 
# All_2000<-merge(PovStunt_2000,forest[(forest$TimePeriod==2000 & forest$Source==Source_Forest & forest$SeriesCode=="AG_LND_FRST"),c(6,7,8,21)], by="GeoAreaCode")
# 
# 
# Comparison00<-select(All_2000,SDG1,SDG2,SDG15)
# 
# ggpairs(Comparison00,lower=list(continuous=wrap("smooth_loess",size=0.1)),
#         title = "Correlation SDGs 2000", 
#         columnLabels = c("Poverty Headcount", "Prevalence of Stunting)", "Forest Area"))
# 
# rm(stunting,poverty,poverty2000, forest, All_2000, Comparison00, PovStunt_2000, Source_Forest, Source_Stunt)

### Indicators from Our World Data (For all Appendix G)

stunting_OWD<- read_excel("~/SDG paper/World_Country indicators/OurWorldData/stunting2.xlsx")

#poverty_OWD<-read_excel("~/SDG paper/World_Country indicators/OurWorldData/poverty.xlsx") #For extreme poverty

poverty320_OWD<-read_excel("~/SDG paper/World_Country indicators/OurWorldData/poverty320.xlsx") #For living less than 3.20

forest_OWD<- read_excel("~/SDG paper/World_Country indicators/OurWorldData/forest.xlsx")

# Appendix G.1: Number of countries

ggplot(stunting_OWD,aes(x=Year))+
  geom_bar()+ggtitle("Countries with Stunting Rates")+xlab("Year")

ggplot(poverty320_OWD,aes(x=Year))+
  geom_bar()+ggtitle("Countries with Poverty Rates")+xlab("Year")

ggplot(forest_OWD[forest_OWD$Year>2000,],aes(x=Year))+
  geom_bar()+ggtitle("Countries with Forest Rates")+xlab("Year")


# Appendix G.2: Comparison for 2000

PovStunt_2000_OWD<-merge(stunting_OWD[stunting_OWD$Year==2000,c(2,4)], 
                         poverty320_OWD[(poverty320_OWD$Year==2000),c(2,4)], 
                         by="Code")

All_2000_OWD<-merge(PovStunt_2000_OWD,forest_OWD[(forest_OWD$Year==2000),c(2,4)], by="Code")
All_2000_OWD$LogForest<-log(All_2000_OWD$`Forest cover`)


ggpairs(All_2000_OWD[,c(2,3,5)],lower=list(continuous=wrap("smooth_loess",size=0.1)),
        title = "Cross-country Correlation SDGs 2000", 
        columnLabels = c("Prevalence Stunting","Poverty Headcount", "Forest Area (Log)"))


# #Comparison for 2016
# 
# PovStunt_2016_OWD<-merge(stunting_OWD[stunting_OWD$Year==2016,c(2,4)], 
#                          poverty320_OWD[(poverty320_OWD$Year==2016),c(2,4)], 
#                          by="Code")
# 
# All_2016_OWD<-merge(PovStunt_2016_OWD,forest_OWD[(forest_OWD$Year==2016),c(2,4)], by="Code")
# All_2016_OWD$LogForest<-log(All_2016_OWD$`Forest cover`)
# 
# 
# ggpairs(All_2016_OWD[,c(2,3,5)],lower=list(continuous=wrap("smooth_loess",size=0.1)),
#         title = "Cross-country Correlation SDGs 2016", 
#         columnLabels = c("Prevalence Stunting","Poverty Headcount", "Forest Area (Log)"))
# 

#### Appendix G.3: Box plots

###For each SDG

#District data

PercForest15 <- read_excel("~/SDG paper/Indicators/PercForest2015Dis.xlsx")
PercForest15 <- rename(PercForest15,Dcode="DCode")

DistrictData<-select(LaoSDG_ALL_LC,Dcode,Pov15,Stunt15,PerNatural15)
DistrictData<-DistrictData %>%
  inner_join(PercForest15,by="Dcode") %>%
  rename("Location"=Dcode,
         "SDG2"=Stunt15,
         "SDG1"=Pov15,
         "SDG15"=PercForest)

DistrictData$SDG15<-DistrictData$SDG15*100
DistrictData$Code<-"Subnational Laos"

# Stunting

Stunting16<-stunting_OWD[stunting_OWD$Year==2016,c(2,4)]
Stunting16$`Prevalence of stunting, height for age (% of children under 5)` <- Stunting16$`Prevalence of stunting, height for age (% of children under 5)` /100
Stunting16<- Stunting16 %>%
  rename("Location"=Code,
         "SDG2"="Prevalence of stunting, height for age (% of children under 5)")
Stunting16$Code<-"Cross Country"

AllStunt16<-rbind(Stunting16,DistrictData[,c(1,3,6)])

IQR_S16_C<-IQR(as.matrix(AllStunt16[AllStunt16$Code=="Cross Country", c("SDG2") ]))
IQR_S16_D<-IQR(as.matrix(AllStunt16[AllStunt16$Code=="Subnational Laos", c("SDG2") ]))
#n_obs<-data.frame(Countries=as.numeric(count(AllStunt16[AllStunt16$Code=="Cross Country", c("SDG2") ])),
           #       Districts=as.numeric(count(AllStunt16[AllStunt16$Code=="Subnational Laos", c("SDG2") ])))

p1<-ggplot(AllStunt16, aes(x=Code, y=SDG2, fill=Code)) + 
  geom_boxplot()+ 
  #geom_jitter(color="black", size=0.2, alpha=10) +
  labs(x="Geographical Level", y="Stunting Rates", title = "Interquartile Ranges:", 
       subtitle= paste("A. Cross Country=",signif(IQR_S16_C, 2), 
                       " Subnational Laos =",signif(IQR_S16_D, 2)))+
  theme(legend.position="none")+ 
  geom_text(aes(label=paste("n=",..count..)), y=0.58, stat='count', size=4)

# ggplot(AllStunt16, aes(x=Code, y=SDG2, fill=Code)) + 
#   geom_violin()

## Poverty

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

AllPov16<-rbind(Poverty16,DistrictData[,c(1,2,6)])

IQR_P16_C<-IQR(as.matrix(AllPov16[AllPov16$Code=="Cross Country", c("SDG1") ]))
IQR_P16_D<-IQR(as.matrix(AllPov16[AllPov16$Code=="Subnational Laos", c("SDG1") ]))
#n_obs<-data.frame(Countries=as.numeric(count(AllPov16[AllPov16$Code=="Cross Country", c("SDG1") ])),
           #       Districts=as.numeric(count(AllPov16[AllPov16$Code=="Subnational Laos", c("SDG1") ])))

p2<-ggplot(AllPov16, aes(x=Code, y=SDG1, fill=Code)) + 
  geom_boxplot()+ 
  #geom_jitter(color="black", size=0.2, alpha=0.5)+
  labs(x="Geographical Level", y="Poverty Rates", title = " ", 
       subtitle= paste("B. Cross Country=",signif(IQR_P16_C, 2), 
                       " Subnational Laos =",signif(IQR_P16_D, 2)))+
  theme(legend.position="none")+ 
  geom_text(aes(label=paste("n=",..count..)), y= 0.95, stat='count', size=4)

# ggplot(AllPov16, aes(x=Code, y=SDG1, fill=Code)) + 
#   geom_violin()

## Forests

Forest15<-forest_OWD[(forest_OWD$Year==2015),c(2,4)]
Forest15<- Forest15 %>%
  rename("Location"=Code,
         "SDG15"="Forest cover")
Forest15$Code<-"Cross Country"

AllFor15<-rbind(Forest15,DistrictData[,c(1,5,6)])

IQR_F15_C<-IQR(as.matrix(AllFor15[AllFor15$Code=="Cross Country", c("SDG15") ]))
IQR_F15_D<-IQR(as.matrix(AllFor15[AllFor15$Code=="Subnational Laos", c("SDG15") ]))
#n_obs<-data.frame(Countries=as.numeric(count(AllFor15[AllFor15$Code=="Cross Country", c("SDG15") ])),
           #       Districts=as.numeric(count(AllFor15[AllFor15$Code=="Subnational Laos", c("SDG15") ])))


p3<-ggplot(AllFor15, aes(x=Code, y=SDG15, fill=Code)) + 
  geom_boxplot()+ 
  #geom_jitter(color="black", size=0.2, alpha=0.5)+
  labs(x="Geographical Level", y="Forest Area", title = " ", subtitle= paste("C. Cross Country=",signif(IQR_F15_C, 2),
                     " Subnational Laos =",signif(IQR_F15_D, 2)))+
  theme(legend.position="none")+ 
  geom_text(aes(label=paste("n=",..count..)), y=102, stat='count', size=4)

# ggplot(AllFor16, aes(x=Code, y=SDG15, fill=Code)) + 
#   geom_violin()

grid.arrange(p1,p2,p3,nrow=1)

rm(DistrictData,forest_OWD,Forest15,PercForest15,Poverty16,stunting_OWD,Stunting16,n_obs)
rm(list = ls(pattern = "^All"))
rm(list = ls(pattern = "^IQR"))
rm(list = ls(pattern = "^p"))

## Other analysis: 
#Stack databases 2000

# All_2000_OWD$poverty<- All_2000_OWD$poverty/100
# All_2000_OWD$`Prevalence of stunting, height for age (% of children under 5)` <- All_2000_OWD$`Prevalence of stunting, height for age (% of children under 5)` /100
# 
# CountryData00<-All_2000_OWD[,c(-5)]
# CountryData00<-CountryData00 %>%
#   rename("Location"=Code,
#          "SDG2"="Prevalence of stunting, height for age (% of children under 5)",
#          "SDG1"=poverty ,
#          "SDG15"="Forest cover")
# CountryData00$Code<-"Cross Country"
# 
# DistrictData<-select(LaoSDG_ALL_LC,Dcode,Pov15,Stunt15,PerNatural15)
# DistrictData<-DistrictData %>%
#   rename("Location"=Dcode,
#          "SDG2"=Stunt15,
#          "SDG1"=Pov15 ,
#          "SDG15"=PerNatural15)
# 
# DistrictData$SDG15<-DistrictData$SDG15*100
# DistrictData$Code<-"Subnational Laos"
# 
# AllData00<-rbind(CountryData00,DistrictData)
# 
# p1<-ggplot(AllData00, aes(x=Code, y=SDG1)) + 
#   geom_boxplot()
# 
# p2<-ggplot(AllData00, aes(x=Code, y=SDG2)) + 
#   geom_boxplot()
# 
# p3<-ggplot(AllData00, aes(x=Code, y=SDG15)) + 
#   geom_boxplot()
# 
# grid.arrange(p1,p2,p3,nrow=1)
# 
# #Stack databases 2016
# 
# All_2016_OWD$poverty<- All_2016_OWD$poverty/100
# All_2016_OWD$`Prevalence of stunting, height for age (% of children under 5)` <- All_2016_OWD$`Prevalence of stunting, height for age (% of children under 5)` /100
# 
# CountryData16<-All_2016_OWD[,c(-5)]
# CountryData16<-CountryData16 %>%
#   rename("Location"=Code,
#          "SDG2"="Prevalence of stunting, height for age (% of children under 5)",
#          "SDG1"=poverty ,
#          "SDG15"="Forest cover")
# CountryData16$Code<-"Cross Country"
# 
# AllData16<-rbind(CountryData16,DistrictData)
# 
# p1<-ggplot(AllData16, aes(x=Code, y=SDG1)) + 
#   geom_boxplot()
# 
# p2<-ggplot(AllData16, aes(x=Code, y=SDG2)) + 
#   geom_boxplot()
# 
# p3<-ggplot(AllData16, aes(x=Code, y=SDG15)) + 
#   geom_boxplot()
# 
# grid.arrange(p1,p2,p3,nrow=1)
# rm(All_2000_OWD,All_2016_OWD,AllData00,AllData16,CountryData00,CountryData16,DistrictData,p1,p2,p3,PovStunt_2000_OWD,PovStunt_2016_OWD)


### Appendix H.2 Population

#Change with population grow (1200x400) FINAL

# fitS_pop<-lm(StuntChange~tot_popgr, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),])
# fitP_pop<-lm(PovChange~tot_popgr, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),])
# fitN_pop<-lm(NatChange~tot_popgr, LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),])

Corr_pop<-select(LaoSDG_ALL_LC,PovChange,`PovChange 1T 10%`,StuntChange,`StuntChangeSE 1T 10%`,NatChange, tot_popgr) #Final one
Corr_Pp<-rcorr(as.matrix(Corr_pop[(Corr_pop$`PovChange 1T 10%`==1),c(1,6)] ))
Corr_Sp<-rcorr(as.matrix(Corr_pop[(Corr_pop$`StuntChangeSE 1T 10%`==1),c(3,6)] ))
Corr_Np<-rcorr(as.matrix(Corr_pop[,c(5,6)] ))

p1<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),], aes(x=tot_popgr, y=StuntChange))+ #tot_popgr vs Stunt
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`StuntChangeSE 1T 10%`)),size=2)+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`StuntChangeSE 1T 10%`==1 & LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),],method='lm', formula= y~x, se=TRUE)+
  xlab("Population Growth") + ylab("Stunting Change (p.p.)")+
  scale_colour_discrete(name="Significance Stunting",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("C. Corr =",signif(Corr_Sp$r[1,2], 2),
                     " P value =",signif(Corr_Sp$P[1,2], 2)))+ 
  theme(plot.title = element_text(size=12))

p2<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),], aes(x=tot_popgr, y=PovChange))+ #Pov vs Nat
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(`PovChange 1T 10%`)),size=2)+
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$`PovChange 1T 10%`==1 & LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),],method='lm', formula= y~x, se=TRUE)+
  xlab("Population Growth") + ylab("Poverty Change (p.p.)")+
  scale_colour_discrete(name="Significance Poverty",labels=c("No","Yes"))+
  theme(legend.position = "bottom")+
  labs(title = paste("C. Corr =",signif(Corr_Pp$r[1,2], 2),
                     " P value =",signif(Corr_Pp$P[1,2], 2)))+ 
  theme(plot.title = element_text(size=12))

p3<-ggplot(LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),], aes(x=tot_popgr, y=NatChange))+ #Stunt vs Nat
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  geom_point(aes(colour=factor(LandUseType)),size=2)+ #aes(colour=factor(LandUseType)) or colour="turquoise3"
  geom_smooth(data=LaoSDG_ALL_LC[(LaoSDG_ALL_LC$tot_popgr<0.5 & LaoSDG_ALL_LC$tot_popgr>-0.2),],method='lm', formula= y~x, se=TRUE)+
  xlab("Population Growth") + ylab("Natural Area Change (p.p.)")+
  scale_colour_manual(name="Increase in agri. land area",values=c("turquoise3","red", "blue"), labels=c("Normal","Low","High"))+ #name="Land Use Type",
  theme(legend.position = "bottom")+
  labs(title = paste("C. Corr =",signif(Corr_Np$r[1,2], 2),
                     " P value =",signif(Corr_Np$P[1,2], 2)))+ 
  theme(plot.title = element_text(size=12))

#When checking population effect use ,size=(tot_popgr-min(tot_popgr)) on aes()
grid.arrange(p3,p2,p1,nrow=1)

rm(list = ls(pattern = "^Corr"))
rm(list = ls(pattern = "^p"))
rm(LaoSDG_ALL_LC)

