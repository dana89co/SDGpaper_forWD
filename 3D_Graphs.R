#Title: 3D graphs for SDG Paper
#Author: Diana C. Garcia Rojas
#Packages 
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
library(htmlwidgets)
library(zip)
#Download Data
LaoSDG_ALL_LC <- read_excel("~/SDG paper/Graphs/IndicatorPSN_1221.xlsx")

LaoSDG_ALL_LC$Natpop<-LaoSDG_ALL_LC$Natpop15-LaoSDG_ALL_LC$Natpop05
LaoSDG_ALL_LC$LNatpop<-LaoSDG_ALL_LC$LNatpop15-LaoSDG_ALL_LC$LNatpop05

LaoSDG_ALL_LC$Croppop<-LaoSDG_ALL_LC$Croppop15-LaoSDG_ALL_LC$Croppop05
LaoSDG_ALL_LC$LCroppop<-LaoSDG_ALL_LC$LCroppop15-LaoSDG_ALL_LC$LCroppop05

LaoSDG_ALL_LC$Prov<-round(LaoSDG_ALL_LC$Dcode/100)
LaoSDG_ALL_LC$tot_pop<-LaoSDG_ALL_LC$tot_pop15-LaoSDG_ALL_LC$tot_pop05
LaoSDG_ALL_LC$tot_popgr<-(LaoSDG_ALL_LC$tot_pop15-LaoSDG_ALL_LC$tot_pop05)/LaoSDG_ALL_LC$tot_pop05
LaoSDG_ALL_LC$RelPopGr<-LaoSDG_ALL_LC$tot_popgr/.15
LaoSDG_ALL_LC$Agric05<-LaoSDG_ALL_LC$cropland05+LaoSDG_ALL_LC$orchard05 +LaoSDG_ALL_LC$grasslands05
LaoSDG_ALL_LC$Agric15<-LaoSDG_ALL_LC$cropland15+LaoSDG_ALL_LC$orchard15 +LaoSDG_ALL_LC$grasslands15
LaoSDG_ALL_LC$Agric<-LaoSDG_ALL_LC$Agric15-LaoSDG_ALL_LC$Agric05
LaoSDG_ALL_LC$AgricPop05<-LaoSDG_ALL_LC$Agric05/LaoSDG_ALL_LC$tot_pop05
LaoSDG_ALL_LC$AgricPop15<-LaoSDG_ALL_LC$Agric15/LaoSDG_ALL_LC$tot_pop15
LaoSDG_ALL_LC$AgricPop<-LaoSDG_ALL_LC$AgricPop15-LaoSDG_ALL_LC$AgricPop05

Status05_PSN<-plot_ly(LaoSDG_ALL_LC, x = ~Pov05, y = ~Stunt05, z = ~LNatpop05, type = "scatter3d", mode = "markers", color=~factor(Province), size= ~tot_pop05, marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 50))
Status05_PSN<-Status05_PSN  %>% layout(scene = list(xaxis = list(title = 'Poverty Rate'),
                                                    yaxis = list(title = 'Stunting Rate'),
                                                    zaxis = list(title = 'Natural Area p.c  (Log)')))

Status15_PSN<-plot_ly(LaoSDG_ALL_LC, x = ~Pov15, y = ~Stunt15, z = ~LNatpop15, type = "scatter3d", mode = "markers", color=~factor(Province), size= ~tot_pop15, marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 50))
Status15_PSN<-Status15_PSN  %>% layout(scene = list(xaxis = list(title = 'Poverty Rate'),
                                                    yaxis = list(title = 'Stunting Rate'),
                                                    zaxis = list(title = 'Natural Area p.c  (Log)')))

Change_PSN<-plot_ly(LaoSDG_ALL_LC, x = ~PovChange, y = ~StuntChange, z = ~(-NatChange), type = "scatter3d", mode = "markers", color=~factor(Province), 
                    size= ~tot_pop15, marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 50))
Change_PSN<-Change_PSN  %>% layout(scene = list(xaxis = list(title = 'Poverty Change'),
                                                yaxis = list(title = 'Stunting Change'),
                                                zaxis = list(title = 'Natural Degradation')))

Change_PSC<-plot_ly(LaoSDG_ALL_LC, x = ~PovChange, y = ~StuntChange, z = ~cropland, type = "scatter3d", mode = "markers", color=~factor(Province), 
                    size= ~tot_pop15, marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 50))
Change_PSC<-Change_PSC  %>% layout(scene = list(xaxis = list(title = 'Poverty Change'),
                                                yaxis = list(title = 'Stunting Change'),
                                                zaxis = list(title = 'Cropland Area Change')))

Change_PSN2<-plot_ly(LaoSDG_ALL_LC, x = ~PovChange, y = ~StuntChange, z = ~(-Natpop), type = "scatter3d", mode = "markers", color=~factor(Province), 
                     size= ~RelPopGr, marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 50))
Change_PSN2<-Change_PSN2  %>% layout(scene = list(xaxis = list(title = 'Poverty Change'),
                                                  yaxis = list(title = 'Stunting Change'),
                                                  zaxis = list(title = 'Natural Degradation p.c ')))

Change_PSC2<-plot_ly(LaoSDG_ALL_LC, x = ~PovChange, y = ~StuntChange, z = ~Croppop, type = "scatter3d", mode = "markers", color=~factor(Province), 
                     size= ~RelPopGr, marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 50))
Change_PSC2<-Change_PSC2  %>% layout(scene = list(xaxis = list(title = 'Poverty Change'),
                                                  yaxis = list(title = 'Stunting Change'),
                                                  zaxis = list(title = 'Cropland Area p.c Change')))

Change_PSA<-plot_ly(LaoSDG_ALL_LC, x = ~PovChange, y = ~StuntChange, z = ~Agric, color=~factor(Province),
                    size= ~RelPopGr, marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 50))
Change_PSA<-Change_PSA  %>% layout(scene = list(xaxis = list(title = 'Poverty Change'),
                                                yaxis = list(title = 'Stunting Change'),
                                                zaxis = list(title = 'Agricultural Area Change')))

Change_PSA2<-plot_ly(LaoSDG_ALL_LC, x = ~PovChange, y = ~StuntChange, z = ~AgricPop, color=~factor(Province),
                     size= ~RelPopGr, marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 50))
Change_PSA2<-Change_PSA2  %>% layout(scene = list(xaxis = list(title = 'Poverty Change'),
                                                  yaxis = list(title = 'Stunting Change'),
                                                  zaxis = list(title = 'Agricultural Area p.c. Change')))

#Save files:

saveWidget(Status05_PSN, "Status05_PSN.html", selfcontained = F, libdir = "lib05")
zip::zip("Status05_PSN.zip", c("Status05_PSN.html", "lib05"))
