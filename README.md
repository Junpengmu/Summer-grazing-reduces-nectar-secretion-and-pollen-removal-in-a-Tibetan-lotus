# Summer-grazing-reduces-nectar-secretion-and-pollen-removal-in-a-Tibetan-lotus
data and R code 
## Figure 1 box plot (traits of grazing)


##data
library(ggplot2)
library(reshape2)
mu<- read.csv("gf1.csv")
reshape2::melt(mu,id.vars="tr")->df1
write.csv(df1,"gf1-1.csv")  # f1-1.csv


##figure
mugg <- read.csv("gf1-1.csv")
##ggplot
library(ggplot2)
library(ggthemes)
library(multcompView)
library(dplyr)
library(agricolae)
library(ggpubr)
library(reshape2)
library(ggprism)

a<-ggplot(mugg,aes(tr,value,fill=tr))+
  geom_boxplot(size=0.6)+ 
  #geom_jitter(aes(group=as.factor(tr)),shape=21,color="green3", size=1.8, alpha=0.9,width = 0.2, height = 0.1)+
  labs(x=NULL,y=NULL)+ stat_summary(fun = "mean", geom = "point", shape =1,size = 2.0, color = "blue")+
  theme_classic()+
  ##stat_compare_means( aes(label = ..p.signif..), label.x = , label.y = , size=4, color="black")+##
  scale_fill_prism(palette = "floral")
##
fig<-a+facet_wrap(~variable,scales="free")+theme_test()+theme(axis.title.x =element_text(size=12),axis.text.x =element_text(size=12), axis.text.y =element_text(size=12),legend.text =element_text(size=12))+theme_bw() ###
fig
ggsave("fig.tiff",plot=fig1,width = 32, height = 21, units = "cm")
##加点


###Figure 2
##
library(ggplot2)
library(ggpmisc)
##linetype = tr
mu1 <- read.csv("gf2.csv")
##
sp1<-ggplot(mu1, aes(x=vrm, y=pr, color=pr)) + 
  geom_point()+
  facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(a)", x="vrm", y = "pr",size=4)+ 
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",
          after_stat(p.value.label), sep = "")),size=3,color="black")

a<-sp1+scale_color_gradientn(colours = rainbow(5))
a
##
sp2<-ggplot(mu1, aes(x=nvm, y=vrm, color=vrm)) + 
  geom_point()+
  facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(b)", x="nvm", y = "vrm",size=4)+ 
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",after_stat(p.value.label), sep = "")),size=3,color="black")

b<-sp2+scale_color_gradientn(colours = rainbow(5))
b

##
sp3<-ggplot(mu1, aes(x=nvf, y=vrf, color=vrf)) + 
  geom_point()+facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(c)", x="nvf", y = "vrf",size=4)+ 
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",after_stat(p.value.label), sep = "")),size=3,color="black")

c<-sp3+scale_color_gradientn(colours = rainbow(5))
c

##
sp4<-ggplot(mu1, aes(x=nsc, y=nvm, color=nvm)) + 
  geom_point()+
  facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(d)", x="nsc", y = "nvm",size=4)+ 
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",after_stat(p.value.label), sep = "")),size=3,color="black")

d<-sp4+scale_color_gradientn(colours = rainbow(5))
d

##
sp5<-ggplot(mu1, aes(x=nsc, y=nvf, color=nvf)) + 
  geom_point()+facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+ labs(title="(e)", x="nsc", y = "nvf",size=4)+
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",after_stat(p.value.label), sep = "")),size=3,color="black")

e<-sp5+scale_color_gradientn(colours = rainbow(5))
e

##
sp6<-ggplot(mu1, aes(x=agb, y=nsc, color=nsc)) + 
  geom_point()+facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(f)", x="agb", y = "nsc",size=4)+
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",after_stat(p.value.label), sep = "")),size=3,color="black")

f<-sp6+scale_color_gradientn(colours = rainbow(5))
f


library(cowplot)
cowplot::plot_grid(a,b,c,d,e,f,nrow=3)

##library(patchwork)
##(sp1+sp2+sp3)/(sp4+sp5+sp6)



###Figure 3

##
library(ggplot2)
library(ggpmisc)

##linetype = tr
mu1 <- read.csv("gf2.csv")
##
sp1<-ggplot(mu1, aes(x=vrf, y=ssr, color=ssr)) + 
  geom_point()+
  facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(a)", x="vrf", y = "ssr",size=4)+ 
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",
                                     after_stat(p.value.label), sep = "")),size=3,color="black")

a<-sp1+scale_color_gradientn(colours = rainbow(5))
a


##linetype = tr
mu1 <- read.csv("gf6.csv")
##
sp2<-ggplot(mu1, aes(x=vrf, y=ssr, color=ssr)) + 
  geom_point()+
  facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(b)", x="vrf", y = "ssr",size=4)+ 
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",after_stat(p.value.label), sep = "")),size=3,color="black")

b<-sp2+scale_color_gradientn(colours = rainbow(5))
b


library(patchwork)
a/b


###Figure 4
##
library(ggplot2)
library(reshape2)
mu<- read.csv("gf5a.csv")
reshape2::melt(mu,id.vars="sp")->df1
df1
library(plyr)
library(agricolae)
summbySppHab<- ddply(df1, c("sp","variable"), summarise, N    = length(variable),
                     mean = mean(value), min=min(value),max=max(value), var=var(value),
                     sd = sd(value), se = sd / sqrt(N))
summbySppHab

p0 <- ggplot(summbySppHab, aes(x=sp, y=mean, fill=variable),linetype = "10", size = 0.4) + 
  geom_bar(stat="identity", position=position_dodge(width = 0.9)) +labs(title="(a) Number of pollen depoistion",x="",y="")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2,color="dark gray",position=position_dodge(0.9))
p1<-p0 + scale_fill_brewer(palette="Greens",direction=-1) + theme_minimal()
a<-p1+coord_flip()
a

##
library(ggplot2)
library(reshape2)
mu<- read.csv("gf5b.csv")
##reshape2::melt(mu,id.vars="sp")->df1
#df1

##reshape2::melt(df1,id.vars=c("sp","variable"),perecnt=value/sum(vaule))->df2
#df2


p2<-ggplot(mu, aes(x=sp, y=percent , fill = variable), linetype = "10", size = 0.4)+
  geom_col(width = 0.8, position = "stack") +  ##position ="stack" 为数据
  #geom_col(width = 0.8, position = position_dodge(width = 0.8), color = "forestgreen") +
  #geom_text(aes(y = percent/2, label = scales::percent(percent)),position = position_dodge(width = 0.8)) +
  scale_y_continuous(labels = scales::percent)+
  #theme_classic() +
  theme_minimal()+
  scale_fill_brewer(palette = "Greens",direction=-1) +
  #scale_fill_manual(values=c("green pollen"="#32a676","red pollen"="#dd8a0b","unstained pollen"="darkkhaki"))+
  #theme(panel.grid.minor = element_line(),panel.grid.major = element_line())+
  labs(title="(b) Percentage of pollen depoistion",x="",y="")
#scale_y_continuous(expand=c(0,0),limits=c(0,1))+
#scale_fill_manual(values=c("red","green","blue"))+

b<-p2+ coord_flip()
b

library(patchwork)
a+b
a/b



###Figure 5

library(ggplot2)
library(reshape2)
mu<- read.csv("gf5.csv")
reshape2::melt(mu,id.vars="tr")->df1
write.csv(df1,"gf5-1.csv")  #另存为 f5-1.csv
library(ggthemes)
library(multcompView)
library(dplyr)
library(agricolae)
library(ggpubr)
library(ggprism)
mul<- read.csv("gf5-1.csv")

b<-ggplot(mul,aes(tr,value,fill=tr))+
  geom_boxplot(size=0.4)+ 
  #geom_jitter(aes(group=as.factor(tr)),shape=1,color="green3", size=1.8, alpha=0.9,width = 0.2, height = 0.1)+
  labs(x=NULL,y=NULL)+ stat_summary(fun = "mean", geom = "point", shape =1,size = 2.0, color = "blue")+
  theme_classic()+
  ##stat_compare_means( aes(label = ..p.signif..), label.x = , label.y = , size=4, color="black")+##多重比较
  #scale_fill_prism(palette = "floral")
  scale_fill_brewer(palette = "Greens",direction=-1)
##
fig3<-b+facet_wrap(~variable,scales="free")+theme_test()+theme(axis.title.x =element_text(size=12),axis.text.x =element_text(size=12), axis.text.y =element_text(size=12),legend.text =element_text(size=12))+theme_bw() ###全框坐标轴,分组作图
fig3
###ggsave("fig3.tiff",plot=fig1,width = 32, height = 21, units = "cm")


###Figure 6
##
library(ggplot2)
library(ggpmisc)
##linetype = tr
mu1 <- read.csv("gf6.csv")
#
sp1<-ggplot(mu1, aes(x=vrm, y=pr, color=pr)) + 
  geom_point()+
  facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(a)", x="vrm", y = "pr",size=4)+ 
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",
                                     after_stat(p.value.label), sep = "")),size=3,color="black")

a<-sp1+scale_color_gradientn(colours = rainbow(5))
a
##
sp2<-ggplot(mu1, aes(x=nvm, y=vrm, color=vrm)) + 
  geom_point()+
  facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(b)", x="nvm", y = "vrm",size=4)+ 
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",after_stat(p.value.label), sep = "")),size=3,color="black")

b<-sp2+scale_color_gradientn(colours = rainbow(5))
b

##
sp3<-ggplot(mu1, aes(x=nvf, y=vrf, color=vrf)) + 
  geom_point()+facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(c)", x="nvf", y = "vrf",size=4)+ 
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",after_stat(p.value.label), sep = "")),size=3,color="black")

c<-sp3+scale_color_gradientn(colours = rainbow(5))
c

##
sp4<-ggplot(mu1, aes(x=nsc, y=nvm, color=nvm)) + 
  geom_point()+
  facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(d)", x="nsc", y = "nvm",size=4)+ 
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",after_stat(p.value.label), sep = "")),size=3,color="black")

d<-sp4+scale_color_gradientn(colours = rainbow(5))
d

##
sp5<-ggplot(mu1, aes(x=nsc, y=nvf, color=nvf)) + 
  geom_point()+facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+ labs(title="(e)", x="nsc", y = "nvf",size=4)+
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",after_stat(p.value.label), sep = "")),size=3,color="black")

e<-sp5+scale_color_gradientn(colours = rainbow(5))
e

##
sp6<-ggplot(mu1, aes(x=agb, y=nsc, color=nsc)) + 
  geom_point()+facet_wrap(~tr, scales = "free") +
  geom_smooth(method=lm,level=0.95)+labs(title="(f)", x="agb", y = "nsc",size=4)+
  stat_correlation(aes(label = paste(after_stat(rr.label), "*\", \"*",after_stat(p.value.label), sep = "")),size=3,color="black")

f<-sp6+scale_color_gradientn(colours = rainbow(5))
f



library(cowplot)
cowplot::plot_grid(a,b,c,d,e,f,nrow=3)





###Figure S3  (Ranked Cross-Correlations of summer and winter grazing)

library(lares)
windowsFonts(RMN=windowsFont("Times New Roman"))###  

## Fig 2a  (correlation of flower traits for sgp)
nf2a<-read.csv("fs2a.csv")##data
#corr_cross(nf2b, type = 2, contains="bio",top = NA)##点状图Cross-Correlation max values per category
a<-corr_cross(nf2a, rm.na = TRUE, max_pvalue = 0.05, top = 30)+ theme(axis.text = element_text(family="RMN",colour="black",size = 14))  ##条状图Show only most relevant results filtered by pvalue
#corr_cross(nf2b, plot = FALSE, top = 30)##相关结果（数据）Only data with no plot
#corr_cross(nf2a, contains = c("bio", "nv"))###Cross-Correlation for certain variables
a
## Fig 2b  (correlation of flower traits for wgp)
nf2b<-read.csv("fs2b.csv")##data
b<-corr_cross(nf2b, rm.na = TRUE, max_pvalue = 0.05, top = 30)+ theme(axis.text = element_text(family="RMN",colour="black",size = 14))  ##条状图Show only most relevant results filtered by pvalue
b

library(cowplot)
cowplot::plot_grid(a,b,nrow=1)


## Figure S4

library(car)         # extracts model results
library(MASS)        # provides "birthwt" dataset
#library(ISLR)       # provides "Wage" dataset
library(tictoc)      # checks running time
library(sjPlot)      # visualizes model results
library(glmulti)     # finds the BEST model
library(flextable)   # beautifies tables
library(tidyverse)   # provides a lot of useful stuff !!! 
library(performance) # checks and compares quality of models


##a summer (model selection)## vrm
mug1<-read.csv("fs4a.csv")

mu01_model<-glmulti(vrm  ~ nvm+ ncm+ cn+ fn+ pn,
                    data   = mug1, 
                    crit   = aicc,       # AICC corrected AIC for small samples
                    level  = 1,          # 2 with interactions, 1 without  
                    method = "h",        # "d", or "h", or "g"
                    family = gaussian, 
                    fitfunction = glm,   # Type of model (LM, GLM etc.)
                    confsetsize = 100)   # Keep 100 best models

summary(mu01_model)
coef(mu01_model)
plot(mu01_model)
print(mu01_model)

weightable(mu01_model)[1:8,] %>% 
  regulartable() %>%       # beautifying tables
  autofit()

a<-plot(mu01_model, type = "s",col=2:7)



##b winter (model selection)##vrm
mug2<-read.csv("fs4b.csv")

mu02_model<-glmulti(vrm  ~ nvm+ ncm+ cn+ fn+ pn,
                    data   = mug2, 
                    crit   = aicc,       # AICC corrected AIC for small samples
                    level  = 1,          # 2 with interactions, 1 without  
                    method = "h",        # "d", or "h", or "g"
                    family = gaussian, 
                    fitfunction = glm,   # Type of model (LM, GLM etc.)
                    confsetsize = 100)   # Keep 100 best models

summary(mu02_model)
coef(mu02_model)
plot(mu02_model)
print(mu02_model)

weightable(mu02_model)[1:8,] %>% 
  regulartable() %>%       # beautifying tables
  autofit()

b<-plot(mu02_model, type = "s",col=2:7)

##c summer (model selection)## vrf

mug1<-read.csv("fs4a.csv")

mu03_model<-glmulti(vrf  ~ nvf+ ncf+ cn+ fn,
                    data   = mug1, 
                    crit   = aicc,       # AICC corrected AIC for small samples
                    level  = 1,          # 2 with interactions, 1 without  
                    method = "h",        # "d", or "h", or "g"
                    family = gaussian, 
                    fitfunction = glm,   # Type of model (LM, GLM etc.)
                    confsetsize = 100)   # Keep 100 best models

summary(mu03_model)
coef(mu03_model)
plot(mu03_model)
print(mu03_model)

weightable(mu03_model)[1:8,] %>% 
  regulartable() %>%       # beautifying tables
  autofit()

c<-plot(mu03_model, type = "s",col=2:7)

##d winter (model selection)##vrm

mug4<-read.csv("fs4b.csv")

mu04_model<-glmulti(vrf  ~ nvf+ ncf+ cn+ fn,      # "d", or "h", or "g"
                    data   = mug1, 
                    crit   = aicc,       # AICC corrected AIC for small samples
                    level  = 1,          # 2 with interactions, 1 without  
                    method = "h",        # "d", or "h", or "g"
                    family = gaussian, 
                    fitfunction = glm,   # Type of model (LM, GLM etc.)
                    confsetsize = 100)   # Keep 100 best models

summary(mu04_model)
coef(mu04_model)
plot(mu04_model)
print(mu04_model)

weightable(mu04_model)[1:8,] %>% 
  regulartable() %>%       # beautifying tables
  autofit()

d<-plot(mu04_model, type = "s",col=2:7)


## Figure S5   ##seed set ratio and seed number##
##a (boxplot)
library(ggplot2)
library(ggthemes)
library(multcompView)
library(dplyr)
library(agricolae)
library(ggpubr)
library(reshape2)
library(ggprism)


## a ## seed set ratio ## grazing
##data
nf3a <- read.csv("gf2.csv", head=T)
##
windowsFonts(A=windowsFont("Times New Roman"))
##figure
a<-ggplot(nf3a, aes(x=tr, y=ssr,fill=tr)) + 
  #stat_boxplot(geom = "errorbar", width=0.1, size=0.8)+
  geom_boxplot()+
  #geom_jitter(width=0.2, alpha=0.4,color="blue")+
  scale_fill_manual(values=c("#C77CFF","#619CFF"))+
  stat_summary(fun = "mean", geom = "point", shape =1,size = 4, color = "black")+
  theme_classic()+
  theme_test()+
  theme(axis.text=element_text(family = "A",size=14),axis.title=element_text(family = "A", size=14,face="bold"),legend.position=c(0.85,0.85))+
  labs(title="",x="", y = "Seed set ratio",size=14,family = "A")+
  scale_y_continuous(breaks=(seq(0.50, 0.9, 0.10)),limits=c(0.50,0.9))
#stat_compare_means(method = "t.test")
a


## b ## seed set ratio ## lar
##data
nf3b <- read.csv("gf6.csv", head=T)
##

##figure
b<-ggplot(nf3b, aes(x=tr, y=ssr,fill=tr)) + 
  #stat_boxplot(geom = "errorbar", width=0.1, size=0.8)+
  geom_boxplot()+
  #geom_jitter(width=0.2, alpha=0.4,color="blue")+
  scale_fill_manual(values=c("#C77CFF","#619CFF"))+
  stat_summary(fun = "mean", geom = "point", shape =1,size = 4, color = "black")+
  theme_classic()+
  theme_test()+
  theme(axis.text=element_text(family = "A",size=14),axis.title=element_text(family = "A", size=14,face="bold"),legend.position=c(0.85,0.85))+
  labs(title="",x="", y = "",size=14,family = "A")+
  scale_y_continuous(breaks=(seq(0.50, 0.9, 0.10)),limits=c(0.50,0.9))
#stat_compare_means(method = "t.test")
b


## c ## seed number ## grazing
##data
nf3a <- read.csv("gf2.csv", head=T)
##
##windowsFonts(A=windowsFont("Times New Roman"))
##figure
c<-ggplot(nf3a, aes(x=tr, y=sdn,fill=tr)) + 
  #stat_boxplot(geom = "errorbar", width=0.1, size=0.8)+
  geom_boxplot()+
  #geom_jitter(width=0.2, alpha=0.4,color="blue")+
  scale_fill_manual(values=c("#C77CFF","#619CFF"))+
  stat_summary(fun = "mean", geom = "point", shape =1,size = 4, color = "black")+
  theme_classic()+
  theme_test()+
  theme(axis.text=element_text(family = "A",size=14),axis.title=element_text(family = "A", size=14,face="bold"),legend.position=c(0.85,0.85))+
  labs(title="",x="", y = "Seed number per capitula",size=14,family = "A")+
  scale_y_continuous(breaks=(seq(7.0, 14.0, 1.0)),limits=c(7.0,14.0))
#stat_compare_means(method = "t.test")
c

## b ## seed number ## lar
##data
nf3b <- read.csv("gf6.csv", head=T)
##
d<-ggplot(nf3b, aes(x=tr, y=sdn,fill=tr)) + 
  #stat_boxplot(geom = "errorbar", width=0.1, size=0.8)+
  geom_boxplot()+
  #geom_jitter(width=0.2, alpha=0.4,color="blue")+
  scale_fill_manual(values=c("#C77CFF","#619CFF"))+
  stat_summary(fun = "mean", geom = "point", shape =1,size = 4, color = "black")+
  theme_classic()+
  theme_test()+
  theme(axis.text=element_text(family = "A",size=14),axis.title=element_text(family = "A", size=14,face="bold"),legend.position=c(0.85,0.85))+
  labs(title="",x="", y = "",size=14,family = "A")+
  scale_y_continuous(breaks=(seq(7.0, 14.0, 1.0)),limits=c(7.0,14.0))
#stat_compare_means(method = "t.test")
d

library(cowplot)
cowplot::plot_grid(a,b,c,d,nrow=2)



## Figure S6

library(car)         # extracts model results
library(MASS)        # provides "birthwt" dataset
#library(ISLR)       # provides "Wage" dataset
library(tictoc)      # checks running time
library(sjPlot)      # visualizes model results
library(glmulti)     # finds the BEST model
library(flextable)   # beautifies tables
library(tidyverse)   # provides a lot of useful stuff !!! 
library(performance) # checks and compares quality of models


##a 30% (model selection)##vrm
mug3<-read.csv("fs4c.csv")

mu01_model<-glmulti(vrm  ~ nvm+ ncm+ cn+ fn+ pn,
                    data   = mug3, 
                    crit   = aicc,       # AICC corrected AIC for small samples
                    level  = 1,          # 2 with interactions, 1 without  
                    method = "h",        # "d", or "h", or "g"
                    family = gaussian, 
                    fitfunction = glm,   # Type of model (LM, GLM etc.)
                    confsetsize = 100)   # Keep 100 best models

summary(mu01_model)
coef(mu01_model)
plot(mu01_model)
print(mu01_model)

weightable(mu01_model)[1:8,] %>% 
  regulartable() %>%       # beautifying tables
  autofit()

a<-plot(mu01_model, type = "s",col=2:7)

##b 0% (model selection)##vrm
mug4<-read.csv("fs4d.csv")

mu02_model<-glmulti(vrm  ~ nvm+ ncm+ cn+ fn+ pn,
                    data   = mug4, 
                    crit   = aicc,       # AICC corrected AIC for small samples
                    level  = 1,          # 2 with interactions, 1 without  
                    method = "h",        # "d", or "h", or "g"
                    family = gaussian, 
                    fitfunction = glm,   # Type of model (LM, GLM etc.)
                    confsetsize = 100)   # Keep 100 best models

summary(mu02_model)
coef(mu02_model)
plot(mu02_model)
print(mu02_model)

weightable(mu02_model)[1:8,] %>% 
  regulartable() %>%       # beautifying tables
  autofit()

b<-plot(mu02_model, type = "s",col=2:7)


##c 30% (model selection)##vrf
mug3<-read.csv("fs4c.csv")

mu03_model<-glmulti(vrf  ~ nvf+ ncf+ cn+ fn,
                    data   = mug3, 
                    crit   = aicc,       # AICC corrected AIC for small samples
                    level  = 1,          # 2 with interactions, 1 without  
                    method = "h",        # "d", or "h", or "g"
                    family = gaussian, 
                    fitfunction = glm,   # Type of model (LM, GLM etc.)
                    confsetsize = 100)   # Keep 100 best models

summary(mu03_model)
coef(mu03_model)
plot(mu03_model)
print(mu03_model)

weightable(mu03_model)[1:8,] %>% 
  regulartable() %>%       # beautifying tables
  autofit()

c<-plot(mu03_model, type = "s",col=2:7)


##d 0% (model selection)##vrf
mug4<-read.csv("fs4d.csv")

mu04_model<-glmulti(vrf  ~ nvf+ ncf+ cn+ fn,
                    data   = mug4, 
                    crit   = aicc,       # AICC corrected AIC for small samples
                    level  = 1,          # 2 with interactions, 1 without  
                    method = "h",        # "d", or "h", or "g"
                    family = gaussian, 
                    fitfunction = glm,   # Type of model (LM, GLM etc.)
                    confsetsize = 100)   # Keep 100 best models

summary(mu04_model)
coef(mu04_model)
plot(mu04_model)
print(mu04_model)

weightable(mu04_model)[1:8,] %>% 
  regulartable() %>%       # beautifying tables
  autofit()

d<-plot(mu04_model, type = "s",col=2:7)


###Figure S7  (Ranked Cross-Correlations of leaf area remove)

library(lares)
windowsFonts(RMN=windowsFont("Times New Roman"))###  修改字体

## Fig S7a  (correlation of flower traits for 30%)##
nf2c<-read.csv("fs3a.csv")##data
#corr_cross(nf2b, type = 2, contains="bio",top = NA)##点状图Cross-Correlation max values per category
a<-corr_cross(nf2c, rm.na = TRUE, max_pvalue = 0.05, top = 30)+ theme(axis.text = element_text(family="RMN",colour="black",size = 14))  ##条状图Show only most relevant results filtered by pvalue
#corr_cross(nf2b, plot = FALSE, top = 30)##相关结果（数据）Only data with no plot
#corr_cross(nf2a, contains = c("bio", "nv"))###Cross-Correlation for certain variables
a
## Fig S7a  (correlation of flower traits for 0%)#3vrm
nf2d<-read.csv("fs3b.csv")##data
b<-corr_cross(nf2d, rm.na = TRUE, max_pvalue = 0.05, top = 30)+ theme(axis.text = element_text(family="RMN",colour="black",size = 14))  ##条状图Show only most relevant results filtered by pvalue
b



library(cowplot)
cowplot::plot_grid(a,b,nrow=1)










##Table S1
mut<-read.csv("gts1.csv")

### nectar volume male (nvm)
model1=lmer(nvm~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model1)

### nectar concentration male (ncm)
model2=lmer(ncm~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model2)

### nectar volume female (nvf)
model3=lmer(nvf~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model3)

### nectar concentration female (ncf)
model4=lmer(ncf~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model4)


## capitula number per plant
model5=lmer(cn~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model5)


## flower number per capitula
model6=lmer(fn~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model6)

## pollen number per anther

model7=lmer(pn~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model7)


## pollen removal

model8=lmer(pr~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model8)


## visitation rate of male

model9=lmer(vrm~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model9)

## visitation rate of female

model10=lmer(vrf~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model10)


## NSC 

model11=lmer(nsc~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model11)


## seed number per capitula

model12=lmer(sdn~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model12)

## ssr

model13=lmer(ssr~tr+agb+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model13)

## agb

model14=lmer(agb~tr+sp+sn+sk+ (1 |plot)+(1|site),data=mut)
summary(model14)


##model1=glmer(fn~tr+agb+sw+sn+sk+ (1 |plot|site), family = "poisson",data=mut)
##summary(model1)



##Table S2
mut<-read.csv("gts2.csv")

### nectar volume male (nvm)
model1=lmer(nvm~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model1)

### nectar concentration male (ncm)
model2=lmer(ncm~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model2)

### nectar volume female (nvf)
model3=lmer(nvf~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model3)

### nectar concentration female (ncf)
model4=lmer(ncf~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model4)


## capitula number per plant
model5=lmer(cn~tr+agb+(1 |plot)+(1|site),data=mut)
summary(model5)


## flower number per capitula
model6=lmer(fn~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model6)

## pollen number per anther

model7=lmer(pn~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model7)


## pollen removal

model8=lmer(pr~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model8)


## visitation rate of male

model9=lmer(vrm~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model9)

## visitation rate of female

model10=lmer(vrf~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model10)


## NSC 

model11=lmer(nsc~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model11)


## seed number per capitula

model12=lmer(sdn~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model12)

## ssr

model13=lmer(ssr~tr+agb+ (1 |plot)+(1|site),data=mut)
summary(model13)

## agb

model14=lmer(agb~tr+ (1 |plot)+(1|site),data=mut)
summary(model14)
