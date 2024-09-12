####FIG1
load("Fig 1 Data.RData")

library(ggplot2)
library(ggpubr)

p <- c("Southern Asia","Western Africa","Eastern Africa","Southern Africa","Central Africa")

for(i in p){
  dat <- subset(m,region==i)
  
  assign(i,ggplot(data = dat, aes(x = tmeanWT, y =RR)) +
           theme_bw()+
           geom_ribbon(aes(ymax = upper, ymin = lower), fill = "#F0A73A", alpha = 0.3)+
           geom_line(colour = "#F0A73A",size=1.5)+
           geom_hline(yintercept = 1, colour = "black", linetype = "dashed") +
           labs(x = "Average temperature during pregnancy (°C)", 
                y = "Odds ratio for LBW",title =i ,subtitle = dat$p.nonlinear[1]) +
           theme(
             axis.title.x = element_text(family = "serif",vjust = 1,size = 30),
             axis.title.y = element_text(family = "serif",vjust = 1,size=30),
             axis.text.x = element_text(family = "serif",size = 30),
             axis.text.y = element_text(family = "serif",size = 30),
             text = element_text(family = "serif",size = 30),
             plot.subtitle = element_text(family = "serif",vjust=0.5,hjust=1,size = 30,face="italic"),
             plot.title = element_text(family = "serif",vjust=-3.5,hjust=0,size = 36)))
  
}

a <- ggarrange(`Southern Asia`,`Western Africa`,`Eastern Africa`,`Southern Africa`,`Central Africa`,
               ncol = 1,nrow = 5)



for(i in p){
  dat <- subset(n,region==i)
  
  assign(i,ggplot(data = dat, aes(x = tmeanWT, y = beta)) +
           theme_bw()+
           geom_ribbon(aes(ymax = upper, ymin = lower), fill = "#3ABF99", alpha = 0.3)+
           geom_line(colour = "#3ABF99",size=1.5)+
           geom_hline(yintercept = 1, colour = "black", linetype = "dashed") +
           labs(x = "Average temperature during pregnancy (°C)", 
                y = "Changes in mean birth weight (g)",title =i ,subtitle = dat$p.nonlinear[1]) +
           theme(
             axis.title.x = element_text(family = "serif",vjust = 1,size = 30),
             axis.title.y = element_text(family = "serif",vjust = 1,size=30),
             axis.text.x = element_text(family = "serif",size = 30),
             axis.text.y = element_text(family = "serif",size = 30),
             text = element_text(family = "serif",size = 30),
             plot.subtitle = element_text(family = "serif",vjust=0.5,hjust=1,size = 30,face="italic"),
             plot.title = element_text(family = "serif",vjust=-3.5,hjust=0,size = 36)))
  
}

b <- ggarrange(`Southern Asia`,`Western Africa`,`Eastern Africa`,`Southern Africa`,`Central Africa`,
               ncol = 1,nrow = 5)


ggarrange(b,a,ncol = 2,nrow = 1,labels="auto",font.label = list(size = 36,family = "serif"))


ggsave("fig1.jpg", path = "",width = 24,height = 40,dpi=300,limitsize = FALSE)


####FIG2

load("Fig 2 Data.RData")

library(forestploter)
library(grid)

###FIG2a

p <- p0[1:9,]

subgps <- c(2:3,5:6,8:9)
p$Region[subgps] <- paste0("    ", p$Region[subgps])

p$`       Heat (> 90th), β (95% CI)` <- paste(rep("      ",8), collapse = " ")
p$`       Cold (< 10th), β (95% CI)` <- paste(rep("      ",8), collapse = " ")
p$` ` <- "     "

p$subgroup <- NA


tm <- forest_theme(base_size = 14,
                   base_family="serif",
                   ci_pch = 20,
                   ci_lty = 1,
                   ci_col = c("indianred","steelblue"),
                   legend_value =c("Heat","Cold"),
                   ci_lwd = 3,
                   ci_Theight = NULL,
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   legend_position="none",
                   title_just = c("center"))

pt <- forest(p[,c(1:3,10,12,11)],
             est = list(p$OR,p$subgroup,p$subgroup,p$OR1),
             lower = list(p$Lower,p$subgroup,p$subgroup,p$Lower1), 
             upper = list(p$Upper,p$subgroup,p$subgroup,p$Upper1),
             ci_column = c(4,6),
             ref_line = 1,
             xlog = FALSE,
             theme = tm,
             title = "(a) Association between heat and cold exposure during pregnancy and mean birth weight (g)")


g <- edit_plot(pt,
               row = c(1,4,7),
               gp = gpar(fontface = "bold"))


g <- add_border(g, part = "header")

jpeg(file = "fig2a.jpg", res = 300, width = 4100, height = 1000)
plot(g)
dev.off()


###FIG2b

p <- p0[10:18,]

subgps <- c(2:3,5:6,8:9)
p$Region[subgps] <- paste0("    ", p$Region[subgps])

p$`       Heat (> 90th), OR (95% CI)` <- paste(rep("      ",8), collapse = " ")
p$`       Cold (< 10th), OR (95% CI)` <- paste(rep("      ",8), collapse = " ")
p$` ` <- "     "
p$subgroup <- NA

tm <- forest_theme(base_size = 14,
                   base_family="serif",
                   ci_pch = 20,
                   ci_lty = 1,
                   ci_col = c("indianred","steelblue"),
                   legend_value =c("Heat","Cold"),
                   ci_lwd = 3,
                   ci_Theight = NULL,
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   legend_position="none",
                   title_just = c("center"))

pt <- forest(p[,c(1:3,10,12,11)],
             est = list(p$OR,p$subgroup,p$subgroup,p$OR1),
             lower = list(p$Lower,p$subgroup,p$subgroup,p$Lower1), 
             upper = list(p$Upper,p$subgroup,p$subgroup,p$Upper1),
             ci_column = c(4,6),
             ref_line = 1,
             xlog = TRUE,
             theme = tm,
             title = "(b) Association between heat and cold exposure during pregnancy and LBW")


g1 <- edit_plot(pt,
                row = c(1,4,7),
                gp = gpar(fontface = "bold"))


g1 <- add_border(g1, part = "header")

jpeg(file = "fig2b.jpg", res = 300, width = 4100, height = 1000)
plot(g1)
dev.off()





####FIG3
load("Fig 3 Data.RData")

library(ggplot2)
library(ggpubr)
library(paletteer)


####FIG3a
m <- paletteer_d("palettesForR::Tango")

p1 <- ggplot(data = df, aes(x = Year, y = value, colour = type)) + geom_line(size=3)+theme_classic()+
  scale_color_manual(name="", values=m[c(8,20,11)])+
  geom_ribbon(aes(x = Year,ymax = max, ymin = min, fill = type), alpha = 0.3,show.legend = FALSE, color = NA )+
  scale_fill_manual(name="", values=m[c(7,19,10)])+
  ylab("Annual average temperature (°C)")+
  scale_x_continuous(breaks=c(1990,1994,1999,2004,2009,2014,2018))+
  guides(fill = guide_legend(nrow = 1, override.aes = list(size=4)))+
  theme(plot.title = element_text(hjust = 0.7,family = "serif",size = 35),
        axis.title.x = element_text(family = "serif",vjust = 1,size = 32),
        axis.title.y = element_text(family = "serif",vjust = 1,size=32),
        axis.text.x = element_text(family = "serif",size = 32),
        axis.text.y = element_text(family = "serif",size = 32),
        text = element_text(family = "serif",size = 32),
        legend.title = element_text(family = "serif",size = 32),
        legend.text = element_text(family = "serif",size=32),
        legend.position = "bottom") 


####FIG3b
k <- paletteer_d("rcartocolor::RedOr")
j <- paletteer_d("rcartocolor::Teal")
n <- paletteer_d("awtools::bpalette")


p0 <- ggplot()+geom_polygon(data=line1,aes(x=long,y=lat,group=group),col="grey",alpha=0,fill="white",size=0.5)+
  theme_bw()+
  
  geom_point(data=sample,aes(x=LONGNUM,y=LATNUM,colour=`Annual average temperature (°C)` ))+
  scale_color_manual(values = c(j[4],k),
                     labels = c("< 0","0 ~ 20","20 ~ 22","22 ~ 24","24 ~ 26","26 ~ 28","28 ~ 30","> 30"))+
  scale_x_continuous(breaks=c(-25,0,25,50,75,100),labels = c("25°W","0°","25°E","50°E","75°E","100°E"))+
  scale_y_continuous(breaks=c(-20,0,20),labels = c("20°S","0°","20°N"))+
  guides(color = guide_legend(nrow = 1, override.aes = list(size=10)))+
  geom_polygon(data=linesas,aes(x=long,y=lat,group=group),col=n[13],alpha=0,fill="white",size=2)+
  geom_polygon(data=linesaf,aes(x=long,y=lat,group=group),col=n[6],alpha=0,fill="white",size=2)+
  geom_polygon(data=linemaf,aes(x=long,y=lat,group=group),col=n[3],alpha=0,fill="white",size=2)+
  geom_polygon(data=linewaf,aes(x=long,y=lat,group=group),col=n[9],alpha=0,fill="white",size=2)+
  geom_polygon(data=lineeaf,aes(x=long,y=lat,group=group),col=n[2],alpha=0,fill="white",size=2)+
  theme(plot.title = element_text(hjust = 0.5,family = "serif",size = 32),
        axis.text.x = element_text(family = "serif",size = 32),
        axis.text.y = element_text(family = "serif",size = 32),
        legend.title = element_text(family = "serif",size = 32),
        legend.text = element_text(family = "serif",size=32),
        legend.position = "bottom") +
  coord_cartesian(xlim=c(-20,100),ylim=c(-35,35))+
  xlab(NULL)+
  ylab(NULL)


ggarrange(p0,p1,ncol=1,labels = 'auto',font.label = list(size = 35))

ggsave("fig3.jpg", path = "C:/Users/朱钲宏/Desktop/返修0731/figdata1/figdata/",width = 24,height = 30,dpi=300,limitsize = FALSE)



####FIG4
load("Fig 4 Data.RData")

library(ggplot2)
library(ggtext)
library(paletteer)
library(patchwork)


n <- paletteer_d("awtools::bpalette")

lab <- c(dat1$Country[1:8],expression(bold('Southern Africa')),"",dat1$Country[10:12],expression(bold('Central Africa')),"",
         dat1$Country[14:20],expression(bold('Eastern Africa')),"",dat1$Country[22:31],expression(bold('Western Africa')),"",
         dat1$Country[33:35],expression(bold('Southern Asia')))

p1 <- ggplot(data=dat1,aes(x = z, y = rato)) +
  coord_flip()+
  theme_classic()+ 
  geom_bar(aes(fill = FQ),stat = 'identity')+
  scale_fill_manual(values = n[c(3,2,6,13,9)])+
  xlab(NULL)+
  ylab("Baseline LBW rate (%)")+
  labs(fill = "Regions")+
  guides(fill=guide_legend(label.theme = element_text(size = 44,family = "serif"),title.theme = element_text(size = 44,family = "serif")))+
  theme(legend.position = "bottom",
        axis.title.x = element_text(family = "serif",size=44),
        axis.text=element_text(family = "serif",size=44,colour = "black"),
        axis.text.x=element_text(vjust = 0.5),
        text = element_text(family = "serif"),
        axis.line.x=element_line(size=3),
        axis.line.y=element_line(size=3))+
  scale_x_continuous(limits=c(0,41),breaks=c(1:40),
                     labels = lab[1:40])

m <- paletteer_d("palettesForR::Tango")

p22 <- ggplot(data=dat2) +
  coord_flip()+
  theme_classic()+
  geom_crossbar(aes(x = z, y = 0, ymin = 0, ymax = x2, fill = FQ),
                fatten = 0, colour = "white") +
  geom_crossbar(aes(x = z, y = 0, ymin = 0,ymax = x5),fatten = 0,width = .6,fill = "grey",
                alpha = .8,colour = "transparent",show.legend = FALSE)+
  # geom_crossbar(data=dat3,aes(x = z, y = 0, ymin = 0, ymax = x2, fill = FQ),
  #               fatten = 0, colour = "white") +
  # geom_crossbar(data=dat3,aes(x = z, y = 0, ymin = 0,ymax = x5),fatten = 0,width = .6,fill = "grey",
  #               alpha = .8,colour = "transparent",show.legend = FALSE)+
  scale_fill_manual(values = n[c(3,2,6,13,9)])+
  xlab(" ")+
  ylab("PARP for Heat (%)")+
  guides(fill="none",color="none")+
  geom_hline(yintercept = 0, color = "black", size = 3) +
  scale_x_continuous(limits=c(0,41),breaks=NULL)+
  scale_y_continuous(limits=c(0,6.6),expand = c (0, 0),breaks=c(0,2,4),labels = c("0","2","4"))+
  theme(
    legend.position = "none",
    plot.title = element_textbox(size = 50, hjust = 0.5, face = "bold"),
    axis.text=element_text(family = "serif",size=50,colour = "black"),
    axis.text.x=element_text(vjust = 0.5),
    axis.title.x = element_text(size = 50,family = "serif"),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_line(size = 2,linetype ="dashed",color = "grey"),
    panel.grid.minor = element_blank(),
    axis.line.x=element_line(size=3),
    axis.line.y=element_blank()
  )




p2 <- ggplot(data=dat2) +
  coord_flip()+
  theme_classic()+
  # geom_crossbar(aes(x = z, y = 0, ymin = 0, ymax = x2, fill = FQ),
  #               fatten = 0, colour = "white") +
  # geom_crossbar(aes(x = z, y = 0, ymin = 0,ymax = x5),fatten = 0,width = .6,fill = "grey",
  #               alpha = .8,colour = "transparent",show.legend = FALSE)+
  geom_crossbar(data=dat3,aes(x = z, y = 0, ymin = 0, ymax = x2, fill = FQ),
                fatten = 0, colour = "white") +
  geom_crossbar(data=dat3,aes(x = z, y = 0, ymin = 0,ymax = x5),fatten = 0,width = .6,fill = "grey",
                alpha = .8,colour = "transparent",show.legend = FALSE)+
  scale_fill_manual(values = n[c(3,2,6,13,9)])+
  xlab(" ")+
  ylab("PARP for Cold (%)")+
  guides(fill="none",color="none")+
  geom_hline(yintercept = 0, color = "black", size = 3) +
  scale_x_continuous(limits=c(0,41),breaks=NULL)+
  scale_y_continuous(limits=c(-7.5,0),expand = c (0, 0),breaks=c(-4,-2,0),labels = c("4","2","0"))+
  theme(
    legend.position = "none",
    plot.title = element_textbox(size = 50, hjust = 0.5, face = "bold"),
    axis.text=element_text(family = "serif",size=50,colour = "black"),
    axis.text.x=element_text(vjust = 0.5),
    axis.title.x = element_text(size = 50,family = "serif"),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_line(size = 2,linetype ="dashed",color = "grey"),
    panel.grid.minor = element_blank(),
    axis.line.x=element_line(size=3),
    axis.line.y=element_blank()
  )


p2 <- p2+
  annotate("rect", xmin = 38.5, xmax = 40.5,ymin = -7.5, ymax = -0.5,fill = "black")+
  annotate("text", x = 39.5, y = -6.25, 
           label = expression(bold('Factual')), 
           size = 14, hjust = 0.5,
           family = "serif", color = "white")+
  annotate("rect", xmin = 38.7, xmax = 40.3,ymin = -5, ymax = -0.5,fill = "grey")+
  annotate("text", x = 39.5, y = -2.75, 
           label = expression(bold('Counterfactual')), 
           size = 14, hjust = 0.5,
           family = "serif", color = "white")+
  annotate("text", x = 41, y = -4, 
           label = expression(bold('Scenarios')), 
           size = 18, hjust = 0.5,
           family = "serif", color = "black")


p3 <- ggplot(data=dat2) +
  coord_flip()+
  theme_classic()+
  geom_bar(aes(x = z, y = x11), show.legend = NA,stat = "identity", width = 0.2) + 
  geom_point(aes(x = z, y = x11,color = FQ),show.legend = NA,size = 12) + 
  geom_bar(data=dat3,aes(x = z, y = x11), show.legend = NA,stat = "identity", width = 0.2) + 
  geom_point(data=dat3,aes(x = z, y = x11,color = FQ), show.legend = NA,size = 12) +        
  geom_hline(yintercept = 0, color = "black", size = 3) + 
  scale_color_manual(values = n[c(3,2,6,13,9)])+
  xlab(" ")+
  ylab("Attributable proportion (%)")+
  guides(fill="none",color="none")+
  geom_text(data=dat2,aes(x=z,y=x11+15, label=x13),color="black", size=14,family = "serif")+
  geom_text(data=dat3,aes(x=z,y=x11-15, label=x13),color="black", size=14,family = "serif")+
  scale_x_continuous(limits=c(0,41),breaks=NULL)+
  scale_y_continuous(limits=c(-100,120),expand = c (0, 0),breaks=c(-70,-35,0,35,70),labels = c("70","Cold","0","Heat","70"))+
  theme(
    legend.position = "none",
    plot.title = element_textbox(size = 44, hjust = 0.5, face = "bold"),
    axis.text=element_text(family = "serif",size=44,colour = "black"),
    axis.text.x=element_text(vjust = 0.5),
    axis.title.x = element_text(size = 44,family = "serif"),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_line(size = 2,linetype ="dashed",color = "grey"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x=element_line(size=3),
    axis.line.y=element_blank()
  )

pp <- p1+p2+p3+plot_layout(widths=c(1,1.5,1.5),ncol = 3,guides = 'collect')  + 
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 60,family = "serif"),legend.position = "bottom") 

ggsave("fig4.jpg",path = "",width =40 ,height = 32,limitsize = FALSE) 



####FIG5
load("Fig 5 Data.RData")

library(ggplot2)
library(ggpubr)
library(paletteer)

m <- paletteer_d("colorBlindness::Blue2Orange12Steps")
n <- paletteer_d("awtools::bpalette")




p1 <- ggplot()+geom_tile(data=dat1,aes(x=X,y=Y,fill=pop.))+
  theme_minimal()+
  geom_polygon(data=line1,aes(x=long,y=lat,group=group),col="grey",alpha=0,fill="white",size=0.5)+
  scale_fill_manual(values = c(m[c(1,3,4,6,7,9,11)],n[1]),
                    labels = c("< -250","-250 ~ -50","-50 ~ -5","-5 ~ 0","0 ~ 5","5 ~ 50","50 ~ 250","> 250"))+
  labs(title = "(a) aLBWs, 2000-2018",fill="aLBWs") +
  scale_x_continuous(breaks=c(-25,0,25,50,75,100),labels = c("25°W","0°","25°E","50°E","75°E","100°E"))+
  scale_y_continuous(breaks=c(-20,0,20),labels = c("20°S","0°","20°N"))+
  theme(plot.title = element_text(hjust = 0.5,family = "serif",size = 35),
        axis.title.x = element_text(family = "serif",vjust = 1,size = 25),
        axis.title.y = element_text(family = "serif",vjust = 1,size=25),
        axis.text.x = element_text(family = "serif", size = 25),
        axis.text.y = element_text(family = "serif",size = 25),
        text = element_text(family = "serif",size = 25),
        legend.title = element_text(family = "serif",size = 25),
        legend.text = element_text(family = "serif",size=25),
        legend.position = "bottom") +
  coord_cartesian(xlim=c(-20,100),ylim=c(-35,35))+
  xlab(NULL)+
  ylab(NULL)+ guides(fill = guide_legend(nrow = 1))





p2 <- ggplot()+geom_tile(data=dat2,aes(x=X,y=Y,fill=pop.))+
  theme_minimal()+
  geom_polygon(data=line1,aes(x=long,y=lat,group=group),col="grey",alpha=0,fill="white",size=0.5)+
  scale_fill_manual(values = c(m[c(1,3,4,6,7,9,11)],n[1]),
                    labels = c("< -250","-250 ~ -50","-50 ~ -5","-5 ~ 0","0 ~ 5","5 ~ 50","50 ~ 250","> 250"))+
  labs(title = "(b) aLBWs, 2010-2018 minus 2000-2009",fill="aLBWs") +
  scale_x_continuous(breaks=c(-25,0,25,50,75,100),labels = c("25°W","0°","25°E","50°E","75°E","100°E"))+
  scale_y_continuous(breaks=c(-20,0,20),labels = c("20°S","0°","20°N"))+
  theme(plot.title = element_text(hjust = 0.5,family = "serif",size = 35),
        axis.title.x = element_text(family = "serif",vjust = 1,size = 25),
        axis.title.y = element_text(family = "serif",vjust = 1,size=25),
        axis.text.x = element_text(family = "serif", size = 25),
        axis.text.y = element_text(family = "serif",size = 25),
        text = element_text(family = "serif",size = 25),
        legend.title = element_text(family = "serif",size = 25),
        legend.text = element_text(family = "serif",size=25),
        legend.position = "bottom") +
  coord_cartesian(xlim=c(-20,100),ylim=c(-35,35))+
  xlab(NULL)+
  ylab(NULL)+ guides(fill = F)

m <- paletteer_d("ggsci::teal_material")

n <- paletteer_d("ggsci::pink_material")


p3 <- ggplot()+geom_tile(data=dat3,aes(x=X,y=Y,fill=pec1))+
  theme_minimal()+
  geom_polygon(data=line1,aes(x=long,y=lat,group=group),col="grey",alpha=0,fill="white",size=0.5)+
  scale_fill_manual(values = c(m[c(9,6,3,1)],n[c(1,3,6,9)]),
                    labels = c("< -250","-250 ~ -100","-100 ~ -10","-10 ~ 0","0 ~ 10","10 ~ 100","100 ~ 250","> 250"))+
  labs(title = "(c) aLBWs per million, 2000-2018",fill="aLBWs per million") +
  scale_x_continuous(breaks=c(-25,0,25,50,75,100),labels = c("25°W","0°","25°E","50°E","75°E","100°E"))+
  scale_y_continuous(breaks=c(-20,0,20),labels = c("20°S","0°","20°N"))+
  theme(plot.title = element_text(hjust = 0.7,family = "serif",size = 35),
        axis.title.x = element_text(family = "serif",vjust = 1,size = 25),
        axis.title.y = element_text(family = "serif",vjust = 1,size=25),
        axis.text.x = element_text(family = "serif",size = 25),
        axis.text.y = element_text(family = "serif",size = 25),
        text = element_text(family = "serif",size = 25),
        legend.title = element_text(family = "serif",size = 25),
        legend.text = element_text(family = "serif",size=25),
        legend.position = "bottom") +
  coord_cartesian(xlim=c(-20,100),ylim=c(-35,35))+
  xlab(NULL)+
  ylab(NULL)+ guides(fill = guide_legend(nrow = 1))



p4 <- ggplot()+geom_tile(data=dat4,aes(x=X,y=Y,fill=pec1))+
  theme_minimal()+
  geom_polygon(data=line1,aes(x=long,y=lat,group=group),col="grey",alpha=0,fill="white",size=0.5)+
  scale_fill_manual(values = c(m[c(9,6,3,1)],n[c(1,3,6,9)]),
                    labels = c("< -250","-250 ~ -100","-100 ~ -10","-10 ~ 0","0 ~ 10","10 ~ 100","100 ~ 250","> 250"))+
  scale_x_continuous(breaks=c(-25,0,25,50,75,100),labels = c("25°W","0°","25°E","50°E","75°E","100°E"))+
  scale_y_continuous(breaks=c(-20,0,20),labels = c("20°S","0°","20°N"))+
  labs(title = "(d) aLBWs per million, 2010-2018 minus 2000-2009",fill="aLBWs per million") +
  theme(plot.title = element_text(hjust = 0.7,family = "serif",size = 35),
        axis.title.x = element_text(family = "serif",vjust = 1,size = 25),
        axis.title.y = element_text(family = "serif",vjust = 1,size=25),
        axis.text.x = element_text(family = "serif",size = 25),
        axis.text.y = element_text(family = "serif",size = 25),
        text = element_text(family = "serif",size = 25),
        legend.title = element_text(family = "serif",size = 25),
        legend.text = element_text(family = "serif",size=25),
        legend.position = "bottom") +
  coord_cartesian(xlim=c(-20,100),ylim=c(-35,35))+
  xlab(NULL)+
  ylab(NULL)+ guides(fill = FALSE)


a <- ggarrange(p1, p2,  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

b <- ggarrange(p3, p4,  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

c <- ggarrange(a, b,  ncol=1, nrow=2)


ggsave("fig5.jpg", path = "",width = 28,height = 20,limitsize = FALSE) 






