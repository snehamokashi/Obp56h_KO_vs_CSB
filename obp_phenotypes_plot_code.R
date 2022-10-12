library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

## data import etc

activity<-as.data.frame(activity_total_for_r_ko)
heat_shock<-as.data.frame(heat_shock_for_r_ko)
surv<-as.data.frame(starv_ko_for_r)
sleep<-as.data.frame(sleep_proportion_for_r_ko)

## total activity

activity_F<-filter(activity, sex=="F")
activity_M<-filter(activity, sex=="M")

# activity plot

act_plot_F<- ggplot(activity_F,aes(Line,mean_value))+geom_boxplot(fill="pink")+theme_classic()+ggtitle("Total activity (females)")+ ylab("Counts")+theme(aspect.ratio=1)+theme(plot.title = element_text(hjust = 0.5))+xlab("")+scale_y_continuous(limits = c(0, 3000))
act_plot_M<- ggplot(activity_M,aes(x=Line,y=mean_value))+geom_boxplot(fill="lightblue")+theme_classic()+ggtitle("Total activity (males)")+ ylab("Counts")+theme(aspect.ratio=1)+theme(plot.title = element_text(hjust = 0.5))+xlab("")+scale_y_continuous(limits = c(0, 3000))

## heat shock

hs_F<-filter(heat_shock, sex=="F")
hs_M<-filter(heat_shock,sex=="M")

# heat shock plot

hs_plot_F<- ggplot(hs_F,aes(line,Percent_alive))+geom_boxplot(fill="pink")+theme_classic()+ggtitle("Percent alive after heat shock (females)")+ ylab("Percent alive")+theme(aspect.ratio=1)+theme(plot.title = element_text(hjust = 0.5))+scale_y_continuous(limits = c(0, 100))+xlab("")

hs_plot_M<- ggplot(hs_M,aes(line,Percent_alive))+geom_boxplot(fill="lightblue")+theme_classic()+ggtitle("Percent alive after heat shock (males)")+ ylab("Percent alive")+theme(aspect.ratio=1)+theme(plot.title = element_text(hjust = 0.5))+scale_y_continuous(limits = c(0, 100))+xlab("")

## sleep prop day time

slp_day<-filter(sleep,Light_status=="Day")
slp_d_m<-filter(slp_day,sex=="M")
slp_d_f<-filter(slp_day,sex=="F")

# day sleep plot

day_slp_plot_F<- ggplot(slp_d_f,aes(line,mean_sleep_per_ind))+geom_boxplot(fill="pink")+theme_classic()+ggtitle("Proportion of sleep during day (females)")+ ylab("sleep proportion")+theme(aspect.ratio=1)+theme(plot.title = element_text(hjust = 0.5))+xlab("")+scale_y_continuous(limits = c(0,1.0))

day_slp_plot_M<- ggplot(slp_d_m,aes(line,mean_sleep_per_ind))+geom_boxplot(fill="lightblue")+theme_classic()+ggtitle("Proportion of sleep during day (males)")+ ylab("sleep proportion")+theme(aspect.ratio=1)+theme(plot.title = element_text(hjust = 0.5))+xlab("")+scale_y_continuous(limits = c(0,1.0))

## night sleep

slp_night<-filter(sleep,Light_status=="Night")
slp_n_m<-filter(slp_night,sex=="M")
slp_n_f<-filter(slp_night,sex=="F")

# night sleep plot

night_slp_plot_F<- ggplot(slp_n_f,aes(line,mean_sleep_per_ind))+geom_boxplot(fill="pink")+theme_classic()+ggtitle("Proportion of sleep during night (females)")+ ylab("sleep proportion")+theme(aspect.ratio=1)+theme(plot.title = element_text(hjust = 0.5))+xlab("")+scale_y_continuous(limits = c(0,1.0))

night_slp_plot_M<- ggplot(slp_n_m,aes(line,mean_sleep_per_ind))+geom_boxplot(fill="lightblue")+theme_classic()+ggtitle("Proportion of sleep during night (males)")+ ylab("sleep proportion")+theme(aspect.ratio=1)+theme(plot.title = element_text(hjust = 0.5))+xlab("")+scale_y_continuous(limits = c(0,1.0))

# survival curves

library(ggfortify)
library(survival)
library(ggsurvfit)
library(survminer)

fit_f <- survfit(Surv(Lifespan_under_starvation_hours) ~ Line, data = surv_lifespan_f)
surv_curv_f<-ggsurvplot(fit_f,palette = c("black","red"))+ggtitle("Survival curves under starvation (females)")

fit_m <- survfit(Surv(Lifespan_under_starvation_hours) ~ Line, data = surv_lifespan_m)
surv_curv_m<-ggsurvplot(fit_m,palette = c("black","red"))+ggtitle("Survival curves under starvation (males)")

### putting all plots together

fig_1<-plot_grid(surv_curv_m,surv_curv_f,lifespan_plot_M,lifespan_plot_F,hs_plot_M,hs_plot_F,labels="AUTO", ncol=2,align="h")
fig_1

fig_2<-plot_grid(act_plot_M,act_plot_F,day_slp_plot_M,day_slp_plot_F,night_slp_plot_M,night_slp_plot_F, labels = "AUTO", ncol=2, align="h")
fig_2
# to be fine tuned in Illustrator; need to add asterisks for significance based on ANOVAs done in SAS Studio







