#libraries
library(tidyverse)
library(foreign)
library(readstata13)
library(plm)
library(lmtest)

#read data
cai_data<-read.dta13("/Users/jisangyu/Dropbox/AGEC936/data/HW data/Cai_AEJ_sample_net_savings.dta") %>%
  mutate(
    treatment=as_factor(treatment),
    hhno=as_factor(hhno),
    year=as_factor(year),
  )

#Year x treatment
DT <- sapply(2001:2008, function(l) {
  1*( (cai_data$year == l) & (cai_data$treatment == 1) )
})
DT <-as.data.frame(DT)
colnames(DT)<-c(paste0("DT",2001:2008))
cai_data<-cbind.data.frame(cai_data,DT)

#Pre-trend regression
pretrend_reg <- cai_data %>% 
  plm(totalnetsaving ~ year  + DT2001 + DT2003 + DT2004 + DT2005 + DT2006 + DT2007 + DT2008, 
      data=., model="within",
      index=c("hhno"))
results<-coeftest(pretrend_reg, vcov=function(x) vcovHC(x, cluster=c("group"))) 
results<-as.data.frame.matrix(results)

#plot
plot <- tibble(
  sd = results[[2]][8:14],
  mean = results[[1]][8:14],
  year = c(2001, 2003:2008))

plot %>% 
  ggplot(aes(x = year, y = mean)) + 
  ylab(expression(delta[t]))+
  geom_rect(aes(xmin=2003, xmax=2008, ymin=-Inf, ymax=Inf), fill = "cyan", alpha = 0.01)+
  geom_point()+
  geom_text(aes(label = year), hjust=-0.002, vjust = -0.03)+
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean - sd*1.96, ymax = mean + sd*1.96), width = 0.2,
                position = position_dodge(0.05))