library(tidyverse)
library(survival)
library(survminer)

########################
#### DADOS----
########################
df = read.csv2("dados7.csv", sep = ";")
df <- dados7
colnames(df) = c("V", "tempo", "censura")
head(df)
str(df)
attach(df)

########################
#### Kaplan-Meier
########################
ekm_nulo = survfit(Surv(tempo, censura) ~ 1)
summary(ekm_nulo)
plot(ekm_nulo)

ggsurvplot(ekm_nulo, df,
           legend.title = "")

ekm_v = survfit(Surv(tempo, censura) ~ V)
summary(ekm_v)
plot(ekm_v)

ggsurvplot(ekm_v, df,
           legend.title = "")

########################
#### Ajuste pelo modelo Weibull (potencia inversa)
########################
Xi = -log(df$V) # Variavel stress defini??o potencia inversa 
m1 = survreg(Surv(tempo,censura) ~ Xi, dist="weibull")  
m1
summary(m1)

# observar kappa, o parametro fixo do modelo (independente do nivel de stress o k ? fixo)
k = 1/m1$scale 
k

# observar parametros \beta_o e \beta_1 (de potencia inversa)
deltha = exp(m1$coef[1]) # coef[1] seria o \beta_0
deltha
gama = m1$coef[2] # coef[2] seria o \beta_1
gama
# definir os niveis de fatores para encontrar o \lambda de cada um
vi = as.numeric(levels(factor(df$V)))
lambda = deltha*vi^(-gama) # formula da potencia inversa (lamda serio o nosso \theta)
lambda 
st <- ekm_v$surv
list.wei<-as.data.frame(summary(m1))
par(mfrow=c(1,4))
ekm.v50 <- survfit(Surv(df$tempo[V==50],df$censura[V==50])~1)  
tempo50<-log(ekm.v50$time)

ekm.v25 <- survfit(Surv(df$tempo[V==25],df$censura[V==25])~1)  
tempo25<-log(ekm.v25$time)

ekm.v10 <- survfit(Surv(df$tempo[V==10],df$censura[V==10])~1)  
tempo10<-log(ekm.v10$time)

ekm.v5 <- survfit(Surv(df$tempo[V==5],df$censura[V==5])~1)  
tempo5<-log(ekm.v5$time)
surv50 <- log(-log(ekm.v50$surv))
surv25 <- log(-log(ekm.v25$surv))
surv10 <- log(-log(ekm.v10$surv))
surv5 <- log(-log(ekm.v5$surv))
par(mfrow=c(1,4))
plot(tempo50, surv50, pch=19)
plot(tempo25, surv25, pch=19)
plot(tempo10, surv10, pch=19)
plot(tempo5, surv5, pch=19)