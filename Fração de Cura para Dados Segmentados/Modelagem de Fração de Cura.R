library(tidyverse)
library(ggpattern)
library(gridExtra)
library(survival)
library(flexsurvcure)
library(survminer)
library(cuRe)

### Dados
setwd("")
set.seed(4)

df = read.csv2("telecom_customer_churn.csv", sep = ",")
head(df)
summary(df)
str(df)

df2 = data.frame(df$Tenure.in.Months, df$Internet.Type, 
                 df$Customer.Status, df$Number.of.Dependents,
                 df$Age, df$Number.of.Referrals,
                 df$Avg.Monthly.GB.Download, df$Total.Revenue)
head(df2)

df2$df.Customer.Status = as.factor(
  ifelse(df2$df.Customer.Status=="Churned",1,0)
)
df2$df.Customer.Status

colnames(df2) = c("tempo", "tipo", "churn", "dependentes", "idade", 
                  "refencias", "gb", "total_R")
head(df2)

# dados descritiva
df3 = df2
df3$churn = as.factor(ifelse(df3$churn == "1", "Sim", "Não"))

#### ANALISE DESCRITIVA
#########################
cores2 = c("rosybrown3", "royalblue4")

ggplot(data = df3, aes(x = churn, fill = churn, pattern = churn)) +
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 40,
                   pattern_density = 0.1,
                   pattern_spacing = 0.03,
                   pattern_key_scale_factor = 0.6)+
  geom_label(aes(label=..count..),stat="count", 
             fontface = "bold",
             fill = "white")+
  scale_fill_manual(values = cores2) +
  scale_pattern_fill_manual(values = cores2) +
  labs(title = "Ocorrência de Churn", y = "", x = "") +
  cowplot::theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5)) 

# observacoes de churn segmentada por tipo de internet 
df_cable = df3 %>% 
  filter(df3$tipo == "Cable")

df_fibra = df3 %>% 
  filter(df3$tipo == "Fiber Optic")

df_DSL = df3 %>% 
  filter(df3$tipo == "DSL")

df_plus = df3 %>% 
  filter(df3$tipo == "")

df_plus


# Ocorrencia de churn por plano
gg_cable = ggplot(data = df_cable, 
                  aes(x = churn, fill = churn, pattern = churn)) +
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 40,
                   pattern_density = 0.1,
                   pattern_spacing = 0.03,
                   pattern_key_scale_factor = 0.6)+
  geom_label(aes(label=..count..),stat="count", 
             fontface = "bold",
             fill = "white")+
  scale_fill_manual(values = cores2) +
  scale_pattern_fill_manual(values = cores2) +
  labs(title = "Cable", y = "", x = "") +
  cowplot::theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5)) 

gg_cable

gg_fibra = ggplot(data = df_fibra, 
                  aes(x = churn, fill = churn, pattern = churn)) +
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 40,
                   pattern_density = 0.1,
                   pattern_spacing = 0.03,
                   pattern_key_scale_factor = 0.6)+
  geom_label(aes(label=..count..),stat="count", 
             fontface = "bold",
             fill = "white")+
  scale_fill_manual(values = cores2) +
  scale_pattern_fill_manual(values = cores2) +
  labs(title = "Fibra", y = "", x = "") +
  cowplot::theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5)) 

gg_fibra

gg_DSL = ggplot(data = df_DSL,
                aes(x = churn, fill = churn, pattern = churn)) +
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 40,
                   pattern_density = 0.1,
                   pattern_spacing = 0.03,
                   pattern_key_scale_factor = 0.6)+
  geom_label(aes(label=..count..),stat="count", 
             fontface = "bold",
             fill = "white")+
  scale_fill_manual(values = cores2) +
  scale_pattern_fill_manual(values = cores2) +
  labs(title = "DSL", y = "", x = "") +
  cowplot::theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5))

gg_DSL

gg_plus = ggplot(data = df_plus, 
                 aes(x = churn, fill = churn, pattern = churn)) +
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 40,
                   pattern_density = 0.1,
                   pattern_spacing = 0.03,
                   pattern_key_scale_factor = 0.6)+
  geom_label(aes(label=..count..),stat="count", 
             fontface = "bold",
             fill = "white")+
  scale_fill_manual(values = cores2) +
  scale_pattern_fill_manual(values = cores2) +
  labs(title = "Plus", y = "", x = "") +
  cowplot::theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5))

gg_plus


grid.arrange(gg_cable, gg_fibra, gg_DSL, gg_plus, ncol = 2)


########################
#### Kaplan-Meier
########################

# observacoes de churn segmentada por tipo de internet 

df2$churn = as.integer(df2$churn)

df_cable = df2 %>% 
  filter(df2$tipo == "Cable")

df_fibra = df2 %>% 
  filter(df2$tipo == "Fiber Optic")

df_DSL = df2 %>% 
  filter(df2$tipo == "DSL")

df_plus = df2 %>% 
  filter(df2$tipo == "")

df_plus

ekm = survfit(Surv(df2$tempo, df2$churn) ~ df2$tipo)
summary(ekm)

ggsurvplot(fit = ekm, data = df2,
           legend.title = "",
           legend.labs = c("Plus", "Cable", "DSL", "Fibra"),
           palette = c("orchid1","salmon", 
                       "darkturquoise", "olivedrab4"))

######################
# Modelo de mistura padrao Exponencial
######################
# cable

M2_cable = flexsurvcure(Surv(tempo, churn) ~ 1, 
                        data=df_cable,
                        link="logistic", 
                        dist = "exp" , 
                        mixture=T)
M2_cable
summary(M2_cable, t=seq(from=0,to=80,by=5),
        type="survival", tidy=T)
AIC(M2_cable)
BIC(M2_cable)

# fibra
M2_fibra = flexsurvcure(Surv(tempo, churn) ~ 1, 
                        data=df_fibra, 
                        link="logistic", 
                        dist = "exp" , 
                        mixture=T)

M2_fibra
summary(M2_fibra, t=seq(from=0,to=80,by=5),
        type="survival", tidy=T)
AIC(M2_fibra)
BIC(M2_fibra)

# DSL
M2_DSL = flexsurvcure(Surv(tempo, churn) ~ 1, 
                      data=df_DSL, 
                      link="logistic",
                      dist = "exp" , 
                      mixture=T)

M2_DSL
summary(M2_DSL, t=seq(from=0,to=80,by=5),
        type="survival", tidy=T)
AIC(M2_DSL)
BIC(M2_DSL)


# padrao
M2_plus = flexsurvcure(Surv(tempo, churn) ~ 1, 
                       data=df_plus, 
                       link="logistic",
                       dist = "exp", 
                       mixture=T)

M2_plus
summary(M2_plus, t=seq(from=0,to=80,by=5),
        type="survival", tidy=T)
AIC(M2_plus)
BIC(M2_plus)


# plot
ggsurvplot(M2_cable, data = df_cable,
           legend.title = "", 
           legend.labs = c("Cable"),
           color = "salmon",
           size = 1,
           ggtheme = theme_classic2(base_size=16, 
                                    base_family = "Arial"),
           font.family = "Arial")

ggsurvplot(M2_fibra, data = df_fibra,
           legend.title = "", 
           legend.labs = c("Fibra"),
           color = "olivedrab4")

ggsurvplot(M2_DSL, data = df_DSL,
           legend.title = "", 
           legend.labs = c("DSL"),
           color = "darkturquoise")

ggsurvplot(M2_plus, data = df_plus,
           legend.title = "", 
           legend.labs = c("Plus"),
           color = "orchid1")



######################
# Modelo com fracao de cura Weibull
######################
# cable

M3_cable = flexsurvcure(Surv(tempo, churn) ~ 1, 
                        data=df_cable,
                        link="logistic", 
                        dist = "weibull", 
                        mixture=T)
M3_cable
summary(M3_cable, t=seq(from=0,to=80,by=5), 
        type="survival", tidy=T)
AIC(M3_cable)
BIC(M3_cable)

# fibra
M3_fibra = flexsurvcure(Surv(tempo, churn) ~ 1, 
                        data=df_fibra, 
                        link="logistic", 
                        dist = "weibull" , 
                        mixture=T)

M3_fibra
summary(M3_fibra, t=seq(from=0,to=80,by=5), 
        type="survival", tidy=T)
AIC(M3_fibra)
BIC(M3_fibra)

# DSL
M3_DSL = flexsurvcure(Surv(tempo, churn) ~ 1, 
                      data=df_DSL, 
                      link="logistic", 
                      dist = "weibull" , 
                      mixture=T)

M3_DSL
summary(M3_DSL, t=seq(from=0,to=80,by=5), 
        type="survival", tidy=T)
AIC(M3_DSL)
BIC(M3_DSL)


# padrao
M3_plus = flexsurvcure(Surv(tempo, churn) ~ 1, 
                       data=df_plus, 
                       link="logistic", 
                       dist = "weibull" , 
                       mixture=T)

M3_plus
summary(M3_plus, t=seq(from=0,to=80,by=5), 
        type="survival", tidy=T)
AIC(M3_plus)
BIC(M3_plus)


# plot
ggsurvplot(M3_cable, data = df_cable,
           legend.title = "", 
           legend.labs = c("Cable"),
           color = "salmon"
           #conf.int = F,
)

ggsurvplot(M3_fibra, data = df_fibra,
           legend.title = "", 
           legend.labs = c("Fibra"),
           color = "olivedrab4")

ggsurvplot(M3_DSL, data = df_DSL,
           legend.title = "", 
           legend.labs = c("DSL"),
           color = "darkturquoise")

ggsurvplot(M3_plus, data = df_plus,
           legend.title = "", 
           legend.labs = c("Plus"),
           color = "orchid1")

######################
# Modelo com fracao de cura Gombertz
######################
# cable
M4_cable = flexsurvcure(Surv(tempo, churn) ~ 1, 
                        data=df_cable, 
                        link="logistic", 
                        dist = "gompertz" , 
                        mixture=T)
M4_cable
summary(M4_cable, t=seq(from=0,to=80,by=5),
        type="survival", tidy=T)
AIC(M4_cable)
BIC(M4_cable)

# fibra
M4_fibra = flexsurvcure(Surv(tempo, churn) ~ 1, 
                        data=df_fibra,
                        link="logistic",
                        dist = "gompertz" , 
                        mixture=T)

M4_fibra
summary(M4_fibra, t=seq(from=0,to=80,by=5), 
        type="survival", tidy=T)
AIC(M4_fibra)
BIC(M4_fibra)

# DSL
M4_DSL = flexsurvcure(Surv(tempo, churn) ~ 1, 
                      data=df_DSL, 
                      link="logistic", 
                      dist = "gompertz" , 
                      mixture=T)

M4_DSL
summary(M4_DSL, t=seq(from=0,to=80,by=5), 
        type="survival", tidy=T)
AIC(M4_DSL)
BIC(M4_DSL)


# padrao
M4_plus = flexsurvcure(Surv(tempo, churn) ~ 1, 
                       data=df_plus, 
                       link="logistic", 
                       dist = "gompertz", 
                       mixture=T)

M4_plus
summary(M4_plus, t=seq(from=0,to=80,by=5),
        type="survival", tidy=T)
AIC(M4_plus)
BIC(M4_plus)


# plot
ggsurvplot(M4_cable, data = df_cable,
           legend.title = "", 
           legend.labs = c("Cable"),
           color = "salmon"
           #conf.int = F,
)

ggsurvplot(M4_fibra, data = df_fibra,
           legend.title = "", 
           legend.labs = c("Fibra"),
           color = "olivedrab4")

ggsurvplot(M4_DSL, data = df_DSL,
           legend.title = "", 
           legend.labs = c("DSL"),
           color = "darkturquoise")

ggsurvplot(M4_plus, data = df_plus,
           legend.title = "", 
           legend.labs = c("Plus"),
           color = "orchid1")


##############################################
#######   REGRESSAO     ######################
##############################################

######################
# Modelo com fracao de cura Weibull
######################
# Considerando a variavel idade

# Cable
fit_cable = fit.cure.model(Surv(tempo, churn) ~ idade, 
                           data = df_cable, 
                           formula.surv = list(~ idade),
                           type = "mixture", 
                           dist = "weibull", 
                           link = "logit")
summary(fit_cable)
AIC(fit_cable)
AIC(M3_cable)
BIC(fit_cable)
mean(df_cable$idade)
hist(df_cable$gb)

predict(fit_cable, 
        data.frame(idade=mean(df_cable$idade)), 
        type ="curerate")

exp(1.3469636 + mean(df_cable$idade)*-0.0243441)/ 
  (1 + exp(1.3469636 + mean(df_cable$idade)*-0.0243441)) 

# Fibra
fit_fibra = fit.cure.model(Surv(tempo, churn) ~ idade, 
                           data = df_fibra, 
                           formula.surv = list(~ idade),
                           type = "mixture", 
                           dist = "exp", 
                           link = "logit")
summary(fit_fibra)
AIC(fit_fibra)
AIC(M3_fibra)
BIC(fit_fibra)
mean(df_fibra$idade)
predict(fit_fibra, 
        data.frame(idade=mean(df_fibra$idade)), 
        type ="curerate")

exp(0.1056116 + mean(df_fibra$idade)*-0.0115213)/ 
  (1 + exp(0.1056116 + mean(df_fibra$idade)*(-0.0115213))) 


# DSL
fit_DSL = fit.cure.model(Surv(tempo, churn) ~ idade, 
                         data = df_DSL, 
                         formula.surv = list(~ idade),
                         type = "mixture", 
                         dist = "weibull", 
                         link = "logit")
summary(fit_DSL)
AIC(fit_DSL)
AIC(M3_DSL)
BIC(fit_DSL)
mean(df_DSL$idade)
#hist(df_cable$gb)

predict(fit_DSL, 
        data.frame(idade=mean(df_DSL$idade)),
        type ="curerate")

exp(2.1242700 + mean(df_cable$idade)*-0.0275710)/
  (1 + exp(2.1242700 + mean(df_cable$idade)*-0.0275710)) 

# Plus
fit_plus = fit.cure.model(Surv(tempo, churn) ~ idade, 
                          data = df_plus, 
                          formula.surv = list(~ idade),
                          type = "mixture", 
                          dist = "weibull", 
                          link = "logit")
summary(fit_plus)
AIC(fit_plus)
BIC(fit_plus)
AIC(M3_plus)

mean(df_plus$idade)
#hist(df_cable$gb)

predict(fit_plus, 
        data.frame(idade=mean(df_plus$idade)),
        type ="curerate")
