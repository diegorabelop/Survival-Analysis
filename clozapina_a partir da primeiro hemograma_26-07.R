library(haven)
Clozapina_25_07 <- read_sav("Clozapina_25-07.sav")
clozapina <- Clozapina_25_07
#http://www.sthda.com/english/wiki/survival-analysis-basics
#https://www.datacamp.com/community/tutorials/survival-analysis-R
#https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(plyr)
library(survminer)
library(haven)

#clozapina1500 <- subset(cloza_1500_1999, select = c(1, 2, 3))
#clozapina1500 <- na.omit(clozapina1500)

clozapina$CLOZAPINA_RECEITA <- revalue(clozapina$CLOZAPINA_RECEITA, c(Yes = "TRUE", No = "FALSE"))
clozapina$CLOZAPINA_RECEITA <- as.logical(clozapina$CLOZAPINA_RECEITA)

clozapina$neutropenia1000 <- revalue(clozapina$neutropenia1000, c(Yes = "TRUE", No = "FALSE"))
clozapina$neutropenia1000 <- as.logical(clozapina$neutropenia1000)

clozapina$neutropenia1500 <- revalue(clozapina$neutropenia1500, c(Yes = "TRUE", No = "FALSE"))
clozapina$neutropenia1500 <- as.logical(clozapina$neutropenia1500)

clozapina$neutropenia2000 <- revalue(clozapina$neutropenia2000, c(Yes = "TRUE", No = "FALSE"))
clozapina$neutropenia2000 <- as.logical(clozapina$neutropenia2000)






clozapina$days_until_first_neutropenia1000 <- as.numeric(clozapina$days_until_first_neutropenia1000)
clozapina$days_until_first_neutropenia1500 <- as.numeric(clozapina$days_until_first_neutropenia1500)
clozapina$days_until_first_neutropenia2000 <- as.numeric(clozapina$days_until_first_neutropenia2000)


#clozapina$neutropenia1500_after_one_year <- revalue(clozapina$neutropenia1500_after_one_year, c("Yes" = "1", "No" = "0"))

#clozapina$neutropenia1500_after_one_year <- as.numeric(clozapina$neutropenia1500_after_one_year)

#clozapina$neutropenia1000_after_one_year <- revalue(clozapina$neutropenia1000_after_one_year, c("Yes" = "1", "No" = "0"))

#clozapina$neutropenia1000_after_one_year <- as.numeric(clozapina$neutropenia1000_after_one_year)

#df$neutropenia_1000 <- revalue(df$neutropenia1000, c(No = "FALSE", Yes = "TRUE"))
#df$neutropenia_1000 <- as.logical(df$neutropenia_1000)
#####1000####
km_fit1000 <- survfit(Surv(days_until_first_neutropenia1000, neutropenia1000) ~ CLOZAPINA_RECEITA, data=clozapina)
summary(km_fit1000)$table
autoplot(km_fit1000)
surv_summary(km_fit1000) #tabela 
head(km_fit1000)
#Log-rank test
surv_diff1000 <- survdiff(Surv(days_until_first_neutropenia1000, neutropenia1000) ~ CLOZAPINA_RECEITA, data = clozapina)
surv_diff1000

library(survival)
library(survminer)
sobrevida_1000 <- ggsurvplot(km_fit1000,
                             pval = FALSE, conf.int = TRUE, 
                             risk.table = FALSE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             ggtheme = theme_classic(), # Change ggplot2 theme
                             palette = c("midnightblue","red4"),
                             ylab = "Survival Probability (%)",
                             xlab = "Time (days)",
                             fun = "pct"
                             
)
sobrevida_1000


# cox regression 
library(plyr)
clozapina$ETNIA <- revalue(clozapina$ETNIA, c(BRANCA = "White", PRETA = "Non-white", PARDA = "Non-white", INDÍGENA = "Non-white", AMARELA = "Non-white"))
clozapina$ETNIA <- as.factor(clozapina$ETNIA)
clozapina$SEXO <- as.factor(clozapina$SEXO)


clozapina$CLOZAPINA_RECEITA_nova <- revalue(clozapina$CLOZAPINA_RECEITA, c(Yes = "TRUE", No = "FALSE"))
clozapina$CLOZAPINA_RECEITA <- as.logical(clozapina$CLOZAPINA_RECEITA)
cox_1000 <- coxph(Surv(days_until_first_neutropenia1000, neutropenia1000) ~  SEXO1 + IDADE_ATEND + Etnia1 + Clozapina_receita1, 
                  data = clozapina) #vejo a hazard ratio em exp(-coef)
cox_1000
summary(cox_1000)

#ggforest(cox_1000, data = clozapina)

#### 1500 ####
km_fit1500 <- survfit(Surv(days_until_first_neutropenia1500, neutropenia1500) ~ CLOZAPINA_RECEITA, data=clozapina)
summary(km_fit1500)$table
autoplot(km_fit1500)
surv_summary(km_fit1500) #tabela 
head(km_fit1500)
#Log-rank test
surv_diff1500 <- survdiff(Surv(days_until_first_neutropenia1500, neutropenia1500) ~ CLOZAPINA_RECEITA, data = clozapina)
surv_diff1500

library(survival)
library(survminer)
sobrevida_1500 <- ggsurvplot(km_fit1500,
                             pval = FALSE, conf.int = TRUE, 
                             risk.table = FALSE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             ggtheme = theme_classic(), # Change ggplot2 theme
                             palette = c("midnightblue","red4"),
                             ylab = "Survival Probability (%)",
                             xlab = "Time (days)",
                             fun = "pct"
                             
)
sobrevida_1500

#cox regression
cox_1500 <- coxph(Surv(days_until_first_neutropenia1500, neutropenia1500) ~  SEXO1 + IDADE_ATEND + Etnia1 + Clozapina_receita1, 
                  data = clozapina) #vejo a hazard ratio em exp(-coef)
cox_1500
summary(cox_1500)

######20000#####
km_fit2000 <- survfit(Surv(days_until_first_neutropenia2000, neutropenia2000) ~ CLOZAPINA_RECEITA, data=clozapina)
summary(km_fit2000)$table
autoplot(km_fit2000)
surv_summary(km_fit2000) #tabela 
head(km_fit2000)
#Log-rank test
surv_diff2000 <- survdiff(Surv(days_until_first_neutropenia2000, neutropenia2000) ~ CLOZAPINA_RECEITA, data = clozapina)
surv_diff2000

library(survival)
library(survminer)
sobrevida_2000 <- ggsurvplot(km_fit2000,
                             pval = FALSE, conf.int = TRUE, 
                             risk.table = FALSE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             ggtheme = theme_classic(), # Change ggplot2 theme
                             palette = c("midnightblue","red4"),
                             ylab = "Survival Probability (%)",
                             xlab = "Time (days)",
                             fun = "pct"
                             
)
sobrevida_2000

# cox regression
cox_2000 <- coxph(Surv(days_until_first_neutropenia2000, neutropenia2000) ~  SEXO1 + IDADE_ATEND + Etnia1 + Clozapina_receita1, 
                  data = clozapina) #vejo a hazard ratio em exp(-coef)
cox_2000
summary(cox_2000)






library(survival)
library(survminer)
ggsurvplot(km_fit1500,
           pval = TRUE, conf.int = TRUE, 
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF")
)


ggsurvplot(km_fit1000,
           pval = TRUE, conf.int = TRUE, 
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF")
)

ggsurvplot(km_fit2000,
           pval = TRUE, conf.int = TRUE, 
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF")
)


ggsurvplot(km_fit1500,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))
ggsurvplot(km_fit,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           fun = "event")

ggsurvplot(km_fit1500,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           fun = "event")


ggsurvplot(km_fit,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
)

ggsurvplot(km_fit2000,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
)
ggsurvplot(km_fit1500,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           fun = "cumhaz")



library(RColorBrewer)
library(wesanderson)
sobrevida_2000 <- ggsurvplot(km_fit2000,
                             pval = FALSE, conf.int = TRUE, 
                             risk.table = FALSE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             ggtheme = theme_classic(), # Change ggplot2 theme
                             palette = c("midnightblue","red4"),
                             ylab = "Survival Probability (%)",
                             xlab = "Time (days)",
                             fun = "pct"
                             
)

sobrevida_2000
sobrevida_1500 <- ggsurvplot(km_fit1500,
                             pval = TRUE, conf.int = TRUE, 
                             risk.table = FALSE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             ggtheme = theme_classic(), # Change ggplot2 theme
                             palette = c("midnightblue","red4"),
                             ylab = "Survival Probability (%)",
                             xlab = "Time (days)",
                             fun = "pct"
                             
)
sobrevida_1500

sobrevida_1000 <- ggsurvplot(km_fit1000,
                             pval = FALSE, conf.int = TRUE, 
                             risk.table = FALSE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             ggtheme = theme_classic(), # Change ggplot2 theme
                             palette = c("midnightblue","red4"),
                             ylab = "Survival Probability (%)",
                             xlab = "Time (days)",
                             fun = "pct"
                             
)


##### Cox Regression 
#explicação: http://www.sthda.com/english/wiki/cox-proportional-hazards-model#:~:text=(%22survminer%22)-,R%20function%20to%20compute%20the%20Cox%20model%3A%20coxph(),hazards%20regression%20model%20in%20R.&text=formula%3A%20is%20linear%20model%20with,Surv(time%2C%20event). 

library(plyr)
clozapina1500$ETNIA <- revalue(clozapina1500$ETNIA, c(BRANCA = "White", PRETA = "Non-white", PARDA = "Non-white", INDÍGENA = "Non-white", AMARELA = "Non-white"))
clozapina1500$ETNIA <- as.factor(clozapina1500$ETNIA)

fit.1500 <- coxph(Surv(days_until_neutrofilos1500and1999, neutropenia1500_1999) ~  SEXO + IDADE_ATUAL + ETNIA + CLOZAPINA_RECEITA, 
                  data = clozapina1500)
fit.1500
summary(fit.1500)

ggforest(fit.1500, data = clozapina1500)

clozapina$ETNIA <- revalue(clozapina$ETNIA, c(BRANCA = "White", PRETA = "Non-white", PARDA = "Non-white", INDÍGENA = "Non-white", AMARELA = "Non-white"))
clozapina$ETNIA <- as.factor(clozapina$ETNIA)


clozapina$SEXO <- as.factor(clozapina$SEXO)
clozapina$neutropenia_2000 <- as.factor(clozapina$neutropenia_2000)
clozapina$days_until_healthy2000 <- as.numeric(clozapina$days_until_healthy2000)
fit.2000 <- coxph(Surv(days_until_healthy2000, neutropenia_2000) ~ SEXO + IDADE_ATUAL + ETNIA + CLOZAPINA_RECEITA, 
                  data = clozapina)
fit.2000
library(ggplot2)
library(survminer)
library(survival)

library("survival")
library("broom")
library("ggplot2")
library("dplyr")

#extraindo valores
covariates <- c("SEXO", "IDADE_ATUAL",  "ETNIA")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(days_until_neutrofilos1500and1999, neutropenia1500_1999)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = clozapina1500)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=4)
                         wald.test<-signif(x$wald["test"], digits=4)
                         beta<-signif(x$coef[1], digits=4);#coeficient beta
                         HR <-signif(x$coef[2], digits=4);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 4)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],4)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

summary(fit.1500)
