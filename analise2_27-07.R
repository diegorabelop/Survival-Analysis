
#Análise 1. Contém todos os pacientes que não usam clozapina e, dentre aqueles que usam clozapina, somente os "saudáveis quanto ao número de neutrófilos durante um ano estão neste banco. Assim, foram excluídos indivíduos que usam clozapina e tiveram neutrófilos igual ou abaixo de 2000 nos primeiros 365 dias.

library(haven)
cloz_neutro_df <- read_sav("cloz_neutro_df.sav")
clozapina <- cloz_neutro_df

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

clozapina$days_until_first_neutropenia1000 <- as.numeric(clozapina$days_until_first_neutropenia1000)
km_fit1000 <- survfit(Surv(days_until_first_neutropenia1000, neutropenia1000) ~ CLOZAPINA_RECEITA, data=clozapina)
summary(km_fit1000)$table
library(ggplot2)
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

ggsurvplot(km_fit1000,
           pval = TRUE, conf.int = TRUE, 
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF")
)
