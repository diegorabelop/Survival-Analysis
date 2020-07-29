library(haven)
clozapina_1500_1999_26_07 <- read_sav("clozapina/graficos/clozapina_1500_1999_26-07.sav")
clozapina <- clozapina_1500_1999_26_07
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

#clozapina$neutropenia1500_1999 <- as.factor(clozapina$neutropenia1500_1999)
#clozapina$neutropenia1500_1999 <- revalue(clozapina$neutropenia1500_1999, c(1 = "TRUE", 0 = "FALSE"))
#clozapina$neutropenia1500_1999 <- as.logical(clozapina$neutropenia1500_1999)
clozapina$neutropenia_1500_1999_logical <- as.logical(clozapina$neutropenia_1500_1999_logical)
clozapina$days_until_neutrofilos1500and1999 <- as.numeric(clozapina$days_until_neutrofilos1500and1999)
km_fit1500_1999 <- survfit(Surv(days_until_neutrofilos1500and1999, neutropenia_1500_1999_logical) ~ CLOZAPINA_RECEITA, data=clozapina)
summary(km_fit1500_1999)$table
autoplot(km_fit1500_1999)
surv_summary(km_fit1500_1999) #tabela 
head(km_fit1500_1999)
#Log-rank test
surv_diff1500_1999 <- survdiff(Surv(days_until_neutrofilos1500and1999, neutropenia_1500_1999_logical) ~ CLOZAPINA_RECEITA, data = clozapina)
surv_diff1500_1999
library(RColorBrewer)
library(wesanderson)
sobrevida_1500_1999 <- ggsurvplot(km_fit1500_1999,
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
sobrevida_1500_1999
