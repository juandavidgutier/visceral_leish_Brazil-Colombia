library(dagitty)
library(lavaan)
library(CondIndTests)
library(dplyr)
library(GGally)
library(na.tools)
library(MASS)
library(ggplot2)
library(ggeffects)
library(forestplot)


data_final <- read.csv("D:/clases/UDES/articulo visceral/reviewers/data_visceral.csv")
data_final <- data_final %>% na.rm(data_final)
str(data_final)

data_nozero <- data_final

data_nozero$LEBS_by_month10k <- data_nozero$LEBS_by_month10k + 0.001
data_nozero$Episode <- as.factor(data_nozero$Episode)
data_nozero$Biome <- as.factor(data_nozero$Biome)
data_nozero <- as.data.frame(data_nozero)

#regression model
model_reg <- glm(LEBS_by_month10k ~ Suitability + Episode + Biome, maxit = 10000, 
                 data = data_nozero,  family = Gamma(link = "log"))
summary(model_reg) 

#association values
suitability_asso <- as.numeric(exp(model_reg$coefficients))
print(suitability_asso)

#confidence intervals
suitability_interval <- exp(confint.default(model_reg))
print(suitability_interval)



#plot suitability
mydf <- ggpredict(model_reg, terms = "Suitability [all]")
p <- ggplot(mydf, aes(x, predicted)) +
  geom_line(size=1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)
p <- p + ggtitle("")
p <- p + labs(x = expression(paste("% Area suitable")))
p <- p + labs(y = expression(paste("Incidence per month of VL (LEBS)")))
p <- p +   theme (plot.title = element_text(size=rel(2), #Tamaño relativo de la letra del t?tulo
                                            vjust=2, #Justificaci?n vertical, para separarlo del gr?fico
                                            hjust = 0.5,
                                            face="bold"))
p <- p + theme(axis.title.y = element_text(size = rel(2.5), angle = 90))
p <- p + theme(axis.title.x = element_text(size = rel(2.5), angle = 00))
p <- p + theme(axis.text.x = element_text(size=20))
p <- p + theme(axis.text.y = element_text(size=20))
p


#plot episode
mydf1 <- ggpredict(model_reg, terms = "Episode [all]")
p <- ggplot(mydf1, aes(x, predicted)) +
  geom_point(position = position_dodge(.1)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(.1)
  )
p <- p + ggtitle("")
p <- p + labs(x = expression(paste("Episode")))
p <- p + labs(y = expression(paste("Incidence per month of VL (LEBS)")))
p <- p +   theme (plot.title = element_text(size=rel(2), #Tamaño relativo de la letra del t?tulo
                                            vjust=2, #Justificaci?n vertical, para separarlo del gr?fico
                                            hjust = 0.5,
                                            face="bold"))
p <- p + theme(axis.title.y = element_text(size = rel(2.5), angle = 90))
p <- p + theme(axis.title.x = element_text(size = rel(2.5), angle = 00))
p <- p + theme(axis.text.x = element_text(size=20))
p <- p + theme(axis.text.y = element_text(size=20))
p



#plot biome
mydf2 <- ggpredict(model_reg, terms = "Biome [all]")
p <- ggplot(mydf2, aes(x, predicted)) +
  geom_point(position = position_dodge(.1)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(.1)
  )
p <- p + ggtitle("")
p <- p + labs(x = expression(paste("Biome")))
p <- p + labs(y = expression(paste("Incidence per month of VL (LEBS)")))
p <- p +   theme (plot.title = element_text(size=rel(2), #Tamaño relativo de la letra del t?tulo
                                            vjust=2, #Justificaci?n vertical, para separarlo del gr?fico
                                            hjust = 0.5,
                                            face="bold"))
p <- p + theme(axis.title.y = element_text(size = rel(2.5), angle = 90))
p <- p + theme(axis.title.x = element_text(size = rel(2.5), angle = 00))
p <- p + theme(axis.text.x = element_text(size=20))
p <- p + theme(axis.text.y = element_text(size=20))
p










