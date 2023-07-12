# Prac3
# read and summary
library(bio2020)
pea_dat<-read.csv("Data/Peas.csv")
summary(pea_dat)
mean(pea_dat) # error
str(pea_dat)
head(pea_dat)
library(tidyr)

# Change longer
pea_dat_long <- pivot_longer(
data=pea_dat, cols=NitroGrow:Control,
names_to="Treatment",
values_to="Yield")
mean(Yield~NULL,data=pea_dat_long)
gf_boxplot(Yield~Treatment,data=pea_dat_long)%>%
gf_labs(x="treat",y="yield")
pea_dat_long$Block <- as.factor(pea_dat_long$Block)
pea_dat_long$Treatment <- as.factor(pea_dat_long$Treatment)
summary(pea_dat_long)

#Analyse
pea_dat_lm1 <- lm(Yield ~ Treatment, data=pea_dat_long)
anova(pea_dat_lm1)
pea_dat_lm2 <- lm(Yield ~ Treatment + Block, data=pea_dat_long)
anova(pea_dat_lm2)
TukeyHSD(pea_dat_lm2, which="Treatment")
plot(TukeyHSD(pea_dat_lm2, which="Treatment"))
