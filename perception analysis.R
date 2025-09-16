perceptionsq_demography <- read_csv("tesi/DATI tesi/perceptionsq demography.csv")
view(perceptionsq_demography)

names(perceptionsq_demography)[names(perceptionsq_demography) == "...1"] <- "Data"


perception_median_quarter <- read_csv("tesi/DATI tesi/annual perception median quarter.csv")

median_CPI_quarter <- read_csv("tesi/DATI tesi/median CPI quarter.csv")
view(median_CPI_quarter)
view(perception_median_quarter)

median_CPI_quarter <- median_CPI_quarter %>% slice(1:n()-1)

mismatchq <- annual_perception_median_quarter$Median - median_CPI_quarter$Median
plot(mismatchq, type="l")


deltapercm <- perceptionsq_demography$Male - median_CPI_quarter$Median
deltapercf <- perceptionsq_demography$Female - median_CPI_quarter$Median

plot(mismatchq, type='l',ylim=c(-4,5))
lines(deltapercf, col='pink',lwd=3)
lines(deltapercm, col='blue',lwd=3)


lmqm <- lm(mismatchq  ~ deltapercf)
lmqf <- lm(mismatchq  ~ deltapercm)

summary(lmqm)
summary(lmqf)

deltaperctop <- perceptionsq_demography$Top - median_CPI_quarter$Median
deltapercbottom <- perceptionsq_demography$Bottom- median_CPI_quarter$Median

plot(mismatchq, type='l',ylim=c(-4,5))
lines(deltaperctop, col='pink',lwd=3)
lines(deltapercbottom, col='blue',lwd=3)


lmqbottom <-lm(mismatchq  ~ deltapercbottom )
lmqtop <- lm(mismatchq  ~ deltaperctop)
summary(lmqbottom)
summary(lmqtop)
















