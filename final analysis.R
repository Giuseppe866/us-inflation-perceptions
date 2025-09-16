deltammedian <- -(mfyeduc$px1_med_m - median_CPI_annual_rate$Median)
deltafmedian <- -(mfyeduc$px1_med_f - median_CPI_annual_rate$Median)

deltay13median <- -(mfyeduc$px1_med_y13 - median_CPI_annual_rate$Median)
deltay33median <- -(mfyeduc$px1_med_y33 - median_CPI_annual_rate$Median)

deltaehsmedian <- -(mfyeduc$px1_med_ehs - median_CPI_annual_rate$Median)
deltaecdmedian <- -(mfyeduc$px1_med_ecd - median_CPI_annual_rate$Median)

plot(mismatch2, type='l')
lines(deltafmedian, col='pink',lwd=3)
lines(deltammedian, col='blue',lwd=3)

lm5 <- lm(mismatch2 ~ deltammedian + deltafmedian)
summary(lm5)


lm6 <- lm(mismatch2 ~ deltay13median + deltay33median)
summary(lm6)
plot(mismatch2, type='l',ylim=c(-4,5))
lines(deltay13median, col='pink',lwd=3)
lines(deltay33median, col='blue',lwd=3)

lm7 <- lm(mismatch2 ~ deltaehsmedian + deltaecdmedian)
summary(lm7)

plot(mismatch2, type='l',ylim=c(-4,5))
lines(deltaehsmedian, col='pink',lwd=3)
lines(deltaecdmedian, col='blue',lwd=3)





