mfyeduc <- read_csv("tesi/DATI tesi/mfyeduc.csv", 
                    comment = "\"")
View(mfyeduc)

start_date <- as.Date("2016-01-01")  
end_date <- as.Date("2024-02-01")    
dates <- seq(start_date, end_date, by = "month")
mfyeduc <- mutate(mfyeduc, dates)
view(mfyeduc)
view(median_CPI_annual_rate)
view(expected_change_in_prices)

mfyeduc <- mfyeduc %>% slice(-1)

deltamf <- mfyeduc$px1_med_m-mfyeduc$px1_med_f
deltafm <- mfyeduc$px1_med_f-mfyeduc$px1_med_m

ggplot()+
  geom_line(aes(x=median_CPI_annual_rate$DATE,y=median_CPI_annual_rate$Median, colour="realized"))+
  geom_line(aes(x=expected_change_in_prices$dates,y=expected_change_in_prices$Median, colour="expectations"))+
  geom_line(aes(x=mfyeduc$dates,y=mfyeduc$px1_med_m, colour="med m"))+
  geom_line(aes(x=mfyeduc$dates,y=mfyeduc$px1_med_f, colour="med f"))+
  xlab("date")+
  ylab("values") 


ggplot()+
  geom_line(aes(x=mfyeduc$dates,y=mismatch2, colour="mismatch2"))+
  geom_line(aes(x=mfyeduc$dates,y=deltamf, colour="delta F M"))+
  xlab("date")+
  ylab("values") 

deltay33y13 <- mfyeduc$px1_med_y33-mfyeduc$px1_med_y13

ggplot()+
  geom_line(aes(x=mfyeduc$dates,y=mfyeduc$px1_med_y13, colour="Bottom 33"))+
  geom_line(aes(x=mfyeduc$dates,y=mfyeduc$px1_med_y33, colour="Top 33"))+
  xlab("date")+
  ylab("values") 

ggplot()+
  geom_line(aes(x=mfyeduc$dates,y=mfyeduc$px1_med_ehs, colour="High school degree"))+
  geom_line(aes(x=mfyeduc$dates,y=mfyeduc$px1_med_ecd, colour="Graduate degree"))+
  xlab("date")+
  ylab("values") 

deltaecdehs <- mfyeduc$px1_med_ecd-mfyeduc$px1_med_ehs



lm4 <- lm(mismatch2 ~ deltay33y13)
summary(lm4)

checkresiduals(lm4)

lagmismatch21 <- lag(mismatch2,1)
lagmismatch22 <- lag(mismatch2,2)
lagmismatch23 <- lag(mismatch2,3)

lm5 <- lm(mismatch2 ~ lagmismatch21 + lagmismatch22 + lagmismatch23 + deltay33y13)
summary(lm5)

checkresiduals(lm5)

