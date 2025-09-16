median_CPI_annual_rate <- read_csv("DATI tesi/median CPI annual rate.csv", 
                                   comment = "\"")
median_CPI_annual_rate <- median_CPI_annual_rate %>% slice(-1)

names(median_CPI_annual_rate)[names(median_CPI_annual_rate) == "MEDCPIM158SFRBCLE"] <- "Median"

median_CPI_annual_rate <- median_CPI_annual_rate %>% slice(1:n()-1)

ggplot()+
  geom_line(aes(x=median_CPI_annual_rate$DATE,y=median_CPI_annual_rate$Median, colour="realized"))+
  geom_line(aes(x=expected_change_in_prices$dates,y=expected_change_in_prices$Median, colour="expectations"))+
  xlab("date")+
  ylab("values") 

median_CPI_annual_rate <- mutate(median_CPI_annual_rate, ID=row_number())
median_CPI_annual_rate

ID2 <- median_CPI_annual_rate$ID^2

median_CPI_annual_rate <- median_CPI_annual_rate %>%
  mutate(ID2 = ID2)

lag <- lag(median_CPI_annual_rate$Median,1)

expected_change_in_prices <- mutate(expected_change_in_prices, ID=row_number())

lm2 <- lm(median_CPI_annual_rate$Median ~ lag + median_CPI_annual_rate$ID + median_CPI_annual_rate$ID2 + expected_change_in_prices$Median)

summary(lm2)





