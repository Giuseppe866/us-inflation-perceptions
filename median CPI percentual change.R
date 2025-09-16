
median_CPI_percentual_change <- read_csv("DATI tesi/median CPI percentual change.csv", 
                                         comment = "\"")


names(median_CPI_percentual_change)[names(median_CPI_percentual_change) == "MEDCPIM158SFRBCLE_CHG"] <- "% change"

view(median_CPI_percentual_change)

expected_change_in_prices <- read_csv("DATI tesi/expected change in prices.csv", 
                                      comment = "\"")

start_date <- as.Date("2016-01-01")  
end_date <- as.Date("2024-02-01")    
dates <- seq(start_date, end_date, by = "month")
expected_change_in_prices <- mutate(expected_change_in_prices, dates)
view(expected_change_in_prices)

expected_change_in_prices <- expected_change_in_prices %>% select(-Month, -Year,
                                                                  -Down,- Same, -starts_with('Up'),
                                                                  -starts_with('DK'),- 'NA')
                                                                  
expected_change_in_prices <- expected_change_in_prices %>% select(-`75th Percentile`)
expected_change_in_prices <- expected_change_in_prices %>% select(- `25th Percentile`,
                                                                  - Mean,- Variance, - `Standard Deviation`)
expected_change_in_prices <- expected_change_in_prices %>% select(-`Interquartile Range (75th-25th)`)
expected_change_in_prices
lag1 <- lag(expected_change_in_prices$Median,1)
xx <- ((expected_change_in_prices$Median-lag1)/lag1)*12

expected_change_in_prices <- expected_change_in_prices %>% select(- percentual_change_expected, -'% change expected', - x )
expected_change_in_prices
expected_change_in_prices <- expected_change_in_prices %>%
  mutate(percentual_change_expected = xx)
expected_change_in_prices <- expected_change_in_prices %>% select(-xx)


expected_change_in_prices

plot(median_CPI_percentual_change$`% change`, type="l")
plot(expected_change_in_prices$percentual_change_expected, type="l")

percentage_change_realized <-median_CPI_percentual_change$`% change`/100
median_CPI_percentual_change <- median_CPI_percentual_change %>%
  mutate(percentual_change_realized = percentage_change_realized)
median_CPI_percentual_change


median_CPI_percentual_change <- median_CPI_percentual_change %>% slice(-1)
median_CPI_percentual_change

expected_change_in_prices <- expected_change_in_prices %>% slice(-1)
expected_change_in_prices

view(median_CPI_percentual_change)
view(expected_change_in_prices)

median_CPI_percentual_change <- median_CPI_percentual_change %>% slice(1:n()-1)
median_CPI_percentual_change

mismatch <- median_CPI_percentual_change$`% change`- expected_change_in_prices$percentual_change_expected
mismatch

expected_change_in_prices

ggplot()+
  geom_line(aes(x=median_CPI_percentual_change$DATE,y=median_CPI_percentual_change$`% change`, colour="realized"))+
  geom_line(aes(x=expected_change_in_prices$dates,y=expected_change_in_prices$percentual_change_expected, colour="expectations"))+
  xlab("date")+
  ylab("values") 

plot(mismatch, type="l")

x <- lag(median_CPI_percentual_change$`% change`,1)

expected_change_in_prices <- 
  expected_change_in_prices %>% 
  mutate(
    Jan = ifelse(month(dates)==1,1,0),
    Feb = ifelse(month(dates)==2,1,0),
    Mar = ifelse(month(dates)==3,1,0),
    Apr = ifelse(month(dates)==4,1,0),
    May = ifelse(month(dates)==5,1,0),
    Jun = ifelse(month(dates)==6,1,0),
    Jul = ifelse(month(dates)==7,1,0),
    Aug = ifelse(month(dates)==8,1,0),
    Sep = ifelse(month(dates)==9,1,0),
    Oct = ifelse(month(dates)==10,1,0),
    Nov = ifelse(month(dates)==11,1,0)
  )

expected_change_in_prices

pacf(median_CPI_percentual_change$`% change`)


lm1 <- lm(median_CPI_percentual_change$`% change`~ x + expected_change_in_prices$percentual_change_expected + expected_change_in_prices$Jan
          +expected_change_in_prices$Feb
          +expected_change_in_prices$Mar
          +expected_change_in_prices$Apr
          +expected_change_in_prices$May
          +expected_change_in_prices$Jun
          +expected_change_in_prices$Jul
          +expected_change_in_prices$Aug
          +expected_change_in_prices$Sep
          +expected_change_in_prices$Oct
          +expected_change_in_prices$Nov)
summary(lm1)


pacf(expected_change_in_prices$percentual_change_expected)

