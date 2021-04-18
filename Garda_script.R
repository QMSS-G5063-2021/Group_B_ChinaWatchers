library(readxl)
library(tidyverse)
library(plotly)

cwd <- getwd()
chinaInv_filepath <- file.path(cwd, "data", "ChinaInv.xlsx")

ChinaInv <- read_excel(chinaInv_filepath)
chinainv_clean <- ChinaInv %>%
  rename(Quantity = `Quantity in Millions`,
         ShareSize = `Share Size`,
         TransactionParty = `Transaction Party`)
write.csv(chinainv_clean, file = "chinainv_clean.csv", row.names = F)

InvestorYearly <- chinainv_clean %>% 
  group_by(Year, Investor) %>% 
  summarise(TotalInvestment = sum(Quantity, na.rm = T),
            .groups = "keep") 

byInvestor <- chinainv_clean %>% 
  group_by(Investor) %>% 
  summarise(TotalInvestment = sum(Quantity, na.rm=T)) %>% 
  arrange(desc(TotalInvestment))

CountryYearly <- chinainv_clean %>% 
  group_by(Year, Country) %>% 
  summarise(TotalInvestment = sum(Quantity, na.rm = T),
            .groups = "keep")

byCountry <- CountryYearly %>% 
  group_by(Country) %>% 
  summarise(TotalInvestment = sum(TotalInvestment, na.rm = T)) %>% 
  arrange(desc(TotalInvestment))

SectorYearly <- chinainv_clean %>% 
  group_by(Year, Sector) %>% 
  summarise(TotalInvestment = sum(Quantity, na.rm = T),
            .groups = "keep") 

bySector <- SectorYearly %>% 
  group_by(Sector) %>% 
  summarise(TotalInvestment = sum(TotalInvestment, na.rm = T)) %>% 
  arrange(desc(TotalInvestment))

#Visualizations

#SectorYearly
sectorViz <- SectorYearly %>% 
  ggplot(aes(x = Year, y = TotalInvestment)) +
  labs(title = "Chinese Foreign Investments by Sector (2005-2020)",
       y = "Total Investments (in millions USD)") +
  geom_line(aes(color = Sector)) +
  ggthemes::theme_tufte() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
ggplotly(sectorViz)

#Top 10 Countries, Yearly
top10countries <- byCountry %>% 
  head(10) %>% 
  select(Country) %>% 
  pull()

Top10CountryYearly <- CountryYearly %>% 
  filter(Country %in% top10countries)

countryViz <- Top10CountryYearly %>% 
  ggplot(aes(x = Year, y = TotalInvestment)) +
  geom_line(aes(color = Country)) +
  labs(title = "Chinese Foreign Investments by Top 10 Destinations (2005-2020)",
       y = "Total Investments (in millions USD)") +
  
  scale_color_brewer(palette = "Paired") +
  ggthemes::theme_tufte() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplotly(countryViz)

#Top 10 Investors, Yearly
top10Investors <- byInvestor %>% 
  head(10) %>% 
  select(Investor) %>% 
  pull()

Top10InvestorYearly <- InvestorYearly %>% 
  filter(Investor %in% top10Investors)

investorViz <- Top10InvestorYearly %>% 
  ggplot(aes(x = Year, y = TotalInvestment)) +
  geom_line(aes(color = Investor)) +
  labs(title = "Chinese Foreign Investments by Largest 10 Investors (2005-2020)",
       y = "Total Investments (in millions USD)") +
  ggthemes::theme_tufte() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplotly(investorViz)