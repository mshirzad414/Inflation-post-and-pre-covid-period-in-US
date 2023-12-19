rm(list = ls())
library(dplyr)
library(ggplot2)

core<- read.csv("pce1.csv")
glimpse(core)
colnames(core)[colnames(core) == "PCEPI_NBD20180101"] <- "pce"
core %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(PCE_change = ((pce - lag(pce, n = 12)) / lag(pce, n = 12)) * 100) %>% # Calculate year-over-year percent change within each quarter
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = PCE_change, color = "PCE Change"), linetype = "solid") +
  geom_hline(aes(yintercept = 2, linetype = '2% Fed Target'), color = 'red', size = 1) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(x =' Core PCE inflation vs Fed 2% inflation target' , y = 'Percentage change in core PCE', title = ' Inflation pre vs Post Covid  (Quaterly Average)') +
  theme_minimal() +
  scale_color_manual(values = c("PCE Change" = "blue"), name = NULL)+
  scale_linetype_manual(values = c("2% Fed Target" = "dashed"), name = NULL)



