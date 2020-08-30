library(dplyr)
covid <- tbl_df(covid)
covid <- covid %>% mutate(Date = as.Date(Date_of_report))

zuidholland <- covid %>% filter(Province == "Zuid-Holland" & Municipality_name != "") 

latest <- c()
if(weekdays(Sys.Date()) == "zaterdag") {
  latest <- Sys.Date() - 1
} else if(weekdays(Sys.Date()) == "zondag") {
  latest <- Sys.Date() - 2
} else {
  latest <- Sys.Date()
}
                  
top5 <- head(zuidholland %>% filter(Date == latest) %>% arrange(desc(Deceased)) %>%
                select(Municipality_name), 5)
top5 <- pull(top5, Municipality_name)

library(ggplot2)
library(directlabels)
ggplot(filter(zuidholland, Municipality_name %in% top5), aes(x = Date, y = Deceased, group = Municipality_name, colour = Municipality_name)) + 
  geom_line(size = 1) +
  scale_colour_discrete(guide = 'none') +
  xlim(min(zuidholland$Date), max(zuidholland$Date) + 40) +
  geom_dl(aes(label = Municipality_name), method = list("last.points"), cex = 0.8) +
  ggtitle("Cumulative amount of Covid-19 deaths", subtitle = "Top 5 Zuid-Holland Municipalities")
