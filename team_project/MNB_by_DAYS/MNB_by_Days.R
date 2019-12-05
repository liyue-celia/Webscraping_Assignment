# At first, because the structure of the dateframe we want is different from that of the data table on the web page, we came up with the idea of crawling data by day to get the data format we need. But the problem is that if you want to get a year's data, you need to cycle 365 times, which takes a lot of time. It will take us about 10-15 minutes to run it once. But the advantage of this is that we can get the structure we want directly.
# Later, under the guidance of Mihaly, we transposed the data crawled by month. This allows us not to cycle too many times, and then we get better results. We put this part of the results in MNB_by_months. In this operation, we save a lot of running time and learn the application of transpose.
# Because we have learned a lot of valuable experience in these two operations, so we put both assignments here.
# Thank you very much for your help and look forward to seeing you again next semester.

library(lubridate)
library(data.table)
library(httr)
library(ggplot2)
library(dplyr)

# data will download from website https://www.mnb.hu/en
# the search result base is https://www.mnb.hu/en/arfolyam-tablazat?deviza=rbCurrencyAll&devizaSelected=EUR&datefrom=15%2F11%2F2019&datetill=15%2F11%2F2019&order=1
# we will down load 1 year data

#f irst of all, we would like to scrape the link for one day

my_mnb <- function(start_Date){
  y <- year(start_Date)
  m <- month(start_Date)
  m <- ifelse(m < 10,
              paste0('0', m),
              m)
  d <- day(start_Date)
  d <- ifelse(d < 10,
              paste0('0', d),
              d)
 

  my_base_url <- paste0('https://www.mnb.hu/en/arfolyam-tablazat?deviza=rbCurrencyAll&devizaSelected=EUR&datefrom=',
                    d,
                    '%2F',
                    m,
                    '%2F',
                    y,
                    '&datetill=',
                    d,
                    '%2F',
                    m,
                    '%2F',
                    y,
                    '&order=1',sep = "")
  # make dataframe of one page
  t <- tryCatch(read_html(my_base_url),
                error = function(cond) { return(NA) })
  
  if (!is.na(t)) {
      
      my_Date <- t %>% 
        html_nodes('.rotate:nth-child(1) span') %>% 
        html_text()
      
      title <- t %>% 
        html_nodes('tr:nth-child(1) th div') %>% 
        html_text()
      
      title1 <- t %>% 
        html_nodes('tr:nth-child(2) span') %>% 
        html_text()
      
      my_summary <- t %>% 
        html_nodes('.rotate+ td div') %>%
        html_text()
      
      
      my_unit <- t %>% 
        html_nodes('tr~ tr+ tr span') %>% 
        html_text()
      if (length(my_summary) > 0) {
          DF <- data.frame('mnb_date' = my_Date, 'ccy_name' =title, 'ccy_full_name' = title1, 'unit' = my_unit, 'mnb_rate' = my_summary)
      } else {
          DF <- data.frame('mnb_date' = NULL, 'ccy_name' = NULL, 'ccy_full_name' = NULL, 'unit' = NULL, 'mnb_rate' = NULL)
      }
  } else {
    return(NA)
  }
 return(DF)

}
# since we have oneday rates, we would like to scrape one year 


# in this case, we would like to use seq to get one year dates and based on this dates, using my_mnb function to get one year data,
rate_his <- rbindlist(lapply(seq(from       = date('2019-12-02'),
                                 by         = -1,
                                                             length.out = 365),my_mnb))



# Adjust the data format in dataframe for data analysis and plot
rate_his1 <- rate_his
  rate_his1$mnb_rate <- as.numeric(as.character(rate_his1$mnb_rate))
  rate_his1$ccy_name <- as.character(rate_his1$ccy_name)
  rate_his1$ccy_full_name <- as.character(rate_his1$ccy_full_name)
  rate_his1$unit <- as.numeric(as.character(rate_his1$unit))
  rate_his1$mnb_date <- as.Date(rate_his1$mnb_date, format = '%d %b %Y')
  
# Store adjusted dataas csv and RDs

write.table(rate_his1, 'MNB_by_Days.csv', sep = ",", dec = ".", col.names = TRUE)
saveRDS(rate_his1, file = "/Users/Celia/Desktop/webscraping/Assignment/MNB_by_Days.rds")
  
# make some plots
rate_his1 %>%  filter(rate_his1$ccy_name  %in% c('EUR','USD','GBP','CNY')) %>% 
ggplot(mapping = aes(x= mnb_date,y =mnb_rate,color= ccy_name)) +
  geom_line()+theme_bw()+
  labs(x = 'Date', y ='Rate', title = 'MNB_RATE')

rate_his1 %>%  
  ggplot(mapping = aes(y= mnb_rate,fill =mnb_rate,color = ccy_name)) +
  geom_boxplot()


rate_his1 %>%  
  ggplot() +
  geom_point(mapping = aes(x= mnb_date,y =mnb_rate,color= ccy_name))+theme_bw()+
  labs(x = 'Date', y ='Rate', title = 'MNB_RATE')

