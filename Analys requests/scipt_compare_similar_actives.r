##сейчас сравниваем активы в индексе и не в индексе похожие
##критерий похожести - бета и рыночкая капитализация


##отбираем активы, которые какое-то время  были вне индекса (от 20 до 80 наблюдений), а потом были в индексе (от 20 до 80 наблюдений)

##были в индексе больше от 20 до 80 наблюдений
sp500_sometimes_in_index <- Holdings_1 %>% filter(ETF.Ticker == 'SPY') %>% group_by(Iid, GVKey) %>% tally() %>% filter(n >= 20 & n <= 80)  %>% filter(!is.na(GVKey))

sp500_sometimes_in_index %<>% inner_join(Returns, by = c('GVKey', 'Iid')) 
colnames(sp500_sometimes_in_index)[4] <- 'Date'
sp500_sometimes_in_index$Date <- as.Date(sp500_sometimes_in_index$Date)
Holdings_1$Date <- as.Date(Holdings_1$Date)
sp500_sometimes_in_index %<>% left_join(Holdings_1 %>% filter(ETF.Ticker == 'SPY') %>% select(-ETF.Ticker), by = c('GVKey', 'Iid', 'Date'))
sp500_sometimes_in_index %<>% filter(as.Date(Date) >= '2010-01-31')
sp500_sometimes_in_index$in_index <- NA
sp500_sometimes_in_index$in_index[which(sp500_sometimes_in_index$new == 1)] <- 1
sp500_sometimes_in_index$in_index[which(sp500_sometimes_in_index$exclude == 1)] <- 0
for(i in c(1:nrow(sp500_sometimes_in_index))){
  if(i == 1){
    sp500_sometimes_in_index$in_index[i] <- ifelse(sp500_sometimes_in_index$GVKey[i] %in% Holdings$GVKey[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')] &
                                                     sp500_sometimes_in_index$Iid[i] %in% Holdings$Iid[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')], 1, 0)
  }
  if(i > 1){
    if(sp500_sometimes_in_index$GVKey[i] != sp500_sometimes_in_index$GVKey[i-1]){
      sp500_sometimes_in_index$in_index[i] <- ifelse(sp500_sometimes_in_index$GVKey[i] %in% Holdings$GVKey[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')] &
                                                       sp500_sometimes_in_index$Iid[i] %in% Holdings$Iid[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')], 1, 0)
    }
  }
  if(is.na(sp500_sometimes_in_index$in_index[i])){
    sp500_sometimes_in_index$in_index[i] <- sp500_sometimes_in_index$in_index[i-1]
  }
}
rm(i)

sp500_sometimes_in_index %<>% filter(as.Date(Date) <= data[length(data)])
sp500_sometimes_in_index %<>% select(-n)
sp500_sometimes_in_index %<>% inner_join(sp500_sometimes_in_index %>% group_by(GVKey, Iid) %>% arrange(as.Date(Date)) %>% mutate(n = c(1:n())) %>% 
                                           filter(n == 1 & in_index == 0) %>% select(GVKey, Iid) %>% unique(), by = c('GVKey', 'Iid'))
sp500_sometimes_in_index %<>% group_by(GVKey, Iid) %>% mutate(n_in_index = n_distinct(Date[which(in_index == 1)]),
                                                              n_out_index = n_distinct(Date[which(in_index == 0)])) %>% ungroup() %>%
  filter(n_in_index >= 20 & n_in_index <= 80 & n_out_index >= 20 & n_out_index <= 80) %>% select(-n_in_index, -n_out_index)

##отбираем активы, которые не были в sp500
no_sp500 <- Returns %>% anti_join(Holdings_1 %>% filter(ETF.Ticker == 'SPY'), by = c('GVKey', 'Iid')) %>% filter(DataDate >= data[1] & DataDate <= data[length(data)])
colnames(no_sp500)[3] <- 'Date'
no_sp500$Date <- as.Date(no_sp500$Date)
sp500_sometimes_in_index_1 <- inner_join(sp500_sometimes_in_index, no_sp500, by = c('Date')) %>%
  filter(in_index == 0 & MkVal.y >= 0.8*MkVal.x & MkVal.y <= 1.2*MkVal.x & Beta3Yr.y >= 0.8*Beta3Yr.x & Beta3Yr.y <= 1.2*Beta3Yr.x)
sp500_sometimes_in_index_1 %<>% select(GVKey.x, Iid.x, in_index, GVKey.y, Iid.y) %>% unique()
colnames(sp500_sometimes_in_index_1)[c(1,2)] <- c('GVKey', 'Iid')
sp500_sometimes_in_index_1 %<>% inner_join(sp500_sometimes_in_index %>% select(GVKey, Iid, Date, in_index), by = c('GVKey', 'Iid'))
colnames(Returns)[3] <- 'Date'
sp500_sometimes_in_index_1 %<>% inner_join(Returns %>% select(-EarningsYield), by = c('GVKey', 'Iid', 'Date'))
sp500_sometimes_in_index_1 %<>% filter(as.Date(Date) >= data[1] & as.Date(Date) <= data[length(data)])
colnames(sp500_sometimes_in_index_1)[c(1,2)] <- c('GVKey_main', 'Iid_main')
colnames(sp500_sometimes_in_index_1)[c(4,5)] <- c('GVKey', 'Iid')
sp500_sometimes_in_index_1 %<>% select(-in_index.x) %>% unique()
colnames(sp500_sometimes_in_index_1)[c(6)] <- c('in_index')
sp500_sometimes_in_index_1 %<>% inner_join(Returns %>% select(-EarningsYield), by = c('GVKey', 'Iid', 'Date'))
sp500_sometimes_in_index_1 %<>% group_by(GVKey_main, Iid_main, GVKey, Iid) %>% mutate(all = n_distinct(Date),
                                                                                      out_index = n_distinct(Date[which(in_index == 0)])) %>% ungroup()
sp500_sometimes_in_index_1$equal_beta <- ifelse(sp500_sometimes_in_index_1$Beta3Yr.y >= 0.8*sp500_sometimes_in_index_1$Beta3Yr.x & 
                                                  sp500_sometimes_in_index_1$Beta3Yr.y <= 1.2*sp500_sometimes_in_index_1$Beta3Yr.x, 1, 0)
sp500_sometimes_in_index_1$equal_beta[which(sp500_sometimes_in_index_1$in_index == 1)] <- NA
sp500_sometimes_in_index_1$equal_mkval <- ifelse(sp500_sometimes_in_index_1$MkVal.y >= 0.8*sp500_sometimes_in_index_1$MkVal.x & 
                                                  sp500_sometimes_in_index_1$MkVal.y <= 1.2*sp500_sometimes_in_index_1$MkVal.x, 1, 0)
sp500_sometimes_in_index_1$equal_mkval[which(sp500_sometimes_in_index_1$in_index == 1)] <- NA
sp500_sometimes_in_index_1 %<>% group_by(GVKey_main, Iid_main, GVKey, Iid) %>% mutate(n_equal_beta = n_distinct(Date[which(equal_beta == 1)]),
                                                                                      n_equak_mkval = n_distinct(Date[which(equal_mkval == 1)]))
sp500_sometimes_in_index_1 %<>% filter(out_index >= 20)
sp500_sometimes_in_index_1 %<>% filter(0.4*out_index <= n_equal_beta & 0.4*out_index<=n_equak_mkval)
group_sp500_sometimes_in_index_1 <- data.frame(type = c('Активы в индексе до индекса', 'Активы в индексе после индекса', 'Активы вне индекса до индекса', 'Активы вне индекса после индекса'),
                                               return_mean = c(mean(sp500_sometimes_in_index_1$MonthlyReturn.x[which(sp500_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                          mean(sp500_sometimes_in_index_1$MonthlyReturn.x[which(sp500_sometimes_in_index_1$in_index == 1)], na.rm = TRUE),
                                                          mean(sp500_sometimes_in_index_1$MonthlyReturn.y[which(sp500_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                          mean(sp500_sometimes_in_index_1$MonthlyReturn.y[which(sp500_sometimes_in_index_1$in_index == 1)], na.rm = TRUE)),
                                               return_median = c(median(sp500_sometimes_in_index_1$MonthlyReturn.x[which(sp500_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                                 median(sp500_sometimes_in_index_1$MonthlyReturn.x[which(sp500_sometimes_in_index_1$in_index == 1)], na.rm = TRUE),
                                                                 median(sp500_sometimes_in_index_1$MonthlyReturn.y[which(sp500_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                                 median(sp500_sometimes_in_index_1$MonthlyReturn.y[which(sp500_sometimes_in_index_1$in_index == 1)], na.rm = TRUE)),
                                               count = c(sp500_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 0) %>% select(GVKey_main, Iid_main) %>% n_distinct(),
                                                         sp500_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 1) %>% select(GVKey_main, Iid_main) %>% n_distinct(),
                                                         sp500_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 0) %>% select(GVKey, Iid) %>% n_distinct(),
                                                         sp500_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 1) %>% select(GVKey, Iid) %>% n_distinct()))

##russell 1000
russel1_sometimes_in_index <- Holdings_1 %>% filter(ETF.Ticker == 'IWB') %>% group_by(Iid, GVKey) %>% tally() %>% filter(n >= 20 & n <= 80)  %>% filter(!is.na(GVKey))

russel1_sometimes_in_index %<>% inner_join(Returns, by = c('GVKey', 'Iid')) 
colnames(russel1_sometimes_in_index)[4] <- 'Date'
russel1_sometimes_in_index$Date <- as.Date(russel1_sometimes_in_index$Date)
russel1_sometimes_in_index %<>% left_join(Holdings_1 %>% filter(ETF.Ticker == 'IWB') %>% select(-ETF.Ticker), by = c('GVKey', 'Iid', 'Date'))
russel1_sometimes_in_index %<>% filter(as.Date(Date) >= '2010-01-31')
russel1_sometimes_in_index$in_index <- NA
russel1_sometimes_in_index$in_index[which(russel1_sometimes_in_index$new == 1)] <- 1
russel1_sometimes_in_index$in_index[which(russel1_sometimes_in_index$exclude == 1)] <- 0
for(i in c(1:nrow(russel1_sometimes_in_index))){
  if(i == 1){
    russel1_sometimes_in_index$in_index[i] <- ifelse(russel1_sometimes_in_index$GVKey[i] %in% Holdings$GVKey[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')] &
                                                       russel1_sometimes_in_index$Iid[i] %in% Holdings$Iid[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')], 1, 0)
  }
  if(i > 1){
    if(russel1_sometimes_in_index$GVKey[i] != russel1_sometimes_in_index$GVKey[i-1]){
      russel1_sometimes_in_index$in_index[i] <- ifelse(russel1_sometimes_in_index$GVKey[i] %in% Holdings$GVKey[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')] &
                                                         russel1_sometimes_in_index$Iid[i] %in% Holdings$Iid[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')], 1, 0)
    }
  }
  if(is.na(russel1_sometimes_in_index$in_index[i])){
    russel1_sometimes_in_index$in_index[i] <- russel1_sometimes_in_index$in_index[i-1]
  }
}
rm(i)

russel1_sometimes_in_index %<>% filter(as.Date(Date) <= data[length(data)])
russel1_sometimes_in_index %<>% select(-n)
russel1_sometimes_in_index %<>% inner_join(russel1_sometimes_in_index %>% group_by(GVKey, Iid) %>% arrange(as.Date(Date)) %>% mutate(n = c(1:n())) %>% 
                                           filter(n == 1 & in_index == 0) %>% select(GVKey, Iid) %>% unique(), by = c('GVKey', 'Iid'))
russel1_sometimes_in_index %<>% group_by(GVKey, Iid) %>% mutate(n_in_index = n_distinct(Date[which(in_index == 1)]),
                                                              n_out_index = n_distinct(Date[which(in_index == 0)])) %>% ungroup() %>%
  filter(n_in_index >= 20 & n_in_index <= 80 & n_out_index >= 20 & n_out_index <= 80) %>% select(-n_in_index, -n_out_index)

##отбираем активы, которые не были в sp500
no_russel1 <- Returns %>% anti_join(Holdings_1 %>% filter(ETF.Ticker == 'IWB'), by = c('GVKey', 'Iid')) %>% filter(Date >= data[1] & Date <= data[length(data)])
no_russel1$Date <- as.Date(no_russel1$Date)
russel1_sometimes_in_index_1 <- inner_join(russel1_sometimes_in_index, no_sp500, by = c('Date')) %>%
  filter(in_index == 0 & MkVal.y >= 0.9*MkVal.x & MkVal.y <= 1.1*MkVal.x & Beta3Yr.y >= 0.8*Beta3Yr.x & Beta3Yr.y <= 1.2*Beta3Yr.x)
russel1_sometimes_in_index_1 %<>% select(GVKey.x, Iid.x, in_index, GVKey.y, Iid.y) %>% unique()
colnames(russel1_sometimes_in_index_1)[c(1,2)] <- c('GVKey', 'Iid')
russel1_sometimes_in_index_1 %<>% inner_join(sp500_sometimes_in_index %>% select(GVKey, Iid, Date, in_index), by = c('GVKey', 'Iid'))
colnames(Returns)[3] <- 'Date'
russel1_sometimes_in_index_1 %<>% inner_join(Returns %>% select(-EarningsYield), by = c('GVKey', 'Iid', 'Date'))
russel1_sometimes_in_index_1 %<>% filter(as.Date(Date) >= data[1] & as.Date(Date) <= data[length(data)])
russel1_sometimes_in_index_1 %<>% select(-in_index.x) %>% unique()
colnames(russel1_sometimes_in_index_1)[c(1,2)] <- c('GVKey_main', 'Iid_main')
colnames(russel1_sometimes_in_index_1)[c(3,4)] <- c('GVKey', 'Iid')
colnames(russel1_sometimes_in_index_1)[c(6)] <- c('in_index')
russel1_sometimes_in_index_1 %<>% inner_join(Returns %>% select(-EarningsYield), by = c('GVKey', 'Iid', 'Date'))
colnames(russel1_sometimes_in_index_1)[c(1,2)] <- c('GVKey_main', 'Iid_main')
russel1_sometimes_in_index_1 %<>% group_by(GVKey_main, Iid_main, GVKey, Iid) %>% mutate(all = n_distinct(Date),
                                                                                      out_index = n_distinct(Date[which(in_index == 0)])) %>% ungroup()
russel1_sometimes_in_index_1$equal_beta <- ifelse(russel1_sometimes_in_index_1$Beta3Yr.y >= 0.8*russel1_sometimes_in_index_1$Beta3Yr.x & 
                                                    russel1_sometimes_in_index_1$Beta3Yr.y <= 1.2*russel1_sometimes_in_index_1$Beta3Yr.x, 1, 0)
russel1_sometimes_in_index_1$equal_beta[which(russel1_sometimes_in_index_1$in_index == 1)] <- NA
russel1_sometimes_in_index_1$equal_mkval <- ifelse(russel1_sometimes_in_index_1$MkVal.y >= 0.8*russel1_sometimes_in_index_1$MkVal.x & 
                                                     russel1_sometimes_in_index_1$MkVal.y <= 1.2*russel1_sometimes_in_index_1$MkVal.x, 1, 0)
russel1_sometimes_in_index_1$equal_mkval[which(russel1_sometimes_in_index_1$in_index == 1)] <- NA
russel1_sometimes_in_index_1 %<>% group_by(GVKey_main, Iid_main, GVKey, Iid) %>% mutate(n_equal_beta = n_distinct(Date[which(equal_beta == 1)]),
                                                                                      n_equak_mkval = n_distinct(Date[which(equal_mkval == 1)]))
russel1_sometimes_in_index_1 %<>% filter(out_index >= 20)
russel1_sometimes_in_index_1 %<>% filter(0.2*out_index <= n_equal_beta & 0.2*out_index<=n_equak_mkval)
group_russel1_sometimes_in_index_1 <- data.frame(type = c('Активы в индексе до индекса', 'Активы в индексе после индекса', 'Активы вне индекса до индекса', 'Активы вне индекса после индекса'),
                                               return_mean = c(mean(russel1_sometimes_in_index_1$MonthlyReturn.x[which(russel1_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                               mean(russel1_sometimes_in_index_1$MonthlyReturn.x[which(russel1_sometimes_in_index_1$in_index == 1)], na.rm = TRUE),
                                                               mean(russel1_sometimes_in_index_1$MonthlyReturn.y[which(russel1_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                               mean(russel1_sometimes_in_index_1$MonthlyReturn.y[which(russel1_sometimes_in_index_1$in_index == 1)], na.rm = TRUE)),
                                               return_median = c(median(russel1_sometimes_in_index_1$MonthlyReturn.x[which(russel1_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                                 median(russel1_sometimes_in_index_1$MonthlyReturn.x[which(russel1_sometimes_in_index_1$in_index == 1)], na.rm = TRUE),
                                                                 median(russel1_sometimes_in_index_1$MonthlyReturn.y[which(russel1_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                                 median(russel1_sometimes_in_index_1$MonthlyReturn.y[which(russel1_sometimes_in_index_1$in_index == 1)], na.rm = TRUE)),
                                               count = c(russel1_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 0) %>% select(GVKey_main, Iid_main) %>% n_distinct(),
                                                         russel1_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 1) %>% select(GVKey_main, Iid_main) %>% n_distinct(),
                                                         russel1_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 0) %>% select(GVKey, Iid) %>% n_distinct(),
                                                         russel1_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 1) %>% select(GVKey, Iid) %>% n_distinct()))

##russell 3000
russel3_sometimes_in_index <- Holdings_1 %>% filter(ETF.Ticker == 'IWV') %>% group_by(Iid, GVKey) %>% tally() %>% filter(n >= 20 & n <= 80)  %>% filter(!is.na(GVKey))

russel3_sometimes_in_index %<>% inner_join(Returns, by = c('GVKey', 'Iid')) 
colnames(russel3_sometimes_in_index)[4] <- 'Date'
russel3_sometimes_in_index$Date <- as.Date(russel3_sometimes_in_index$Date)
russel1_sometimes_in_index %<>% left_join(Holdings_1 %>% filter(ETF.Ticker == 'IWV') %>% select(-ETF.Ticker), by = c('GVKey', 'Iid', 'Date'))
russel1_sometimes_in_index %<>% filter(as.Date(Date) >= '2010-01-31')
russel1_sometimes_in_index$in_index <- NA
russel1_sometimes_in_index$in_index[which(russel1_sometimes_in_index$new == 1)] <- 1
russel1_sometimes_in_index$in_index[which(russel1_sometimes_in_index$exclude == 1)] <- 0
for(i in c(1:nrow(russel1_sometimes_in_index))){
  if(i == 1){
    russel1_sometimes_in_index$in_index[i] <- ifelse(russel1_sometimes_in_index$GVKey[i] %in% Holdings$GVKey[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')] &
                                                       russel1_sometimes_in_index$Iid[i] %in% Holdings$Iid[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')], 1, 0)
  }
  if(i > 1){
    if(russel1_sometimes_in_index$GVKey[i] != russel1_sometimes_in_index$GVKey[i-1]){
      russel1_sometimes_in_index$in_index[i] <- ifelse(russel1_sometimes_in_index$GVKey[i] %in% Holdings$GVKey[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')] &
                                                         russel1_sometimes_in_index$Iid[i] %in% Holdings$Iid[which(Holdings$Date == '2010-01-31' & Holdings$ETF.Ticker == 'SPY')], 1, 0)
    }
  }
  if(is.na(russel1_sometimes_in_index$in_index[i])){
    russel1_sometimes_in_index$in_index[i] <- russel1_sometimes_in_index$in_index[i-1]
  }
}
rm(i)

russel1_sometimes_in_index %<>% filter(as.Date(Date) <= data[length(data)])
russel1_sometimes_in_index %<>% select(-n)
russel1_sometimes_in_index %<>% inner_join(russel1_sometimes_in_index %>% group_by(GVKey, Iid) %>% arrange(as.Date(Date)) %>% mutate(n = c(1:n())) %>% 
                                             filter(n == 1 & in_index == 0) %>% select(GVKey, Iid) %>% unique(), by = c('GVKey', 'Iid'))
russel1_sometimes_in_index %<>% group_by(GVKey, Iid) %>% mutate(n_in_index = n_distinct(Date[which(in_index == 1)]),
                                                                n_out_index = n_distinct(Date[which(in_index == 0)])) %>% ungroup() %>%
  filter(n_in_index >= 20 & n_in_index <= 80 & n_out_index >= 20 & n_out_index <= 80) %>% select(-n_in_index, -n_out_index)

##отбираем активы, которые не были в sp500
no_russel1 <- Returns %>% anti_join(Holdings_1 %>% filter(ETF.Ticker == 'IWB'), by = c('GVKey', 'Iid')) %>% filter(Date >= data[1] & Date <= data[length(data)])
no_russel1$Date <- as.Date(no_russel1$Date)
russel1_sometimes_in_index_1 <- inner_join(russel1_sometimes_in_index, no_sp500, by = c('Date')) %>%
  filter(in_index == 0 & MkVal.y >= 0.9*MkVal.x & MkVal.y <= 1.1*MkVal.x & Beta3Yr.y >= 0.8*Beta3Yr.x & Beta3Yr.y <= 1.2*Beta3Yr.x)
russel1_sometimes_in_index_1 %<>% select(GVKey.x, Iid.x, in_index, GVKey.y, Iid.y) %>% unique()
colnames(russel1_sometimes_in_index_1)[c(1,2)] <- c('GVKey', 'Iid')
russel1_sometimes_in_index_1 %<>% inner_join(sp500_sometimes_in_index %>% select(GVKey, Iid, Date, in_index), by = c('GVKey', 'Iid'))
colnames(Returns)[3] <- 'Date'
russel1_sometimes_in_index_1 %<>% inner_join(Returns %>% select(-EarningsYield), by = c('GVKey', 'Iid', 'Date'))
russel1_sometimes_in_index_1 %<>% filter(as.Date(Date) >= data[1] & as.Date(Date) <= data[length(data)])
russel1_sometimes_in_index_1 %<>% select(-in_index.x) %>% unique()
colnames(russel1_sometimes_in_index_1)[c(1,2)] <- c('GVKey_main', 'Iid_main')
colnames(russel1_sometimes_in_index_1)[c(3,4)] <- c('GVKey', 'Iid')
colnames(russel1_sometimes_in_index_1)[c(6)] <- c('in_index')
russel1_sometimes_in_index_1 %<>% inner_join(Returns %>% select(-EarningsYield), by = c('GVKey', 'Iid', 'Date'))
colnames(russel1_sometimes_in_index_1)[c(1,2)] <- c('GVKey_main', 'Iid_main')
russel1_sometimes_in_index_1 %<>% group_by(GVKey_main, Iid_main, GVKey, Iid) %>% mutate(all = n_distinct(Date),
                                                                                        out_index = n_distinct(Date[which(in_index == 0)])) %>% ungroup()
russel1_sometimes_in_index_1$equal_beta <- ifelse(russel1_sometimes_in_index_1$Beta3Yr.y >= 0.8*russel1_sometimes_in_index_1$Beta3Yr.x & 
                                                    russel1_sometimes_in_index_1$Beta3Yr.y <= 1.2*russel1_sometimes_in_index_1$Beta3Yr.x, 1, 0)
russel1_sometimes_in_index_1$equal_beta[which(russel1_sometimes_in_index_1$in_index == 1)] <- NA
russel1_sometimes_in_index_1$equal_mkval <- ifelse(russel1_sometimes_in_index_1$MkVal.y >= 0.8*russel1_sometimes_in_index_1$MkVal.x & 
                                                     russel1_sometimes_in_index_1$MkVal.y <= 1.2*russel1_sometimes_in_index_1$MkVal.x, 1, 0)
russel1_sometimes_in_index_1$equal_mkval[which(russel1_sometimes_in_index_1$in_index == 1)] <- NA
russel1_sometimes_in_index_1 %<>% group_by(GVKey_main, Iid_main, GVKey, Iid) %>% mutate(n_equal_beta = n_distinct(Date[which(equal_beta == 1)]),
                                                                                        n_equak_mkval = n_distinct(Date[which(equal_mkval == 1)]))
russel1_sometimes_in_index_1 %<>% filter(out_index >= 20)
russel1_sometimes_in_index_1 %<>% filter(0.2*out_index <= n_equal_beta & 0.2*out_index<=n_equak_mkval)
group_russel1_sometimes_in_index_1 <- data.frame(type = c('Активы в индексе до индекса', 'Активы в индексе после индекса', 'Активы вне индекса до индекса', 'Активы вне индекса после индекса'),
                                                 return_mean = c(mean(russel1_sometimes_in_index_1$MonthlyReturn.x[which(russel1_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                                 mean(russel1_sometimes_in_index_1$MonthlyReturn.x[which(russel1_sometimes_in_index_1$in_index == 1)], na.rm = TRUE),
                                                                 mean(russel1_sometimes_in_index_1$MonthlyReturn.y[which(russel1_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                                 mean(russel1_sometimes_in_index_1$MonthlyReturn.y[which(russel1_sometimes_in_index_1$in_index == 1)], na.rm = TRUE)),
                                                 return_median = c(median(russel1_sometimes_in_index_1$MonthlyReturn.x[which(russel1_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                                   median(russel1_sometimes_in_index_1$MonthlyReturn.x[which(russel1_sometimes_in_index_1$in_index == 1)], na.rm = TRUE),
                                                                   median(russel1_sometimes_in_index_1$MonthlyReturn.y[which(russel1_sometimes_in_index_1$in_index == 0)], na.rm = TRUE),
                                                                   median(russel1_sometimes_in_index_1$MonthlyReturn.y[which(russel1_sometimes_in_index_1$in_index == 1)], na.rm = TRUE)),
                                                 count = c(russel1_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 0) %>% select(GVKey_main, Iid_main) %>% n_distinct(),
                                                           russel1_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 1) %>% select(GVKey_main, Iid_main) %>% n_distinct(),
                                                           russel1_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 0) %>% select(GVKey, Iid) %>% n_distinct(),
                                                           russel1_sometimes_in_index_1 %>% ungroup() %>% filter(in_index == 1) %>% select(GVKey, Iid) %>% n_distinct()))
