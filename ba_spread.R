library(tidyverse)

ticker_dat <- read_csv('tickers.csv')

get_prices <- function(ticker){
    tidyquant::tq_get(ticker, get = "stock.prices", from = '1990-01-02') %>% mutate(ticker = ticker)
}

spread_dat <- map_df(ticker_dat$ticker, get_prices) %>%
    mutate(ba_spread = high - low,
           ba_spread_pct = ba_spread/((high+low)/2)) %>%
    group_by(ticker) %>%
    mutate(prior_ba_spread_pct = lag(ba_spread_pct, n = 1, order_by = date)) %>%
    select(date, ticker, prior_ba_spread_pct) %>%
    slice(-1) %>%
    spread(ticker, prior_ba_spread_pct)

#write_csv(spread_dat, 'spread_dat.csv', na = '')
#spread_dat <- read_csv('spread_dat.csv')

vix_dat <- bind_rows(read_csv('vixarchive.csv'), read_csv('vixcurrent.csv')) %>%
    mutate(vix_delta = vix_close - lag(vix_close, n = 1, order_by = date),
           vix_change = case_when(
               vix_delta > 0 ~ 'higher volatility',
               vix_delta <= 0 ~ 'lower volatility'
           )) %>%
    slice(-1) %>%
    select(date, vix_change)

#write_csv(vix_dat, 'vix_dat.csv', na = '')
#vix_dat <- read_csv('vix_dat.csv')

dat <- inner_join(spread_dat, vix_dat, by = 'date')

plot_dat <- dat %>%
    gather(key = 'ticker', value = 'ba_spread_pct', AAPL:T) %>%
    filter(lubridate::year(date) == 2008)


plot_func <- function(abbrev){
    ggplot(data = plot_dat %>% filter(ticker == abbrev)) +
        geom_point(mapping = aes(x = date, y = ba_spread_pct, color = vix_change)) +
        geom_line(mapping = aes(x = date, y = ba_spread_pct), color = "light grey") +
        ggtitle(label = abbrev)
}

ggs <- map(unique(plot_dat$ticker), plot_func)

