
### This program performs a seasonal adjustment on the fly using the X-13ARIMA-SEATS, seasonal and ggplot2

### Inspired by blog post ellisp.github.io/blog/2015/10/10/X13ARIMA-SEATS/

### Data used - Consumer Credit - G.19 (http://www.federalreserve.gov/releases/g19/current/)

### FRB provides seasonally adjusted value of Consumer Credit - these functions are for illustrative purposes

library(seasonal)
library(tidyr)
library(dplyr)
library(showtext)
library(ggplot2)
library(scales)
library(proto)
library(readr)
library(lubridate)

options( stringsAsFactors=F ) 

# set path to X13-SEATS and check it's working
Sys.setenv(X13_PATH = "./winx13/x13as")
checkX13()

###########################################################################################################
    # This function creates a ggplot stat for plotting seasonal adjustments

        StatSeas <- proto(ggplot2:::Stat, {    
          required_aes <- c("x", "y")
          default_geom <- function(.) GeomLine
          objname <- "seasadj"
          calculate_groups <- function(., data, scales, ...){
            .super$calculate_groups(., data, scales, ...)
          }
          calculate <- function(., data, scales, frequency, start, ...) {
            y_ts <- ts(data$y, frequency = frequency, start = start)
            y_sa <- seasonal::final(seasonal::seas(y_ts, ...))
            result <- data.frame(x = data$x, y = as.numeric(y_sa))
            return(result)
          }
        }) 
        
        stat_seas <- StatSeas$new
        
##########################################################################################################
    # This functions reads the Consumer Credit data and convers it into a long format
    read_consumer_credit_data <- function()
    {
            # load the data for US Consumer Credit Outstanding - Revolving Credit
            # http://www.federalreserve.gov/releases/g19/HIST/cc_hist_mt_levels.html
            revolving.credit <- read.csv("FRB_G19-Revolving_credit_mod.csv", header=TRUE, sep=",")
            revolving.credit$Month = ymd(paste0(revolving.credit$Month,"-","01"))

            ## Convert from wide to long format
            revolving.credit <- gather(revolving.credit, Group, Value, Revolving.consumer.credit.owned.and.securitized:
                                         Consumer.revolving.credit.owned.by.nonfinancial.business)
            revolving.credit       
    }


##########################################################################################################
    #This function plots a time series, its auto correlation function and spectrun
    
    plot_time_series_acf_spectrum <- function(df,group_value,start_date,end_date)
    {
      df <- df %>%
        filter(Month >= start_date, 
               Month <= end_date) %>%
        filter(Group == group_value)
      
      df_ts <- ts(df$Value,start=c(year(start_date),month(start_date)),frequency=12)
      
      par(mfrow = c(2, 2))
      plot(df_ts, xlab = "")
      acf(df_ts, main = "")
      pacf(df_ts, main = "")
      spectrum(df_ts, main = "")
      
    }


##########################################################################################################
      
    #This function plots the original and the seasonal adjustment for a particular group of values from the long df
    plot_seasonal_adjustment_for_one_group <- function(df,group_value,start_date,end_date)
    {
      
          df <- df %>%
                filter(Month >= start_date, 
                       Month <= end_date) %>%
                filter(Group == group_value)
        
         df_ts <- ts(df$Value,start=c(year(start_date),month(start_date)),frequency=12)
         
         mod <- seas(df_ts)
         
         df_sa <- data_frame(
           Time = time(df_ts),
           Original = df_ts,
           SA = final(mod))
         
         df_sa %>%
           gather("variable", "value", -Time) %>% 
           mutate(variable = gsub("SA", "Seasonally adjusted by SEATS", variable)) %>%
           ggplot(aes(x = Time, y = value, colour = variable)) +
           geom_line() +
           labs(colour = "", x = "") +
           scale_y_continuous("Value of transactions ($m)", label = dollar) +
           ggtitle("US Revolving credit owned and securitized") +
           theme(legend.position = "bottom")
    }
    
        

##########################################################################################################
      # This function plots seasonal adjustments for all groups as facet graphs
        
      plot_seasonal_adjutsment_for_all_groups  <- function(df,start_date,end_date)
        {
        
        df <- df %>%
          filter(Month >= start_date, 
                 Month <= end_date)
        
        df %>%
          ggplot(aes(x = Month, y = Value, colour = Group)) +
          geom_line(alpha = 0.3) +
          stat_seas(frequency = 12, start = c(year(start_date), month(start_date))) +
          facet_wrap( ~ Group, scales = "free_y", ncol = 2) +
          labs(x = "", y = "SEATS-seasonally adjusted monthly transaction value ($m)",
               title = "Consumer Credit in US") +
          theme(legend.position = "none")

      }
##########################################################################################################
        
    input.data <- read_consumer_credit_data()
    
    plot_time_series_acf_spectrum(input.data,"Revolving.consumer.credit.owned.and.securitized",
                                           ymd("2005-01-01"),ymd("2015-01-01"))
    
    plot_seasonal_adjustment_for_one_group(input.data,"Revolving.consumer.credit.owned.and.securitized",
                                           ymd("2005-01-01"),ymd("2015-01-01"))
    
    plot_seasonal_adjutsment_for_all_groups(input.data,
                                           ymd("2005-01-01"),ymd("2015-01-01"))
    

    #Run a Shiny app to explore the seasonal data
    input.data <- input.data %>%
      filter(Month >= ymd("2005-01-01"), 
             Month <= ymd("2015-01-01")) %>%
      filter(Group == "Revolving.consumer.credit.owned.and.securitized")
    
    input.data_ts <- ts(input.data$Value,start=c(year(ymd("2005-01-01")),month(ymd("2005-01-01"))),frequency=12)
    
    mod <- seas(input.data_ts)
    
    inspect(mod)
    