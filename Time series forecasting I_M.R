iss <- read.csv("C:/Users/User/Documents/nn.csv",sep = ",")

iss <- iss[-c(5)]

iss <- filter(iss,Ratio!=0.0 & dayname=="Monday" & Ratio>=0.55)


##### filter + smooth curve

filter(iss, dayname == "Monday") %>% ggplot(aes(x=date  , y=Ratio, rm.na=T)) +
  geom_point(aes(colour =hour)) + 
  geom_smooth(method='lm')
predictor <-  lm(formula =date~hour+Ratio,data=iss)

summary(predictor)

#### (continous lines graph)
iss %>% ggplot(aes(x=date,y=Ratio,group=1))+
  geom_line(aes(y=Ratio),col="red")+
  theme(axis.title.x = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x="Date",y="Day Closing Price")

# replace all NaN with zeros
transition_dataframe[is.na(transition_dataframe)] = 0

##########################

final <- as.data.frame(iss)

class(final)

tempdf <- ts(final$Ratio, frequency=3*3)

plot.ts(tempdf)

fit=Arima(tempdf$hour,seasonal=list(order=c(xxx,xxx,xxx),period=xxx)
          #####g1 <- iss %>%
            #group_by(hour) %>%
            #ggplot(aes(Ratio,avg_variance)) +
            #geom_point(aes(color =dayname ))       
          
####

tsclean(iris,replace.missing = TRUE)

##
ggplot(iss, aes(hour , Ratio)) + geom_line()

### exp smoothing

hourly_ms <- msts(iss$Ratio, seasonal.periods = c(7,12))  ### seasonal time periods

hourly_fit <- tbats(hourly_ms) ## exponential smoothing with trends

hourly_pred <-  forecast(hourly_fit,h=72) ## forecasting with seasons

plot(hourly_pred) ## plot forecasting

stlmy <- stlm(hourly_ms, s.window = "periodic", method = "ets") ## to seasonalize data

stlm_pred <- forecast(stlmy, h =24)  ## forecast

plot(stlm_pred) ## its plot

holt(iss$Ratio,h=10,level=c(80,90,95),alpha = NULL,beta=NULL,phi = NULL) ### holt winters time seriers forecasting






