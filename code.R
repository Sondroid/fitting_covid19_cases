setwd("G:/내 드라이브/SNU/2021-1/회귀분석 및 실습/midterm")
getwd()

library(nls2)
library(dplyr)
library(ggplot2)
#--------------------------------------

df <- read.csv("global_confirmed_cases_210420.csv")
df_RUS <- filter(df, CountryCode == "RUS")

df_RUS$Date <- df_RUS$Date %>% as.Date("%Y.%m.%d")

ggplot(data = df_RUS, mapping = aes(Days, Cases)) +
  geom_line(size = 1.3, colour = 'blue') +
  ggtitle("Days After Start vs Cumulative Cases") +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="black", vjust=2))


View(df_RUS)
#--------------------------------------

df_RUS_train <- df_RUS %>% filter(Date <= "2021-03-20")
df_RUS_test <- df_RUS %>% filter(Date > "2021-03-20")

View(df_RUS_train)
#--------------------------------------
# Linear
linear.fit <- lm(Cases ~ Days, data = df_RUS_train)
summary(linear.fit)
predict.linear <- predict(linear.fit, df_RUS_test)

# Logistic
logistic.formula <- Cases ~ a / (1 + exp(b-(c * Days)))
logistic.grid <- expand.grid(a = max(df_RUS_train$Cases),
                             b = seq(1, 100, 1),
                             c = seq(0.01, 1, 0.01))
logistic.fit1 <- nls2(logistic.formula, data = df_RUS_train, start = logistic.grid,
                      algorithm = "brute-force")
logistic.fit2 <- nls2(logistic.formula, data = df_RUS_train, start = coef(logistic.fit1),
                      algorithm = "default")
summary(logistic.fit2)
predict.logistic <- predict(logistic.fit2, df_RUS_test)

# Gompertz
Gompertz.formula <- Cases ~ a * exp(-b * exp(-c * Days))
Gompertz.grid <- expand.grid(a = max(df_RUS_train$Cases), 
                             b = seq(1, 100, 1), 
                             c = seq(0, 1, 0.01))
Gompertz.fit1 <- nls2(Gompertz.formula, data = df_RUS_train, start = Gompertz.grid,
                      algorithm = "brute-force")
Gompertz.fit2 <- nls2(Gompertz.formula, data = df_RUS, start = coef(Gompertz.fit1),
                      algorithm = "default")
summary(Gompertz.fit2)
predict.Gompertz <- predict(Gompertz.fit2, df_RUS_test)

#--------------------------------------
getRsq <- function(y, yhat){
  Rsq <- 1 - (sum((y-yhat)^2) / sum((y-mean(y))^2))
  return(Rsq)
}

getMSE <- function(y, yhat){
  MSE <- sum((y-yhat)^2)/length(y)
  return(MSE)
}

#--------------------------------------

model.linear = linear.fit
model.logistic = logistic.fit2
model.Gompertz = Gompertz.fit2


y_hat_linear = predict(model.linear, df_RUS)
y_hat_logistic = predict(model.logistic, df_RUS)
y_hat_Gompertz = predict(model.Gompertz, df_RUS)

predict <- data.frame(Days = df_RUS$Days, predict.linear, predict.logistic, predict.Gompertz)
head(predict)


df_fitted <- data.frame(Days = df_RUS$Days,
                          y_hat_linear,
                          y_hat_logistic,
                          y_hat_Gompertz)

df_predict <- data.frame(x = rep(df_fitted$Days,3), 
                         yhat_cases = c(y_hat_linear, y_hat_logistic, y_hat_Gompertz),
                         yhat_difference = c(y_hat_linear - c(0, y_hat_linear[-length(y_hat_linear)]),  
                                             y_hat_logistic - c(0, y_hat_logistic[-length(y_hat_logistic)]),
                                             y_hat_Gompertz - c(0, y_hat_Gompertz[-length(y_hat_Gompertz)])),
                         type = rep(c("Linear model", "Logistic model", "Gompertz model"), 
                                    each = nrow(df_fitted)))

df_predict$type <- factor(df_predict$type,
                          levels = c("Linear model", "Logistic model", "Gompertz model"))
head(df_predict)


t0 <- df_RUS$Date[1]
model_labels <- c("Linear model", "Logistic model", "Gompertz model")
models <- list(model.linear, model.logistic, model.gompertz)

col_list <- c("red", "blue", "green")
shape_list <- c("Linear model"="dashed", "Logistic model"="solid", "Gompertz model"="dotdash")

p_1 <- ggplot(data=df_RUS, aes(x = Days, y = Cases)) +
  geom_point(color='black', shape = 1, size=5) +
  theme_bw() +
  labs(title = paste0("COVID-19 Cases"),
       subtitle = paste0("Russia", " / ", "Cumulated"), 
       x = paste0('Days Since ', as.character(t0)),
       y = 'Number of Cases') +
  geom_line(data = df_predict,
            aes(x = x,y = yhat_cases, colour = type, linetype = type), size=1.5)+
  scale_color_manual(name = "Model",
                     labels = model_labels,
                     values = col_list) +   
  scale_linetype_manual(name = "Model",
                        labels = model_labels,
                        values = shape_list) +
  theme(plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=2),
        plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="maroon", vjust=2),
        axis.text=element_text(size=14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, hjust = 0),
        axis.title=element_text(size=16, colour = "black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
p_1

data.frame(Model = c("Linear regression"), 
           beta0_hat = coef(model.linear)[1], 
           beta1_hat = coef(model.linear)[2],
           R_squared = summary(model.linear)$adj.r.squared)

data.frame(Model = c("Logistic model", "Gompertz model"),
           a = c(coef(model.logistic)['a'], coef(model.Gompertz)['a']),
           b = c(coef(model.logistic)['b'], coef(model.Gompertz)['b']),
           c = c(coef(model.logistic)['c'], coef(model.Gompertz)['c']),
           R_squared = c(getRsq(),))

getMSE <- function(y, yhat){
  MSE <- sum((y-yhat)^2)/length(y)
  return(MSE)
}

MSE_test_logit_daily <- getMSE(y = df_RUS_test$Difference, (y_hat_linear - c(0, y_hat_linear[-length(y_hat_linear)]))[df_RUS_test$Days])
MSE_test_bert_daily <- getMSE(y = df_RUS_test$Difference, (y_hat_logistic - c(0, y_hat_logistic[-length(y_hat_logistic)]))[df_RUS_test$Days])
MSE_test_gomp_daily <- getMSE(y = df_RUS_test$Difference, (y_hat_Gompertz - c(0, y_hat_Gompertz[-length(y_hat_Gompertz)]))[df_RUS_test$Days])


------------------------------------------------------
vacc <- read.csv("covid_vaccine.csv")
str(vacc)

vacc_RUS <- vacc %>% filter(CountryCode == "RUS")
vacc_RUS$Date <- vacc_RUS$Date %>% as.Date("%Y.%m.%d")
vacc_RUS <- vacc_RUS %>% mutate(Days = as.integer(Date - vacc_RUS$Date[1] + 1))

vacc_RUS_train <- vacc_RUS %>% filter(Date <= "2021-03-20")


plot(vacc_RUS$Days, vacc_RUS$Difference)

fit_1 <- glm(Difference ~ Days, data = vacc_RUS_train, family = poisson)
fitted(fit_1)

seg_fit_1 <- segmented(fit_1, seg.Z = ~ Days,
                       npsi = 2, control = seg.control(it.max = 10000, n.boot = 50))
fitted(seg_fit_1)
predict(seg_fit_1, vacc_RUS$Days)
predict(seg_fit_1)

glm.fit(fit_1, vacc_RUS)

vacc_predict = data.frame(x = vacc_RUS$Days, 
                          yhat_cases = exp(predict(seg_fit_1, vacc_RUS)))
head(vacc_predict)

---
fit_2 <- glm(Difference ~ (Days) + (people_vaccinated), data = vacc_RUS_train, family = poisson)
fit_2 <- glm(Difference ~ (Days) + (people_vaccinated), data = vacc_RUS_train, family = poisson)


seg_fit_1 <- segmented(fit_1, seg.Z = ~ Days,
                       npsi = 2, control = seg.control(it.max = 10000, n.boot = 50))

lm(Difference ~ Days + people_vaccinated, data = vacc_RUS_train)

vacc_predict = data.frame(x = vacc_RUS$Days, 
                          yhat_cases = exp(predict(seg_fit_1, vacc_RUS)))

t1 <- vacc_RUS$Date[1]
p_2 <- ggplot(data=vacc_RUS, aes(x = Days, y = Difference)) +
  geom_point(color='black', shape = 1, size=5) +
  theme_bw() +
  labs(title = paste0("COVID-19 Cases"),
       subtitle = paste0("Russia", " / ", "daily"), 
       x = paste0('Days Since ', as.character(t1)),
       y = 'Number of Cases') +
  geom_line(data = vacc_predict,
            aes(x = x, y = yhat_cases), size=1.5)+
  theme(plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=2),
        plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="maroon", vjust=2),
        axis.text=element_text(size=14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, hjust = 0),
        axis.title=element_text(size=16, colour = "black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
p_2
