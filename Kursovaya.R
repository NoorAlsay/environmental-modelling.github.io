library("tidyverse")
#library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
setwd("C:/NOUR")
eddypro <- read_csv("eddypro.csv", comment = "[", skip = 1)
eddypro = eddypro[-1,]
eddypro = eddypro[eddypro$DOY >150 & eddypro$DOY <240, ] #  выбрали летние дни
eddypro = eddypro[eddypro$daytime == "T", ]
# eddypro$co2_flux - то что мы моделируем
tbl = eddypro
tbl = select(tbl, -(roll))
tbl = tbl %>% mutate_if(is.character, factor)
tbl[tbl == -9999] = NA

#переименовали колонки, так чтобы в них не было лишних символов
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(tbl)

#берем только те колонки, которые содержат числа

tbl_numeric = tbl[,sapply(tbl,is.numeric) ]

cor_td = cor(drop_na(tbl_numeric))
cor_td = as.tibble(cor_td)
variables = names(cor_td)[cor_td$co2_flux^2 >0.16]
variables = variables[!is.na(variables)]

formula1 = co2_flux~LE+rand_err_LE+rand_err_co2_flux+h2o_flux+rand_err_h2o_flux+sonic_temperature+air_temperature+air_density+air_molar_volume+RH+un_LE+un_co2_flux+un_h2o_flux+mean_value+h2o_var+w_div_co2_cov+w_div_h2o_cov+co2_signal_strength_7200+h2o_signal_strength_7200+flowrate

model1 = lm(formula1,data =tbl_numeric)
summary(model1)

formula2 = co2_flux ~ LE + rand_err_LE + h2o_flux +un_LE+un_co2_flux+un_h2o_flux+h2o_var+w_div_co2_cov+co2_signal_strength_7200+h2o_signal_strength_7200+flowrate   

model2 = lm(formula2,data =tbl_numeric)
summary(model2)
  
formula3 = co2_flux ~ LE + rand_err_LE + h2o_flux +un_LE+un_co2_flux+un_h2o_flux+w_div_co2_cov+co2_signal_strength_7200  
model3 = lm(formula3,data =tbl_numeric) 
summary(model3)  

formula4 = co2_flux ~ LE + h2o_flux +un_LE+un_co2_flux+un_h2o_flux+w_div_co2_cov+co2_signal_strength_7200  
model4 = lm(formula4,data =tbl_numeric) 
summary(model4)  
