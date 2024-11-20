### Structural equation modelling of 250 points from the study area
library(tidyverse)
library(lavaan)

data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRWzT11vBFDWMxmJeYGGsWPgUM7t6Icyt4MeTM4o8VEJw9T8kGpQu-rgF18xF_IvvfxxPlmQzTJgciS/pub?gid=1123931434&single=true&output=csv") |> select(-Points_ID)

datastd <- data |> 
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
datastd


psych::pairs.panels(data %>% select(dist2river,elevation,CorProtAr,rainfall,cec,burnfreq,hills,woody))
psych::pairs.panels(datastd %>% select(dist2river,elevation,CorProtAr,rainfall,cec,burnfreq,hills,woody))

#Response variables: woody
#Predictor variables: dist2river, elevation, CorProtAr, rainfall, cec, burnfreq, hills
#cec is affected by landform/elevation and also by burn frequency
#rainfall is affected by elevation and landform
#Burnfreq is largely explained by coreprotected areas maybe also by cec
#there are relations between dist2river, hills, elevation, but likely to be small effect

#Stupid model simple multiple regression
multreg_std <- lm(woody~dist2river+elevation+CorProtAr+rainfall+cec+burnfreq+hills, datastd)
summary(multreg_std)

#time for the SEM
woody_model <- ('woody~dist2river + elevation + CorProtAr + rainfall + cec + burnfreq + hills
                rainfall~elevation + hills
                burnfreq~CorProtAr')
woody_model                
woody_fit <- lavaan::sem(woody_model, datastd)
summary(woody_fit, standardized=TRUE,fit.measures=TRUE,rsquare=TRUE)

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR
# CFI=0.787, TLI=0.521, RMSEA=0.265, SRMR=0.050
#Current test model does not fit well

woody_model2 <- ('woody~dist2river + CorProtAr + rainfall + cec + burnfreq + hills
                 rainfall~elevation 
                burnfreq~CorProtAr + rainfall + hills
                cec~burnfreq + rainfall')
woody_fit2 <- lavaan::sem(woody_model2, datastd)
summary(woody_fit2, standardized=T, fit.measures=T,rsquare=T)

#### add distance to river? add hills as direct?
woody_model3 <- ('woody~ cec + burnfreq 
                 rainfall~elevation
                burnfreq~ CorProtAr + rainfall + hills
                cec~burnfreq + rainfall')
woody_fit3 <- lavaan::sem(woody_model3, datastd)
summary(woody_fit3, standardized=T, fit.measures=T,rsquare=T)

#MODEL 3: SRMR 0.054, RMSEA 0.182, CFI 0.908, TLI 0.817

woody_model4 <- ('woody~ cec + burnfreq + dist2river
                 rainfall~elevation + dist2river
                burnfreq~ CorProtAr + rainfall + hills + dist2river
                cec~burnfreq + rainfall + elevation + dist2river
                 rainfall~~burnfreq')
woody_fit4 <- lavaan::sem(woody_model4, datastd)
summary(woody_fit4, standardized=T, fit.measures=T,rsquare=T)



