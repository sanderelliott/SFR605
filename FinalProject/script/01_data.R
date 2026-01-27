library(tidyr)
library(dplyr)


getwd()
SNS <- read.csv("FinalProject/data/SNSdet_allyrs.csv")

glimpse(SNS)

## Remove columns for types of tags not used in analysis
SNScln <- SNS %>% 
  select(! SensorType, avgSensorValue, avgComputedValue, Frequency, avgPower, AntennaID, AltFishID)
