require(readr)
require(lubridate)
require(dplyr)
require(data.world)

df <- read.csv("https://query.data.world/s/1bf1rqx0f351otahiiji5vy6q", header = T)  %>% 
    dplyr::mutate(ratio_agr = (Total.employment.in.agriculture..thousands.*1000/Population),
                  ratio_ind =  (Total.employment.in.industry..thousands.*1000/Population), 
                  ratio_serv = (Total.employment.in.services..thousands.*1000/Population)) %>% 
    dplyr::filter(Income.Class %in% c('High Income','Upper Middle Income','Lower Middle Income','Low Income'))

    globals <- df %>% dplyr::select(Happiness.Score) %>% dplyr::distinct()