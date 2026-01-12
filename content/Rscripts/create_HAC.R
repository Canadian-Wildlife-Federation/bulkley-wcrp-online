install.packages('jsonlite')
library(jsonlite)
library(forcats)
library(dplyr)
library(ggplot2)



tracktable<-read.csv("C:/Users/DanielM/Documents/GitHub/bulkley-wcrp-online/content/Rscripts/combined_tracking_table_crossigns_BULK.csv")

tracktable$rank_total_upstr_hab[tracktable$rank_total_upstr_hab == ""] <- NA
tracktable <- tracktable[!is.na(tracktable$rank_total_upstr_hab), ] #keep only ranked barriers

tracktable<- tracktable %>% arrange(desc(avg_gain_per_barrier)) #sort by avg_gain_per_barrier descending

tracktable <- tracktable %>%
  mutate(cum_avg_gain = cumsum(avg_gain_per_barrier)) #create cumulative avg_gain_per_barrier column

tracktable_top20<-tracktable %>% slice_max(order_by = avg_gain_per_barrier, n = 20) #set limit to number of structures in HAC (eg n=20)


ggplot(tracktable_top20, aes(x = seq_along(cum_avg_gain), y = cum_avg_gain)) +
  geom_line() +
  geom_point() +
  labs(x = "Index", y = "Cumulative Average Gain")