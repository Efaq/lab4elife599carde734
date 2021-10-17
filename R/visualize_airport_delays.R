visualize_airport_delays <- function () 
{
  install.packages("nycflights13")
  data(package = "nycflights13")  
  install.packages("dplyr")
  library(dplyr)
  
  
  data("flights", package = "nycflights13")  

  data("airports", package = "nycflights13")  
  
  #get only the columns that we need of flights - origins
  flights_treated_ori <- select(flights, origin, dep_delay)
  
  #get only the columns that we need of flights - destination
  flights_treated_dest <- select(flights, dest, arr_delay)
  
  #add new column faa with origin information
  flights_treated_ori2<- mutate(flights_treated_ori, faa = origin)
  
  #add new column faa with destination information
  flights_treated_dest2<- mutate(flights_treated_dest, faa = dest)
  
  #get only the columns that we need of airports
  airports_treated <- select(airports, name, faa, lat, lon)
  
  
  #join information from airports to flights origin dataset 
  
 joined_data_ori <-left_join(flights_treated_ori2, airports_treated, by = "faa")
 
 #join information from airports to flights destination dataset 
 
 joined_data_dest <-left_join(flights_treated_dest2, airports_treated, by = "faa")
 
 
 #take negative values from delays variables (not delayed)
 
joined_data_ori2 <- filter(joined_data_ori, dep_delay >0 )
joined_data_dest2 <- filter(joined_data_dest, arr_delay >0 )

#calculte delay mean
 
 group_ori_mean <- joined_data_ori2 %>% group_by(origin) %>% summarise(avg_delay =mean(dep_delay) )

 group_dest_mean <- joined_data_dest2 %>% group_by(dest) %>% summarise(avg_delay =mean(arr_delay) )
 
# join info again to get airport info (and exclude dep_delay)

 origin_delays <-distinct(left_join(group_ori_mean, select(joined_data_ori2, - dep_delay), by = "origin"))
 
 # join info again to get airport info (and exclude arr_delay)
 
 destination_delays <-distinct(left_join(group_dest_mean, select(joined_data_dest2, - arr_delay), by = "dest"))
 
  
  
  
  
}