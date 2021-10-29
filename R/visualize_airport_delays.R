#'Visualize airport delays
#'
#'\code{visualize_airport_delays} returns plots concerning the mean delay of flights for different airports by longitude and latitude. The data used is from nycflights13 package.
#'
#'
#' @examples
#' visualize_airport_delays()
#'
#' @importFrom methods new
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_label
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#' @import nycflights13
#' @usage data(flights)
#' @usage data(airports)
#' @export visualize_airport_delays


visualize_airport_delays <- function () 
{ 

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
  
  group_ori_mean <- joined_data_ori2 %>%
    group_by(origin) %>%
    summarise(avg_delay = mean(dep_delay) )
  
  group_dest_mean <- joined_data_dest2 %>% group_by(dest) %>% summarise(avg_delay =mean(arr_delay) )
  
  # join info again to get airport info (and exclude dep_delay)
  
  origin_delays <- arrange((as.data.frame(distinct(left_join(group_ori_mean, select(joined_data_ori2, - dep_delay), by = "origin")))),desc(avg_delay) )
  
  # join info again to get airport info (and exclude arr_delay)
  
  destination_delays <-distinct(left_join(group_dest_mean, select(joined_data_dest2, - arr_delay), by = "dest"))
  destination_delays_head<-head(arrange(destination_delays,desc(avg_delay) ))
  
  
  
  p1<- ggplot(data = as.data.frame(origin_delays)) + 
    geom_point(mapping=aes(x=lat, y=lon)) + 
    labs(title = "Flights Delay - Origin",
         x="Latitude",
         y="Longitude")  +
    theme_minimal() + 
    geom_label(mapping=aes(x=lat, y=lon), label=c(origin_delays)[[3]]
    ) 
  
  
  p2<- ggplot(data= as.data.frame(origin_delays), aes(x= paste(c(origin_delays)[[3]]," - " ,c(origin_delays)[[4]]), y=avg_delay )) +
    geom_bar(stat="identity", fill="grey")+
    geom_text(aes(label=round(c(origin_delays)[[2]],2)), vjust=-0.3, size=3.5) + 
    theme_minimal() + labs(title = "Flights Delay - Origin",
                           x="Airports",
                           y="Average Delay, in minutes") 
  
  
  
  p3<- ggplot(data = as.data.frame(destination_delays_head)) + 
    geom_point(mapping=aes(x=lat, y=lon)) + 
    labs(title = "Top 6 Flights Delayed - Destination",
         x="Latitude",
         y="Longitude")  +
    theme_minimal() + 
    geom_label(mapping=aes(x=lat, y=lon), label=c(destination_delays_head)[[3]]
    ) 
  
  
  p4<- ggplot(data= as.data.frame(destination_delays_head), aes(x= c(destination_delays_head)[[3]], y=avg_delay )) +
    geom_bar(stat="identity", fill="grey")+
    geom_text(aes(label=round(c(destination_delays_head)[[2]],2)), vjust=-0.3, size=3.5) + 
    theme_minimal() + labs(title = " Top 6 Flights Delayed - Destination",
                           x="Airports",
                           y="Average Delay, in minutes") 
  
  
  
  plist<-list(p1,p2,p3,p4)
  return(plist)
  

}
