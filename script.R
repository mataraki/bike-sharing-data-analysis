#load libs
library(tidyverse)
library(janitor)
library(lubridate)
library(pivottabler)

#peaking and cleaning

#read datasets
bike_trips_datasets = list.files(path = "GoogleDA/CaseStudy1/datasets",
                                 recursive = TRUE,
                                 pattern = ".csv",
                                 full.names = TRUE)
bike_trips_dataframe <- readr::read_csv(bike_trips_datasets, id = "file_name")
#manual cleaning
clean_names(bike_trips_dataframe)
#sneak peek
head(bike_trips_dataframe)
#bold peek
View(bike_trips_dataframe)
#check for NAs
sapply(bike_trips_dataframe, function(x) sum(is.na(x)))
#adding durations
bike_trips_dataframe <- transform(bike_trips_dataframe, duration = difftime(ended_at, started_at, units = "mins"))
#check negative durations
filter(bike_trips_dataframe, duration < 0)
#clean negative durations
bike_trips_dataframe <- filter(bike_trips_dataframe, duration >= 0)
#adding day of weeks based on day of start
bike_trips_dataframe <- transform(bike_trips_dataframe, day_of_week = wday(started_at, week_start = 1))

#analysis part

#on rideable type
#pivot table on membership type vs rideable type
qpvt(bike_trips_dataframe, "rideable_type", "member_casual","n()")
#plot on membership type vs rideable type
ggplot(data=bike_trips_dataframe)+
  geom_bar(mapping=aes(x=member_casual, fill=rideable_type))+
  scale_fill_brewer(type = "div")+
  labs(x="membership type", y="amount", fill="rideable type", title="Membership type vs. rideable type")

#on day of week
#pivot table on membership type vs day of week
qpvt(bike_trips_dataframe, "day_of_week", "member_casual","n()")
#plot on membership type vs day of week
ggplot(data=bike_trips_dataframe)+
  geom_bar(mapping=aes(x=member_casual, fill=factor(day_of_week)))+
  scale_fill_brewer(type = "div")+
  labs(x="membership type", y="amount", fill="day of week", title="Membership type vs. day of week")

#pivot table on membership type vs day of week vs duration
qpvt(bike_trips_dataframe, "day_of_week", "member_casual", "mean(as.numeric(duration))", format="%.1f")
#plot on membership type vs day of week
ggplot(bike_trips_dataframe,aes(color=member_casual))+
  stat_summary(fun="mean", geom="line", mapping=aes(x=day_of_week, y=as.numeric(duration)))+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7))+
  facet_wrap(~member_casual)+
  labs(x="day of week", y="mean duration", color="membership type", title="Membership type vs. day of week vs. mean duration")

#creating dataframe with no start stations missing
no_start_missing <- filter(bike_trips_dataframe, !is.na(start_station_id))
#creating dataframe with top 10 start stations
most_popular_starts <- c(head(no_start_missing %>% count (start_station_name, sort=TRUE), 10)['start_station_name'])
popular_starts_df <- filter(no_start_missing, start_station_name %in% most_popular_starts$start_station_name)
#creating dataframe with no end stations missing
no_end_missing <- filter(bike_trips_dataframe, !is.na(end_station_id))
#creating dataframe with top 10 end stations
most_popular_ends <- c(head(no_end_missing %>% count (end_station_name, sort=TRUE), 10)['end_station_name'])
popular_ends_df <- filter(no_end_missing, end_station_name %in% most_popular_ends$end_station_name)

#pivot table on membership type vs start station name
qpvt(popular_starts_df, "start_station_name", "member_casual", "n()")
#plot on membership type vs start station name
ggplot(data=popular_starts_df)+
  geom_bar(mapping=aes(x=member_casual, fill=start_station_name))+
  scale_fill_brewer(type = "div")+
  labs(x="membership type", y="amount", fill="start station", title="Membership type vs. start stations")
#pivot table on membership type vs end station name
qpvt(popular_ends_df, "end_station_name", "member_casual", "n()")
#plot on membership type vs end station name
ggplot(data=popular_ends_df)+
  geom_bar(mapping=aes(x=member_casual, fill=end_station_name))+
  scale_fill_brewer(type = "div")+
  labs(x="membership type", y="amount", fill="end station", title="Membership type vs. end stations")