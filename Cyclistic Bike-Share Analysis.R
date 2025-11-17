#Truyền thư viện 
library(tidyverse) 
library(conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


#Truyền thư viện đọc và đọc table data
library(readr)
Divvy_Trips_2019_Q1_1_ <- read_csv("Divvy_Trips_2019_Q1  (1).csv")
Divvy_Trips_2020_Q1 <- read_csv("Divvy_Trips_2020_Q1.csv")


#So sánh tên bảng
colnames(Divvy_Trips_2019_Q1_1_ )
colnames(Divvy_Trips_2020_Q1)


# Đổi tên cột trong table 
(Divvy_Trips_2019_Q1_1_  <- rename(Divvy_Trips_2019_Q1_1_ 
                                   ,ride_id = trip_id
                                   ,rideable_type = bikeid
                                   ,started_at = start_time
                                   ,ended_at = end_time
                                   ,start_station_name = from_station_name
                                   ,start_station_id = from_station_id
                                   ,end_station_name = to_station_name
                                   ,end_station_id = to_station_id
                                   ,member_casual = usertype))


# Inspect the dataframes and look for incongruencies
str(Divvy_Trips_2019_Q1_1_)
str(Divvy_Trips_2020_Q1)



#Convert ride_id and rideable_type to character so that they can stack correctly
Divvy_Trips_2019_Q1_1_ <- mutate(Divvy_Trips_2019_Q1_1_, ride_id = as.character(ride_id)
                                 ,rideable_type = as.character(rideable_type))


# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(Divvy_Trips_2019_Q1_1_, Divvy_Trips_2020_Q1)


# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))


# Inspect the new table that has been created
colnames(all_trips) #List of column names
nrow(all_trips) #How many rows are in data frame?
dim(all_trips) #Dimensions of the data frame?
head(all_trips) #See the first 6 rows of data frame. Also tail(all_trips)
str(all_trips) #See list of columns and data types (numeric, character, etc)
summary(all_trips) #Statistical summary of data. Mainly for numerics



# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))


# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)




# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing
#these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)


# Inspect the structure of the columns
str(all_trips)


# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


# Remove "bad" data
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)


# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

#Kiểm tra giá trị thực tế của cột 
unique(all_trips_v2$day_of_week)

# Khi truyền vào giá trị nó chuyển NA, chưa fix được
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sun", "Mon",
                                                                       "Tues", "Wednes", 
                                                                       "Thurs", "Fri", "Satur"))


# analyze ridership data by type and weekday
all_trips_v2 %>%
  mutate(
    started_at = ymd_hms(started_at),     # ép từ chr -> datetime
    weekday = wday(started_at, label = TRUE, abbr = FALSE)  # tạo weekday
  ) %>%
  group_by(member_casual, weekday) %>%
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length, na.rm = TRUE)
  ) %>%
  arrange(member_casual, weekday)


# analyze ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#Tạo hiệu xuất
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#Xuất file
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
                      all_trips_v2$day_of_week, FUN = mean)


# Xuất file vào Desktop
write.csv(counts, "E:/R_Case Study 1/Cyclistic-Bike-Share-Analysis/avg_ride_length.csv", row.names = FALSE)

