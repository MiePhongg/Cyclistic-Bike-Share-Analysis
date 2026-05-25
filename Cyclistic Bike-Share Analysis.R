# ============================================================
# Cyclistic Bike-Share Analysis
# ============================================================

# Load libraries
library(tidyverse)
library(conflicted)
library(readr)
library(lubridate)

# Resolve function conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("lag",    "dplyr")


# ============================================================
# 1. LOAD DATA
# ============================================================

Divvy_Trips_2019_Q1 <- read_csv("E:/case study data/R_Case Study 1/Cyclistic-Bike-Share-Analysis/Divvy_Trips_2019_Q11.csv")
Divvy_Trips_2020_Q1 <- read_csv("E:/case study data/R_Case Study 1/Cyclistic-Bike-Share-Analysis/Divvy_Trips_2020_Q1.csv")


# ============================================================
# 2. ALIGN COLUMN NAMES (rename 2019 to match 2020 schema)
# ============================================================

colnames(Divvy_Trips_2019_Q1)
colnames(Divvy_Trips_2020_Q1)

Divvy_Trips_2019_Q1 <- rename(Divvy_Trips_2019_Q1,
                              ride_id            = trip_id,
                              rideable_type      = bikeid,
                              started_at         = start_time,
                              ended_at           = end_time,
                              start_station_name = from_station_name,
                              start_station_id   = from_station_id,
                              end_station_name   = to_station_name,
                              end_station_id     = to_station_id,
                              member_casual      = usertype
)


# ============================================================
# 3. INSPECT & FIX DATA TYPES
# ============================================================

str(Divvy_Trips_2019_Q1)
str(Divvy_Trips_2020_Q1)

# Convert ride_id and rideable_type to character for consistent stacking
Divvy_Trips_2019_Q1 <- mutate(Divvy_Trips_2019_Q1,
                              ride_id       = as.character(ride_id),
                              rideable_type = as.character(rideable_type)
)


# ============================================================
# 4. COMBINE INTO ONE DATA FRAME
# ============================================================

all_trips <- bind_rows(Divvy_Trips_2019_Q1, Divvy_Trips_2020_Q1)

# Drop columns not present in 2020 data
all_trips <- all_trips %>%
  select(-any_of(c("start_lat", "start_lng", "end_lat", "end_lng",
                   "birthyear", "gender", "tripduration")))


# ============================================================
# 5. STANDARDIZE MEMBER LABELS
# ============================================================

all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer"   = "casual"
  ))

# Verify counts
table(all_trips$member_casual)


# ============================================================
# 6. FEATURE ENGINEERING
# ============================================================

# Parse timestamps
all_trips <- all_trips %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at   = ymd_hms(ended_at)
  )

# Extract date components + season + time_of_day
all_trips <- all_trips %>%
  mutate(
    date        = as.Date(started_at),
    month       = format(date, "%m"),
    day         = format(date, "%d"),
    year        = format(date, "%Y"),
    day_of_week = wday(started_at, label = TRUE, abbr = FALSE),
    weekday     = factor(
      wday(started_at, label = TRUE, abbr = TRUE),
      levels  = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
      ordered = TRUE
    ),
    hour        = hour(started_at),
    
    # Season — used in CASE 4 & 7
    month_int   = as.integer(format(date, "%m")),
    season      = factor(
      case_when(
        month_int %in% c(3,4,5)   ~ "Spring",
        month_int %in% c(6,7,8)   ~ "Summer",
        month_int %in% c(9,10,11) ~ "Fall",
        TRUE                       ~ "Winter"
      ),
      levels = c("Spring","Summer","Fall","Winter")
    ),
    
    # Time of day — used in CASE 6
    time_of_day = factor(
      case_when(
        hour %in% 5:11  ~ "Morning",
        hour %in% 12:17 ~ "Afternoon",
        hour %in% 18:21 ~ "Evening",
        TRUE            ~ "Night"
      ),
      levels = c("Morning","Afternoon","Evening","Night")
    ),
    
    # Ride length in SECONDS (kept for aggregate stats)
    ride_length     = as.numeric(difftime(ended_at, started_at, units = "secs")),
    
    # Ride length in MINUTES (used in all visualizations)
    ride_length_min = ride_length / 60
  )


# ============================================================
# 7. REMOVE BAD DATA
# ============================================================

all_trips_v2 <- all_trips %>%
  filter(
    start_station_name != "HQ QR" | is.na(start_station_name),
    ride_length >= 0          # remove negative durations
  )

# Verify removal
nrow(all_trips) - nrow(all_trips_v2)


# ============================================================
# 8. DESCRIPTIVE STATISTICS
# ============================================================

summary(all_trips_v2$ride_length_min)

# Mean / median / max / min by member type (in minutes)
aggregate(ride_length_min ~ member_casual, data = all_trips_v2, FUN = mean)
aggregate(ride_length_min ~ member_casual, data = all_trips_v2, FUN = median)
aggregate(ride_length_min ~ member_casual, data = all_trips_v2, FUN = max)
aggregate(ride_length_min ~ member_casual, data = all_trips_v2, FUN = min)

# Average ride length by member type x day of week (minutes)
aggregate(ride_length_min ~ member_casual + day_of_week,
          data = all_trips_v2, FUN = mean)


# ============================================================
# 9. VISUALIZATIONS
# Colour palette: casual = #F28E2B (orange), member = #4E79A7 (blue)
# ============================================================

# ── CASE 1: Number of Rides by Weekday & Member Type ────────────────────────

all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE, abbr = TRUE, week_start = 1)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides  = n(),
            average_duration = mean(ride_length_min, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("casual" = "#F28E2B", "member" = "#4E79A7")) +
  labs(
    title    = "Number of Rides by Weekday and Member Type",
    subtitle = "Annual members dominate weekday rides; Casual riders more active on weekends",
    x        = "Day of Week",
    y        = "Number of Rides",
    fill     = "Member Type",
  ) +
  theme_minimal(base_size = 13)


# ── CASE 2: Average Ride Duration by Weekday & Member Type ──────────────────


all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE, abbr = TRUE, week_start = 1)) %>%
  group_by(member_casual, weekday) %>%
  summarise(average_duration_min = mean(ride_length_min, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration_min, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("casual" = "#F28E2B", "member" = "#4E79A7")) +
  labs(
    title    = "Average Ride Duration by Weekday and Member Type",
    subtitle = "Casual riders average ~2x longer trips than members across all days",
    x        = "Day of Week",
    y        = "Average Duration (minutes)",   # <-- FIXED: seconds → minutes
    fill     = "Member Type",
  ) +
  theme_minimal(base_size = 13)


# ── CASE 3: Number of Rides by Month & Member Type ──────────────────────────


all_trips_v2 %>%
  mutate(month_label = factor(
    format(as.Date(started_at), "%b"),
    levels = c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
  )) %>%
  group_by(member_casual, month_label) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = month_label, y = number_of_rides,
             color = member_casual, group = member_casual)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("casual" = "#F28E2B", "member" = "#4E79A7")) +
  labs(
    title    = "Number of Rides by Month and Member Type",
    subtitle = "Both types peak in summer (Jul); lowest in winter (Dec–Feb)",
    x        = "Month",
    y        = "Number of Rides",
    color    = "Member Type",

  ) +
  theme_minimal(base_size = 13)


# ── CASE 4: Number of Rides by Season & Member Type ─────────────────────────


all_trips_v2 %>%
  group_by(member_casual, season) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = season, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("casual" = "#F28E2B", "member" = "#4E79A7")) +
  labs(
    title    = "Number of Rides by Season and Member Type",
    subtitle = "Summer dominates total rides; Casual ridership drops more sharply in Winter",
    x        = "Season",
    y        = "Number of Rides",
    fill     = "Member Type",
  ) +
  theme_minimal(base_size = 13)


# ── CASE 5: Average Ride Duration by Member Type (summary bar) ──────────────


all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(average_duration_min = mean(ride_length_min, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = member_casual, y = average_duration_min, fill = member_casual)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(average_duration_min, 1), " min")),
            vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("casual" = "#F28E2B", "member" = "#4E79A7")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Average Ride Duration by Member Type",
    subtitle = "Casual riders take trips nearly twice as long as Annual members",
    x        = "Member Type",
    y        = "Average Duration (minutes)",
  ) +
  theme_minimal(base_size = 13)


# ── CASE 6: Number of Rides by Hour of Day & Member Type ────────────────────


all_trips_v2 %>%
  group_by(member_casual, hour) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = hour, y = number_of_rides,
             color = member_casual, group = member_casual)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("casual" = "#F28E2B", "member" = "#4E79A7")) +
  labs(
    title    = "Number of Rides by Hour of Day and Member Type",
    subtitle = "Annual members show commute peaks (8AM & 5PM); Casual rises through the afternoon",
    x        = "Hour of Day (0 = midnight)",
    y        = "Number of Rides",
    color    = "Member Type",
  ) +
  theme_minimal(base_size = 13)


# ── CASE 7: Average Ride Duration by Season & Member Type ───────────────────

all_trips_v2 %>%
  group_by(member_casual, season) %>%
  summarise(average_duration_min = mean(ride_length_min, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = season, y = average_duration_min, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("casual" = "#F28E2B", "member" = "#4E79A7")) +
  labs(
    title    = "Average Ride Duration by Season and Member Type",
    subtitle = "Casual members ride longest in Spring; Annual members peak in Summer",
    x        = "Season",
    y        = "Average Duration (minutes)",
    fill     = "Member Type",
  ) +
  theme_minimal(base_size = 13)


# ============================================================
# 10. EXPORT SUMMARY FILE
# ============================================================

counts <- all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    number_of_rides      = n(),
    avg_ride_length_min  = mean(ride_length_min, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(counts,
          "E:/case study data/R_Case Study 1/Cyclistic-Bike-Share-Analysis/1avg_ride_length.csv",
          row.names = FALSE)

message("Export complete.")
