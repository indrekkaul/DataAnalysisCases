# Bike Share Case Study

## Introduction
This project analyzes the differences in usage patterns between casual riders and annual members of Cyclistic, a fictional bike-share company.

## Scenario
The director of marketing believes the company’s future success depends on maximizing the number of annual memberships.
Therefore, I want to understand how casual riders and annual members use Cyclistic bikes differently. From these
insights, I need to design a new marketing strategy to convert casual riders into annual members. But first, 
Cyclistic executives must approve my recommendations, so they must be backed up with compelling data insights and professional data visualizations.
The director has assigned to me the first question to answer: How do annual members and casual riders use Cyclistic bikes differently?

## Ask
1. How do annual members and casual riders use Cyclistic bikes differently?
2. Whay would casual riders buy annual membership?

## Prepare
1. Download Cyclistic's historical trip data to anlayze. -> I desided to download 1 year period from May 2023 to April 2024.
   [Cyclistic's historical trip data is available here.](https://divvy-tripdata.s3.amazonaws.com/index.html)
---
title: "Bike_Ride_Case_Study"
author: "Indrek Kaul"
date: "2024-05-16"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Bike Ride Case Study

# Introduction

This project analyzes the differences in usage patterns between casual riders and annual members of Cyclistic bike-share company.

# Scenario

The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, I want to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, I need to design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve my recommendations, so they must be backed up with compelling data insights and professional data visualizations. The director has assigned to me the first question to answer: How do annual members and casual riders use Cyclistic bikes differently?

# Ask

1.  How do annual members and casual riders use Cyclistic bikes differently?
2.  Why would casual riders buy an annual membership?

# Prepare and clean data

1.  Download Cyclistic's historical trip data to analyze. -\> I decided to download 1 year period from May 2023 to April 2024. [Cyclistic's historical trip data is available here](https://divvy-tripdata.s3.amazonaws.com/index.html)
2.  Analyze what format the data is. -\> Data is divided into 12 CSV files with one-month periods. Each file contains \~600,000 records. I decided to use R for cleaning and analyses.
3.  Import data.

```{r}
data1 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202305-divvy-tripdata.csv")
data2 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202306-divvy-tripdata.csv")
data3 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202307-divvy-tripdata.csv")
data4 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202308-divvy-tripdata.csv")
data5 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202309-divvy-tripdata.csv")
data6 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202310-divvy-tripdata.csv")
data7 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202311-divvy-tripdata.csv")
data8 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202312-divvy-tripdata.csv")
data9 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202401-divvy-tripdata.csv")
data10 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202402-divvy-tripdata.csv")
data11 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202403-divvy-tripdata.csv")
data12 <- read.csv("C:/Users/indre/Desktop/analy/R-Programming/Case Study/tripdata_datasets/202404-divvy-tripdata.csv")
```

4.  Combine data into a single data set.

```{r}
library(tidyverse)
tripdata_from_202305_to_202404 <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12)
```

5.  View how many empty fields are in the data set.

```{r}
missing_values <- sapply(tripdata_from_202304_to_202404, function(x) sum(is.na(x)))
missing_values_df <- data.frame(Columns = names(missing_values), MissingCount = missing_values)
print(missing_values_df)
```

#### This shows that the data set is missing some values in "end_lat" and in "end_lng" columns. I decided that for the current analysis, this is irrelevant.

5.  Verify that date and time filed is in correct format.

```{r}
str(tripdata_from_202305_to_202404)
```

#### The "started_at" and "ended_at" is in TEXT. For the analysis I need to convert to right format.

6.  Convert "started_at" and "ended_at" fields in to dateTime format.

```{r}
tripdata_from_202305_to_202404$started_at <- as.POSIXct(tripdata_from_202305_to_202404$started_at, format="%Y-%m-%d %H:%M:%S")
tripdata_from_202305_to_202404$ended_at <- as.POSIXct(tripdata_from_202305_to_202404$ended_at, format="%Y-%m-%d %H:%M:%S")
```

7.  Add a new column "trip_duration" and calculate

```{r}
tripdata_from_202305_to_202404$trip_duration <- difftime(tripdata_from_202305_to_202404$ended_at, tripdata_from_202305_to_202404$started_at, units = "mins")
```

8.  Add a new column "weekday"

```{r}
library(lubridate)
tripdata_from_202305_to_202404$weekday <- wday(tripdata_from_202305_to_202404$started_at, label = TRUE, week_start = 1)
```

9.  Add a new column "hour"

```{r}
library(lubridate)
tripdata_from_202305_to_202404$hour <- hour(tripdata_from_202305_to_202404$started_at)
```

7.  Add a new column "ride_length"
```{r}
tripdata_from_202305_to_202404 <- tripdata_from_202305_to_202404 %>%
    mutate(ride_length_minutes = as.numeric(difftime(ended_at, started_at, units = "mins")))
``` 
10.  Calculate average ride lenght for casual and for members
```{r}
casual_avg_ride_length <- tripdata_from_202305_to_202404 %>%
     filter(member_casual == "casual" & ride_length_minutes > 0) %>%
     summarise(avg_ride_length = mean(ride_length_minutes))

member_avg_ride_length <- tripdata_from_202305_to_202404 %>%
     filter(member_casual == "member" & ride_length_minutes > 0) %>%
     summarise(avg_ride_length = mean(ride_length_minutes))

avg_ride_lengths <- data.frame(
     user_type = c("Casual", "Member"),
    avg_ride_length = c(casual_avg_ride_length$avg_ride_length, member_avg_ride_length$avg_ride_length))
``` 

## Analayse data

1.  Wich day member and casual user differ

```{r Weekday, echo=FALSE}
library(ggplot2)
library(scales)
ggplot(tripdata_from_202305_to_202404, aes(x = weekday, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Cyclistic kasutajate sõidud nädalapäevade lõikes",
       x = "",
       y = "Sõitude arv",
       fill = "Kasutajatüüp") +
  scale_y_continuous(labels = unit_format(unit = 'M', scale = 1e-6)) +
  theme_minimal()
```

2.  In wich hour they start raiding

```{r Hour, echo=FALSE}
ggplot(tripdata_from_202305_to_202404, aes(x = factor(hour), fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Cyclistic kasutajate sõidud tundide lõikes",
       x = "Tund",
       y = "Sõitude arv",
       fill = "Kasutajatüüp") +
  scale_y_continuous(labels = unit_format(unit = 'M', scale = 1e-6)) +
  theme_minimal()
```

3.  Show usage casual user rides in weekends R, L and P

```{r R;L;P,  echo=FALSE}
tripdata_from_202305_to_202404 %>%
  filter(weekday %in% c("L", "P")) %>%
  filter(member_casual == "casual") %>%
  ggplot(aes(x = factor(hour), fill = weekday)) +
  geom_bar(position = "dodge") +
  labs(title = "Cyclistic casual sõidud tundide lõikes Nädalavahetusel",
       x = "Tund",
       y = "Sõitude arv",
       fill = "Nädalapäev") +
  scale_y_continuous(labels = unit_format(unit = 'M', scale = 1e-6)) +
  facet_wrap(~ weekday, ncol = 1) +
  theme_minimal()
```

```{r R;L;P,  echo=FALSE}
tripdata_from_202305_to_202404 %>%
  filter(weekday %in% c("L", "P")) %>%
  filter(member_casual == "member") %>%
  ggplot(aes(x = factor(hour), fill = weekday)) +
  geom_bar(position = "dodge") +
  labs(title = "Cyclistic member sõidud tundide lõikes Nädalavahetusel",
       x = "Tund",
       y = "Sõitude arv",
       fill = "Nädalapäev") +
  scale_y_continuous(labels = unit_format(unit = 'M', scale = 1e-6)) +
  facet_wrap(~ weekday, ncol = 1) +
  theme_minimal()
```

4.  Show usage casual user rides in workdays E,T,K,N,

```{r R;L;P,  echo=FALSE}
tripdata_from_202305_to_202404 %>%
  filter(weekday %in% c("E", "T", "K", "N", "R")) %>%
  filter(member_casual == "casual") %>%
  ggplot(aes(x = factor(hour), fill = weekday)) +
  geom_bar(position = "dodge") +
  labs(title = "Cyclistic casual sõidud tundide lõikes",
       x = "Tund",
       y = "Sõitude arv",
       fill = "Nädalapäev") +
  scale_y_continuous(labels = unit_format(unit = 'M', scale = 1e-6)) +
  facet_wrap(~ weekday, ncol = 1) +
  theme_minimal()
```

```{r R;L;P,  echo=FALSE}
tripdata_from_202305_to_202404 %>%
  filter(weekday %in% c("E", "T", "K", "N", "R")) %>%
  filter(member_casual == "member") %>%
  ggplot(aes(x = factor(hour), fill = weekday)) +
  geom_bar(position = "dodge") +
  labs(title = "Cyclistic member sõidud tundide lõikes",
       x = "Tund",
       y = "Sõitude arv",
       fill = "Nädalapäev") +
  scale_y_continuous(labels = unit_format(unit = 'M', scale = 1e-6)) +
  facet_wrap(~ weekday, ncol = 1) +
  theme_minimal()
```
4.  Show usage member rides in workdays E,T,K,N,

```{r R;L;P,  echo=FALSE}
tripdata_from_202305_to_202404 %>%
  filter(weekday %in% c("E", "T", "K", "N", "R")) %>%
  filter(member_casual == "member") %>%
  ggplot(aes(x = factor(hour), fill = weekday)) +
  geom_bar(position = "dodge") +
  labs(title = "Cyclistic member sõidud tundide lõikes",
       x = "",
       y = "Sõitude arv",
       fill = "Week Day") +
  scale_y_continuous(labels = unit_format(unit = 'M', scale = 1e-6)) +
  facet_wrap(~ weekday, ncol = 1) +
  theme_minimal()
```

5.  Show member top 15 starting stations

```{r}
tripdata_from_202305_to_202404 %>%
  filter(member_casual == "member" & start_station_name != "") %>%
  group_by(start_station_name) %>%
  summarise(total_trips = n()) %>%
  arrange(desc(total_trips)) %>%
  slice_head(n = 20) %>%
  mutate(start_station_name = reorder(start_station_name, total_trips)) %>% 
  ggplot(aes(x = start_station_name, y = total_trips)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 15 member starting stations",
       x = "",
       y = "") +
  scale_fill_manual(values = c("blue")) +
  coord_flip()

```

6.  Show casual top 15 starting stations

```{r}
tripdata_from_202305_to_202404 %>%
  filter(member_casual == "casual" & start_station_name != "") %>%
  group_by(start_station_name) %>%
  summarise(total_trips = n()) %>%
  arrange(desc(total_trips)) %>%
  slice_head(n = 20) %>%
  mutate(start_station_name = reorder(start_station_name, total_trips)) %>% 
  ggplot(aes(x = start_station_name, y = total_trips)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Top 15 casual starting stations",
       x = "",
       y = "") +
  scale_fill_manual(values = c("red")) +
  coord_flip()

```

7.  Show casual top 15 starting stations in workdays

```{r}
tripdata_from_202305_to_202404 %>%
  filter(member_casual == "member" & start_station_name != "") %>%
  filter(weekday %in% c("E", "T", "K", "N", "R")) %>%
  group_by(start_station_name) %>%
  summarise(total_trips = n()) %>%
  arrange(desc(total_trips)) %>%
  slice_head(n = 20) %>%
  mutate(start_station_name = reorder(start_station_name, total_trips)) %>% 
  ggplot(aes(x = start_station_name, y = total_trips)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 15 casual starting stations in workdays",
       x = "",
       y = "") +
  scale_fill_manual(values = c("blue")) +
  coord_flip()

```

8.  Show member top 15 starting stations in workdays

```{r}
tripdata_from_202305_to_202404 %>%
  filter(member_casual == "casual" & start_station_name != "") %>%
  filter(weekday %in% c("E", "T", "K", "N", "R")) %>%
  group_by(start_station_name) %>%
  summarise(total_trips = n()) %>%
  arrange(desc(total_trips)) %>%
  slice_head(n = 20) %>%
  mutate(start_station_name = reorder(start_station_name, total_trips)) %>% 
  ggplot(aes(x = start_station_name, y = total_trips)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Top 15 casual starting stations", subtitle =  "in workdays",
       x = "",
       y = "") +
  scale_fill_manual(values = c("red")) +
  coord_flip()

```

9.  Show casual top 15 starting stations in weekends

```{r}
tripdata_from_202305_to_202404 %>%
  filter(member_casual == "casual" & start_station_name != "") %>%
  filter(weekday %in% c("L", "P")) %>%
  group_by(start_station_name) %>%
  summarise(total_trips = n()) %>%
  arrange(desc(total_trips)) %>%
  slice_head(n = 20) %>%
  mutate(start_station_name = reorder(start_station_name, total_trips)) %>% 
  ggplot(aes(x = start_station_name, y = total_trips)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Top 15 casual starting stations", subtitle = "in weekends",
       x = "",
       y = "") +
  scale_fill_manual(values = c("red")) +
  coord_flip()

```

10. Show member top 15 starting stations in weekends

```{r}
tripdata_from_202305_to_202404 %>%
  filter(member_casual == "member" & start_station_name != "") %>%
  filter(weekday %in% c("L", "P")) %>%
  group_by(start_station_name) %>%
  summarise(total_trips = n()) %>%
  arrange(desc(total_trips)) %>%
  slice_head(n = 20) %>%
  mutate(start_station_name = reorder(start_station_name, total_trips)) %>% 
  ggplot(aes(x = start_station_name, y = total_trips)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 15 member starting stations", subtitle = "in weekends",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(size = 12, face = "bold"), axis.text.y = element_text(size = 11, face = "bold")) +
  scale_fill_manual(values = c("blue")) +
  coord_flip()

```

11. Show calual and member average trip duration
```{r}
ggplot(avg_ride_lengths, aes(x = user_type, y = avg_ride_length, fill = user_type)) +
     geom_bar(stat = "identity") +
     geom_text(aes(label = paste(round(avg_ride_length, 2), "min")), 
               position = position_stack(vjust = 0.5), 
               color = "black", 
               size = 5) + # Lisa see rida, et muuta teksti suurust
     labs(title = "Keskmine sõidupikkus kasutaja lõikes",
          x = "",
          y = "Minutites",
          fill = "") +
     theme_minimal()
```

EXPORT
```{r}
write.csv(tripdata_from_202305_to_202404, "tripdata_from_202305_to_202404.csv", row.names = TRUE)
```
