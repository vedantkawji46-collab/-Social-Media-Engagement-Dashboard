
# Load Libraries

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)


# Load Data

df <- read_excel("D:/Book2.xlsx")


# Convert & Clean

df$post_datetime <- ymd_hms(df$post_datetime)

df <- df %>%
  filter(!is.na(post_datetime),
         !is.na(likes),
         !is.na(comments),
         !is.na(reach),
         !is.na(engagement_rate))


# Feature Engineering

df <- df %>%
  mutate(
    hour = hour(post_datetime),
    day = wday(post_datetime, label = TRUE),
    month = month(post_datetime, label = TRUE),
    total_engagement = likes + comments + shares + saves
  )


# Basic Checks

str(df)
summary(df)


# ANALYSIS (ACTUAL RESULTS)


# Best Posting Hour
best_hour <- df %>%
  group_by(hour) %>%
  summarise(avg_engagement = mean(engagement_rate, na.rm = TRUE)) %>%
  arrange(desc(avg_engagement))

print(best_hour)

# Best Posting Day
best_day <- df %>%
  group_by(day) %>%
  summarise(avg_engagement = mean(engagement_rate, na.rm = TRUE)) %>%
  arrange(desc(avg_engagement))

print(best_day)

# Best Content Category
best_content <- df %>%
  group_by(content_category) %>%
  summarise(avg_engagement = mean(engagement_rate, na.rm = TRUE)) %>%
  arrange(desc(avg_engagement))

print(best_content)

# Correlation Analysis
correlation <- cor(df$reach, df$engagement_rate, use = "complete.obs")
print(paste("Correlation between Reach and Engagement Rate:", correlation))


# VISUALIZATIONS


# 1. Engagement by Hour
ggplot(df, aes(x = hour, y = engagement_rate)) +
  stat_summary(fun = mean, geom = "bar", fill = "blue") +
  labs(title = "Engagement Rate by Hour")

# 2. Engagement by Day
ggplot(df, aes(x = day, y = engagement_rate)) +
  stat_summary(fun = mean, geom = "bar", fill = "green") +
  labs(title = "Engagement Rate by Day")

# 3. Reach vs Engagement
ggplot(df, aes(x = reach, y = engagement_rate)) +
  geom_point(alpha = 0.5) +
  labs(title = "Reach vs Engagement")

# 4. Monthly Trend
ggplot(df, aes(x = month, y = engagement_rate)) +
  stat_summary(fun = mean, geom = "line", group = 1) +
  labs(title = "Monthly Engagement Trend")

# 5. Content Category Performance
ggplot(df, aes(x = content_category, y = engagement_rate)) +
  stat_summary(fun = mean, geom = "bar", fill = "orange") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Engagement by Content Category")


# KEY INSIGHTS (OUTPUT)


cat("\n--- KEY INSIGHTS ---\n")

cat("\nBest Posting Hour:\n")
print(best_hour[1, ])

cat("\nBest Posting Day:\n")
print(best_day[1, ])

cat("\nTop Performing Content Category:\n")
print(best_content[1, ])

cat("\nInsight:\n")
cat("Higher reach does NOT always guarantee higher engagement.\n")

# Export Clean Data

write.csv(df, "D:/cleaned_data.csv", row.names = FALSE)
