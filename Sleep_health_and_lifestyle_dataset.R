# installing all the libraries
install.packages('dplyr')
library(dplyr)
install.packages('tidyr')
library(tidyr)
install.packages('tidyverse')
library(tidyverse)
install.packages('crayon')
library(crayon)
install.packages("ggplot2")
library(ggplot2)
install.packages('plotly')
library(plotly)
install.packages('plotrix')
library(plotrix)
installed.packages('datasets')
library(datasets)
library(MASS)
library(stats)

# Loading the dataset and manipulating it for further analysis
df<- read.csv("C:\\Users\\rehan\\Desktop\\Projects\\sleep_health_and_lifestyle_dataset\\Sleep_health_and_lifestyle_dataset.csv")
head(df)
df<-df[,-c(0:1)]
head(df)
df <- df %>%
  mutate(BMI.Category = ifelse(BMI.Category == "Normal Weight", "Normal", BMI.Category))
df <- df %>%
  separate(Blood.Pressure, into = c("systolic", "diastolic"), sep = "/", convert = TRUE) %>%
  mutate(
    Blood.Pressure_status = case_when(
      systolic > 120 | diastolic > 80 ~ "High",
      systolic < 100 | diastolic < 60 ~ "Low",
      TRUE ~ "Normal"
    )
  )
head(df)
df<-df[,-c(9:10)]
str(df)
any(is.na(df))
summary(df)
head(df)
View(df)

# Creating a plot to analyze Stress Level by Occupation and Gender
ggplot(df, aes(x = Occupation, fill = Sleep.Disorder)) +
  geom_bar(position = "dodge") +
  facet_grid(Gender ~ .) +  # Gender as rows
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    plot.background = element_rect(fill = "grey", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "grey30"),
    panel.grid.minor = element_line(color = "grey10"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )


# Analyzing the relationship between sleep duration and stress level by age group and gender
s1 <- ggplot(df, aes(x = Age, y = Sleep.Duration, color = as.factor(Gender))) +
  geom_line() + geom_point() +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "grey", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "grey20"),
    panel.grid.minor = element_line(color = "grey10"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )
s2 <- ggplot(df, aes(x = Age, y = Stress.Level, color = as.factor(Gender))) +
  geom_line() + geom_point() +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "grey", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "grey20"),
    panel.grid.minor = element_line(color = "grey10"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )

s1 / s2

# Exploring how Body Mass Index (BMI) correlates with Blood Pressure across 
# different age groups and genders
b1<- ggplot(df, aes(x = BMI.Category, y = Age, fill = Gender)) +
  geom_violin(trim = TRUE, scale = "width") +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "grey", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "grey30"),
    panel.grid.minor = element_line(color = "grey10"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  ) +
  labs(
    title = "Distribution of Age by BMI and Gender",
    x = "BMI",
    y = "Age",
    fill = "Gender"
  )
b2<- ggplot(df, aes(x = Blood.Pressure_status, y = Age, fill = Gender)) +
  geom_violin(trim = TRUE, scale = "width") +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "grey", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "grey30"),
    panel.grid.minor = element_line(color = "grey10"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  ) +
  labs(
    title = "Distribution of Age by Blood Pressure Status and Gender",
    x = "Blood Pressure Status",
    y = "Age",
    fill = "Gender"
  )
  
b1 / b2

# Analyzing the impact of daily step count on heart health
#h1 <- ggplot(df, aes(x = Age, y = Daily.Steps, color = as.factor(Gender))) +
#geom_line() + geom_point() +
h1 <- ggplot(df, aes(x = Age, y = Daily.Steps, fill = as.factor(Gender))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "grey", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "grey20"),
    panel.grid.minor = element_line(color = "grey10"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )
h2 <- ggplot(df, aes(x = Age, y = Heart.Rate, color = as.factor(Gender))) +
  geom_line() + geom_point() +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "grey", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "grey20"),
    panel.grid.minor = element_line(color = "grey10"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )

h1 / h2

