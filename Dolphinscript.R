install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)


install.packages("tidyr")
library(tidyr)

color01 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c')
color02 <- c('#f1eef6','#d0d1e6','#a6bddb', '#74a9cf', '#2b8cbe', '#045a8d')



d_data <- read_excel("/Users/ebell23/Downloads/EB_EastCoast_Bottlenose_2017-2019.xlsx", sheet = 2)
str(d_data)

colnames(d_data)

table(d_data$Findings_of_HI)      #get # of variables in his column (CBD, Y, N)

#Addingg Interaction Type into dataset by combining columns  
d_data2 <- d_data %>%       #add Interaction Type
  select(`Findings_of_HI`, `Boat_Collision`, `Fishery_Interaction`, `Other_HI`) %>%
  pivot_longer(cols = everything(), names_to = "Interaction_Type", values_to = "Findings_of_HI") %>%
  filter(!is.na(Findings_of_HI)) %>%
  group_by(Interaction_Type, Findings_of_HI) %>%
  summarise(Count = n(), .groups = "drop")        #drop # of occurences

unique(d_data2$Interaction_Type)      #checking that it's in dataset


#Histogram of Findings of HI by Count wit the types 
HI_histo <- ggplot(d_data2, aes(x = Findings_of_HI, y = Count, fill = Interaction_Type)) +
  geom_col(colour="gray20", alpha=1.0 ) +
  ylab("Count") +
  xlab("Findings of HI") +
  scale_fill_manual(values = color01) + 
  facet_wrap(~Interaction_Type) +
  theme_minimal()
HI_histo 

#Without facet wrap
ggp <- ggplot(data = d_data2, aes(x = Findings_of_HI, y = Count, fill = Interaction_Type) ) #simple way to do it
ggp
ggp + geom_col(alpha = 0.4) +
  ggtitle("Findings of Human Interaction with Bottlenose Dolphin Strandings") +
  labs(x = "Findings of Human Interaction", y = "# of Findings") 

#With facet wrap
ggp <- ggplot(data = d_data2, aes(x = Findings_of_HI, y = Count, fill = Interaction_Type) ) #simple way to do it
ggp
ggp + geom_col(alpha = 0.4) +
  ggtitle("Findings of Human Interaction with Bottlenose Dolphin Strandings") +
  labs(x = "Findings of Human Interaction", y = "# of Findings") +
  facet_wrap(~Interaction_Type, scales = "free")    #each subplot to have its own scales

#Statistics Metrics
summary_table <- d_data2 %>%
  group_by(Interaction_Type, Findings_of_HI) %>%
  summarise(Total_Count = sum(Count), .groups = "drop")

summary(d_data2$Count)
print(summary_table)

#Bar graph for 
d_data %>% ggplot(aes(x = Findings_of_HI)) +      
  geom_bar()         
    
#Bar graoh of Observation Status with color
obs <- d_data %>% ggplot(aes(x = Observation_Status, fill = Observation_Status )) +
  geom_bar() +
    theme_minimal() + labs (x = "Observation Status", y = "# of Dolphins Observed", title = "Number of Dolphine Observed as Stranded") +
                               scale_fill_manual(values = color02)
obs       

#AgexSex Class

#Adding a new column
d_data_summary <- d_data %>%
  filter(!is.na(Sex) & !is.na(Age_Class)) %>%  # Remove NA values
  group_by(Sex, Age_Class) %>%
  summarise(Count = n(), .groups = "drop")  # Create the Count column
print(d_data_summary)

d_data_summary$Count <- as.numeric(d_data_summary$Count)      #makes sure all count is in numeric values


#Graphing it
sex_class1 <- ggplot(data = d_data_summary, aes(x = Sex, y = Count, fill = Age_Class))
sex_class1
sex_class1 + geom_col(alpha = 0.6) +
  ggtitle("Sex and Age Class Distributions") +
  labs(x = "Sex", y = "# of Strandings")
#with facet wrap
sex_class1 <- ggplot(data = d_data_summary, aes(x = Sex, y = Count, fill = Age_Class))
sex_class1
sex_class1 + geom_col(alpha = 0.6) +
  ggtitle("Sex and Age Class Distributions") +
  labs(x = "Sex", y = "# of Strandings") +
  facet_wrap(~Age_Class, scales = "free")


summary_table <- d_data_summary %>%
  group_by(Age_Class, Sex) %>%
  summarise(Total_Count = sum(Count), .groups = "drop")
summary_table


# Convert year to factor or integer
dolphin_data <- d_data %>% mutate(year = as.integer(Year_of_Observation))

# **1. Dolphin Strandings Over the Years**
trend_plot <- ggplot(dolphin_data, aes(x = year)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Dolphin Strandings Over the Years", 
       x = "Year", y = "Number of Strandings") +
  theme_minimal()
print(trend_plot)

# ** Chi-Square for Obs and Findings
table(d_data$Observation_Status, d_data$Findings_of_HI)       #Check distribution
d_data$Observation_Status <- as.factor(d_data$Observation_Status)
d_data$Findings_of_HI <- as.factor(d_data$Findings_of_HI)
contingency_table <- table(d_data$Observation_Status, d_data$Findings_of_HI)
print(contingency_table)
chi_test <- chisq.test(contingency_table)
print(chi_test)

chi_test$expected  # View expected values
#Visualize Relationship
ggplot(d_data, aes(x = Observation_Status, fill = Findings_of_HI)) +
  geom_bar(position = "dodge") +  
  labs(title = "Observation Status vs. Human Interaction Findings",
       x = "Observation Status", y = "Count") +
  theme_minimal()

table(d_data$Observation_Date)
 
# ** Trends in dolphin strandings over months or years     
library(dplyr)
library(lubridate)

# Convert date column to Date format
d_data$Observation_Date <- as.Date(d_data$Observation_Date, format="%Y-%m-%d")  

# Extract Year and Month
d_data$Year <- year(d_data$Observation_Date)
d_data$Month <- month(d_data$Observation_Date, label = TRUE)  # 'label=TRUE' gives month names
#Count Strandings per year
strandings_per_year <- d_data %>%
  group_by(Year) %>%
  summarise(Count = n(), .groups = "drop")

print(strandings_per_year)

year <- ggplot(strandings_per_year, aes(x = Year, y = Count)) +
  geom_line(linewidth= 1, color = "blue") +  
  geom_point(size = 3) +
  labs(title = "Yearly Trends in Dolphin Strandings",
       x = "Year", y = "Number of Strandings") +
  theme_minimal()
year

# Extract Month (across all years)
strandings_per_month <- d_data %>%
  group_by(Month) %>%
  summarise(Count = n(), .groups = "drop")

print(strandings_per_month)

ggplot(strandings_per_month, aes(x = Month, y = Count, group = 1)) +
  geom_line(linewidth = 1, color = "darkred") +  
  geom_point(size = 3) +
  labs(title = "Monthly Trends in Dolphin Strandings",
       x = "Month", y = "Number of Strandings") +
  theme_minimal()

#Year and Month Combined
strandings_year_month <- d_data %>%
  group_by(Year, Month) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(strandings_year_month, aes(x = Month, y = Count, group = Year, color = as.factor(Year))) +
  geom_line(linewidth = 1) +
  labs(title = "Monthly Trends in Dolphin Strandings by Year",
       x = "Month", y = "Number of Strandings", color = "Year") +
  theme_minimal()

