# 
# # Load data file, e.g "1001_iCub-RoboAuto_ScreenExp1_2022_2024-05-15_17h33.49.237.csv"
# 
# library(readr)
# 
# dataset <- read_csv("1001_iCub-RoboAuto_ScreenExp1_2022_2024-05-15_17h33.49.237.csv")
# 
# #omit first row (has na values)
# 
# dataset <- dataset[-1, ]
#   
# # Subset the dataset - leave only the columns that are useful
# 
# variables_columns_rts <- c(1,2,12:17,52)
# dataset_rts <- dataset[variables_rts]
# 
# # Calculate average RTs for each condition of robot personality
#   
# typeof(dataset_rts$robot_personality) #check datatype of robot personality
# dataset_rts$robot_personality <- as.factor(dataset_rts$robot_personality) #change type to integer
# typeof(dataset_rts$robot_personality) #check if datatype is integer
# 
# avg_rts <- aggregate(PARTICIPANT_RESPONSE.rt ~ robot_personality, data = dataset_rts, FUN = mean)
# print(avg_rts) #calculate mean for rts grouped by robot personality and print
# 
# #create dataset for robot personality = HelpfulAutonomy and include control vs autonomy
# 
# helpful_rts <- dataset_rts[dataset_rts$robot_personality == "HelpfulAutonomy", ]
# 
# helpful_rts_columns <- c(1,2,7:9)
# helpful_rts <- helpful_rts[helpful_rts_columns]
# 
# #calculate avg rts for robot_help 1 and robot_help 0 
# 
# avg_rts_helpful <- aggregate(PARTICIPANT_RESPONSE.rt ~ robot_help, data = helpful_rts, FUN = mean)
# print(avg_rts_helpful) 

# start over
#install package dplyr
install.packages("dplyr")

#set up libraries
library(readr)
library(dplyr)

# Load and clean dataset
dataset <- read_csv("1001_iCub-RoboAuto_ScreenExp1_2022_2024-05-15_17h33.49.237.csv")

# Select relevant columns
variables_columns_rts <- c(1,2,12:17,52)
dataset_rts <- dataset[, variables_columns_rts]

# Ensure robot_personality is a factor
dataset_rts$robot_personality <- as.factor(dataset_rts$robot_personality)

# Average RTs for each robot personality
avg_rts <- dataset_rts %>%
  group_by(robot_personality) %>%
  summarise(avg_rt = mean(PARTICIPANT_RESPONSE.rt, na.rm = TRUE)
  )

print(avg_rts)

# Function to subset by personality and get avg RT by robot_help
get_avg_rts_by_help <- function(personality_label) {
  dataset_rts %>%
    filter(robot_personality == personality_label) %>%
    select(1, 2, 7:9) %>%
    group_by(robot_help) %>%
    summarise(avg_rt = mean(PARTICIPANT_RESPONSE.rt, na.rm = TRUE)) %>%
    mutate(robot_personality = personality_label)
}

# Apply function to multiple personalities
personalities <- c("HelpfulAutonomy", "BrokenRobot", "MaliciousAutonomy")

all_avg_rts_by_help <- lapply(personalities, get_avg_rts_by_help) %>%
  bind_rows()

print(all_avg_rts_by_help)

