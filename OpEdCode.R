library(ggplot2)

gaming <- read.csv("gamingstudy.csv")


# Removing outliers of hours played per week (considering quartile 25%-75% of data)
Q1 <- quantile(gaming$Hours, 0.25, na.rm = TRUE)
Q3 <- quantile(gaming$Hours, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

gaming <- gaming[gaming$Hours >= lower_bound & gaming$Hours <= upper_bound, ]

gaming$GAD_T = as.numeric(gaming$GAD_T)
gaming$SPIN_T = as.numeric(gaming$SPIN_T)
gaming$SWL_T = as.numeric(gaming$SWL_T)
gaming$Hours = as.numeric(gaming$Hours)

ggplot(gaming, aes(x = GAD_T)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "dodge", color = "black") +
  labs(title = "Distribution of GAD Scores (Generalized Anxiety Disorder)", x = "GAD_T Score", y = "Frequency") +
  theme_minimal()

ggplot(gaming, aes(x = SPIN_T)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "dodge", color = "black") +
  labs(title = "Distribution of SPIN Scores (Social Phobia Inventory)", x = "SPIN_T Score", y = "Frequency") +
  theme_minimal()

ggplot(gaming, aes(x = SWL_T)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "dodge", color = "black") +
  labs(title = "Distribution of SWL Scores (Satisfaction With Life)", x = "SWL_T Score", y = "Frequency") +
  theme_minimal()

ggplot(gaming, aes(x = Hours)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "dodge", color = "black") +
  labs(title = "Histogram of Hours Played Per Week", x = "Hours Per Week", y = "Frequency") +
  theme_minimal()

### Some metadata and charts
gaming$levels <- ifelse(gaming$Hours < 10, "Less than 10", 
                        ifelse(gaming$Hours <= 20, "10-20", "More than 20"))

# Count the number of occurrences for each level
levels_counts <- as.data.frame(table(gaming$levels))
colnames(levels_counts) <- c("levels", "n")

# Creating the percentages and labels
levels_counts$percentage <- levels_counts$n / sum(levels_counts$n) * 100
levels_counts$label <- paste(levels_counts$levels, "\n", round(levels_counts$percentage, 1), "%", sep = "")

# Creating a pie-chart
ggplot(levels_counts, aes(x = "", y = percentage, fill = levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(title = "Gaming Time Distribution") +
  scale_fill_brewer(palette = "Pastel1")

# Observation: Filter to people under 30 to see if there are any differences
under25 <- gaming[gaming$Age < 30, ]

levels_counts_under25 <- as.data.frame(table(under25$levels))
colnames(levels_counts_under25) <- c("levels", "count")
levels_counts_under25$percentage <- levels_counts_under25$count / sum(levels_counts_under25$count) * 100

print(levels_counts_under25)


### T-test for all individuals that play more than 20 hours
# hours_mean ~ 20 hours per week
hours_mean <- mean(gaming$Hours, na.rm = TRUE)
print(hours_mean)

# Separate into two groups, whether they play more than the avg amount per week
gaming$hours_threshold <- ifelse(gaming$Hours > 20, "More than 20 hours", "20 hours or less")

# Perform a t-test comparing GAD_T between these two groups
t_test_gad_hours_group <- t.test(GAD_T ~ hours_threshold, data = gaming, na.rm = TRUE)
print(t_test_gad_hours_group)



### T-test for employed individuals or students that play between 3 and 15 hours
gaming_employed = gaming[gaming$Work == "Employed" | gaming$Work == "Student at college / university", ]

# hours for employed ~20 hours
new_hours_mean <- mean(gaming_employed$Hours, na.rm = TRUE)

gaming_employed$levels <- ifelse(gaming_employed$Hours < 3, "Little to None", 
                                 ifelse(gaming_employed$Hours <= 15, "Sometimes", "More than 15 hours"))

subset_two_groups <- gaming_employed[gaming_employed$levels %in% c("Little to None", "Sometimes"), ]

t_test_result <- t.test(GAD_T ~ levels, data = subset_two_groups)
print(t_test_result)

### T-test for employed individuals or students that play more than 15 hours
subset_two_groups <- gaming_employed[gaming_employed$levels %in% c("Sometimes", "More than 15 hours"), ]

t_test_result <- t.test(GAD_T ~ levels, data = subset_two_groups)
print(t_test_result)


### SPIN (social phobia inventory score) for those who play 3-15 hours (sometimes)
subset_two_groups <- gaming_employed[gaming_employed$levels %in% c("Little to None", "Sometimes"), ]

t_test_result <- t.test(SPIN_T ~ levels, data = subset_two_groups)
print(t_test_result)

### A linear regression model on SWL_T
model <- lm(SWL_T ~ Hours + Degree + Work, data = gaming)
summary(model)



### Box plot on GAD_T
ggplot(gaming_employed[!is.na(gaming_employed$levels), ], aes(x = levels, y = GAD_T)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Anxiety (GAD_T) Across Gaming Frequency (Employed or Students)",
       x = "Gaming Frequency", y = "GAD_T Score")


### More visualizations, focusing on SWL_T (Satisfaction with life)
# By their work status
mean_swl_work <- aggregate(SWL_T ~ Work, data = gaming, FUN = function(x) mean(x, na.rm = TRUE))

ggplot(mean_swl_work, aes(x = reorder(Work, SWL_T), y = SWL_T)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average SWL_T by Work Status", x = "Work Status", y = "Mean SWL_T") +
  theme_minimal() +
  coord_flip()

# By how many hours they play
mean_swl_levels <- aggregate(SWL_T ~ levels, data = gaming, FUN = function(x) mean(x, na.rm = TRUE))

ggplot(mean_swl_levels, aes(x = levels, y = SWL_T, fill = levels)) +
  geom_bar(stat = "identity") +
  labs(title = "SWL_T by Gaming Frequency", x = "Gaming Level", y = "Mean SWL_T") +
  theme_minimal()



