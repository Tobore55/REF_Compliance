
# Install and load necessary packages if not already installed
install.packages("survival")
install.packages("survminer")
install.packages("tidyverse")


# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(knitr)
library(kableExtra)
library(survival)
library(survminer)
library(tidyverse)

# Load the cleaned dataset
cleaned_file_path <- "C:/Users/mtakporokah/OneDrive - University of Plymouth/dissertation/open access guide/cleaned_openaccess.xlsx"
cleaned_data <- read_excel(cleaned_file_path)

# Display the first few rows of the cleaned dataset
head(cleaned_data)

# Display the structure of the cleaned dataset
str(cleaned_data)

# Display a summary of the cleaned dataset
summary(cleaned_data)

openaccess_data<-cleaned_data
names(openaccess_data)
#############################################


# Assuming these are the actual column names based on your previous input
actual_column_names <- colnames(openaccess_data)

# Shorten compliance issues and exceptions column names
shorten_colnames <- c(
  "OA_Embargo_Max" = "OA Compliance Issue - Embargo exceeds maximum",
  "OA_Item_Not_Live" = "OA Compliance Issue - Item not live",
  "OA_Missed_Deadline" = "OA Compliance Issue - Missed deposit deadline",
  "OA_Missing_Eff_Date" = "OA Compliance Issue - Missing effective date",
  "OA_Missing_Eff_Date_DD" = "OA Compliance Issue - Missing effective date - could not determine deposit deadline",
  "OA_Missing_Pub_Date" = "OA Compliance Issue - Missing publication date - could not determine embargo period",
  "OA_No_File" = "OA Compliance Issue - No file of compliant version",
  "OA_Overridden_Noncompliant" = "OA Compliance Issue - Overridden to be non-compliant",
  "OA_Awaiting_Deposit" = "OA Awaiting Deposit",
  "OA_Exception_Access1" = "OA Policy Exception - Access1",
  "OA_Exception_Access2" = "OA Policy Exception - Access2",
  "OA_Exception_Access3" = "OA Policy Exception - Access3",
  "OA_Exception_Deposit1" = "OA Policy Exception - Deposit1",
  "OA_Exception_Deposit2" = "OA Policy Exception - Deposit2",
  "OA_Exception_Deposit3" = "OA Policy Exception - Deposit3",
  "OA_Exception_Deposit4" = "OA Policy Exception - Deposit4",
  "OA_Exception_Deposit5" = "OA Policy Exception - Deposit5",
  "OA_Exception_Deposit6" = "OA Policy Exception - Deposit6",
  "OA_Exception_Deposit7" = "OA Policy Exception - Deposit7",
  "OA_Exception_DoNotRequest" = "OA Policy Exception - DoNotRequest",
  "OA_Exception_Other" = "OA Policy Exception - Other",
  "OA_Exception_Tech1" = "OA Policy Exception - Tech1",
  "OA_Exception_Tech2" = "OA Policy Exception - Tech2"
)

# Verify column names exist in the dataset before renaming
existing_cols <- intersect(actual_column_names, names(shorten_colnames))

# Create a named vector with existing columns
shorten_colnames_existing <- shorten_colnames[existing_cols]

print("Columns to be renamed:")
print(shorten_colnames_existing)

# Rename columns using the named vector
openaccess_data <- openaccess_data %>% rename(!!!shorten_colnames_existing)

# Display the new column names
print("New Column Names:")
print(colnames(openaccess_data))

# Perform Descriptive Analysis

# Distribution of publication types
publication_type_dist <- openaccess_data %>%
  group_by(`Publication Type`) %>%
  summarise(Count = n())

print("Distribution of Publication Types:")
print(publication_type_dist)

# Plot distribution of publication types
ggplot(publication_type_dist, aes(x = `Publication Type`, y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribution of Publication Types", x = "Publication Type", y = "Count")

# Distribution of primary groups
primary_group_dist <- openaccess_data %>%
  group_by(`Primary Group`) %>%
  summarise(Count = n())

print("Distribution of Primary Groups:")
print(primary_group_dist)

# Plot distribution of primary groups
ggplot(primary_group_dist, aes(x = `Primary Group`, y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribution of Primary Groups", x = "Primary Group", y = "Count")

# Distribution of compliance issues
compliance_issues_summary <- openaccess_data %>%
  summarise(
    OA_Embargo_Max = sum(`OA Compliance Issue - Embargo exceeds maximum`),
    OA_Item_Not_Live = sum(`OA Compliance Issue - Item not live`),
    OA_Missed_Deadline = sum(`OA Compliance Issue - Missed deposit deadline`),
    OA_Missing_Eff_Date = sum(`OA Compliance Issue - Missing effective date`),
    OA_MED_depositdeadline_notdetermined = sum(`OA Compliance Issue - Missing effective date - could not determine deposit deadline`),
    OA_missing_publication_date = sum(`OA Compliance Issue - Missing publication date - could not determine embargo period`),
    OA_No_file_compliant_version = sum(`OA Compliance Issue - No file of compliant version`),
    OA_compliance_overridden_non_compiant = sum(`OA Compliance Issue - Overridden to be non-compliant`)
  )

print("Summary of Compliance Issues:")
print(compliance_issues_summary)

# Plot summary of compliance issues




# Assuming compliance_issues_summary is your data frame
data_long <- compliance_issues_summary %>%
  pivot_longer(cols = everything(), names_to = "Issue", values_to = "Count") %>%
  arrange(desc(Count)) %>%
  slice(1:5)

ggplot(data_long, aes(x = reorder(Issue, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Count), hjust = -0.3) +
  coord_flip() +
  labs(title = "Top 5 Compliance Issues", x = "Issue", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor.x = element_blank()
  )



#########################################################################################
# Calculate overall compliance rate
overall_compliance <- openaccess_data %>%
  summarise(
    Compliant = sum(
      `OA Compliance Issue - Embargo exceeds maximum` == 0 &
        `OA Compliance Issue - Item not live` == 0 &
        `OA Compliance Issue - Missed deposit deadline` == 0 &
        `OA Compliance Issue - Missing effective date` == 0 &
       `OA Compliance Issue - Missing effective date - could not determine deposit deadline`== 0 &
        `OA Compliance Issue - Missing publication date - could not determine embargo period` == 0 &
        `OA Compliance Issue - No file of compliant version` == 0 &
        `OA Compliance Issue - Overridden to be non-compliant` == 0 
    ) / n() * 100
  )

print("Overall Compliance Rate:")
print(overall_compliance)
# Calculate non-compliance rate
overall_compliance <- overall_compliance %>%
  mutate(Non_Compliant = 100 - Compliant)

print("Overall Compliance and Non-Compliance Rates:")
print(overall_compliance)
###visualize with a pie chart
# Prepare data for the doughnut chart
compliance_data <- data.frame(
  Category = c("Compliant", "Non-Compliant"),
  Rate = c(overall_compliance$Compliant, overall_compliance$Non_Compliant)
)

# Create a doughnut chart
ggplot(compliance_data, aes(x = 2, y = Rate, fill = Category)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +
  coord_polar(theta = "y") +
  theme_void() + # Remove background, grid, and numeric labels
  geom_text(aes(label = paste0(round(Rate, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "black") +
  labs(title = "Overall Compliance and Non-Compliance Rates") +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("Compliant" ="orange", "Non-Compliant" = "grey90" )) +
  xlim(0.5, 2.5) # Adjust x-axis limits to create the doughnut effect

#############################################################################################################
# Calculate compliance rate by primary group
compliance_by_primary_group <- openaccess_data %>%
  group_by(`Primary Group`) %>%
  summarise(
    Total = n(),
    Compliant = sum(
      `OA Compliance Issue - Embargo exceeds maximum` == 0 &
        `OA Compliance Issue - Item not live` == 0 &
        `OA Compliance Issue - Missed deposit deadline` == 0 &
        `OA Compliance Issue - Missing effective date` == 0 &
        `OA Compliance Issue - Missing effective date - could not determine deposit deadline`== 0 &
        `OA Compliance Issue - Missing publication date - could not determine embargo period` == 0 &
        `OA Compliance Issue - No file of compliant version` == 0 &
        `OA Compliance Issue - Overridden to be non-compliant` == 0 
    ),
    Compliance_Rate = (Compliant / Total) * 100
  )

print("Compliance Rate by Primary Group:")
print(compliance_by_primary_group)

###TABLE
# Install necessary packages if not already installed
if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr")
}
if (!requireNamespace("kableExtra", quietly = TRUE)) {
  install.packages("kableExtra")
}
# Convert to tibble
compliance_by_primary_group <- as_tibble(compliance_by_primary_group)

# Create a formatted table
formatted_table <- compliance_by_primary_group %>%
  kable(format = "html", col.names = c("Primary Group", "Total", "Compliant", "Compliance Rate (%)")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# Display the formatted table
print(formatted_table)

###


# Plot compliance rate by primary group with refined visuals
ggplot(compliance_by_primary_group, aes(x = reorder(`Primary Group`, -Compliance_Rate), y = Compliance_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Compliance_Rate, 1), "%")), 
            vjust = -0.3, color = "black", size = 3.5) + # Adjust text size and position
  labs(title = "Compliance Rate by Primary Group", 
       x = "Primary Group", 
       y = "Compliance Rate (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1), # Rotate x-axis text
    axis.text.y = element_text(size = 12)
  ) +
  ylim(0, 100) # Ensure the y-axis goes from 0 to 100
###################################################################################################
# Calculate compliance rate by publication type
compliance_by_publication_type <- openaccess_data %>%
  group_by(`Publication Type`) %>%
  summarise(
    Total = n(),
    Compliant = sum(
      `OA Compliance Issue - Embargo exceeds maximum` == 0 &
        `OA Compliance Issue - Item not live` == 0 &
        `OA Compliance Issue - Missed deposit deadline` == 0 &
        `OA Compliance Issue - Missing effective date` == 0
    ),
    Compliance_Rate = (Compliant / Total) * 100
  )



print("Compliance Rate by Publication Type:")
print(compliance_by_publication_type)

by_publication_table <-compliance_by_publication_type %>%
  kable(format = "html", col.names = c("publication type", "Total", "Compliant", "Compliance Rate (%)")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(by_publication_table)



# Plot compliance rate by publication type with enhanced visuals
ggplot(compliance_by_publication_type, aes(x = reorder(`Publication Type`, -Compliance_Rate), y = Compliance_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = paste0(round(Compliance_Rate, 1), "%")), 
            hjust = 1.1, color = "black", size = 3.5) + # Adjust text size and position
  coord_flip() +
  labs(title = "Compliance Rate by Publication Type", 
       x = "Publication Type", 
       y = "Compliance Rate (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank(), # Remove y-axis grid lines for better clarity
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 100) # Ensure the y-axis goes from 0 to 100
#######################################################################################################################
# Convert numeric dates to actual dates assuming the origin is January 1, 1900
openaccess_data <- openaccess_data %>%
  mutate(Publication_Date = as.Date(`publication-date`, origin = "1899-12-30")) # Excel date origin

# Check for any NA values in Publication_Date after conversion
print(sum(is.na(openaccess_data$Publication_Date)))

# Display a sample of the Publication_Date column
print(head(openaccess_data$Publication_Date, 50))

# Extract year and filter out invalid years
openaccess_data <- openaccess_data %>%
  mutate(Year = as.numeric(format(Publication_Date, "%Y"))) %>%
  filter(!is.na(Year) & Year >= 2000 & Year <= as.numeric(format(Sys.Date(), "%Y")))

# Display unique years to ensure they are within a reasonable range
print(unique(openaccess_data$Year))

# Filter to include only 2021, 2022, and 2023
openaccess_data <- openaccess_data %>%
  filter(Year %in% c(2021, 2022, 2023))

# Display unique years to ensure they are within the expected range
print(unique(openaccess_data$Year))

# Calculate compliance rate by year and primary group
compliance_by_year_group <- openaccess_data %>%
  group_by(Year, `Primary Group`) %>%
  summarise(
    Total = n(),
    Compliant = sum(
      `OA Compliance Issue - Embargo exceeds maximum` == 0 &
        `OA Compliance Issue - Item not live` == 0 &
        `OA Compliance Issue - Missed deposit deadline` == 0 &
        `OA Compliance Issue - Missing effective date` == 0 &
        `OA Compliance Issue - Missing effective date - could not determine deposit deadline`== 0 &
        `OA Compliance Issue - Missing publication date - could not determine embargo period` == 0 &
        `OA Compliance Issue - No file of compliant version` == 0 &
        `OA Compliance Issue - Overridden to be non-compliant` == 0 
    ),
    Compliance_Rate = (Compliant / Total) * 100
  ) %>%
  arrange(Year, `Primary Group`)

print("Compliance Rate by Year and Primary Group:")
print(compliance_by_year_group)

# Plot compliance rate by year and primary group
ggplot(compliance_by_year_group, aes(x = Year, y = Compliance_Rate, color = `Primary Group`, group = `Primary Group`)) +
  geom_line() +
  geom_point() +
  labs(title = "Compliance Rate by Primary Group Over Time", 
       x = "Year", 
       y = "Compliance Rate (%)",
       color = "Primary Group") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = unique(compliance_by_year_group$Year))
##############################################################################################################################
# Aggregate data by month
# Ensure there are no NA values in Publication_Date
openaccess_data <- openaccess_data %>%
  filter(!is.na(Publication_Date))

# Extract year and filter to include only 2021, 2022, and 2023
openaccess_data <- openaccess_data %>%
  mutate(Year = as.numeric(format(Publication_Date, "%Y"))) %>%
  filter(Year %in% c(2021, 2022, 2023))

# Display unique years to ensure they are within the expected range
print(unique(openaccess_data$Year))

# Aggregate data by month
openaccess_data <- openaccess_data %>%
  mutate(Month = format(Publication_Date, "%Y-%m")) %>%
  group_by(Month, `Primary Group`) %>%
  summarise(
    Total = n(),
    Compliant = sum(
      `OA Compliance Issue - Embargo exceeds maximum` == 0 &
        `OA Compliance Issue - Item not live` == 0 &
        `OA Compliance Issue - Missed deposit deadline` == 0 &
        `OA Compliance Issue - Missing effective date` == 0 &
        `OA Compliance Issue - Missing effective date - could not determine deposit deadline`== 0 &
        `OA Compliance Issue - Missing publication date - could not determine embargo period` == 0 &
        `OA Compliance Issue - No file of compliant version` == 0 &
        `OA Compliance Issue - Overridden to be non-compliant` == 0 
    ),
    Compliance_Rate = (Compliant / Total) * 100
  ) %>%
  arrange(Month, `Primary Group`)

# Convert Month to Date type for plotting
openaccess_data <- openaccess_data %>%
  mutate(Month = as.Date(paste0(Month, "-01")))
##########################################################################################################
# Plot compliance rate by month and primary group
ggplot(openaccess_data, aes(x = Month, y = Compliance_Rate, color = `Primary Group`, group = `Primary Group`)) +
  geom_line() +
  geom_point() +
  labs(title = "Compliance Rate by Primary Group Over Time (Monthly)", 
       x = "Month", 
       y = "Compliance Rate (%)",
       color = "Primary Group") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")
#############################################################################
# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the cleaned dataset
cleaned_file_path <- "C:/Users/mtakporokah/OneDrive - University of Plymouth/dissertation/open access guide/cleaned_openaccess.xlsx"
cleaned_data <- read_excel(cleaned_file_path)

# Display the first few rows of the cleaned dataset
head(cleaned_data)

# Display the structure of the cleaned dataset
str(cleaned_data)

# Display a summary of the cleaned dataset
summary(cleaned_data)

openaccess_data <- cleaned_data

# Convert dates from Excel numeric format to Date format
openaccess_data <- openaccess_data %>%
  mutate(Publication_Date = as.Date(`publication-date`, origin = "1899-12-30"))

# Extract year and filter to include only 2021, 2022, and 2023
openaccess_data <- openaccess_data %>%
  mutate(Year = as.numeric(format(Publication_Date, "%Y"))) %>%
  filter(Year %in% c(2021, 2022, 2023))

# Aggregate data by compliance issue and primary group
compliance_issues_by_group <- openaccess_data %>%
  select(`Primary Group`, starts_with("OA Compliance Issue")) %>%
  pivot_longer(cols = starts_with("OA Compliance Issue"), names_to = "Compliance_Issue", values_to = "Issue_Present") %>%
  group_by(`Primary Group`, Compliance_Issue) %>%
  summarise(
    Total = n(),
    Issues = sum(Issue_Present == 1),
    Compliance_Issue_Rate = (Issues / Total) * 100
  ) %>%
  arrange(`Primary Group`, desc(Compliance_Issue_Rate))

# Identify the top 5 compliance issues overall
top_issues <- compliance_issues_by_group %>%
  group_by(Compliance_Issue) %>%
  summarise(Total_Issues = sum(Issues)) %>%
  arrange(desc(Total_Issues)) %>%
  slice(1:5) %>%
  pull(Compliance_Issue)

# Filter the aggregated data to include only the top 5 issues
compliance_issues_by_group_top5 <- compliance_issues_by_group %>%
  filter(Compliance_Issue %in% top_issues)

# Shorten legend labels to the last phrase of each compliance issue
compliance_issues_by_group_top5 <- compliance_issues_by_group_top5 %>%
  mutate(Compliance_Issue_Short = sapply(strsplit(as.character(Compliance_Issue), " - "), tail, 1))

print("Compliance Issues Rate by Primary Group for Top 5 Issues:")
print(compliance_issues_by_group_top5)

######################################################################################################
# Load required libraries
library(ggplot2)
library(scales)
library(dplyr)

# Function to abbreviate the x-axis labels uniquely
abbreviate_labels <- function(labels) {
  sapply(labels, function(label) {
    words <- unlist(strsplit(label, " "))
    abbreviation <- paste0(substr(words, 1, 1), collapse = "")
    return(abbreviation)
  })
}

# Ensure unique abbreviations
make_unique <- function(abbreviations) {
  unique_abbreviations <- abbreviations
  counts <- table(abbreviations)
  for (abbr in names(counts)) {
    if (counts[abbr] > 1) {
      indices <- which(abbreviations == abbr)
      for (i in seq_along(indices)) {
        unique_abbreviations[indices[i]] <- paste0(abbr, i)
      }
    }
  }
  return(unique_abbreviations)
}

# Apply the abbreviation function and ensure uniqueness
abbreviated_labels <- abbreviate_labels(compliance_issues_by_group_top5$`Primary Group`)
unique_abbreviated_labels <- make_unique(abbreviated_labels)

# Map the original labels to their unique abbreviations
label_mapping <- setNames(unique_abbreviated_labels, compliance_issues_by_group_top5$`Primary Group`)

# Plot compliance issues rate by primary group for top 5 issues
ggplot(compliance_issues_by_group_top5, aes(x = `Primary Group`, y = Compliance_Issue_Rate, fill = Compliance_Issue_Short)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Compliance_Issue_Rate, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 4) +  # Adjust text size for readability
  labs(
    title = "Top 5 Compliance Issues Rate by Primary Group", 
    x = "Primary Group", 
    y = "Issue Rate (%)", 
    fill = "Compliance Issue"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  scale_fill_brewer(palette = "Set3") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = unique_abbreviated_labels) +
  geom_text(aes(x = `Primary Group`, y = -0.02, label = compliance_issues_by_group_top5$`Primary Group`),
            angle = 45, hjust = 1, vjust = 1, size = 3, color = "black")


###########################################################################################################
#####################################################################################################


###################################################
###################################################

# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

# Load the cleaned dataset
cleaned_file_path <- "C:/Users/mtakporokah/OneDrive - University of Plymouth/dissertation/open access guide/cleaned_openaccess.xlsx"
cleaned_data <- read_excel(cleaned_file_path)

# Convert dates from Excel numeric format to Date format
openaccess_data <- cleaned_data %>%
  mutate(Publication_Date = as.Date(`publication_date`, origin = "1899-12-30"))

# Identify and filter out invalid dates (considering valid dates range from 2021 to 2023)
valid_date_range <- as.Date(c("2021-01-01", "2023-12-31"))
openaccess_data <- openaccess_data %>%
  filter(Publication_Date >= valid_date_range[1] & Publication_Date <= valid_date_range[2])

# Create a binary compliance variable for each publication
openaccess_data <- openaccess_data %>%
  mutate(Compliant = ifelse(
    `OA Compliance Issue - Embargo exceeds maximum` == 0 &
      `OA Compliance Issue - Item not live` == 0 &
      `OA Compliance Issue - Missed deposit deadline` == 0 &
      `OA Compliance Issue - Missing effective date` == 0 &
      `OA Compliance Issue - Missing effective date - could not determine deposit deadline` == 0 &
      `OA Compliance Issue - Missing publication date - could not determine embargo period` == 0 &
      `OA Compliance Issue - No file of compliant version` == 0 &
      `OA Compliance Issue - Overridden to be non-compliant` == 0, 1, 0)
  )

# Ensure the `Primary Group` is treated as a factor
openaccess_data <- openaccess_data %>%
  mutate(`Primary Group` = as.factor(`Primary Group`))

# Aggregate data by month and primary group
openaccess_data <- openaccess_data %>%
  mutate(YearMonth = format(floor_date(Publication_Date, "month"), "%Y-%m")) %>%
  group_by(YearMonth, `Primary Group`) %>%
  summarise(
    Total = n(),
    Compliant = sum(Compliant),
    Compliance_Rate = (Compliant / Total) * 100
  ) %>%
  ungroup()

# Ensure YearMonth is a factor
openaccess_data <- openaccess_data %>%
  mutate(YearMonth = as.factor(YearMonth))

# Check the structure of the data
str(openaccess_data)

# Perform ANOVA on compliance rates
anova_result <- aov(Compliance_Rate ~ `Primary Group` + YearMonth, data = openaccess_data)

# Display the full ANOVA table
anova_summary <- summary(anova_result)
print(anova_summary)




# Check the structure and first few rows of the data
str(openaccess_data)
head(openaccess_data)

# Check the structure and first few rows of the data
str(openaccess_data)
head(openaccess_data)

# Perform ANOVA on compliance rates focusing only on Primary Group
anova_result <- aov(Compliance_Rate ~ `Primary Group`, data = openaccess_data)

# Display the full ANOVA table
anova_summary <- summary(anova_result)
print(anova_summary)

# Check the structure of the ANOVA model
str(anova_result)
# Perform pairwise t-tests with Holm adjustment for multiple comparisons
pairwise_t_test_result <- pairwise.t.test(openaccess_data$Compliance_Rate, openaccess_data$`Primary Group`, p.adjust.method = "holm")

# Display the pairwise t-test results
print(pairwise_t_test_result)
######################################################################################
# Ensure dates are in the correct format and calculate the necessary periods
data$acceptance_date <- as.Date(data$acceptance_date, format="%Y-%m-%d")
data$deposit_date <- as.Date(data$deposit_date, format="%Y-%m-%d")  # Assuming deposit_date is online_publication_date if not explicitly available
data$publication_date <- as.Date(data$publication_date, format="%Y-%m-%d")

# Calculate the time to deposit (acceptance to deposit)
data <- data %>%
  mutate(time_to_deposit = as.numeric(difftime(deposit_date, acceptance_date, units = "days")))

# Calculate the embargo period (deposit to publication)
data <- data %>%
  mutate(embargo_period = as.numeric(difftime(publication_date, deposit_date, units = "days")))

# Select relevant numerical columns for correlation analysis
correlation_data <- data %>%
  select(time_to_deposit, embargo_period) %>%
  drop_na()
# Compute the correlation matrix
correlation_matrix <- cor(correlation_data, use="complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Load necessary library for visualization
library(ggcorrplot)

# Create a heatmap of the correlation matrix
ggcorrplot(correlation_matrix, method = "circle", type = "lower", 
           title = "Correlation Matrix", lab = TRUE)

# Alternatively, use a pairplot for a detailed visualization
library(GGally)
ggpairs(correlation_data)
###################################################################################

# Load necessary libraries
library(tidyverse)
library(readxl)

# Load the dataset
data <- read_excel("C:/Users/mtakporokah/OneDrive - University of Plymouth/dissertation/open access guide/cleaned_openaccess.xlsx")

# Convert the date columns to Date format (assuming dates are stored as serial numbers)
data$acceptance_date <- as.Date(data$acceptance_date, origin = "1899-12-30")
data$First_Deposit_Date <- as.Date(data$`First Deposit Date`, origin = "1899-12-30")

# Calculate time between acceptance and deposit (deposit period)
data$time_to_deposit <- as.numeric(difftime(data$First_Deposit_Date, data$acceptance_date, units = "days"))

# Ensure the values make sense (e.g., no negative values)
data$time_to_deposit[data$time_to_deposit < 0] <- NA

# Remove records where the deposit date is missing (if desired)
data <- data %>% filter(!is.na(time_to_deposit))

# Create a censoring indicator (1 = event occurred, 0 = censored)
# Assuming you have a censoring column indicating if the event (deposit) has occurred
data$censored <- ifelse(is.na(data$First_Deposit_Date), 0, 1)

# Kaplan-Meier survival curve for the deposit period
km_fit_deposit <- survfit(Surv(time_to_deposit, censored) ~ 1, data = data)

# Plot the survival curve
ggsurvplot(km_fit_deposit, data = data, xlab = "Days", ylab = "Survival Probability",
           title = "Kaplan-Meier Curve for Deposit Period")
# Cox Proportional Hazards model to analyze factors influencing deposit time
cox_model_deposit <- coxph(Surv(time_to_deposit, censored) ~ `Publication Type` + `Primary Group` + `OA Compliance Issue - Missed deposit deadline`, data = data)

# Display the summary of the Cox model
summary(cox_model_deposit)






##########################################
# Create the Kaplan-Meier plot without the extra annotations
km_plot <- ggsurvplot(
  km_fit_deposit, 
  data = data, 
  xlab = "Days", 
  ylab = "Survival Probability",
  title = "Kaplan-Meier Curve for Deposit Period",
  xlim = c(0, 120),  # Adjust the x-axis limit to focus on the first 120 days
  surv.median.line = "hv",  # Add median survival line
  ggtheme = theme_minimal()
)

# Add a vertical line at 90 days and annotate
km_plot$plot <- km_plot$plot +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +  # Add x-axis ticks every 10 days
  geom_vline(xintercept = 90, linetype = "dashed", color = "red") +
  annotate("text", x = 95, y = 0.5, label = "90 days", color = "red", angle = 90, vjust = -0.5)

# Print the updated plot
print(km_plot)




