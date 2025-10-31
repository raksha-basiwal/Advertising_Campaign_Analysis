#Raksha Basiwal
#U ID-  U54420560

library(dplyr)
library(stargazer)

#Verify the presence of any missing values.
sum(is.na(Abandoned))
sum(is.na(Reservation))

#Examine the dataset for duplicate entries.
sum(duplicated(Abandoned))
sum(duplicated(Abandoned$Caller_ID))

sum(duplicated(Reservation))
sum(duplicated(Reservation$Caller_ID))

#Analysis of Test and Control Group Distribution
sum(Abandoned$Test_Control == "test" )
sum(Abandoned$Test_Control == "control")
sum(Reservation$Test_Control == "test" )
sum(Reservation$Test_Control == "control")

# Load ggplot2 library
library(ggplot2)

# Create the data frame with counts
data <- data.frame(
  Group = c("Test", "Control", "Test", "Control"),
  Dataset = c("Abandoned", "Abandoned", "Reservation", "Reservation"),
  Count = c(4266, 4176, 18728, 2086)
)

# Create the bar chart with count labels
ggplot(data, aes(x = Dataset, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, # Adjusts the vertical position of the text
            size = 3) + # Size of the text
  labs(title = "Counts of Test and Control Groups in Abandoned and Reservation Datasets",
       x = "Dataset",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Test" = "blue", "Control" = "orange"))



#Segmented by State Availability
if_state <- Abandoned[complete.cases(Abandoned['Address']),]
table(if_state$Test_Control)

stats_overview = Abandoned %>%
  group_by(Address, Test_Control) %>%
  summarize(Count = n())

print(stats_overview)

Reservation_stats = Reservation %>%
  group_by(Address, Test_Control) %>%
  summarize(Count = n())

print(Reservation_stats)
#The Abandoned dataset shows a relatively balanced distribution overall, as well as at the state level.
#The Reserved dataset displays an uneven distribution both overall and at the state level.


#Perform matching using different keys and create logical vectors to represent each condition.
emailmatch = Abandoned$Email[complete.cases(Abandoned$Email)] %in% Reservation$Email[complete.cases(Reservation$Email)]
incom_match = Abandoned$Incoming_Phone[complete.cases(Abandoned$Incoming_Phone)] %in% Reservation$Incoming_Phone[complete.cases(Reservation$Incoming_Phone)]
contactmatch = Abandoned$Contact_Phone[complete.cases(Abandoned$Contact_Phone)] %in% Reservation$Contact_Phone[complete.cases(Reservation$Contact_Phone)]
incom_contact_match = Abandoned$Incoming_Phone[complete.cases(Abandoned$Incoming_Phone)] %in% Reservation$Contact_Phone[complete.cases(Reservation$Contact_Phone)]
contact_incom_match = Abandoned$Contact_Phone[complete.cases(Abandoned$Contact_Phone)] %in% Reservation$Incoming_Phone[complete.cases(Reservation$Incoming_Phone)]

#Generate flags for matched records

Abandoned$emailmatch = 0
Abandoned$emailmatch[complete.cases(Abandoned$Email)] = 1 * emailmatch

Abandoned$incom_match = 0
Abandoned$incom_match[complete.cases(Abandoned$Incoming_Phone)] = 1 * incom_match

Abandoned$contactmatch= 0
Abandoned$contactmatch[complete.cases(Abandoned$Contact_Phone)] = 1 * contactmatch

Abandoned$incom_contact_match= 0
Abandoned$incom_contact_match[complete.cases(Abandoned$Incoming_Phone)] = 1 * incom_contact_match

Abandoned$contact_incom_match= 0
Abandoned$contact_incom_match[complete.cases(Abandoned$Contact_Phone)] = 1 * contact_incom_match


# Logical filter for matched records of customers who purchased
Abandoned$pur = 1 * ( Abandoned$emailmatch | Abandoned$incom_match |Abandoned$contactmatch | Abandoned$incom_contact_match | Abandoned$contact_incom_match)  

# Create additional columns for analyses
Abandoned$email = 1 * complete.cases(Abandoned$Email)
Abandoned$state = 1 * complete.cases(Abandoned$Address)
Abandoned$treat = ifelse(Abandoned$Test_Control == "test", 1, 0)

tab = table(Abandoned$pur, Abandoned$treat)
# Adding row labels for 'Outcome'
rownames(tab) = c("Not Purchased", "Purchased")
# Adding column labels for 'Treatment'
colnames(tab) = c("Control Group", "Treatment Group")
print(tab)

#Filter out unmatched records
unmatchable_Abandoned <- Abandoned[Abandoned$pur == 0, ]
head(unmatchable_Abandoned)

#Dropping unmatching data and selecting matched (purchased) records
Abandoned_match <- Abandoned[Abandoned$pur == 1, ]

#Cross-tabulate both purchased and non-purchased records
tab = table(Abandoned$pur, Abandoned$treat)
rownames(tab) = c("Not Purchased", "Purchased")
# Add column labels for 'Outcome'
colnames(tab) = c("Control Group", "Treatment Group")
print(tab)


all_states = Abandoned$Address[!is.na(Abandoned$Address)] 
set.seed(123)  # Setting a seed for reproducibility
random_states = sample(all_states, 5)

cross_tabulations = list()

for (state in random_states) {
  subset_data = Abandoned[Abandoned$Address == state, ]
  cross_tabulation =  table(subset_data$pur, subset_data$treat)
  rownames( cross_tabulation ) =  c("Not Purchased", "Purchased")
  colnames( cross_tabulation ) = c("Control Group", "Treatment Group")
  cross_tabulations[[state]] = cross_tabulation
}

# Print the cross-tabulations
for (state in random_states) {
  cat("Cross-tabulation for State:", state, "\n")
  print(cross_tabulations[[state]])
  cat("\n")
}


#Cleaning dataset

# Remove multiple columns
abandon_clean = Abandoned %>%
  select(-(2:17))

#Changing index of columns and their column names
abandon_clean = abandon_clean %>%
  select(1, 5, 2, 4,3:ncol(abandon_clean))
colnames(abandon_clean) = c("Customer_ID", "Test_Group","Outcome","State_Available",
                        "Email_Available")

#Exporting the clean data set as a csv
write.csv(abandon_clean, file = "abandon_clean.csv", row.names = FALSE)



#Statistical tests
# Run regression analyses

outcome_1 = lm(Outcome ~ Test_Group, data = abandon_clean)
summary(outcome_1)


outcome_2 = aov(Outcome ~ Test_Group, data = abandon_clean)
summary(outcome_2)


outcome_3 = lm(Outcome ~ Test_Group + State_Available + Email_Available , data = abandon_clean)
summary(outcome_3)

outcome_4 = lm(Outcome ~ Test_Group + State_Available + Email_Available + State_Available*Test_Group + Email_Available*Test_Group, data = abandon_clean)
summary(outcome_4)

#logistic model 
logmodel = glm(Outcome ~ Test_Group + State_Available + Email_Available + State_Available*Test_Group + Email_Available*Test_Group ,family = binomial(link="logit"), data = abandon_clean)
summary(logmodel)
# Generate summary table
stargazer(outcome_1,outcome_3,outcome_4,logmodel, type = "text")
