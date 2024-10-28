#libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(gplots)
library(stringr)
library(vegan)
library(tidyr)

#reading the file
Abundance <- read_excel("Abundance.xlsx")


#selecting all the names of bacterias and making it into one table 
column_names_3_to_184 <- colnames(Abundance)[3:184]
Property = Abundance[, c(column_names_3_to_184)]

# Adjusted to match row names
mat <-  matrix(Property, nrow = 182, byrow = TRUE) 
rownames(mat) <- c(column_names_3_to_184)
colnames(mat) <- c("value")
mat_df <- data.frame(Row = rownames(mat), mat, stringsAsFactors = FALSE)

#adding the column time, treatment and property into one data sheet
variable_P <- rep(mat_df$Row, each=84)
variable_Time <- rep(Abundance$Campaign, time=182) 
variable_Treatment <- rep(Abundance$Treatment, time =182)
all_values<- unlist(Abundance[, column_names_3_to_184])


#making one summary data 
summary_data <- data.frame(time = variable_Time, treatment = variable_Treatment, property = variable_P, value = all_values)

#extracting the name of the property based on its filo
filo <- str_extract(variable_P, "P_[^_]+")

#combining time and treatment names
summary_data$time_and_treatment <- paste(summary_data$time, summary_data$treatment)
#keeping the order of time 
summary_data$time_and_treatment <- factor(summary_data$time_and_treatment, levels = unique(summary_data$time_and_treatment))


#making the graph
ggplot(summary_data, aes(x = time_and_treatment, y = filo, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Heatmap of Bacterial Abundance",
       x = "Date",
       y = "Bacteria Property") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 12),  
        plot.margin = margin(2, 2, 2, 2)) 


#------------------------------------------------------------------------------------------------

#NMDS ANALYS 

#making a new dataframe 
sorted_data_frame <- summary_data[order(summary_data$treatment), ]

Treatment=sorted_data_frame$treatment
Value=sorted_data_frame$value
Time = sorted_data_frame$time

df <- data.frame(
  treatment =Treatment,
  value = Value, 
  time = Time

)

df <- df %>% 
  group_by(treatment) %>% 
  mutate(row_index = row_number()) 

#making all the 0 values to 0.1 because analys can't be done with 0 values 
df[df == 0.00] <- 0.1

wide_df <- pivot_wider(df, names_from = treatment, values_from = value)
vars_to_include <- colnames(wide_df)[3:9]  # include all the treatments in one variable 
data_subset <- wide_df[,c(vars_to_include)]


# Perform NMDS Analysis itself

set.seed(123)
nmds_result <- metaMDS(data_subset, distance = "bray")

#Plot based on stress level results
plot(nmds_result)


# Create a data frame with NMDS coordinates
nmds_data <- as.data.frame(scores(nmds_result))

nmds_data[,c(vars_to_include)] <- data_subset

nmds_data$Date <- wide_df$time

vars_to_include_nmds <- colnames(nmds_data)[5:11]


# Plot NMDS for each treatment
ggplot(nmds_data, aes(x = sites.NMDS1, y = sites.NMDS2, color = CC, shape = Date)) +
  geom_point() +
  labs(title = "Relationship between NMDS1 and NMDS2",
      x = "NMDS1",
      y = "NMDS2",
      color = "Treatments' scale") +
      theme_minimal() + 
      facet_wrap(~ paste0(vars_to_include))




