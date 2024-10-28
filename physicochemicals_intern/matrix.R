# Create your matrix
library(readxl)
library(reshape2)
library(ggplot2)
library(dplyr)
table11 <- read_excel("Desktop/table11.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
Property = table11[, c("EC (mS/cm)", "pH", "TOC", "pF (15 atm)", "pF (0,33 atm)", "AW (%)", "TN (%)", "C/N")]

mat <-  matrix(Property, nrow = 8, byrow = TRUE) # Adjusted to match row names
rownames(mat) <- c("EC (mS/cm)", "pH", "TOC", "pF (15 atm)", "pF (0,33 atm)", "AW (%)", "TN (%)", "C/N")
colnames(mat) <- c("value")


mat_df <- data.frame(Row = rownames(mat), mat, stringsAsFactors = FALSE)


variable_P <- rep(mat_df$Row, each=84)
variable_Time <- rep(table11$time, time=8) 
variable_Treatment <- rep(table11$TREATMENT, time =8)
  

all_values <- c(mat_df$value)

Ec <- all_values$`EC (mS/cm)`
ph <- all_values$pH
t <- all_values$TOC
pf <- all_values$`pF (15 atm)`
pf2 <- all_values$`pF (0,33 atm)`
a <- all_values$`AW (%)`
tn <- all_values$`TN (%)`
cn <- all_values$`C/N`

all_values_together <- c(Ec,ph,t,pf,pf2,a,tn,cn)

data <- data.frame(property = variable_P, value = all_values_together)

sorted_data <- data[order(data$property), ]

summary_data <- data.frame(time = variable_Time, treatment = variable_Treatment, property = variable_P, value = all_values_together)


# Calculate means and standard errors
bar_data <- summary_data %>%
  group_by(time, treatment, property) %>%
  summarise(mean_value = mean(value),
            se_value = sd(value) / sqrt(n()))

# Plotting bar charts
p1 <- ggplot(bar_data, aes(x = time, y = mean_value, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                position = position_dodge(width = 0.9), width = 0.25) +
  facet_wrap(~property, scales = "free_y") +
  labs(x = "Date", y = "Value") +
  theme_minimal()


print(p1)


# Plotting grouped bar chart
p2 <- ggplot(bar_data, aes(x = property, y = mean_value, fill = time)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                position = position_dodge(width = 0.9), width = 0.5) +
  facet_wrap(~treatment) +
  labs(x = "Property", y = "Value") +
  theme_minimal()

print(p2)


anova_data<- data.frame(value_anova = all_values_together,group_anova = variable_Time )

str(anova_data)


# Plot boxplot
 ggplot(anova_data, aes(x = group_anova, y = value_anova)) +
  geom_boxplot() +
  labs(x = "Date", y = "Value of Properties") +
  theme_minimal()










