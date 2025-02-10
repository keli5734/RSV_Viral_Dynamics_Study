library(tidyverse)
library(minpack.lm)

# Set the path to the folder containing the CSV files
folder_path <- "PCR_Culture/falsey_etal/"  # Replace with your folder path

# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "qPCR*", full.names = TRUE)

# Read each CSV file into a list of data frames
data_list <- lapply(file_list, read.csv)

N <- lapply(data_list, nrow)
patient <- rep(paste0("subject", 1:length(data_list)), N)
patient <- factor(patient, 
                  levels = unique(patient))
# Optionally, combine all data frames into a single data frame (if they have the same structure)
combined_data1 <- do.call(rbind, data_list)

combined_data1 <- combined_data1 %>% 
  mutate(x = round(x)) %>% 
  mutate(patient = patient) %>% 
  # group_by(patient,x) %>% 
  # summarise(y = mean(y)) %>% 
  ungroup() %>% 
  mutate(test = rep("qPCR"))

ggplot(data = combined_data1) + 
  geom_line(aes(x = x, y = log10(y),  group = patient)) + 
  geom_point(aes(x = x, y = log10(y),  group = patient)) + 
  theme_bw() + 
  xlab("Days post infection") + 
  ylab("log10(viral titer)") + 
  #ggtitle("RSV viral load") +
  scale_x_continuous(breaks = c(2,4,6,8,10,12)) + 
  theme(
    axis.text.x = element_text(face="bold"),
    axis.text.y = element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    plot.title = element_text(face="bold", hjust = 0.5),
    #axis.line = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right") + 
  theme(axis.text.x = element_text(color ="black",
                                   size = 18, angle=0),
        axis.text.y = element_text(color ="black",
                                   size = 18, angle=0),
        text = element_text(size = 18))  + 
  facet_wrap(~patient, nrow =3)


# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "qCulture*", full.names = TRUE)

# Read each CSV file into a list of data frames
data_list <- lapply(file_list, read.csv)

N <- lapply(data_list, nrow)
patient <- rep(paste0("subject", 1:length(data_list)), N)
patient <- factor(patient, 
                  levels = unique(patient))
# Optionally, combine all data frames into a single data frame (if they have the same structure)
combined_data2 <- do.call(rbind, data_list)

combined_data2 <- combined_data2 %>% 
  mutate(x = round(x)) %>% 
  mutate(patient = patient) %>% 
  # group_by(patient,x) %>% 
  # summarise(y = mean(y)) %>% 
  ungroup() %>% 
  mutate(test = "qCulture")

 

ggplot(data = combined_data2) + 
  geom_line(aes(x = x, y = log10(y),  group = patient)) + 
  geom_point(aes(x = x, y = log10(y),  group = patient)) + 
  theme_bw() + 
  xlab("Days post infection") + 
  ylab("log10(viral titer)") + 
  #ggtitle("RSV viral load") +
  scale_x_continuous(breaks = c(2,4,6,8,10,12)) + 
  theme(
    axis.text.x = element_text(face="bold"),
    axis.text.y = element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    plot.title = element_text(face="bold", hjust = 0.5),
    #axis.line = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right") + 
  theme(axis.text.x = element_text(color ="black",
                                   size = 18, angle=0),
        axis.text.y = element_text(color ="black",
                                   size = 18, angle=0),
        text = element_text(size = 18))  + 
  facet_wrap(~patient, nrow =3)




ggplot() + 
  geom_line(data = combined_data1, aes(x = x, y = log10(y),  group = patient, color = "qPCR")) + 
  geom_point(data = combined_data1, aes(x = x, y = log10(y),  group = patient, color = "qPCR")) + 
  geom_line(data = combined_data2, aes(x = x, y = log10(y),  group = patient, color = "qCulture")) + 
  geom_point(data = combined_data2, aes(x = x, y = log10(y),  group = patient, color = "qCulture")) + 
  theme_bw() + 
  xlab("Days post infection") + 
  ylab("log10(viral titer)") + 
  #ggtitle("RSV viral load") +
  scale_x_continuous(breaks = c(2,4,6,8,10,12)) + 
  theme(
    axis.text.x = element_text(face="bold"),
    axis.text.y = element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    plot.title = element_text(face="bold", hjust = 0.5),
    #axis.line = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top") + 
  scale_color_manual(name = "Measurement Method",
                     values = c("qPCR" = "brown1", "qCulture" = "cornflowerblue"),
                     labels = c("qPCR" = "qPCR Data", "qCulture" = "qCulture Data")) +
  theme(axis.text.x = element_text(color ="black",
                                   size = 18, angle=0),
        axis.text.y = element_text(color ="black",
                                   size = 18, angle=0),
        text = element_text(size = 18))  + 
  facet_wrap(~patient, nrow = 3)


combined_data <- data.frame(qPCR = log10(combined_data1$y), 
                            qCulture = log10(combined_data2$y),
                            patient = combined_data1$patient) 

combined_data$qCulture[combined_data$qCulture<0] <- 0
combined_data$qPCR[combined_data$qPCR<0] <- 0


plot(combined_data$qPCR, combined_data$qCulture)


normalised_qCulture <- (combined_data$qCulture - min(combined_data$qCulture)) / (max(combined_data$qCulture) - min(combined_data$qCulture))

combined_data$normalised_qCulture <- normalised_qCulture
#combined_data$qPCR[combined_data$qPCR <0] <- 0


combined_data  <- combined_data %>% 
  mutate(PCR_group = ifelse(qPCR >0 & qPCR < 0.5, 0.5, 
                            ifelse(qPCR >=0.5 & qPCR < 1, 1,
                                   ifelse(qPCR >=1 & qPCR < 1.5, 1.5, 
                                          ifelse(qPCR >=1.5 & qPCR < 2, 2, 
                                                 ifelse(qPCR >=2 & qPCR < 2.5, 2.5, 
                                                        ifelse(qPCR >=2.5 & qPCR < 3, 3, 
                                                               ifelse(qPCR >=3 & qPCR < 3.5, 3.5, 
                                                                      ifelse(qPCR >=3.5 & qPCR < 4, 4,
                                                                             ifelse(qPCR >=4 & qPCR < 4.5, 4.5,
                                                                                    ifelse(qPCR >=4.5 & qPCR < 5, 5,
                                                                                           ifelse(qPCR >=5 & qPCR < 5.5, 5.5,
                                                                                                  ifelse(qPCR >=5.5 & qPCR < 6,6,
                                                                                                         ifelse(qPCR >=6 & qPCR < 6.5,6.5,
                                                                                                                ifelse(qPCR >=6.5 & qPCR < 7,7,
                                                                                                                       ifelse(qPCR >=7 & qPCR < 7.5, 7.5,
                                                                                                                              ifelse(qPCR >=7.5 & qPCR < 8,8,
                                                                                                                                     ifelse(qPCR >= 9 & qPCR < 8.5, 8.5, NA)))))))))))))))))) %>% 
  filter(!is.na(PCR_group))
combined_data_pcr_group <- combined_data %>% 
  group_by(PCR_group) %>% 
  summarise(qc = mean(qCulture),
            low = quantile(qCulture, 0.025),
            upp = quantile(qCulture, 0.975)) 

# ggplot(combined_data_pcr_group, aes(x = PCR_group, y = qc)) +
#   geom_point(size = 3, color = "purple") +
#   geom_errorbar(aes(ymin = low, ymax = upp), width = 0.2, color = "purple") +
#   theme_minimal() +
#   xlab("qPCR log10 (PFUe/ml)") +
#   ylab("Mean qCulture with 95% CI log10 (PFU/ml)") +
#   theme(
#     axis.text.x = element_text(face = "bold"),
#     axis.text.y = element_text(face = "bold"),
#     axis.title.x = element_text(face = "bold"),
#     axis.title.y = element_text(face = "bold")
#   )


# qPCR_culture_children <- vroom::vroom("PCR_Culture/children.csv")
# qPCR_culture_children.df <- data.frame(qCulture = log10(qPCR_culture_children$x), 
#                                        qPCR = log10(qPCR_culture_children$y)) %>% 
#   filter(qCulture < 6.78)  
# 
# plot(qPCR_culture_children.df$qPCR, qPCR_culture_children.df$qCulture, xlim = c(0,9), ylim = c(0,9))


# Fit linear model
linear_model <- lm(qCulture ~ qPCR, data = combined_data)

# Summary of the linear model
summary(linear_model)
AIC(linear_model)
combined_data$linear_model <- predict(linear_model, newdata = combined_data)






# Define the power law function
power_law_function <- function(x, omega, h) {
  omega * x^h
}


# Fit the Hill function using nls
power_law_model <- nls(qCulture ~ power_law_function(qPCR , omega, h), data = combined_data, 
                       start = list(omega = 2, h = 3))

# Summary of the Hill model
summary(power_law_model)
AIC(power_law_model)

# Get predictions from the Hill model
combined_data$power_law_pred <- predict(power_law_model, newdata = combined_data)





# Define the Hill function
hill_function <- function(x, Vmax, K, n) {
  Vmax * (x^n) / (K^n + x^n)
}

# plot(hill_function(combined_data$qPCR, 2, 4, 1))
# lines(combined_data$normalised_qCulture)

# Fit the Hill function using nls with better starting values
hill_model <- nlsLM(qCulture ~ hill_function(qPCR, Vmax, K, n), 
                    data = combined_data, 
                    start = list(Vmax = 6, K = 2, n = 4),
                    control = nls.control(maxiter = 200))



# Summary of the Hill model
summary(hill_model)
AIC(hill_model)


# Get predictions from the Hill model
combined_data$hill_pred <- predict(hill_model, newdata = combined_data)


library(boot)

# Define a function to calculate fitted values
predict_function <- function(data, indices) {
  d <- data[indices, ]
  fit <- nlsLM(qCulture ~ hill_function(qPCR, Vmax, K, n), 
               data = d, 
               start = list(Vmax = 6.5894, K = 5.3857, n = 3.9065),
               control = nls.control(maxiter = 200))
  return(predict(fit, newdata = combined_data))
}

# Perform bootstrapping
set.seed(1253) # For reproducibility
bootstrap_results <- boot(data = combined_data[,c(1,2)], statistic = predict_function, R = 1000)

# Calculate standard errors for the fitted values
std_errors <- apply(bootstrap_results$t, 2, sd)

# Predict fitted values using the original model
combined_data$hill_pred <- predict(hill_model, newdata = combined_data)

# Calculate confidence intervals
combined_data$ci_lower <- combined_data$hill_pred - 1.96 * std_errors
combined_data$ci_upper <- combined_data$hill_pred + 1.96 * std_errors


combined_data$ci_lower[combined_data$ci_lower<0] <- 0


# Plot the fitted Hill model
p2 <- ggplot(combined_data, aes(x = qPCR, y = qCulture)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "#91bfdb", alpha = 0.3)+
  geom_point(size = 3) +
  geom_line(aes(y = hill_pred, color = "hill model" ), size = 1.5) +
  #geom_line(aes(y = power_law_pred, color = "power law model"), size = 1.5) +
  #geom_line(aes(y = linear_model, color = "linear model"), size = 1.5, linetype = 2) +
  theme_bw() +
  scale_color_manual(name = "Method",
                     values = c("hill model" = "#d6604d", "power law model" = "#74add1", "linear model" = "gray"),
                     labels = c("hill model" = "hill model", "power law model" = "power law model")) +
  xlab("qPCR, [log10 viral load (PFUe/ml)]") +
  ylab("viral titer [log10 TCID50/ml]") + 
  theme(
    text = element_text(size = 15),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 15)
  )+
  theme(
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    legend.position = "na", # You can change this to "bottom", "left", "right", or a specific coordinate
    legend.box.background = element_rect(color = "black"),
    legend.box.margin = margin(6, 6, 6, 6)
  ) +
  scale_x_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+ 
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))

#annotate("text", x = 2, y = 4, label = expression(V = 6.6*V_pcr^3.9/(V_pcr^3.9 + 5.39^3.9)), size = 5, color = "black")
p2
