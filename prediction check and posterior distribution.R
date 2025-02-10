library(tidyverse)
library(bayesplot)
library(ggpubr)

input_data <- readRDS("data_stan_53_patients.rds")
data_stan <- readRDS("data_stan_53_patients.rds")

estimated_pars <-  c("mu_v_p","mu_t_p", "mu_lambda_g", "mu_lambda_d","mu_incub",
                     "sigma_v_p", "sigma_t_p", "sigma_lambda_g", "sigma_lambda_d", "sigma_incub",
                     "v_p", "t_p", "lambda_g", "lambda_d", "incub", "sigma")

 
fit_rsv_model <- readRDS("fit_rsv_results_incubation_53_patients_5chains.rds") # model output from Stan

 


# extract posterior samples for selected parameters
posterior_samples_all_model = rstan::extract(fit_rsv_model , pars = estimated_pars, inc_warmup = TRUE, permuted = FALSE)
posterior_samples_merged_after_burnin_model = rstan::extract(fit_rsv_model , pars = estimated_pars)


# color_scheme_set("brewer-Spectral")
# mcmc_trace(posterior_samples_all_model, n_warmup = 2500,
#            facet_args = list(nrow = 4, labeller = label_parsed))


# show all marginal posterior distributions
posterior_sample_group_parameters_vp <-  data.frame(
  v_p1 = posterior_samples_merged_after_burnin_model$mu_v_p[,1],
  v_p2 = posterior_samples_merged_after_burnin_model$mu_v_p[,2],
  v_p3 = posterior_samples_merged_after_burnin_model$mu_v_p[,3]) %>% 
  pivot_longer(cols = v_p1:v_p3, names_to = "group", values_to = "v_p") %>% 
  mutate(group = ifelse(group == "v_p1", "Pediatric group ", 
                        ifelse(group == "v_p2", "Adult group ", 
                               ifelse(group == "v_p3", "Elderly group ", NA)))) %>% 
  mutate(group = factor(group, 
                        levels = c("Pediatric group ", "Adult group ", "Elderly group ")))


posterior_sample_group_parameters_tp <-  data.frame(                                         
  t_p1 = posterior_samples_merged_after_burnin_model$mu_t_p[,1],
  t_p2 = posterior_samples_merged_after_burnin_model$mu_t_p[,2],
  t_p3 = posterior_samples_merged_after_burnin_model$mu_t_p[,3])%>% 
  pivot_longer(cols = t_p1:t_p3, names_to = "group", values_to = "t_p") %>% 
  mutate(group = ifelse(group == "t_p1", "Pediatric group ", 
                        ifelse(group == "t_p2", "Adult group ", 
                               ifelse(group == "t_p3", "Elderly group ", NA)))) %>% 
  mutate(group = factor(group, 
                        levels = c("Pediatric group ", "Adult group ", "Elderly group ")))

posterior_sample_group_parameters_lambda.g <-  data.frame(                                               
  lambda_g1 = posterior_samples_merged_after_burnin_model$mu_lambda_g[,1],
  lambda_g2 = posterior_samples_merged_after_burnin_model$mu_lambda_g[,2],
  lambda_g3 = posterior_samples_merged_after_burnin_model$mu_lambda_g[,3])%>% 
  pivot_longer(cols = lambda_g1:lambda_g3, names_to = "group", values_to = "lambda_g") %>% 
  mutate(group = ifelse(group == "lambda_g1", "Pediatric group ", 
                        ifelse(group == "lambda_g2", "Adult group ", 
                               ifelse(group == "lambda_g3", "Elderly group ", NA)))) %>% 
  mutate(group = factor(group,  
                        levels = c("Pediatric group ", "Adult group ", "Elderly group ")))

posterior_sample_group_parameters_lambda.d <-  data.frame(                                
  lambda_d1 = posterior_samples_merged_after_burnin_model$mu_lambda_d[,1],
  lambda_d2 = posterior_samples_merged_after_burnin_model$mu_lambda_d[,2], 
  lambda_d3 = posterior_samples_merged_after_burnin_model$mu_lambda_d[,3])%>% 
  pivot_longer(cols = lambda_d1:lambda_d3, names_to = "group", values_to = "lambda_d") %>% 
  mutate(group = ifelse(group == "lambda_d1", "Pediatric group ", 
                        ifelse(group == "lambda_d2", "Adult group ", 
                               ifelse(group == "lambda_d3", "Elderly group ", NA)))) %>% 
  mutate(group = factor(group, 
                        levels = c("Pediatric group ", "Adult group ", "Elderly group ")))


posterior_sample_group_parameters_incubation <-  data.frame(                                
  incubation1 = posterior_samples_merged_after_burnin_model$mu_incub[,1],
  incubation2 = posterior_samples_merged_after_burnin_model$mu_lambda_d[,2], 
  incubation3 = posterior_samples_merged_after_burnin_model$mu_incub[,3])%>% 
  pivot_longer(cols = incubation1:incubation3, names_to = "group", values_to = "incubation") %>% 
  mutate(group = ifelse(group == "incubation1", "Pediatric group ", 
                        ifelse(group == "incubation2", "Adult group ", 
                               ifelse(group == "incubation3", "Elderly group ", NA)))) %>% 
  mutate(group = factor(group, 
                        levels = c("Pediatric group ", "Adult group ", "Elderly group ")))


# ================== plot posterior and prior for group-level parameter =================

font_size = 18

median_v_p <- posterior_sample_group_parameters_vp %>% 
  group_by(group) %>% 
  summarise(value = median(v_p),
            low = quantile(v_p, 0.05),
            upp = quantile(v_p, 0.975))


p1 <- posterior_sample_group_parameters_vp %>% 
  #filter(group == "Adult group ") %>% 
  ggplot(aes(x = v_p)) +
  geom_histogram(breaks=seq(0,20,1/10), aes(y = ..density.., fill = group, group = group)) +
  #stat_function(fun = function(x) {dnorm(x, 5, 5)}, aes(color = 'prior')) +
  geom_vline(data=subset(median_v_p, group == "Pediatric group "), aes(xintercept=median_v_p$value[1]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_v_p, group == "Pediatric group "), aes(x=round(median_v_p$value[1],2), y=1, 
                                                                      label=round(median_v_p$value[1],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
 
  geom_vline(data=subset(median_v_p, group == "Adult group "), aes(xintercept=median_v_p$value[2]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_v_p, group == "Adult group "), aes(x=round(median_v_p$value[2],2), y=1, 
                                                                      label=round(median_v_p$value[2],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  
  geom_vline(data=subset(median_v_p, group == "Elderly group "), aes(xintercept=median_v_p$value[3]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_v_p, group == "Elderly group "), aes(x=round(median_v_p$value[3],2), y=1, 
                                                                      label=round(median_v_p$value[3],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  lims(x = c(0,15)) +
  theme_bw() +
  # scale_colour_manual(name="Distribution",
  #                     values=c("prior"="lightblue")) + 
  scale_fill_manual(name="Distribution",
                    values=c("Pediatric group " = "#1f78b4",
                             "Adult group "  = "#fdbf6f",
                             "Elderly group " = "#fb9a99"),
                    guide = "none") + 
  xlab("log10 peak viral load (copy)")  +
  theme(axis.text.x = element_text(color="black",
                                   size = font_size, angle=0),
        axis.text.y = element_text(color="black",
                                   size= font_size, angle=0),
        text = element_text(size = font_size))  + 
  facet_wrap(~group, nrow = 3) +
  scale_y_continuous(limits = c(0,3), breaks = c(0,1,2,3)) + 
  theme(
    axis.text.x = element_text(face="bold"),
    axis.text.y = element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    plot.title = element_text(face="bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "NA",
    legend.title =  element_text(size = 0))
  



median_t_p <- posterior_sample_group_parameters_tp %>% 
  group_by(group) %>% 
  summarise(value = median(t_p), 
            low = quantile(t_p, 0.05),
            upp = quantile(t_p, 0.975))



p2 <- posterior_sample_group_parameters_tp %>% 
  #filter(group == "Elderly group ") %>% 
  ggplot(aes(x = t_p)) +
  geom_histogram(breaks=seq(0,15,1/10), aes(y = ..density.., fill = group, group = group)) +
  #stat_function(fun = function(x) {dnorm(x, 5, 5)}, aes(color = 'prior')) +
  geom_vline(data=subset(median_t_p, group == "Pediatric group "), aes(xintercept=median_t_p$value[1]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_t_p, group == "Pediatric group "), aes(x=round(median_t_p$value[1],2), y = .75, 
                                                                      label=round(median_t_p$value[1],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  
  geom_vline(data=subset(median_t_p, group == "Adult group "), aes(xintercept=median_t_p$value[2]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_t_p, group == "Adult group "), aes(x=round(median_t_p$value[2],2), y = .75, 
                                                                  label=round(median_t_p$value[2],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  
  geom_vline(data=subset(median_t_p, group == "Elderly group "), aes(xintercept=median_t_p$value[3]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_t_p, group == "Elderly group "), aes(x=round(median_t_p$value[3],2), y = 0.75, 
                                                                    label=round(median_t_p$value[3],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  lims(x = c(0,15)) +
  theme_bw() +
  # scale_colour_manual(name="Distribution",
  #                     values=c("prior"="lightblue")) + 
  scale_fill_manual(name="Distribution",
                    values=c("Pediatric group " = "#1f78b4",
                             "Adult group "  = "#fdbf6f",
                             "Elderly group " = "#fb9a99"),
                    guide = "none") + 
  xlab("Time to peak viral load (days)")  +
  theme(axis.text.x = element_text(color="black",
                                   size = font_size, angle=0),
        axis.text.y = element_text(color="black",
                                   size= font_size, angle=0),
        text = element_text(size = font_size))  + 
  facet_wrap(~group, nrow = 3) + 
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2)) + 
  theme(
    axis.text.x = element_text(face="bold"),
    axis.text.y = element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    plot.title = element_text(face="bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "NA",
    legend.title =  element_text(size = 0))
 

p2 



median_lambda.g <- posterior_sample_group_parameters_lambda.g %>% 
  group_by(group) %>% 
  summarise(value = median(lambda_g),
            low = quantile(lambda_g, 0.05),
            upp = quantile(lambda_g, 0.975))

p3 <- posterior_sample_group_parameters_lambda.g %>% 
  #filter(group == "Elderly group ") %>% 
  ggplot(aes(x = lambda_g)) +
  geom_histogram(breaks=seq(0,10,1/10), aes(y = ..density.., fill = group, group = group)) +
  #stat_function(fun = function(x) {dnorm(x, 5, 5)}, aes(color = 'prior')) +
  geom_vline(data=subset(median_lambda.g, group == "Pediatric group "), aes(xintercept=median_lambda.g$value[1]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_lambda.g, group == "Pediatric group "), aes(x=round(median_lambda.g$value[1],2), y=.45, 
                                                                      label=round(median_lambda.g$value[1],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  
  geom_vline(data=subset(median_lambda.g, group == "Adult group "), aes(xintercept=median_lambda.g$value[2]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_lambda.g, group == "Adult group "), aes(x=round(median_lambda.g$value[2],2), y=.45, 
                                                                  label=round(median_lambda.g$value[2],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  
  geom_vline(data=subset(median_lambda.g, group == "Elderly group "), aes(xintercept=median_lambda.g$value[3]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_lambda.g, group == "Elderly group "), aes(x=round(median_lambda.g$value[3],2), y=.45, 
                                                                    label=round(median_lambda.g$value[3],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  lims(x = c(0,10)) +
  theme_bw() +
  # scale_colour_manual(name="Distribution",
  #                     values=c("prior"="lightblue")) + 
  scale_fill_manual(name="Distribution",
                    values=c("Pediatric group " = "#1f78b4",
                             "Adult group "  = "#fdbf6f",
                             "Elderly group " = "#fb9a99"),
                    guide = "none")  + 
  xlab("The growth rate of log10 viral load (copy/day)") +
  theme(axis.text.x = element_text(color="black",
                                   size = font_size, angle=0),
        axis.text.y = element_text(color="black",
                                   size= font_size, angle=0),
        text = element_text(size = font_size))  + 
  facet_wrap(~group, nrow = 3)+
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2)) + 
  theme(
    axis.text.x = element_text(face="bold"),
    axis.text.y = element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    plot.title = element_text(face="bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "NA",
    legend.title =  element_text(size = 0))
 


median_lambda.d <- posterior_sample_group_parameters_lambda.d %>% 
  group_by(group) %>% 
  summarise(value = median(lambda_d),
            low = quantile(lambda_d, 0.05),
            upp = quantile(lambda_d, 0.975))

p4 <- posterior_sample_group_parameters_lambda.d %>% 
  #filter(group == "Elderly group ") %>% 
  ggplot(aes(x = lambda_d)) +
  geom_histogram(breaks=seq(0,10,1/15), aes(y = ..density.., fill = group, group = group)) +
  #stat_function(fun = function(x) {dnorm(x, 5, 5)}, aes(color = 'prior')) +
  geom_vline(data=subset(median_lambda.d, group == "Pediatric group "), aes(xintercept=median_lambda.d$value[1]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_lambda.d, group == "Pediatric group "), aes(x=round(median_lambda.d$value[1],2), y=1.5, 
                                                                           label=round(median_lambda.d$value[1],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  
  geom_vline(data=subset(median_lambda.d, group == "Adult group "), aes(xintercept=median_lambda.d$value[2]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_lambda.d, group == "Adult group "), aes(x=round(median_lambda.d$value[2],2), y=1.5, 
                                                                       label=round(median_lambda.d$value[2],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  
  geom_vline(data=subset(median_lambda.d, group == "Elderly group "), aes(xintercept=median_lambda.d$value[3]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_lambda.d, group == "Elderly group "), aes(x=round(median_lambda.d$value[3],2), y=1.5, 
                                                                         label=round(median_lambda.d$value[3],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  lims(x = c(0,10)) +
  theme_bw() +
  # scale_colour_manual(name="Distribution",
  #                     values=c("prior"="lightblue")) + 
  scale_fill_manual(name="Distribution",
                    values=c("Pediatric group " = "#1f78b4",
                             "Adult group "  = "#fdbf6f",
                             "Elderly group " = "#fb9a99"),
                    guide = "none")  + 
  xlab("The decline rate of log10 viral load (copy/day)") +
  theme(axis.text.x = element_text(color="black",
                                   size = font_size, angle=0),
        axis.text.y = element_text(color="black",
                                   size= font_size, angle=0),
        text = element_text(size = font_size))  + 
  facet_wrap(~group, nrow = 3)+
  scale_y_continuous(limits = c(0,3.5), breaks = c(0,1,2,3)) +
  theme(
    axis.text.x = element_text(face="bold"),
    axis.text.y = element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    plot.title = element_text(face="bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "NA",
    legend.title =  element_text(size = 0))

library(ggpubr)

combined_plots <- ggarrange(p1, p2, p3, p4,
                            ncol=2, nrow=2, 
                            labels = c("A", "B", "C", "D"),
                            align = "v",
                            font.label = list(size = 20, color = "black"))
combined_plots

median_incubation <- posterior_sample_group_parameters_incubation %>% 
  group_by(group) %>% 
  summarise(value = median(incubation))



posterior_sample_group_parameters_incubation %>% 
  filter(group != "Adult group ") %>% 
  ggplot(aes(x = incubation)) +
  geom_histogram(breaks=seq(0,10,1/5), aes(y = ..density.., fill = group, group = group)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = sqrt(3)), color = "black", size = 0.8) +# Add the normal density curve
  geom_vline(data=subset(median_incubation, group == "Pediatric group "), aes(xintercept=median_incubation$value[1]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_incubation, group == "Pediatric group "), aes(x=round(median_incubation$value[1],2), y=.5, 
                                                                           label=round(median_incubation$value[1],2)), 
            vjust=0.5, hjust=-1, color='black', size = 8) +
  geom_vline(data=subset(median_incubation, group == "Elderly group "), aes(xintercept=median_incubation$value[3]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_incubation, group == "Elderly group "), aes(x=round(median_incubation$value[3],2), y=.5, 
                                                                         label=round(median_incubation$value[3],2)), 
            vjust=0.5, hjust=-1, color='black', size = 8) +
  lims(x = c(0,8)) +
  theme_classic() +
  scale_fill_manual(name="Distribution",
                    values=c("Pediatric group " = "#1f78b4",
                             "Adult group "  = "#fdbf6f",
                             "Elderly group " = "#fb9a99"),
                    guide = "none")  + 
  facet_wrap(~group, nrow = 3)+
  xlab("Incubation period (days)") + 
  theme(axis.text.x = element_text(color="black",
                                   size = 20, angle=0),
        axis.text.y = element_text(color="black",
                                   size= 20, angle=0),
        text = element_text(size = 20))   +
  theme(
    axis.text.x = element_text(face="bold"),
    axis.text.y = element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    plot.title = element_text(face="bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "NA",
    legend.title =  element_text(size = 0))

 

# ================== predictive check for each individuals in each age group ================= #

VL_prediction <- function(v_p,t_p,lambda_g,lambda_d,ts,incubation) {
  V_predicted <- 2*10^v_p/( exp(-lambda_g*(ts+incubation-t_p)) +  exp(lambda_d*(ts+incubation-t_p)))
  return(V_predicted)
}

posteriors_combined <- data.frame(group = posterior_sample_group_parameters_vp$group, 
                                  v_p = posterior_sample_group_parameters_vp$v_p,
                                  t_p = posterior_sample_group_parameters_tp$t_p,
                                  lambda_g = posterior_sample_group_parameters_lambda.g$lambda_g,
                                  lambda_d = posterior_sample_group_parameters_lambda.d$lambda_d,
                                  incubation = posterior_sample_group_parameters_incubation$incubation)

posteriors_combined <- posteriors_combined %>% 
  mutate(VL0 = log10(VL_prediction(v_p, t_p, lambda_g, lambda_d, (0-incubation), incubation))) 

median_VL0 <- posteriors_combined %>% 
  group_by(group) %>% 
  summarise(value = median(VL0))

posteriors_combined %>% 
  ggplot(aes(x = VL0)) +
  geom_histogram(breaks=seq(-15,10,1/5), aes(y = ..density.., fill = group, group = group)) +
  geom_vline(data=subset(median_VL0, group == "Pediatric group "), aes(xintercept=median_VL0$value[1]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_VL0, group == "Pediatric group "), aes(x=round(median_VL0$value[1],2), y=.3, 
                                                                           label=round(median_VL0$value[1],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  geom_vline(data=subset(median_VL0, group == "Adult group "), aes(xintercept=median_VL0$value[2]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_VL0, group == "Adult group "), aes(x=round(median_VL0$value[2],2), y=.3, 
                                                                      label=round(median_VL0$value[2],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  geom_vline(data=subset(median_VL0, group == "Elderly group "), aes(xintercept=median_VL0$value[3]),
             linetype="dashed",color = 'black') +
  geom_text(data=subset(median_VL0, group == "Elderly group "), aes(x=round(median_VL0$value[3],2), y=.3, 
                                                                  label=round(median_VL0$value[3],2)), 
            vjust=0.5, hjust=-1, color='black', size = 4) +
  lims(x = c(-15,10)) +
  theme_classic() +
 
  scale_fill_manual(name="Distribution",
                    values=c("Pediatric group " = "#1f78b4",
                             "Adult group "  = "#fdbf6f",
                             "Elderly group " = "#fb9a99"),
                    guide = "none")  + 
  facet_wrap(~group, nrow = 3) + 
  xlab("log10 initial viral load (copy)") +
  theme(axis.text.x = element_text(color="black",
                                   size = 12, angle=0),
        axis.text.y = element_text(color="black",
                                   size= 12, angle=0),
        text = element_text(size = 12))  

# ================== predictive check for initial VL at the group level ================= #


ts <- input_data$ts
time_point <- input_data$N
N_patients <- input_data$M

N_patients_children <- 24
N_patients_adults <- 7
N_patients_elderly <- 22

children_indx <- paste0("V",1:N_patients_children)
adults_indx <- paste0("V", (N_patients_children+1):(N_patients_children+N_patients_adults)) 
elderly_indx <- paste0("V", (N_patients_children+N_patients_adults+1):(N_patients))

v_p <-  as.data.frame(posterior_samples_merged_after_burnin_model$v_p) %>% 
  pivot_longer(cols = V1:V53, names_to = "indx", values_to = "v_p") %>% 
  mutate(group = ifelse(indx %in% children_indx, "Pediatric group ", 
                        ifelse(indx %in% adults_indx, "Adult group ", "Elderly group "))) %>% 
  mutate(group = factor(group, 
                        levels = c("Pediatric group ", "Adult group ", "Elderly group "))) %>% 
  mutate(patient_id = rep(c(1:N_patients), length.out = length(posterior_samples_merged_after_burnin_model$v_p)))



t_p <-  as.data.frame(posterior_samples_merged_after_burnin_model$t_p) %>% 
  pivot_longer(cols = V1:V53, names_to = "indx", values_to = "t_p") %>% 
  mutate(group = ifelse(indx %in% children_indx, "Pediatric group ", 
                        ifelse(indx %in% adults_indx, "Adult group ", "Elderly group "))) %>% 
  mutate(group = factor(group, 
                        levels = c("Pediatric group ", "Adult group ", "Elderly group "))) %>% 
  mutate(patient_id = rep(c(1:N_patients), length.out = length(posterior_samples_merged_after_burnin_model$v_p)))




lambda_g <-  as.data.frame(posterior_samples_merged_after_burnin_model$lambda_g) %>% 
  pivot_longer(cols = V1:V53, names_to = "indx", values_to = "lambda_g") %>% 
  mutate(group = ifelse(indx %in% children_indx, "Pediatric group ", 
                        ifelse(indx %in% adults_indx, "Adult group ", "Elderly group "))) %>% 
  mutate(group = factor(group, 
                        levels = c("Pediatric group ", "Adult group ", "Elderly group "))) %>% 
  mutate(patient_id = rep(c(1:N_patients), length.out = length(posterior_samples_merged_after_burnin_model$v_p)))



lambda_d <- as.data.frame(posterior_samples_merged_after_burnin_model$lambda_d) %>% 
  pivot_longer(cols = V1:V53, names_to = "indx", values_to = "lambda_d") %>% 
  mutate(group = ifelse(indx %in% children_indx, "Pediatric group ", 
                        ifelse(indx %in% adults_indx, "Adult group ", "Elderly group "))) %>% 
  mutate(group = factor(group, 
                        levels = c("Pediatric group ", "Adult group ", "Elderly group "))) %>% 
  mutate(patient_id = rep(c(1:N_patients), length.out = length(posterior_samples_merged_after_burnin_model$v_p)))



incubation <- as.data.frame(posterior_samples_merged_after_burnin_model$incub) %>% 
  pivot_longer(cols = V1:V53, names_to = "indx", values_to = "incub") %>% 
  mutate(group = ifelse(indx %in% children_indx, "Pediatric group ", 
                        ifelse(indx %in% adults_indx, "Adult group ", "Elderly group "))) %>% 
  mutate(group = factor(group, 
                        levels = c("Pediatric group ", "Adult group ", "Elderly group "))) %>% 
  mutate(patient_id = rep(c(1:N_patients), length.out = length(posterior_samples_merged_after_burnin_model$v_p)))




# ================== predictive check for VL at individual level (Pediatric group ) ================= #


v_p_hat <- v_p %>% 
  filter(group == "Pediatric group ") %>% 
  group_by(patient_id) %>% 
  summarise(v_p = median(v_p))

t_p_hat <- t_p %>% 
  filter(group == "Pediatric group ") %>% 
  group_by(patient_id) %>% 
  summarise(t_p = median(t_p))

lambda_g_hat <- lambda_g %>% 
  filter(group == "Pediatric group ") %>% 
  group_by(patient_id) %>% 
  summarise(lambda_g = median(lambda_g))

lambda_d_hat <- lambda_d %>% 
  filter(group == "Pediatric group ") %>% 
  group_by(patient_id) %>% 
  summarise(lambda_d = median(lambda_d))

incub_hat <- incubation %>% 
  filter(group == "Pediatric group ") %>% 
  group_by(patient_id) %>% 
  summarise(incub = median(incub))


t_max <- 25 # max end time point
#t_point <-  c(1:t_max)
t_point <- seq(0,t_max,0.1)
y_hat_children <- matrix(, nrow = length(t_point), ncol = N_patients_children)

for(i in 1:N_patients_children){  
  
  y_hat_children[,i] <- VL_prediction(v_p_hat$v_p[i], 
                                      t_p_hat$t_p[i], 
                                      lambda_g_hat$lambda_g[i], 
                                      lambda_d_hat$lambda_d[i], 
                                      t_point-incub_hat$incub[i],
                                      incub_hat$incub[i])
}



y_hat_children.df <- as.data.frame(y_hat_children) %>% 
  pivot_longer(cols = V1:V24, names_to = "patient", values_to = "VL") %>% 
  mutate(ts = rep(t_point, each = 24)) 

children_data <- data_stan$y[1:N_patients_children,]
children_vl <- as.vector(t(children_data))
#children_vl <- children_vl[children_vl != 0]

ts_data <- data_stan$ts[1:N_patients_children,]
ts_data <- as.vector(t(ts_data))
#ts_data <- ts_data[ts_data != 0]

N <- data_stan$N

children_data.df <- data.frame(ts = ts_data,
                               VL = exp(children_vl)) %>% 
  filter(ts != 0) %>% 
  filter(VL != 1) %>% 
  mutate(patient = rep(paste0("V",1:N_patients_children), N[1:N_patients_children])) %>% 
  mutate(ts = ts + rep(incub_hat$incub, table(patient)))



new_levels <- paste("patient", 1:N_patients_children)

# Update the levels in both data frames
y_hat_children.df$patient <- factor(y_hat_children.df$patient, levels = paste0("V", 1:N_patients_children), labels = new_levels)
children_data.df$patient <- factor(children_data.df$patient, levels = paste0("V", 1:N_patients_children), labels = new_levels)



incub_hat$patient <-  unique(children_data.df$patient)
 
ggplot() + 
  geom_line(data = y_hat_children.df, aes(x = ts, y = log10(VL), group = patient)) + 
  geom_point(data= children_data.df, aes(x = ts, y= log10(VL), group = patient)) + 
  geom_vline(data=subset(incub_hat, patient == "patient 1"), aes(xintercept=incub_hat$incub[1]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 2"), aes(xintercept=incub_hat$incub[2]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 3"), aes(xintercept=incub_hat$incub[3]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 4"), aes(xintercept=incub_hat$incub[4]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 5"), aes(xintercept=incub_hat$incub[5]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 6"), aes(xintercept=incub_hat$incub[6]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 7"), aes(xintercept=incub_hat$incub[7]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 8"), aes(xintercept=incub_hat$incub[8]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 9"), aes(xintercept=incub_hat$incub[9]),
             linetype="dashed",color = 'brown3') + 
  geom_vline(data=subset(incub_hat, patient == "patient 10"), aes(xintercept=incub_hat$incub[10]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 11"), aes(xintercept=incub_hat$incub[11]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 12"), aes(xintercept=incub_hat$incub[12]),
             linetype="dashed",color = 'brown3') +
  
  geom_vline(data=subset(incub_hat, patient == "patient 13"), aes(xintercept=incub_hat$incub[13]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 14"), aes(xintercept=incub_hat$incub[14]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 15"), aes(xintercept=incub_hat$incub[15]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 16"), aes(xintercept=incub_hat$incub[16]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 17"), aes(xintercept=incub_hat$incub[17]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 18"), aes(xintercept=incub_hat$incub[18]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 19"), aes(xintercept=incub_hat$incub[19]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 20"), aes(xintercept=incub_hat$incub[20]),
             linetype="dashed",color = 'brown3') +
  
  geom_vline(data=subset(incub_hat, patient == "patient 21"), aes(xintercept=incub_hat$incub[21]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 22"), aes(xintercept=incub_hat$incub[22]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 23"), aes(xintercept=incub_hat$incub[23]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 24"), aes(xintercept=incub_hat$incub[24]),
             linetype="dashed",color = 'brown3') +
  xlab("days post infection") + 
  ylab("log10 viral load (pfue/ml)") + 
  theme(strip.text = element_text(size = 20),
        axis.title = element_text(size = 20),    # Font size for axis titles
        axis.text = element_text(size = 20)) +
  facet_wrap(~patient) + 
  scale_x_continuous(limits = c(1, 16), breaks = c(2,4,6,8,10,12,14,16)) + 
  scale_y_continuous(limits = c(-2,8), breaks = c(-2,0,2,4,6,8)) +
  theme_bw() + 
  theme(axis.text.x = element_text(color="black",
                                   size = 12, angle=0),
        axis.text.y = element_text(color="black",
                                   size= 12, angle=0),
        text = element_text(size = 12)) 






# ================== predictive check for VL at individual level (Adult group ) ================= #



# VL_prediction_adults <- function(v_p,t_p,lambda_g,lambda_d,ts) {
#   V_predicted <- 2*10^v_p/( exp(-lambda_g*(ts-t_p)) +  exp(lambda_d*(ts-t_p)))
#   return(V_predicted)
# }


y_hat_adults <- matrix(, nrow = length(t_point), ncol = N_patients_adults)

v_p_hat <- v_p %>% 
  filter(group == "Adult group ") %>% 
  group_by(patient_id) %>% 
  summarise(v_p = median(v_p))

t_p_hat <- t_p %>% 
  filter(group == "Adult group ") %>% 
  group_by(patient_id) %>% 
  summarise(t_p = median(t_p))

lambda_g_hat <- lambda_g %>% 
  filter(group == "Adult group ") %>% 
  group_by(patient_id) %>% 
  summarise(lambda_g = median(lambda_g))

lambda_d_hat <- lambda_d %>% 
  filter(group == "Adult group ") %>% 
  group_by(patient_id) %>% 
  summarise(lambda_d = median(lambda_d))

incub_hat <- incubation %>% 
  filter(group == "Adult group ") %>% 
  group_by(patient_id) %>% 
  summarise(incub = median(incub))

 

for(i in 1:N_patients_adults){
  
  y_hat_adults[,i] <- VL_prediction(v_p_hat$v_p[i], 
                                    t_p_hat$t_p[i], 
                                    lambda_g_hat$lambda_g[i], 
                                    lambda_d_hat$lambda_d[i], 
                                    t_point-incub_hat$incub[i], 
                                    incub_hat$incub[i])
}



y_hat_adults.df <- as.data.frame(y_hat_adults) %>% 
  pivot_longer(cols = V1:V7, names_to = "patient", values_to = "VL") %>% 
  mutate(ts = rep(t_point, each = N_patients_adults)) 

adults_data <- data_stan$y[(N_patients_children+1):(N_patients_children+N_patients_adults),]
adults_vl <- as.vector(t(adults_data))
adults_vl <- adults_vl[adults_vl != 0]

ts_data <- ts[(N_patients_children+1):(N_patients_children+N_patients_adults),]
ts_data <- as.vector(t(ts_data))
ts_data <- ts_data[ts_data != 0]

adults_data.df <- data.frame(ts = ts_data,
                             VL = exp(adults_vl)) %>% 
  mutate(patient = rep(paste0("V",1:N_patients_adults), N[ (N_patients_children+1):(N_patients_children+N_patients_adults)])) 

new_levels <- paste("patient", 1:N_patients_adults)

# Update the levels in both data frames
y_hat_adults.df$patient <- factor(y_hat_adults.df$patient, levels = paste0("V", 1:N_patients_adults), labels = new_levels)
adults_data.df$patient <- factor(adults_data.df$patient, levels = paste0("V", 1:N_patients_adults), labels = new_levels)



ggplot() + 
  geom_line(data = y_hat_adults.df, aes(x = ts, y = log10(VL), group = patient)) + 
  geom_point(data= adults_data.df, aes(x = ts, y= log10(VL), group = patient)) + 
  xlab("days post infection") + 
  ylab("log10 viral load (pfue/ml)") + 
  theme(strip.text = element_text(size = 20),
        axis.title = element_text(size = 20),    # Font size for axis titles
        axis.text = element_text(size = 20)) +
  facet_wrap(~patient) + 
  scale_x_continuous(limits = c(1, 12), breaks = c(2,4,6,8,10,12)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(color="black",
                                   size = 12, angle=0),
        axis.text.y = element_text(color="black",
                                   size= 12, angle=0),
        text = element_text(size = 12)) 



# ================== predictive check for VL at individual level (Elderly group ) ================= #
 

v_p_hat <- v_p %>% 
  filter(group == "Elderly group ") %>% 
  group_by(patient_id) %>% 
  summarise(v_p = median(v_p))

t_p_hat <- t_p %>% 
  filter(group == "Elderly group ") %>% 
  group_by(patient_id) %>% 
  summarise(t_p = median(t_p))

lambda_g_hat <- lambda_g %>% 
  filter(group == "Elderly group ") %>% 
  group_by(patient_id) %>% 
  summarise(lambda_g = median(lambda_g))

lambda_d_hat <- lambda_d %>% 
  filter(group == "Elderly group ") %>% 
  group_by(patient_id) %>% 
  summarise(lambda_d = median(lambda_d))

incub_hat <- incubation %>% 
  filter(group == "Elderly group ") %>% 
  group_by(patient_id) %>% 
  summarise(incub = median(incub))



t_max <- 15 # max end time point
t_point <-  seq(0,t_max,0.1)
y_hat_elderly <- matrix(, nrow = length(t_point), ncol = N_patients_elderly)

for(i in 1:N_patients_elderly){  
  
  y_hat_elderly[,i] <- VL_prediction(v_p_hat$v_p[i], 
                                      t_p_hat$t_p[i], 
                                      lambda_g_hat$lambda_g[i], 
                                      lambda_d_hat$lambda_d[i], 
                                      t_point-incub_hat$incub[i],
                                      incub_hat$incub[i])
}




y_hat_elderly.df <- as.data.frame(y_hat_elderly) %>% 
  pivot_longer(cols = V1:V22, names_to = "patient", values_to = "VL") %>% 
  mutate(ts = rep(t_point, each = N_patients_elderly)) 


elderly_data <- data_stan$y[(N_patients_children+N_patients_adults+1):(N_patients),]
elderly_vl <- as.vector(t(elderly_data))
elderly_vl <- elderly_vl[elderly_vl != 0]

ts_data <- ts[(N_patients_children+N_patients_adults+1):(N_patients),]
ts_data <- as.vector(t(ts_data))
ts_data <- ts_data[ts_data != 0]

elderly_data.df <- data.frame(ts = ts_data,
                              VL = exp(elderly_vl)) %>% 
  mutate(patient = rep(paste0("V",1:22), N[(N_patients_children+N_patients_adults+1):(N_patients)])) %>% 
  mutate(ts = ts + rep(incub_hat$incub, table(patient)))

 


new_levels <- paste("patient", 1:22)

# Update the levels in both data frames
y_hat_elderly.df$patient <- factor(y_hat_elderly.df$patient, levels = paste0("V", 1:22), labels = new_levels)
elderly_data.df$patient <- factor(elderly_data.df$patient, levels = paste0("V", 1:22), labels = new_levels)

incub_hat$patient <-  unique(elderly_data.df$patient)

ggplot() + 
  geom_line(data = y_hat_elderly.df, aes(x = ts, y = log10(VL), group = patient)) + 
  geom_point(data= elderly_data.df, aes(x = ts, y= log10(VL), group = patient)) + 
  geom_vline(data=subset(incub_hat, patient == "patient 1"), aes(xintercept=incub_hat$incub[1]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 2"), aes(xintercept=incub_hat$incub[2]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 3"), aes(xintercept=incub_hat$incub[3]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 4"), aes(xintercept=incub_hat$incub[4]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 5"), aes(xintercept=incub_hat$incub[5]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 6"), aes(xintercept=incub_hat$incub[6]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 7"), aes(xintercept=incub_hat$incub[7]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 8"), aes(xintercept=incub_hat$incub[8]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 9"), aes(xintercept=incub_hat$incub[8]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 10"), aes(xintercept=incub_hat$incub[10]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 11"), aes(xintercept=incub_hat$incub[10]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 12"), aes(xintercept=incub_hat$incub[12]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 13"), aes(xintercept=incub_hat$incub[13]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 14"), aes(xintercept=incub_hat$incub[14]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 15"), aes(xintercept=incub_hat$incub[15]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 16"), aes(xintercept=incub_hat$incub[16]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 17"), aes(xintercept=incub_hat$incub[17]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 18"), aes(xintercept=incub_hat$incub[18]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 19"), aes(xintercept=incub_hat$incub[19]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 20"), aes(xintercept=incub_hat$incub[20]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 21"), aes(xintercept=incub_hat$incub[21]),
             linetype="dashed",color = 'brown3') +
  geom_vline(data=subset(incub_hat, patient == "patient 22"), aes(xintercept=incub_hat$incub[22]),
             linetype="dashed",color = 'brown3') +
  xlab("days post infection") + 
  ylab("log10 viral load (pfue/ml)") + 
  theme(strip.text = element_text(size = 20),
        axis.title = element_text(size = 20),    # Font size for axis titles
        axis.text = element_text(size = 20)) +
  facet_wrap(~patient) + 
  scale_x_continuous(limits = c(1, 15), breaks = c(2,4,6,8,10,12,14)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(color="black",
                                   size = 12, angle=0),
        axis.text.y = element_text(color="black",
                                   size= 12, angle=0),
        text = element_text(size = 12)) 



y_hat_children.df$group <- rep("Pediatric group", dim(y_hat_children.df)[1])
y_hat_adults.df$group <- rep("Adult group", dim(y_hat_adults.df)[1])
y_hat_elderly.df$group <- rep("Elderly group", dim(y_hat_elderly.df)[1])

y_hat_combined <- rbind(y_hat_children.df, y_hat_adults.df,y_hat_elderly.df) 
y_hat_combined <- y_hat_combined %>% filter(patient %in% c("patient 1", "patient 2", "patient 3", "patient 4")) %>% 
  mutate(group = factor(group, levels = c("Pediatric group","Adult group", "Elderly group")))

children_data.df$group <- rep("Pediatric group", dim(children_data.df)[1])
adults_data.df$group <- rep("Adult group", dim(adults_data.df)[1])
elderly_data.df$group <- rep("Elderly group", dim(elderly_data.df)[1])

data_combined <- rbind(children_data.df, adults_data.df,elderly_data.df)
data_combined <- data_combined %>% filter(patient %in% c("patient 1", "patient 2", "patient 3", "patient 4"))%>% 
  mutate(group = factor(group, levels = c("Pediatric group","Adult group", "Elderly group")))


incub_hat <- incubation %>% 
  filter(group != "Adult group ") %>% 
  group_by(patient_id, group) %>% 
  summarise(incub = median(incub)) 
incub_hat <- incub_hat[c(1,2,3,4,21,22,23,24),]
incub_hat$patient_id <- rep(c("patient 1", "patient 2","patient 3","patient 4"),2)
colnames(incub_hat) <- c("patient", "group","incub")
incub_hat$group <- factor(rep(c("Pediatric group", "Elderly group"), each = 4),
                          levels = c("Pediatric group", "Elderly group"))


scale_y_custom <- function(group) {
  if (group == "Pediatric group") {
    scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10))
  } else {
    scale_y_continuous()
  }
}

patient_labels <- c(
  "patient 1" = "ID 1",
  "patient 2" = "ID 2",
  "patient 3" = "ID 3",
  "patient 4" = "ID 4"
)

ggplot() + 
  geom_line(data = y_hat_combined, aes(x = ts, y = log10(VL), group = patient), size = 1) + 
  geom_point(data= data_combined, aes(x = ts, y= log10(VL), group = patient), size = 3) + 
  geom_vline(data = incub_hat, aes(xintercept = incub), color = "red", linetype = "dashed", size = 1) + 
  xlab("days post infection") + 
  ylab("log10 viral load (pfue/ml)") + 
  facet_grid(group ~ patient, scales = "free_y", labeller = labeller(patient = patient_labels)) +
  scale_x_continuous(limits = c(1, 15), breaks = c(2,4,6,8,10,12,14)) + 
  scale_y_continuous(limits = c(-4,10), breaks = c(-4,0,4,8))+
  theme_bw() +
  theme(
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 20),    # Font size for axis titles
    axis.text.x = element_text(color="black", size = 20, angle=0),
    axis.text.y = element_text(color="black", size= 20, angle=0),
    text = element_text(size = 20)
  ) 

