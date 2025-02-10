
dodge <- position_dodge(width = 0.5)
 
 


combined_posteriors <- data.frame(data = c(posterior_sample_group_parameters_vp$v_p, 
                                           posterior_sample_group_parameters_tp$t_p,
                                           posterior_sample_group_parameters_lambda.g$lambda_g,
                                           posterior_sample_group_parameters_lambda.d$lambda_d),
                                  category = rep(c("(A) Peak viral load (log10(pfue/ml))", 
                                                   "(B) Time to peak viral load (day)",
                                                   "(C) Viral growth rate (log10(pfue/ml)/day)",
                                                   "(D) Viral decline rate (log10(pfue/ml)/day)"), each = length(posterior_sample_group_parameters_tp$t_p)),
                                  group = rep(c("Pediatric group ", "Adult group ", "Elderly group ")))
combined_posteriors <- combined_posteriors %>% 
  mutate(group = factor(group, levels = c("Pediatric group ", "Adult group ", "Elderly group "))) %>% 
  mutate(category = factor(category, levels = c("(A) Peak viral load (log10(pfue/ml))", 
                                                "(B) Time to peak viral load (day)",
                                                "(C) Viral growth rate (log10(pfue/ml)/day)",
                                                "(D) Viral decline rate (log10(pfue/ml)/day)")))


 combined_posteriors_med <- combined_posteriors %>% 
   group_by(category, group) %>% 
   summarise(med = median(data)) %>% 
   ungroup()


                                  


p1 <- combined_posteriors %>% 
  ggplot(aes(x = group, y = data, fill = group)) +
  
  geom_violin(trim = FALSE) +
  geom_boxplot(width=.1, outlier.colour=NA, position = dodge,aes(group = group, fill = group)) +
   
  geom_text(data=subset(combined_posteriors_med, group == "Pediatric group " & category == "(A) Peak viral load (log10(pfue/ml))"), aes(x=1, y=round(combined_posteriors_med$med[1],2),
                                                                      label=round(combined_posteriors_med$med[1],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  geom_text(data=subset(combined_posteriors_med, group == "Adult group " & category == "(A) Peak viral load (log10(pfue/ml))"), aes(x=2, y=round(combined_posteriors_med$med[2],2),
                                                                                                                       label=round(combined_posteriors_med$med[2],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  geom_text(data=subset(combined_posteriors_med, group == "Elderly group " & category == "(A) Peak viral load (log10(pfue/ml))"), aes(x=3, y=round(combined_posteriors_med$med[3],2),
                                                                                                                   label=round(combined_posteriors_med$med[3],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  
  geom_text(data=subset(combined_posteriors_med, group == "Pediatric group " & category == "(B) Time to peak viral load (day)"), aes(x=1, y=round(combined_posteriors_med$med[4],2),
                                                                                                                       label=round(combined_posteriors_med$med[4],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  geom_text(data=subset(combined_posteriors_med, group == "Adult group " & category == "(B) Time to peak viral load (day)"), aes(x=2, y=round(combined_posteriors_med$med[5],2),
                                                                                                                   label=round(combined_posteriors_med$med[5],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  geom_text(data=subset(combined_posteriors_med, group == "Elderly group " & category == "(B) Time to peak viral load (day)"), aes(x=3, y=round(combined_posteriors_med$med[6],2),
                                                                                                                     label=round(combined_posteriors_med$med[6],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  
  geom_text(data=subset(combined_posteriors_med, group == "Pediatric group " & category == "(C) Viral growth rate (log10(pfue/ml)/day)"), aes(x=1, y=round(combined_posteriors_med$med[7],2),
                                                                                                                               label=round(combined_posteriors_med$med[7],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  geom_text(data=subset(combined_posteriors_med, group == "Adult group " & category == "(C) Viral growth rate (log10(pfue/ml)/day)"), aes(x=2, y=round(combined_posteriors_med$med[8],2),
                                                                                                                           label=round(combined_posteriors_med$med[8],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  geom_text(data=subset(combined_posteriors_med, group == "Elderly group " & category == "(C) Viral growth rate (log10(pfue/ml)/day)"), aes(x=3, y=round(combined_posteriors_med$med[9],2),
                                                                                                                             label=round(combined_posteriors_med$med[9],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  
  geom_text(data=subset(combined_posteriors_med, group == "Pediatric group " & category == "(D) Viral decline rate (log10(pfue/ml)/day)"), aes(x=1, y=round(combined_posteriors_med$med[10],2),
                                                                                                                         label=round(combined_posteriors_med$med[10],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  geom_text(data=subset(combined_posteriors_med, group == "Adult group " & category == "(D) Viral decline rate (log10(pfue/ml)/day)"), aes(x=2, y=round(combined_posteriors_med$med[11],2),
                                                                                                                     label=round(combined_posteriors_med$med[11],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  geom_text(data=subset(combined_posteriors_med, group == "Elderly group " & category == "(D) Viral decline rate (log10(pfue/ml)/day)"), aes(x=3, y=round(combined_posteriors_med$med[12],2),
                                                                                                                       label=round(combined_posteriors_med$med[12],2)),
            vjust=-0, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  
  
  
   
  
  lims(y = c(0, 10)) +
  theme_bw() +
  scale_fill_manual(name="Distribution",
                    values=c("Pediatric group " = "#1f78b4",
                             "Adult group "  = "#fdbf6f",
                             "Elderly group " = "#fb9a99"),
                    guide = "none") + 
  ylab("") +
  xlab("") +
  theme(axis.text.x = element_text(color="black",
                                   size = 15, angle=0),
        axis.text.y = element_text(color="black",
                                   size= 15, angle=0),
        text = element_text(size = 15)) + 
  theme(
    axis.text.x = element_text(face="bold"),
    axis.text.y = element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    plot.title = element_text(face="bold", hjust = 0.5),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    legend.position = "NA",
    legend.title = element_text(size = 0))+ 
  facet_wrap(~category, scales = "free_y") + 
  theme(strip.text = element_text(face = "bold"))
p1


 
 


