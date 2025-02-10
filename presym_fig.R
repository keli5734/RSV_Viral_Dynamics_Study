presym_transmission_ped <- readRDS("presym_transmission_children.rds") # computed values can be found from "presym_prop_predicted_values.zip"
presym_transmission_eld <- readRDS("presym_transmission_elderly.rds")
presym_transmission_adu <- readRDS("presym_transmission_adult.rds")

pre_sym_percentage <- data.frame(values = 100*c(presym_transmission_ped, presym_transmission_adu, presym_transmission_eld),
                                 group = rep(c("Pediatric group", "Adult group", "Elderly group"),
                                             c(length(presym_transmission_ped), length(presym_transmission_adu), length(presym_transmission_eld))
                                 )) %>%
  mutate(group = factor(group, levels = c("Pediatric group", "Adult group", "Elderly group") ))
#mutate(group = factor(group, levels = c("Elderly group", "Pediatric group") ))

pre_sym_percentage_median <- pre_sym_percentage %>%
  group_by(group) %>%
  summarise(value = median(values, na.rm = TRUE)) %>% 
  mutate(group = factor(group, levels = c("Pediatric group", "Adult group" , "Elderly group") ))
#mutate(group = factor(group, levels = c("Elderly group", "Pediatric group")))




dodge <- position_dodge(width = 0)

pre_sym_percentage %>% filter(group == "Adult group") %>% summarise(lb = quantile(values, 0.025), 
                                                                    med = quantile(values, 0.5),
                                                                    ub = quantile(values, 0.975))
                                                                     
                                                                   

Fig4G <- pre_sym_percentage %>% 
  filter(!is.na(values)) %>% 
  #filter(group != "Adult group") %>% 
  ggplot(aes(x = group, y = values, fill = group)) +
  geom_violin(trim = FALSE, linewidth = 1, size = 5, scale = "width") +
  geom_boxplot(width=.03, outlier.colour= NA, position = dodge,aes(group = group, fill = group),linewidth = 1) +
  
  geom_text(data=subset(pre_sym_percentage_median, group == "Pediatric group"), aes(x=1, y=round(pre_sym_percentage_median$value[1],2),
                                                                                    label=round(pre_sym_percentage_median$value[1],2)),
            vjust=-1, hjust=-1.2, color='black', size = 5, fontface = "bold") +
  geom_text(data=subset(pre_sym_percentage_median, group == "Adult group" ), aes(x=2, y=round(pre_sym_percentage_median$value[2],2),
                                                                                 label=round(pre_sym_percentage_median$value[2],2)),
            vjust=-1, hjust=-2.2, color='black', size = 5, fontface = "bold") +
  geom_text(data=subset(pre_sym_percentage_median, group == "Elderly group" ), aes(x=3, y=round(pre_sym_percentage_median$value[3],2),
                                                                                   label=round(pre_sym_percentage_median$value[3],2)),
            vjust=-1, hjust=-2.2, color='black', size = 5, fontface = "bold") +
  
  lims(y = c(-0, 100)) +
  theme_bw() +
  scale_fill_manual(name="Distribution",
                    values=c("Pediatric group" = "#1f78b4",
                             "Adult group"  = "#fdbf6f",
                             "Elderly group" = "#fb9a99"),
                    guide = "none") + 
  ylab("% presymptomatic transmission") +
  xlab("") +
  theme(axis.text.x = element_text(color="black",
                                   size = 12, angle=0),
        axis.text.y = element_text(color="black",
                                   size= 12, angle=0),
        text = element_text(size = 12)) + 
  theme(
    axis.text.x = element_text(face="bold"),
    axis.text.y = element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    plot.title = element_text(face="bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    legend.position = "NA",
    legend.title = element_text(size = 0))+ 
  #facet_wrap(~measurement) + 
  theme(strip.text = element_text(face = "bold"))   
#coord_flip()

Fig4G
