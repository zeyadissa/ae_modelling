source('src/model.R')

# EDA ------

PLOT_predictions <- 
  ggplot2::ggplot(ame_zi_1 %>%
                  filter(time_flag != 'Covid') %>%
                  mutate(time_flag = fct_relevel(time_flag, 
                                            "Pre-2013", "2013 to 2016", "2016 to 2019", 'Post-Covid')),
                ggplot2::aes(x = occupied_ratio, y = draw, color = time_flag, fill = time_flag)) +
  ggdist::stat_lineribbon(ggplot2::aes(fill_ramp = ggplot2::after_stat(level))) +
  ggplot2::scale_y_continuous(labels = scales::label_percent()) +
  ggplot2::scale_fill_viridis_d(option = "plasma", end = 0.8) +
  ggplot2::scale_color_viridis_d(option = "plasma", end = 0.8) +
  ggdist::scale_fill_ramp_discrete(range = c(0.2, 0.7)) +
  ggplot2::geom_hline(yintercept=0.05,linetype=2,col='black')+
  ggplot2::facet_wrap(ggplot2::vars(time_flag), ncol = 4) +
  ggplot2::labs(x = "A&G Occupied beds ratio",
                y = "Predicted proportion of type 1 AE breaches",
                fill = "Covid Flag", color = "Covid Flag",
                fill_ramp = "Credible interval") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

#test per trust
selected_org <- 'RW3'
trust_level_data <- ame_zi_a1 %>% filter(org_code == selected_org)

ggplot2::ggplot(trust_level_data ,ggplot2::aes(x = occupied_ratio, y = draw*100)) +
  geom_point(data=FINAL_reference_data %>% 
               filter(covid_flag == 'pre_covid' & org_code == selected_org),aes(x=occupied_ratio,y=breach_ratio*100),alpha=1,col='black')+
  ggdist::stat_lineribbon(ggplot2::aes(fill_ramp = ggplot2::after_stat(level))) +
  ggdist::scale_fill_ramp_discrete(range = c(0.2, 0.7)) +
  ggplot2::labs(x = "A&G Occupied beds ratio",
                y = "Predicted proportion of type 1 AE breaches",
                fill = "Credible interval", color = "Credible interval",
                fill_ramp = "Credible interval") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::geom_hline(yintercept=5,linetype=2,col='black')+
  ylim(0,50) +
  xlim(50,100)

ggplot2::ggplot(EDA_2) +
  geom_line(aes(x=quarter_date,y=(100-ratio_breach)),col='red') +
  geom_line(aes(x=quarter_date,y=ratio_occupied),col='blue') +
  geom_hline(yintercept=95)+
  theme_bw() +
  ylim(50,100)

#plot of different tests
ggplot(ame_beta_bayes_1, aes(x = draw * 100, fill = factor(occupied_ratio))) +
  geom_vline(xintercept = 0) +
  ggdist::stat_halfeye(.width = c(0.8, 0.95), point_interval = "median_hdi",
               slab_alpha = 0.75) +
  scale_fill_viridis_d(option = "viridis", end = 0.6) +
  labs(x = "Average marginal effect of occupied beds", 
       y = "Density", fill = "Occupied bed rates",
       caption = "80% and 95% credible intervals shown in black") +
  theme(legend.position = "bottom")
