source('src/model.R')

# EDA ------

ggplot(test, aes(x = occupied_ratio, y = estimate)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.4, fill = "#480B6A") +
  geom_line(linewidth = 1, color = "#480B6A") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Ratio occupied", 
       y = "Breaches")+
  geom_hline(yintercept = 0.05,linetype=2,col='black')+
  geom_vline(xintercept = 83,linetype=2,col='black')+
  xlim(50,100)+
  theme_minimal()

ggplot2::ggplot(ame_zi_0,ggplot2::aes(x = occupied_ratio, y = estimate, color = covid_flag, fill = covid_flag)) +
  ggdist::stat_lineribbon(ggplot2::aes(fill_ramp = ggplot2::after_stat(level))) +
  ggplot2::scale_y_continuous(labels = scales::label_percent()) +
  ggplot2::scale_fill_viridis_d(option = "plasma", end = 0.8) +
  ggplot2::scale_color_viridis_d(option = "plasma", end = 0.8) +
  ggdist::scale_fill_ramp_discrete(range = c(0.2, 0.7)) +
  ggplot2::geom_vline(xintercept=83,linetype=2,col='black')+
  ggplot2::geom_hline(yintercept=0.05,linetype=2,col='black')+
  ggplot2::facet_wrap(ggplot2::vars(covid_flag), ncol = 3) +
  ggplot2::labs(x = "A&G Occupied beds ratio",
                y = "Estimated proportion of type 1 AE breaches",
                fill = "Covid Flag", color = "Covid Flag",
                fill_ramp = "Credible interval") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggplot(ame_zi_1,ggplot2::aes(x = occupied_ratio, y = draw, color = covid_flag, fill = covid_flag)) +
  ggdist::stat_lineribbon(ggplot2::aes(fill_ramp = ggplot2::after_stat(level))) +
  ggplot2::scale_y_continuous(labels = scales::label_percent()) +
  ggplot2::scale_fill_viridis_d(option = "plasma", end = 0.8) +
  ggplot2::scale_color_viridis_d(option = "plasma", end = 0.8) +
  ggdist::scale_fill_ramp_discrete(range = c(0.2, 0.7)) +
  ggplot2::geom_vline(xintercept=83,linetype=2,col='black')+
  ggplot2::geom_hline(yintercept=0.05,linetype=2,col='black')+
  ggplot2::facet_wrap(ggplot2::vars(covid_flag), ncol = 3) +
  ggplot2::labs(x = "A&G Occupied beds ratio",
                y = "Predicted proportion of type 1 AE breaches",
                fill = "Covid Flag", color = "Covid Flag",
                fill_ramp = "Credible interval") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggplot(EDA_2) +
  geom_line(aes(x=quarter_date,y=(100-ratio_breach)),col=THF_red) +
  geom_line(aes(x=quarter_date,y=ratio_occupied),col=THF_3_teal) +
  geom_hline(yintercept=95)+
  theme_bw() +
  ylim(50,100)


ggplot2::ggplot(ame_zi_1,ggplot2::aes(x = occupied_ratio, y = draw, color = covid_flag, fill = covid_flag)) +
  ggdist::stat_lineribbon(ggplot2::aes(fill_ramp = ggplot2::after_stat(level))) +
  ggplot2::scale_y_continuous(labels = scales::label_percent()) +
  ggplot2::scale_fill_viridis_d(option = "plasma", end = 0.8) +
  ggplot2::scale_color_viridis_d(option = "plasma", end = 0.8) +
  ggdist::scale_fill_ramp_discrete(range = c(0.2, 0.7)) +
  ggplot2::geom_vline(xintercept=83,linetype=2,col='black')+
  ggplot2::geom_hline(yintercept=0.05,linetype=2,col='black')+
  ggplot2::facet_wrap(ggplot2::vars(covid_flag), ncol = 3) +
  ggplot2::labs(x = "A&G Occupied beds ratio",
       y = "Predicted proportion of all AE breaches",
       fill = "Covid Flag", color = "Covid Flag",
       fill_ramp = "Credible interval") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

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