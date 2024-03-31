source('src/data.R')

#Model specification ----

#formula for ae
ae_formula <- brms::bf(
  # mu (mean) part
  all_breaches ~ occupied_ratio + covid_flag + (1|org_code),
  # phi (precision) part
  phi ~  covid_flag + (1|org_code) ,
  # alpha (zero-inflation) part
  zoi ~ occupied_ratio,
  coi ~ occupied_ratio
)

#priors for brms?
brms::get_prior(
  ae_formula,
  data = FINAL_regression_data,
  family = brms::zero_one_inflated_beta()
)

#save priors
priors <- c(brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
            brms::set_prior("normal(0, 1)", class = "b"))

#actual underlying model
ae_model <- brms::brm(
  ae_formula,
  data = FINAL_regression_data,
  #thinking this should be one-inflated / constrainted to > 50% occupied
  family = brms::zero_one_inflated_beta(),
  #initialise at 0 to speed up process
  init = 0,
  prior = priors,
  #q: is this depth enough? should i increase? no warning about convergence
  #so am sort of happy so far
  control = list(adapt_delta = 0.97,
                 max_treedepth = 12),
  chains = 4, 
  iter = 2000, 
  warmup = 1000,
  cores = 4, 
  seed = 1926, 
  #this is up to system limitation. my device is an M1, so it can really
  #only handle 4 threads max? 4 chains x 1 threads is enough. someone buy me
  #a better computer and I can do this more effectively
  threads = brms::threading(1),
  file = "test/ae_model_v5"
)

# Model outputs : zoib -----

#get average slopes through vars;
#how does this work exactly?
ae_beta_1 <- marginaleffects::avg_comparisons(ae_model,
                                         variables = 'occupied_ratio',
                                         by = 'covid_flag')

ame_zi_0 <- marginaleffects::predictions(ae_model, 
                                         newdata = marginaleffects::datagrid(
                                           covid_flag = unique,
                                           occupied_ratio = seq(0, 100, by = 1)))

ame_zi_1 <- ame_zi_0 %>% 
  marginaleffects::posterior_draws() %>%
  mutate(covid_flag = case_when(
    covid_flag == 'covid' ~ '2. Covid',
    covid_flag == 'post_covid' ~ '3. Post-Covid',
    TRUE ~ '1. Pre-Covid'
  ))

#average predictions
ame_zi_2 <- marginaleffects::avg_predictions(ae_model, 
                                             variables = 'occupied_ratio')

#model
ame_beta_bayes_1 <- ae_model %>% 
  marginaleffects::slopes(variables = "occupied_ratio",
         newdata = marginaleffects::datagrid(occupied_ratio = c(100,83,75))) %>% 
  marginaleffects::posterior_draws()

#comparisons - what are the slopes??
ame_beta_avg_slopes <- ae_model %>% 
  marginaleffects::avg_slopes(variables = "occupied_ratio",
                              by = 'covid_flag')

ame_beta_bayes_slopes <- ae_model %>% 
  marginaleffects::slopes(variables = "occupied_ratio",
                          newdata = marginaleffects::datagrid(
                            occupied_ratio = c(100,83,75),
                            covid_flag = unique))

# Testing with effects:: package. I hate this.
comparative_model <- marginaleffects::comparisons(model = ae_model,
                             newdata = marginaleffects::datagrid(
                               #org_code = unique,
                               covid_flag = 'pre_covid', 
                               occupied_ratio = c(70:100)),
                             comparison = 'difference')

#evidence of change in slope: important?
marginaleffects::plot_slopes(model = ae_model,
                             variable='occupied_ratio',
                             by='covid_flag')

# Model outputs: fraclogit -----

ae_model_frac <- glm(all_breaches ~ 
                       occupied_ratio + 
                       covid_flag + 
                       (1|org_code), 
                     data = FINAL_regression_data, 
                     family = quasibinomial())

ae_model_frac_1 <-marginaleffects::predictions(ae_model_frac, 
                               newdata = marginaleffects::datagrid(
                                 occupied_ratio = seq(0, 100, by = 1),
                                 covid_flag = unique))

ame_fraclogit_slopes <- ae_model_frac %>% 
  marginaleffects::slopes(variables = "occupied_ratio",
                          newdata = marginaleffects::datagrid(
                            occupied_ratio = c(50:100)))
