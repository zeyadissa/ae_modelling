source('src/data.R')

#Model specification ----

#formula for ae
ae_formula <- brms::bf(
  # mu (mean) part
  all_breaches_ratio ~ occupied_ratio + time_flag + (1|org_code),
  # phi (precision) part
  phi ~  time_flag + (1|org_code) ,
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
  control = list(adapt_delta = 0.99,
                 max_treedepth = 12),
  chains = 4, 
  iter = 4000, 
  warmup = 1000,
  cores = 4, 
  seed = 1926, 
  #this is up to system limitation. my device is an M1, so it can really
  #only handle 4 threads max? 4 chains x 1 threads is enough. someone buy me
  #a better computer and I can do this more effectively
  threads = brms::threading(1),
  file = "test/ae_model_v9"
)

# Model outputs : zoib -----

#get average slopes through vars;
#how does this work exactly?
ame_zi_0 <- marginaleffects::predictions(ae_model, 
                                         newdata = marginaleffects::datagrid(
                                           time_flag = unique,
                                           occupied_ratio = seq(0, 100, by = 1)))


ame_zi_1 <- ame_zi_0 %>% 
  marginaleffects::posterior_draws() 

ame_zi_a0 <- marginaleffects::predictions(ae_model, 
                                         newdata = marginaleffects::datagrid(
                                           org_code = unique,
                                           covid_flag = 'Pre-2013',
                                           occupied_ratio = seq(50, 100, by = 1)))


ame_zi_a1 <- ame_zi_a0 %>% 
  marginaleffects::posterior_draws()