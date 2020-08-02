


forecast_llt <- function(llt_fit, h = 10) {
  
  s_draws <- spread_draws(llt_fit, s_obs, s_level, s_slope)
  all_draws <- gather_draws(llt_fit, mu[i], delta[i]) %>%
    ungroup() %>%
    filter(i == max(i)) %>%
    tidyr::spread(.variable, .value) %>%
    inner_join(s_draws, by = c(".chain", ".iteration", ".draw"))
  all_draws_orig <- all_draws
  
  steps <- max(all_draws$i) + 1:h
  
  y_draws <- expand.grid(i = steps, .draw = unique(all_draws$.draw)) %>%
    inner_join(distinct(all_draws, .chain, .iteration, .draw), by = ".draw")
  y_draws$y <- NA
  y_draws$mu <- NA
  y_draws$delta <- NA
  
  for(i in 1:h) {
    all_draws <- all_draws %>%
      mutate(delta = delta + rnorm(n(), 0, s_slope)) %>%
      mutate(mu = mu + delta + rnorm(n(), 0, s_level)) %>%
      mutate(y = mu + rnorm(n(), 0, s_obs))
    
    y_draws[y_draws$i == steps[i],]$y <- all_draws$y
    y_draws[y_draws$i == steps[i],]$mu <- all_draws$mu
    y_draws[y_draws$i == steps[i],]$delta <- all_draws$delta
  }
  
  return(list(y_draws = y_draws, pars_draws = all_draws))
}

set.seed(573)
llt_fc <- forecast_llt(llt_fit)

readr::write_csv(llt_fc$y_draws, "llt_fc_y_draws.csv")

################################################################################

# Given the current trend, what is the probability that one will be
# positive cumulatively across the next 10 steps?

llt_fc_y_draws_cumsum <- llt_fc$y_draws %>%
  group_by(.draw) %>%
  mutate(y_cumsum = cumsum(y))

llt_fc_y_draws_cumsum %>%
  filter(i == 50) %>%
  mutate(positive = y_cumsum > 0) %>%
  pull(positive) %>%
  mean()

llt_fc_y_draws_cumsum %>%
  filter(i == 50) %>%
  mutate(positive = y_cumsum > 0) %>%
  filter(positive)

# What is the average trend/slope I need to achieve a positive sum after
# 10 steps?
llt_fc_y_draws_cumsum %>%
  group_by(.draw) %>%
  mutate(positive = y_cumsum[i == 50] > 0) %>%
  #filter(positive) %>%
  group_by(.draw, positive) %>%
  summarize(
    mu = mean(mu),
    delta = mean(delta)
  ) %>%
  tidyr::gather(pars, .mean, -.draw, -positive) %>%
  ggplot(aes(x = .mean, group = positive, color = positive, fill = positive)) +
  geom_density(alpha = 0.75) +
  facet_grid(.~pars, scales = "free")

# Which of the slopes will let me maximize the probability of achieving a positive result?
# -> maximize prior (which here is the posterior) and the prob of being positive

llt_fc_y_draws_cumsum %>%
  group_by(.draw) %>%
  mutate(positive = y_cumsum[i == 50] > 0) %>%
  #filter(positive) %>%
  group_by(.draw, positive) %>%
  summarize(
    mu = mean(mu),
    delta = mean(delta)
  ) %>%
  mutate(delta_rounded = round(delta)) %>%
  group_by(delta_rounded) %>%
  summarize(n_obs = n(),
            prob_of_delta = n_obs / 8000,
            prob_of_positive_given_delta = mean(positive)) %>%
  mutate(joint = log(prob_of_delta) + log(prob_of_positive_given_delta)) %>%
  ggplot(aes(x = delta_rounded, y = joint)) +
  geom_col()


# What if I must have a probability of positive larger than 90% given the delta?
llt_fc_y_draws_cumsum %>%
  group_by(.draw) %>%
  mutate(positive = y_cumsum[i == 50] > 0) %>%
  #filter(positive) %>%
  group_by(.draw, positive) %>%
  summarize(
    mu = mean(mu),
    delta = mean(delta)
  ) %>%
  mutate(delta_rounded = round(delta)) %>%
  group_by(delta_rounded) %>%
  summarize(n_obs = n(),
            prob_of_delta = n_obs / 8000,
            prob_of_positive_given_delta = mean(positive)) %>%
  mutate(joint = log(prob_of_delta) + log(prob_of_positive_given_delta)) %>%
  mutate(is_prob_positive_given_delta_larger_90 = prob_of_positive_given_delta > 0.9) %>%
  ggplot(aes(x = delta_rounded, y = joint, 
             fill = is_prob_positive_given_delta_larger_90)) +
  geom_col() +
  theme(legend.position = "bottom")











