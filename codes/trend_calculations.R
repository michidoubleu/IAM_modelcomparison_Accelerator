### calculting linear model before and after 2020
before2020_trend <- comp.dat %>% filter(year <= 2020)
after2020_trend <-  comp.dat %>% filter(year >= 2020)

before2020_trend <- before2020_trend %>% na.omit() %>%
  group_by(model, scenario, region, variable, item) %>%
  do(tidy(lm(value ~ year, data = .))) %>%
  filter(term %in% c("(Intercept)", "year")) %>%
  select(model, scenario, region, variable, item, term, estimate) %>%
  spread(term, estimate, fill = NA) %>%
  rename(intercept_before2020 = `(Intercept)`, trend_before_2020 = year)


after2020_trend <- after2020_trend %>% na.omit() %>%
  group_by(model, scenario, region, variable, item) %>%
  do(tidy(lm(value ~ year, data = .))) %>%
  filter(term %in% c("(Intercept)", "year")) %>%
  select(model, scenario, region, variable, item, term, estimate) %>%
  spread(term, estimate, fill = NA) %>%
  rename(intercept_after2020 = `(Intercept)`, trend_after_2020 = year)