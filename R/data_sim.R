#data simulation

library(here)


NROWS = 200

library(tidyverse)

set.seed(1233)
tbl_sim <- tibble(
  'age' = runif(NROWS, 30, 80),
  'body_mass_g' = 2000 * age,
  #'systolic' = 120+(20*scale_this(age)),
  'systolic' = 120*(0.02*age),
  "error" = rnorm(NROWS, 5, 5),
  'treatment' = rep(0:1,each=100)) %>%
  rownames_to_column(var = 'subjID') %>%
  mutate(batch = rbinom(NROWS, 1, 0.7) + 1) %>%
  mutate(plate = rbinom(NROWS, 2, 0.4) + 1) %>%
  mutate(across(.cols = c(batch, plate, treatment), .fns = ~ as.factor(.))) %>%
  mutate(systolic = ifelse(batch == 1, systolic + 15, systolic)) %>%
  mutate(systolic = ifelse(plate == 1, systolic - 12, systolic)) %>%
  mutate(systolic = ifelse(plate == 3, systolic - 30, systolic)) %>%
  mutate(
    systolic = ifelse(batch == 1,   systolic + 30, systolic),
    systolic = ifelse(plate == 1,   systolic - 25, systolic),
    systolic = ifelse(plate == 3,   systolic - 8, systolic),
    systolic = ifelse(treatment == 1, systolic - 15, systolic),
    systolic = systolic + error
  )



experimental_data <- tbl_sim %>% mutate(treatment=ifelse(treatment==0,'placebo','drug'),
                                        treatment=factor(treatment,   levels=c('placebo','drug'))) %>%
  select(-c(error,body_mass_g)) %>% mutate(age=round(age,0)) %>%
  dplyr::rename(machine=plate,site=batch)

write_tsv(experimental_data,'R/inst/experimental_data.tsv')
