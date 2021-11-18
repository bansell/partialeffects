

#TBC: include scale=T default for continuous covariates. 
#TBC: .Rmd file that sources [batch_sim.R] 

extract_partial <- function(model_name, features_keep, simple=T) {
  require(moderndive)
  # Remove covariate effects from data (regression) #
  # This function requires a linear model object (model_name) containing all covariates
  # and a character vector of feature effects to retain (features_keep)
   
  ## EXAMPLE ##
  
   #mod_iris <- lm(Sepal.Length ~ . , data = iris)

   #extract_partial(mod_iris, c('Species','Petal.Length')) %>%
   #ggplot(aes(x=partial_dat, y=Sepal.Length ,col=Species)) + geom_point()

  #MOD <- mod_iris
  #FEAT <- 'Species'
   
  ##### ##### ##### ##### ##### ##### #####

  MOD <- model_name
  if(!missing(features_keep)){FEAT <- features_keep}
  
  MOD_MAT <- model.matrix(MOD) %>% as_tibble() %>% rename(intercept=`(Intercept)`)
  
   STAT_TIDY <- broom::tidy(MOD, conf.int = TRUE)
   CAT_TIDY  <- tidycat::tidy_categorical(STAT_TIDY, MOD)
   STAT_TBL  <- STAT_TIDY %>% janitor::clean_names() %>% 
      mutate(term=ifelse(term=='(Intercept)','intercept', term))
  
  
  #Ensure that the term and associated factors are explicit. 
  # This avoids unwanted soft matching e.g. between batch2 level1, and 'batch21'.
  if(!missing(features_keep)){
   my_features <- CAT_TIDY %>% filter(term!='(Intercept)', variable %in% FEAT) %>% pull(term)
  }else{
    my_features <- CAT_TIDY %>% filter(term!='(Intercept)') %>% pull(term)
   }
  
  if(!missing(features_keep)){
  for_solve <- MOD_MAT %>% select(intercept, unique(my_features)) 
  }else{
    for_solve <- MOD_MAT %>% select(intercept)
  }
  
  #Multiply matrix by betas ( = estimates = coefficients)
  mult <- MOD_MAT #
  mult[] <- NA # overwriting all values; retain colnames
  for(i in colnames(for_solve)){   
    
    beta <- STAT_TBL %>% filter(term ==  i) %>% dplyr::pull(estimate);
    prod  <- MOD_MAT[i] * beta 
    mult[i] <- prod
  }
  
  mult <- mult %>% select_if(!is.na(colSums(.)))
  
  #rowSum products then add residuals to calculate 'partial residuals'
  if(simple==F){
  broom::augment(MOD) %>% mutate(partial_resid = rowSums(mult) + .resid ) 
  }else{
    moderndive::get_regression_points(MOD) %>% mutate(partial_resid = rowSums(mult) + residual ) 
    }  

}
