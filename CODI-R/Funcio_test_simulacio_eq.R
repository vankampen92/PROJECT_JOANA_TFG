test_simulacio_eq <- function( list_itin,           #Lista con Plos itinerarios de una especie
                               list_vectors, #Lista con las columnas que contienen P/A.
                               c, e, 
                               nsims)               #Numero simulaciones
{
  p = c/(c+e) # Ocupancia a l'equilibri d'aquesta especie
  
  # 0. Calcul del NLL_Data from the true data of the itineraris. 
  # NLL_Data de las observaciones
  nlls <- 0
  for (i in 1:length(list_itin)) {
    years         <- colnames(list_itin[[i]])[2:(ncol(list_itin[[i]]) - 1)]
    years_numeric <- as.numeric(years)
    no_of_years   <- length(years)
    
    empirical_seq <- as.vector(list_itin[[i]][,2:(no_of_years+1)])
    
    num_successes  <- sum(empirical_seq == 1)
    num_failures   <- sum(empirical_seq == 0)  
    nlls <- nlls - (num_successes*log(p) + num_failures*log(1-p))   
  }
  NLL_Data = nlls
  
  # 1. Definir vector de NLLs
  NLLs <- c()
  # 2. Para cada simulación:
  for(n in 1:nsims){
    
    nlls <- 0
    for (i in 1:length(list_itin)) {
      years         <- colnames(list_itin[[i]])[2:(ncol(list_itin[[i]]) - 1)]
      years_numeric <- as.numeric(years)
      no_of_years <- length(years)
      
      bernoulli_seq <- rbinom(n = no_of_years, size = 1, prob = p)
      num_successes <- sum(bernoulli_seq == 1)
      num_failures <- sum(bernoulli_seq == 0)  
      nlls <- nlls - (num_successes*log(p) + num_failures*log(1-p))   
    }
    
    # v. Salvar nlls en NLLs
    NLLs[n] <- nlls  
  }
  
  # 3. Calcular el p-valor de las observaciones
  Fn <- ecdf(NLLs) #Calcula la e.c.d.f.
  
  Fn(NLL_Data) 
}                          

