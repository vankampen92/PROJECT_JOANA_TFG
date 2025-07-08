# Test de bondat d'ajust del model de colonitzacio-extincio
test_simulacio_ce <- function( 
    list_itin,           #Lista con Plos itinerarios de una especie
    list_vectors,        #Lista con las columnas que contienen P/A.
    c, e,                # Colonization and extinction parameters
    nsims,               #Numero simulaciones
    NLL )                # NLL de las observaciones)
{
    # 0. busquem el valor de la segona columna que conte la P/A de l'sp per cada itinerari
    #    Aquest valor conte la P/A inicial
    second_column_values <- sapply(list_itin, function(df) df[1, 2])
    # 1. Definir vector de NLLs
    nlls <- c()
    # 2. Para cada simulación:
    for(n in 1:nsims){
      new_data <- list()
      
      for (i in 1:length(list_itin)) {
        years         <- colnames(list_itin[[i]])[2:(ncol(list_itin[[i]]) - 1)]
        years_numeric <- as.numeric(years)
        dt            <- diff(years_numeric)
        tps           <- cetotrans(c, e, dt)
        times         <- length(dt)
        
        # Creacio del data frame a simular (input de PA_simulation)
        itin_sim_df        <- data.frame(sp_latin = list_itin[[i]]$sp_latin)
        itin_sim_df[years] <- c(1:length(years))
        itin_sim_df[1,2]   <- second_column_values[i]
        itin_sim_df$IDitin <- list_itin[[i]]$IDitin
        
        itin_sim <- PA_simulation(itin_sim_df, #datos itinerario 
                                  2,        #columna inicial
                                  tps,      #probabilidades de transicion
                                  times )   #number of transitions to simulate
        
        Final_Time_Column <- length(years) + 1   
        itin_sim_df[1, 3:Final_Time_Column] <- itin_sim[1, 1:times]
        
        new_data[[i]] <- itin_sim_df
      }
      
      # iv. Estimar col-ext con irregular_multiple
      col_ext_res_sim <- irregular_multiple_datasets(new_data, list_vectors, 
                                                     0.0001, 0.0001)
      # v. Salvar la NLL en nlls
      nlls <- c(nlls, col_ext_res_sim$NLL)  
    }
    
                                  
    # 3. Calcular el p-valor de las observaciones
    Fn <- ecdf(nlls) #Calcula la e.c.d.f.
                                
    Fn(NLL) 
}                          

test_simulacio_ce2 <- function(
    list_itin,      # List of data.frames: itineraries for one species
    list_vectors,   # List of columns with P/A (presence/absence) data
    c, e,           # Colonization and extinction parameters
    nsims,          # Number of simulations
    NLL             # NLL value from observations
) {
  # Extract initial P/A value for each itinerary
  initial_PA_values <- sapply(list_itin, function(df) df[1, 2])
  
  # Preallocate vector to store NLLs
  nlls <- numeric(nsims)
  
  for (sim in seq_len(nsims)) {
    simulated_datasets <- vector("list", length(list_itin))
    
    for (i in seq_along(list_itin)) {
      itin <- list_itin[[i]]
      year_cols <- colnames(itin)[2:(ncol(itin) - 1)]
      years_num <- as.numeric(year_cols)
      dt <- diff(years_num)
      
      transition_probs <- cetotrans(c, e, dt)
      num_transitions <- length(dt)
      
      
      simulated_PA <- PA_simulation(
        itin, 2, transition_probs, num_transitions
      )
      
      final_col <- length(year_cols) + 1
      itin[1, 3:final_col] <- simulated_PA[1, 1:num_transitions]
      
      simulated_datasets[[i]] <- itin
    }
    
    # Estimate col-ext from simulated data
    sim_result <- irregular_multiple_datasets(
      simulated_datasets, list_vectors, 0.0001, 0.0001
    )
    
    nlls[sim] <- sim_result$NLL
  }
  
  # Compute p-value of observed NLL against simulated NLLs
  Fn <- ecdf(nlls)
  return(Fn(NLL))
}
                                   
