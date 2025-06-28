# Test de bondat d'ajust del model de colonitzacio-extincio

test_simulacio_ce <- function( list_itin,           #Lista con los itinerarios de una especie
                               list_vectores_total, #Lista con las columnas que contienen P/A.
                               c, e, 
                               nsims,               #Numero simulaciones
                               NLL )                # NLL de las observaciones)
                              {
                                # 0. busquem el valor de la segona columna que conte la P/A de l'sp per cada itinerari
                                second_column_values <- sapply(list_itin, function(df) df[1, 2])
                                # 1. Definir vector de NLLs
                                nlls <- c()
                                # 2. Para cada simulación:
                                for(n in 1:nsims){
                                  # i. Definir lista para meter datos simulados
                                  new_data <- list()
                                  # ii. Para cada itinerario, simular datos:
                                  for (i in 1:length(list_itin){
                                    # a. Obtener años del esquema de muestreo.
                                    years <- colnames(list_itin[[i]])[list_vectores_total[i]]
                                    # b. Obtener probabilidades de transición
                                    dt <- lag(years)
                                    tps <- cetotrans(c, e, dt)
                                    # c. Simulación
                                    itin_sim <- PA_simulation(x = second_column_values[i], #datos itinerario 
                                                              2, #columna inicial
                                                              tps #probabilidades de transicion 
                                    ) #COMPROBAR QUE NO FALTA NINGUN ARG
                                    
                                    # d. Nombrar las columnas de la simulación tal y como estaban en el original
                                    colnames(itin_sim) <- years
                                    
                                    # e. Salvar nuevos datos en lista
                                    new_data[[i]] <- itin_sim #COMPROBAR QUE LA COLUMNA INICIAL SE HA INCLUIDO
                                  }
                                  # iii. Calcular la nueva lista con las columnas que tienen P/A, ya que ha cambiado
                                  
                                  #### ???
                                  
                                  # iv. Estimar col-ext con irregular_multiple
                                  
                                  col_ext_res_sim <- irregular_multiple_datasets(--- ---)
                                  
                                  # v. Salvar la NLL en nlls
                                  nlls <- c(nlls, col_ext_res_sim$NLL)
                                } 
                                
                                # 3. Calcular el p-valor de las observaciones
                                Fn <- ecdf(nlls) #Calcula la e.c.d.f.
                                
                                Fn(NLL) 
                              }
                              