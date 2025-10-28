
#END CALCUL OCUPANCIES Plebejus argus ##################################
#
# Funcio R: Ocupancia teorica en funcio del temps quan la condicio initial es una p_0 generica:
p_occupancy_ce <- function(c, e, t, p_0) {
  value <- (c / (e + c)) * (1 - exp(-(e + c) * t)) + p_0 * exp(-(e + c) * t)
  return(value)
}





