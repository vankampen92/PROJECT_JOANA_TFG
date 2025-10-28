
# END CALCUL OCUPANCIES Plebejus argus Mediterranea Humida ####################
#
# Funcio R: Ocupancia teorica en funcio del temps quan la condicio initial es una p_0 generica:
p_occupancy_ce <- function(c, e, t, p_0) {
  value <- (c / (e + c)) * (1 - exp(-(e + c) * t)) + p_0 * exp(-(e + c) * t)
  return(value)
}

c = colext_Results_df_BR2$C_BR2[3]
e = colext_Results_df_BR2$E_BR2[3]
p_0_1994 <- plebe_chi2$n2[1]/plebe_chi2$N2[1]  

temps <- 1:30

Ocupancia_teorica <- p_occupancy_ce(c, e, temps, p_0_1994)
Ocupancia_teorica <- c(p_0_1994, Ocupancia_teorica)

Ocupancia_Shading_plebarg_BR2 <- data.frame()   
Ocupancia_Shading_plebarg_BR2$year <- plebe_chi2$year
Ocupancia_Shading_plebarg_BR2$n <- plebe_chi2$n2
Ocupancia_Shading_plebarg_BR2$N <- plebe_chi2$N2
Ocupancia_Shading_plebarg_BR2$oc_empirica <- plebe_chi2$n2/plebe_chi2$N2
Ocupancia_Shading_plebarg_BR2$oc_teorica <- Ocupancia_teorica
Ocupancia_Shading_plebarg_BR2$oc_teorica <- Ocupancia_teorica_0
Ocupancia_Shading_plebarg_BR2$oc_teorica <- Ocupancia_teorica_1

gg_occupancy_plebe_BR2 <-
  ggplot(data = presence_94_2024_plebejus_BR2_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue4") + # Línea de tendencia lineal sin error estándar
  labs(title = "Plebejus argus  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_plebejus_BR2_df$year)+0.05, # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_plebejus_BR2_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_plebejus_text_BR2,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
##



