# TFM-modelos-de-panel


# Cargar paquetes necesarios
library(readr)  
library(dplyr)  
library(tidyr)
library(plm)
library(readxl)




vulnerability <- read_csv("resources/vulnerability/vulnerability.csv")

readiness <- read_csv("resources/readiness/readiness.csv")

economics_readiness <- read_csv("resources/readiness/economic.csv")

hdi <- read_csv("resources/indicators/hdi/score.csv")

gdp <- read_csv("resources/indicators/gdp/score.csv")









# Transformar hdi a formato largo
hdi_long <- hdi %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "hdi"
  ) %>%
  mutate(year = as.numeric(year))

head(hdi_long)





# Convertir vulnerability a formato largo
vulnerability_long <- vulnerability %>%
  pivot_longer(
    cols = matches("^\\d{4}$"), 
    names_to = "year",
    values_to = "vulnerability"
  ) %>%
  mutate(year = as.numeric(year))

head(vulnerability_long)


# Convertir readiness a formato largo
readiness_long <- readiness %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "readiness"
  ) %>%
  mutate(year = as.numeric(year))



# Convertir economic readiness a formato largo
economics_readiness_long <- economics_readiness %>%
  pivot_longer(
    cols = matches("^\\d{4}$"), 
    names_to = "year",
    values_to = "economics"
  ) %>%
  mutate(year = as.numeric(year))


# Transformar gdp a formato largo para gdp
gdp_long <- gdp %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "gdp"
  ) %>%
  mutate(year = as.numeric(year))



complete_panel <- readiness_long %>%
  left_join(vulnerability_long, by = c("ISO3", "Name", "year")) %>%
  left_join(economics_readiness_long, by = c("ISO3", "Name", "year")) %>%
  left_join(hdi_long, by = c("ISO3", "Name", "year")) %>%
  left_join(gdp_long, by = c("ISO3", "Name", "year"))



# Crear una copia del panel sin valores cero o negativos
complete_panel_plm <- complete_panel_plm[complete_panel_plm$economics > 0 & complete_panel_plm$vulnerability > 0
                                         & complete_panel_plm$readiness > 0 & complete_panel_plm$hdi > 0
                                         & complete_panel_plm$gdp > 0, ]

complete_panel_d <- pdata.frame(complete_panel, index = c("ISO3", "year"))


# Verifica
head(complete_panel_plm)






########## Todo junto, sumando el modelo con panel dinamico


###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################

############ Con y= readiness

model_fe1 <- plm(readiness ~ vulnerability, data = complete_panel_plm, model = "within")
model_re1 <- plm(readiness ~ vulnerability, data = complete_panel_plm, model = "random")
log_model_fe1 <- plm(log( readiness) ~ log(vulnerability) , data = complete_panel_plm, model = "within")
log_model_re1 <-  plm(log( readiness) ~ log(vulnerability), data = complete_panel_plm, model = "random")







# Modelo Arellano-Bond




# Rezago de economic readiness





complete_panel_d$readiness_lag <- plm::lag(complete_panel_d$readiness, k = 1, index = c("ISO3", "year"))

complete_panel_plm_d1 <- complete_panel_d %>%
  filter(!is.na(readiness_lag))



# Verifica
head(complete_panel_plm_d1)



model_ab1 <- pgmm(
  readiness ~ readiness_lag + vulnerability | lag(readiness, 2),
  data = complete_panel_plm_d1,
  effect = "individual",
  model = "twosteps",
  transformation = "d",
)


log_model_ab1 <- pgmm(
  log(readiness)  ~ log(readiness_lag)  + log(vulnerability)  | lag(log(readiness), 2), 
  data = complete_panel_plm_d1,
  effect = "individual",
  model = "twosteps",
  transformation = "d")



summary(model_fe1)
summary(model_re1)
phtest(model_fe1, model_re1)

summary(log_model_fe1)
summary(log_model_re1)
phtest(log_model_fe1, log_model_re1)

summary(model_ab1, robust = TRUE)
summary(log_model_ab1, robust = TRUE)





############ Con y= hdi


model_fe3 <- plm(hdi ~ vulnerability, data = complete_panel_plm, model = "within")
model_re3 <- plm(hdi ~ vulnerability, data = complete_panel_plm, model = "random")

log_model_fe3 <- plm(log(hdi) ~ log(vulnerability), data = complete_panel_plm, model = "within")
log_model_re3 <- plm(log(hdi) ~ log(vulnerability), data = complete_panel_plm, model = "random")

# Rezago de hdi
complete_panel_d$hdi_lag <- plm::lag(complete_panel_d$hdi, k = 1, index = c("ISO3", "year"))

complete_panel_plm_d3 <- complete_panel_d %>%
  filter(!is.na(hdi_lag))



#######################################################

# Modelo Arellano-Bond
model_ab3 <- pgmm(hdi ~ hdi_lag + vulnerability | lag(hdi, 2), 
                  data = complete_panel_plm_d3,
                 effect = "individual",
                 model = "twosteps",
                 transformation = "d")

log_model_ab3 <- pgmm(log(hdi)  ~ log(hdi_lag)  + log(vulnerability)  | lag(log(hdi) , 2), 
                      data = complete_panel_plm_d3,
                      effect = "individual",
                      model = "twosteps",
                      transformation = "d")


summary(model_fe3)
summary(model_re3)
phtest(model_fe3, model_re3)

summary(log_model_fe3)
summary(log_model_re3)
phtest(log_model_fe3, log_model_re3)


summary(model_ab3, robust = TRUE)
summary(log_model_ab3, robust = TRUE)



###################################################################################################################
#### Pruebo metiendo a idh como explicativa de readiness

model_fe2 <- plm(readiness ~ vulnerability + hdi, data = complete_panel_plm, model = "within")
model_re2 <- plm(readiness ~ vulnerability + hdi , data = complete_panel_plm, model = "random")

log_model_fe2 <- plm(log( readiness) ~ log(vulnerability) + log(hdi) , data = complete_panel_plm, model = "within")
log_model_re2 <-  plm(log( readiness) ~ log(vulnerability) + log(hdi), data = complete_panel_plm, model = "random")


# Asegúrate de que los datos estén ordenados por país y año
complete_panel_d <- complete_panel_d[order(complete_panel_d$ISO3, complete_panel_d$year), ]

# Crear rezagos
complete_panel_d$hdi_lag <- plm::lag(complete_panel_d$hdi, k = 1, index = c("ISO3", "year"))
complete_panel_d$readiness_lag <- plm::lag(complete_panel_d$readiness, k = 1, index = c("ISO3", "year"))

# Filtrar filas con NA en los rezagos
complete_panel_plm_d2 <- complete_panel_d %>%
  filter(!is.na(hdi_lag), !is.na(readiness_lag))


model_ab2 <- pgmm(
  readiness ~ readiness_lag + vulnerability + hdi | lag(readiness, 2),
  data = complete_panel_plm_d2,
  effect = "individual",
  model = "twosteps",
  transformation = "d",
)


log_model_ab2 <- pgmm(
  log(readiness)  ~ log(readiness_lag)  + log(vulnerability) + log(hdi) | lag(log(readiness), 2), 
  data = complete_panel_plm_d2,
  effect = "individual",
  model = "twosteps",
  transformation = "d")



summary(model_fe2)
summary(model_re2)
phtest(model_fe2, model_re2)


summary(log_model_fe2)
summary(log_model_fe2)
phtest(log_model_fe2, log_model_re2)

summary(model_ab2, robust = TRUE)
summary(log_model_ab2, robust = TRUE)


#######################################################################################################
################ Usando PIB percapita como otra medida de robustez 
#################################################################################################

# Combinar capacity y vulnerability 


model_fe4 <- plm(gdp  ~ vulnerability, data = complete_panel_plm, model = "within")
model_re4 <- plm(gdp  ~ vulnerability, data = complete_panel_plm, model = "random")

log_model_fe4 <- plm(log(gdp) ~ log(vulnerability) , data = complete_panel_plm, model = "within")
log_model_re4 <- plm(log(gdp) ~ log(vulnerability) , data = complete_panel_plm, model = "random")



# Rezagos
complete_panel_plm$gdp_lag <- plm::lag(complete_panel_plm$gdp , k = 1, index = c("ISO3", "year"))

complete_panel_plm_d4 <- complete_panel_plm %>%
  filter(!is.na(gdp_lag))



#######################################################

# Modelo Arellano-Bond
model_ab4 <- pgmm(log(gdp)  ~ log(gdp_lag) + vulnerability | lag(gdp, 2),
                  data = complete_panel_plm_d4,
                  effect = "individual",
                  model = "twosteps",
                  transformation = "d")

log_model_ab4 <- pgmm(
  log(gdp)  ~ log(gdp_lag)  + log(vulnerability)  | lag(log(gdp), 2), 
  data = complete_panel_plm_d4,
  effect = "individual",
  model = "twosteps",
  transformation = "d")


summary(model_fe4)
summary(model_re4)
phtest(model_fe4, model_re4)

summary(log_model_fe4)
summary(log_model_re4)
phtest(log_model_fe4, log_model_re4)


summary(model_ab4, robust = TRUE)
summary(log_model_ab4, robust = TRUE)


#####################################################################################
#####################################################################################
#####################################################################################

############## Visualizaciòn 

# Cargar paquetes
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Función actualizada: Elimina NA antes de calcular cuadrantes
crear_grafico <- function(data, x_var, y_var, title, x_label, y_label) {
  
  # Filtrar datos sin NA en las variables clave
  data_clean <- data %>%
    filter(!is.na(vulnerability), !is.na(.data[[y_var]]))
  
  # Calcular medianas para dividir en cuadrantes
  med_vuln <- median(data_clean$vulnerability, na.rm = TRUE)
  med_cap <- median(data_clean[[y_var]], na.rm = TRUE)
  
  # Crear etiquetas de cuadrante
  data_clean <- data_clean %>%
    mutate(
      Cuadrante = case_when(
        .data[[y_var]] >= med_cap & .data$vulnerability < med_vuln ~ "Baja vulnerabilidad y alta capacidad",
        .data[[y_var]] >= med_cap & .data$vulnerability >= med_vuln ~ "Alta vulnerabilidad y alta capacidad",
        .data[[y_var]] < med_cap & .data$vulnerability < med_vuln ~ "Baja vulnerabilidad y baja capacidad",
        .data[[y_var]] < med_cap & .data$vulnerability >= med_vuln ~ "Alta vulnerabilidad y baja capacidad"
      )
    )
  
  # Asignar colores
  colores <- brewer.pal(4, "Set2")
  
  # Crear gráfico
  ggplot(data_clean, aes_string(x = x_var, y = y_var, color = "Cuadrante")) +
    geom_point(alpha = 0.7, size = 2) +
    scale_color_manual(values = colores, name = "Cuadrante") +
    geom_vline(xintercept = med_vuln, linetype = "dashed", color = "gray") +
    geom_hline(yintercept = med_cap, linetype = "dashed", color = "gray") +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      caption = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    ) +
    guides(color = guide_legend(nrow = 2))
}

# 1. Gráfico: Readiness vs. Vulnerability
grafico_readiness <- crear_grafico(
  data = data_panel2,
  x_var = "vulnerability",
  y_var = "readiness",
  title = "",
  x_label = "Vulnerabilidad",
  y_label = "Readiness"
)


# 2. Gráfico: Economic Readiness vs. Vulnerability
grafico_economics <- crear_grafico(
  data = data_panel3,
  x_var = "vulnerability",
  y_var = "economics",
  title = "",
  x_label = "Vulnerabilidad",
  y_label = "Economic Readiness"
)

# 3. Gráfico: IDH vs. Vulnerabilidad
grafico_hdi <- crear_grafico(
  data = data_panel4,
  x_var = "vulnerability",
  y_var = "hdi",
  title = "",
  x_label = "Vulnerabilidad",
  y_label = "Índice de Desarrollo Humano (IDH)"
)


###########################################################################################################################################
###########################################################################################################################################

# 1. Combinar los datos de hdi y readiness
data_hdi_readiness <- left_join(
  data_panel4,  # Contiene hdi y vulnerability
  data_panel2 %>% select(ISO3, Name, year, readiness),  # Solo las columnas necesarias
  by = c("ISO3", "Name", "year")
)

# 2. Verificar que la unión fue correcta
head(data_hdi_readiness)
dim(data_hdi_readiness)

# 3. Calcular el coeficiente de correlación
correlacion <- cor(data_hdi_readiness$hdi, data_hdi_readiness$readiness, use = "complete.obs")
cat("Coeficiente de correlación entre IDH y Readiness:", round(correlacion, 3), "\n")

# 4. Crear el gráfico de dispersión
grafico_hdi_readiness <- ggplot(datzZza_hdi_readiness, aes(x = hdi, y = readiness)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "red", se = TRUE) +
  labs(
    title = "",
    x = "Índice de Desarrollo Humano (IDH)",
    y = "Readiness"
  ) +
  theme_minimal()

# 5. Mostrar el gráfico
print(grafico_hdi_readiness)



##########################################################################################3
# 1. Combinar los datos
data_hdi_readiness <- left_join(
  data_panel4,  # Contiene hdi y vulnerability
  data_panel2 %>% select(ISO3, Name, year, readiness),  # Solo las columnas necesarias
  by = c("ISO3", "Name", "year")
)

# 2. Filtrar solo el año 1995
data_hdi_readiness_1995 <- data_hdi_readiness %>%
  filter(year == 1995)

data_hdi_readiness_2005 <- data_hdi_readiness %>%
  filter(year == 2005)

data_hdi_readiness_2023 <- data_hdi_readiness %>%
  filter(year == 2023)



# 3. Verificar que el filtro fue correcto
head(data_hdi_readiness_1995)
dim(data_hdi_readiness_1995)

# 4. Calcular el coeficiente de correlación (solo para 1995)
correlacion_2023 <- cor(data_hdi_readiness_2023$hdi, data_hdi_readiness_2023$readiness, use = "complete.obs")
cat("Coeficiente de correlación entre IDH y Readiness en 1995:", round(correlacion_2023, 3), "\n")

# 5. Crear el gráfico de dispersión (solo 1995)
grafico_hdi_readiness_2023 <- ggplot(data_hdi_readiness_2023, aes(x = hdi, y = readiness)) +
  geom_point(alpha = 0.7, size = 2, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "red", se = TRUE) +
  labs(
    title = "",
    x = "Índice de Desarrollo Humano (IDH)",
    y = "Readiness"
  ) +
  theme_minimal()

# 6. Mostrar el gráfico
print(grafico_hdi_readiness_2023)

# Modelo con término cuadrático

model_lineal <- lm(readiness ~ hdi , data = data_hdi_readiness)
model_quad <- lm(readiness ~ hdi + I(hdi^2), data = data_hdi_readiness)



model_lineal_1995 <- lm(readiness ~ hdi , data = data_hdi_readiness_1995)
model_quad_1995 <- lm(readiness ~ hdi + I(hdi^2), data = data_hdi_readiness_1995)



model_lineal_2005 <- lm(readiness ~ hdi , data = data_hdi_readiness_2005)
model_quad_2005 <- lm(readiness ~ hdi + I(hdi^2), data = data_hdi_readiness_2005)





model_lineal_2023 <- lm(readiness ~ hdi , data = data_hdi_readiness_2023)
model_quad_2023 <- lm(readiness ~ hdi + I(hdi^2), data = data_hdi_readiness_2023)


summary(model_lineal)
summary(model_quad)

summary(model_lineal_1995)
summary(model_quad_1995)

summary(model_lineal_2005)
summary(model_quad_2005)

summary(model_lineal_2023)
summary(model_quad_2023)

# Prueba de RESET para detectar no linealidad
library(lmtest)


reset_test(model_lineal, power = 2, type = "fitted")

#######################################################################################################################################
#######################################################################################################################################
# Mostrar gráficos
print(grafico_readiness)
print(grafico_economics)
print(grafico_hdi)

##############################
###################### Con visualizacion especifica.






# Lista de países de interés


paises_interes <- c("Spain", "Honduras", "Costa Rica", "Germany", "United States", "China", "India","Haiti" )

# Filtrar datos para los países y años de interés
data_filtrado <- data_panel2 %>%
  filter(Name %in% paises_interes, year %in% c(1995, 2023)) %>%
  arrange(Name, year)  # Ordenar por país y año

# Crear gráfico
grafico_readiness <-ggplot(data_filtrado, aes(x = vulnerability, y = readiness, color = Name, shape = as.factor(year))) +
  # Fondo: todos los países (puntos tenues)
  geom_point(alpha = 0.2, color = "gray50", size = 1) +
  
  # Líneas que conectan los puntos de cada país
  geom_line(aes(group = Name), linetype = "dashed", size = 1) +
  
  # Puntos para 1995 y 2023
  geom_point(size = 4) +
  
  # Tema
  labs(
    title = "",
    x = "Vulnerabilidad",
    y = "Readiness",
    color = "País",
    shape = "Año"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c("1995" = 17, "2023" = 15)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






# Filtrar datos para los países y años de interés
data_filtrado_economics <- data_panel3 %>%
  filter(Name %in% paises_interes, year %in% c(1995, 2023)) %>%
  arrange(Name, year)  # Ordenar por país y año

# Crear gráfico: Economic Readiness
grafico_economics <- ggplot(data_filtrado_economics, aes(x = vulnerability, y = economics, color = Name, shape = as.factor(year))) +
  # Fondo: todos los países (puntos tenues)
  geom_point(data = data_panel3, alpha = 0.2, color = "gray50", size = 1) +
  
  # Líneas que conectan los puntos de cada país
  geom_line(aes(group = Name), linetype = "dashed", size = 1) +
  
  # Puntos para 1995 y 2023
  geom_point(size = 4) +
  
  # Tema
  labs(
    title = "",
    x = "Vulnerabilidad",
    y = "Economic Readiness",
    color = "País",
    shape = "Año"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c("1995" = 17, "2023" = 15)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# Filtrar datos para los países y años de interés
data_filtrado_hdi <- data_panel4 %>%
  filter(Name %in% paises_interes, year %in% c(1995, 2023)) %>%
  arrange(Name, year)  # Ordenar por país y año

# Crear gráfico: IDH
grafico_hdi <- ggplot(data_filtrado_hdi, aes(x = vulnerability, y = hdi, color = Name, shape = as.factor(year))) +
  # Fondo: todos los países (puntos tenues)
  geom_point(data = data_panel4, alpha = 0.2, color = "gray50", size = 1) +
  
  # Líneas que conectan los puntos de cada país
  geom_line(aes(group = Name), linetype = "dashed", size = 1) +
  
  # Puntos para 1995 y 2023
  geom_point(size = 4) +
  
  # Tema
  labs(
    title = "",
    x = "Vulnerabilidad",
    y = "Índice de Desarrollo Humano (IDH)",
    color = "País",
    shape = "Año"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c("1995" = 17, "2023" = 15)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




# Mostrar gráfico
print(grafico_readiness)
print(grafico_economics)
print(grafico_hdi)



