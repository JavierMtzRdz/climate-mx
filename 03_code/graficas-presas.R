
##################################################################
##               Proyecto: Visualizaciones presas               ##
##################################################################
##
## Descipción:     Aquí están las visualizaciones de las presas.
##                 
##
## Author:         Javier Mtz.-Rdz.  
##
## Creation date:  2024-04-15
##
## Email:          
##
## ---------------------------
## Notes:          
## ---------------------------

# Setup ----
## Packages to use ----
pacman::p_load(tidyverse, janitor, writexl, 
              readxl, scales, mytidyfunctions, 
              presupuestoR)

## Set theme ------
theme_set(theme_jmr())

options(ggplot2.discrete.colour = paletas_jmr$general,
        ggplot2.discrete.fill = paletas_jmr$general)

## Specify locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Disable scientific notation ----
options(scipen = 999)

# Load data -----

presas_data <- read_csv("04_data-prcssd/data-presas.csv")

# Vis %

hist_mes <- presas_data %>% 
  group_by(fechamonitoreo) %>% 
  summarise(namoalmac = sum(namoalmac),
            almacenaactual = sum(almacenaactual)) %>% 
  mutate(llenano = almacenaactual/namoalmac)

hist_mes %>% 
  mutate(anio = year(fechamonitoreo),
         fecha_no_anio = update(fechamonitoreo, year = 1)) %>% 
  ggplot(aes(fecha_no_anio, llenano,
             group = anio,
             color = anio,
             linewidth = anio)) +
  geom_line() +
  geom_line(aes(y = ifelse(anio == 2024, llenano,
                           NA),
                fill = "2024"),
            color = "#f94144") +
  geom_point(aes(y = ifelse(fechamonitoreo == max(fechamonitoreo),
                           llenano,
                           NA),
                 fill = "2024"),
             color = "#f94144",
             key_glyph = draw_key_rect) +
  scale_color_jmr("order_blue",
                  reverse = T,
                  discrete = F,
                  breaks = seq(1995, 2024, 5),
                  guide = guide_colorbar(
                    direction = "horizontal",
                    keyheight = unit(3, units = "mm"),
                    keywidth = unit(70, units = "mm"),
                    title.position = 'top',
                    title.hjust = 0.5,
                    label.hjust = 0.5,
                    nrow = 1,
                    byrow = T,
                    label.position = "bottom",
                    order = 1
                  )) +
  scale_fill_manual(values = "#f94144") +
  guides(fill = guide_legend(override.aes = list(size = 3,
                                                 linetype = "solid"),
                             direction = "horizontal",
                             keyheight = unit(3, units = "mm"),
                             keywidth = unit(5, units = "mm"),
                             title.position = 'top',
                             title.hjust = 0.5,
                             label.hjust = 0.5,
                             nrow = 1,
                             byrow = T,
                             label.position = "bottom",
                             order = 2)) +
  labs(fill = element_blank(),
       color = element_blank(),
       x = element_blank(),
       y = "Porcentaje de llenado respecto al NAMO",
       title = "Nivel de llenado de las presas en México",
       caption = "Elaboración con datos de CONAGUA, SNIA. | @javiermtzrd") +
  scale_x_date(date_labels = "%b",
               date_breaks = "2 months",
               expand = expansion(mult = c(0.0, 0.0))) +
  scale_y_continuous(labels = percent) +
  scale_linewidth(range = c(0.5, 1.2),
                  guide = "none") +
  theme(plot.title.position = "panel")


walk(c("png", "jpg"),
     ~ggsave(paste0("02_figs/historiq-presas.", .),
             bg = "transparent",
             width = 200,     
             height = 120,
             units = "mm"))


# Vis 2
presas_data %>% 
  # count(fechamonitoreo) %>% View
  filter(year(fechamonitoreo) >= 2011) %>% 
  mutate(per_range = case_when(llenano <= .2 ~ "[0%, 20%]",
                               llenano <= .4 ~ "(20%, 40%]",
                               llenano <= .6 ~ "(40%, 60%]",
                               llenano <= .8 ~ "(60%, 80%]",
                               T ~ "(80%, 100%]")) %>% 
  count(fechamonitoreo, per_range) %>% 
  mutate(percent_range = n/sum(n), 
         .by = fechamonitoreo) %>% 
  mutate(anio = year(fechamonitoreo),
         fecha_n_anio = format(fechamonitoreo,"%m-%d"),
         fecha_n_anio = as.Date(fecha_n_anio, "%m-%d")) %>% 
  ggplot(aes(x = fecha_n_anio, 
             y = percent_range,
             fill = per_range)) +
  geom_area() +
  facet_wrap(anio~ .,
             ncol = 1,
             strip.position = "right") +
  scale_y_continuous(labels = percent,
                     expand = expansion(mult = c(0.0, 0.0))) +
  scale_x_date(date_labels = "%B",
               date_breaks = "2 months",
               expand = expansion(mult = c(0.0, 0.0))) +
  scale_fill_jmr("bi",
                 guide = guide_legend(override.aes = list(size = 3,
                                                         linetype = "solid"),
                                     direction = "horizontal",
                                     keyheight = unit(3, units = "mm"),
                                     keywidth = unit(5, units = "mm"),
                                     title.position = 'top',
                                     title.hjust = 0.5,
                                     label.hjust = 0.5,
                                     nrow = 1,
                                     byrow = T,
                                     label.position = "bottom",
                                     order = 2)) +
  labs(title = "Presas por rango de nivel de llenado",
       fill = "Nivel de llenado",
       x = element_blank(),
       y = element_blank(),
       caption = paste0(str_wrap("Nota: Cada presa tiene diferente capacidad de almacenamiento, por lo cual esta gráfica no muestra un claro indicador del almacenamiento nacional. La idea sólo es resumir el estado de las 210 presas en el país.", 120),
       "\nElaboración con datos de CONAGUA, SNIA. | @javiermtzrd")) +
  theme(legend.spacing = unit(-10.0, "cm"),
        axis.text.y = element_text(size = 6))
  

walk(c("png", "jpg"),
     ~ggsave(paste0("02_figs/historiq-presas-v2.", .),
             bg = "transparent",
             width = 200,
             height = 250,
             units = "mm"))


