
##################################################################
##                  Proyecto: Sequía en México                  ##
##################################################################
##
## Descipción:     Gráficas sobre la sequía en México.
##                 
##
## Autor:          Javier Mtz.-Rdz.  
##
## Fecha de crea.: 2024-04-15
##
## Email:          javier.mr@stat.ubc.ca
##
## ---------------------------
## Notes:          
## ---------------------------

# Setup ----
## Paquetes a utilizar ----
pacman::p_load(tidyverse, janitor, writexl, readxl, scales, mexicoR,
               sf, rgdal, gganimate, transformr, mytidyfunctions,
               openxlsx, presupuestoR)

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)

# Cargar datos ----
## Cargar mapa municipal ----
municipios <- st_read("01_data-raw/00_muns/00mun.shp") %>% 
  clean_names() %>% 
  st_transform(4326) %>% 
  st_make_valid()

sequias <- read.xlsx("https://smn.conagua.gob.mx/tools/RESOURCES/Monitor%20de%20Sequia%20en%20Mexico/MunicipiosSequia.xlsx") %>% 
  clean_names() %>% 
  transmute(cve_concatenada,
            across(starts_with("x"), as.character)) %>% 
  pivot_longer(-cve_concatenada,
               values_to = "sequia",
               names_to = "fecha") %>% 
  mutate(fecha = str_remove(fecha, "x"),
         fecha = as.Date(as.numeric(fecha), origin = '1899-12-30'),
         year = year(fecha),
         dia = substr(fecha, 9, 10)) %>% 
  filter(year > 2015,
         dia > 20) %>% 
  mutate(sequia = ifelse(is.na(sequia),
                         "Sin sequía",
                         sequia),
         sequia = factor(sequia,
                         levels = rev(c("Sin sequía",
                                        "D0",
                                        "D1",
                                        "D2",
                                        "D3",
                                        "D4"))))

## Generar hexagonos ----
hex <- st_make_grid(municipios, square = F, n = c(120, 120)) %>% 
  st_as_sf() %>% 
  mutate(num = row_number())

hex_points <- hex %>% 
  st_centroid()

map_points_mun <- st_join(hex_points, municipios) %>% 
  filter(!is.na(cvegeo)) %>% 
  st_set_geometry(NULL) %>% 
  select(num, cvegeo)

map_hex <- hex %>% 
  right_join(map_points_mun, 
             by = "num")

dir.create("04_data-prcssd/mexico-hex/", showWarnings = F)
write_sf(map_hex, "04_data-prcssd/mexico-hex/map_hex.shp")

map_hex_edo <- map_hex %>% 
  mutate(cve_edo = substr(cvegeo, 1,2)) %>% 
  count(cve_edo)


# Generar mapa ----  
map_hex %>% 
  left_join(sequias %>% 
              filter(fecha == as.Date(last(fecha))), 
            by = c("cvegeo" = "cve_concatenada")) %>% 
  ggplot() +
  geom_sf(aes(fill = sequia),
          linewidth = 0.1,
          color = "grey35") +
  geom_sf(data = map_hex_edo,
          fill = "transparent",
          linewidth = 0.3,
          color = "grey15") +
  labs(
    title = "Sequía en México",
    subtitle = paste0("Información al ",
                      format(as.Date(sequias %>% 
                                       filter(fecha == last(fecha)) %>% 
                                       .$fecha %>% unique()), "%d de %B de %Y")),
    caption = "Elaboración con datos de CONAGUA, Monitor de Sequía de México. | @javiermtzrd"
  ) +
  coord_sf(ylim = c(15, 33)) +
  # theme(legend.position = c(0.85, 0.8),
  #       panel.background = element_rect(fill = "grey97",
  #                                       color = "transparent")) +
  scale_fill_manual(
    values = rcartocolor::carto_pal(6, "RedOr"),
    breaks = c("Sin sequía",
               "D0",
               "D1",
               "D2",
               "D3",
               "D4"),
    labels = c("Sin sequía" = "Sin sequía",
               "D0" = "Anormalmente seco",
               "D1" = "Sequía moderada",
               "D2" = "Sequía severa",
               "D3" = "Sequía extrema",
               "D4" = "Sequía excepcional"),
    name = "Intensidad de la sequía",
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"), 
      keywidth = unit(2, units = "mm"),
      title.position = 'top',
      title.hjust = 0,
      label.hjust = 0,
      ncol = 1,
      byrow = F,
      reverse = F,
      label.position = "right"
    )
  ) +
  theme_void() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = c(0.2, 0.23),
    plot.title = element_text(hjust = 0.9, 
                              size = 18, 
                              margin = margin(b = -40),
                              face = "bold", 
                              color = "grey20"),
    plot.subtitle = element_text(hjust = 0.9,
                                 size = 14,
                                 vjust = -12,
                                 # margin = margin(b = -100),
                                 color = "gray50"),
    plot.caption =  element_text(color = "gray50",
                                 size = 10, 
                                 hjust = 0,
                                 margin = margin(t = -10)),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank()
  )

walk(c("png", "jpg"),
     ~ggsave(paste0("02_figs/hexagonos-sequia-Mx.", .),
             bg = "transparent",
             width = 200,                  # Ancho de la gráfica
             height = 127,
             units = "mm"))




# Generar gift con transición temporal ----
bd <- sequias %>% 
  group_by(fecha) %>% 
  filter(fecha >= as.Date("2020-01-01"),
         day(fecha) == max(day(fecha))) 

count_dates<- bd %>% 
  count(fecha)

count_dates$fecha[1]

count_frames <- count_dates %>% 
  nrow()

my_animation <- map_hex %>% 
  left_join(bd,
            # fecha = format(fecha, "%d de %B de %Y"),
            # filter(fecha >= as.Date("2021-08-15")), 
            by = c("cvegeo" = "cve_concatenada")) %>% 
  ggplot() +
  geom_sf(aes(fill = sequia),
          linewidth = 0.1,
          color = "grey35") +
  geom_sf(data = map_hex_edo,
          fill = "transparent",
          linewidth = 0.3,
          color = "grey15") +
  transition_manual(fecha) +
  labs(
    title = "Sequía en México",
    subtitle = '\nAl {format({count_dates$fecha[frame]}, "%d del %m de %Y")}',
    caption = "Elaboración con datos de CONAGUA, Monitor de Sequía de México. | @javiermtzrd"
  ) +
  coord_sf(ylim = c(15, 33)) +
  scale_fill_manual(
    values = rcartocolor::carto_pal(6, "RedOr"),
    breaks = c("Sin sequía",
               "D0",
               "D1",
               "D2",
               "D3",
               "D4"),
    labels = c("Sin sequía" = "Sin sequía",
               "D0" = "Anormalmente seco",
               "D1" = "Sequía moderada",
               "D2" = "Sequía severa",
               "D3" = "Sequía extrema",
               "D4" = "Sequía excepcional"),
    name = "Intensidad de la sequía",
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"), 
      keywidth = unit(5, units = "mm"),
      title.position = 'top',
      title.hjust = 0,
      label.hjust = 0,
      ncol = 1,
      byrow = F,
      reverse = F,
      label.position = "right"
    )
  ) +
  theme_map() +
  theme(
    text = ggplot2::element_text(family = "Lato"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.07, 0.07),
    plot.title = element_text(hjust = 0.9, 
                              size = 18, 
                              vjust = -5,
                              face = "bold", 
                              color = "grey20",
                              margin = margin(-10, 0, -10, 0)
    ),
    plot.subtitle = element_text(hjust = 0.9,
                                 size = 14,
                                 vjust = -20,
                                 color = "gray50",
                                 margin = margin(-10, 0, -10, 0)
    ),
    plot.caption =  element_text(color = "gray50",
                                 size = 10, 
                                 hjust = 0),
    legend.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.margin = margin(t = 0.1,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 0)
  )


animate(plot = my_animation,      
        width = 186,      
        height = 120,
        units = "mm",
        res = 300, 
        renderer = gifski_renderer("02_figs/hexagonos-sequia-Mx.gif"),
        fps = 2,
        nframes = count_frames, 
        duration = count_frames/2)


# Gift con transición de estados ----
# Generar gift ----
my_animation_2 <- map_hex %>% 
  left_join(sequias %>% 
              mutate(fecha = format(fecha, "%d de %B de %Y")),
            # filter(fecha >= as.Date("2021-08-15")), 
            by = c("cvegeo" = "cve_concatenada")) %>% 
  ggplot() +
  geom_sf(aes(fill = sequia),
          size = 0.1,
          color = "grey35") +
  geom_sf(data = map_hex_edo,
          fill = "transparent",
          size = 0.3,
          color = "grey15") +
  labs(
    title = "Sequía en México",
    subtitle = "Información al {closest_state}",
    caption = "Elaboración con datos de CONAGUA, Monitor de Sequía de México. | @javiermtzrd"
  ) +
  transition_states(fecha) +
  coord_sf() +
  scale_fill_manual(
    values = rcartocolor::carto_pal(6, "RedOr"),
    breaks = c("Sin sequía",
               "D0",
               "D1",
               "D2",
               "D3",
               "D4"),
    labels = c("Sin sequía" = "Sin sequía",
               "D0" = "Anormalmente seco",
               "D1" = "Sequía moderada",
               "D2" = "Sequía severa",
               "D3" = "Sequía extrema",
               "D4" = "Sequía excepcional"),
    name = "Intensidad de la sequía",
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"), 
      keywidth = unit(5, units = "mm"),
      title.position = 'top',
      title.hjust = 0,
      label.hjust = 0,
      ncol = 1,
      byrow = F,
      reverse = F,
      label.position = "right"
    )
  ) +
  theme_void() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = c(0.2, 0.23),
    plot.title = element_text(hjust = 0.9, 
                              size = 18, 
                              margin = margin(b = -40),
                              face = "bold", 
                              color = "grey20"),
    plot.subtitle = element_text(hjust = 0.9,
                                 size = 14,
                                 vjust = -12,
                                 # margin = margin(b = -100),
                                 color = "gray50"),
    plot.caption =  element_text(color = "gray50",
                                 size = 10, 
                                 hjust = 0,
                                 margin = margin(t = -10)),
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank()
  )

animate(plot = my_animation_2,      
        width = 220,      
        height = 127,
        units = "mm",
        res = 150, 
        renderer = gifski_renderer("02_figs/hexagonos-sequia-Mx-state.gif"),
        # fps = 20,
        nframes = 69, 
        duration = 40)

## Cargar sequia_por ----

sequia_por <- read.table(file = "01_data-raw//porcentaje_de_afectacion_por_sequia_en_mexico.txt", header = TRUE) %>% 
  mutate(., Fecha = row.names(.)) %>% 
  rename(fecha = Fecha) %>% 
  as_tibble() %>% 
  pivot_longer(contains("D"),
               names_to = "sequia_id",
               values_to = "percent") %>% 
  mutate(fecha = lubridate::as_date(fecha),
         percent = percent/100,
         percent = percent - ifelse(is.na(lead(percent)),
                                    0, lead(percent)),
         sequia_id = factor(sequia_id, 
                            levels = c("0","D0", "D1", "D2", "D3", "D4")),
         .by = fecha) %>% 
  complete(fecha, sequia_id) %>% 
  mutate(percent = ifelse(is.na(percent),
                          1 - sum(percent, na.rm = T),
                          percent),
         sequia_nom = recode(sequia_id,
                             "0" = "Sin sequía",
                             "D0" = "Anormalmente seco",
                             "D1" = "Sequía moderada",
                             "D2" = "Sequía severa",
                             "D3" = "Sequía extrema",
                             "D4" = "Sequía excepcional"),
         fecha = if_else(fecha == as.Date("2013-01-01"),
                         as.Date("2012-12-31"), fecha),
         anio = year(fecha),
         fecha_n_anio = format(fecha,"%m-%d"),
         fecha_n_anio = as.Date(fecha_n_anio, "%m-%d"),
         .by = fecha) %>% 
  complete(anio,
           fill = list(fecha_n_anio = as.Date("2024-01-01"))) %>% 
  group_by(sequia_id) %>% 
  conect_value(anio, fecha) %>% 
  mutate(fecha_n_anio = if_else(anio != year(fecha),
                         as.Date(paste0(anio, "-01-01")), fecha),
         fecha_n_anio = update(fecha_n_anio, year = 2010)) 

sequia_por %>% 
  filter(anio >= 2011) %>% 
  ggplot(aes(x = fecha_n_anio,
             y = percent,
             fill = sequia_id)) +
  geom_area() +
  facet_wrap(anio~ .,
             ncol = 1,
             strip.position = "right") +
  ylim(0, 1) +
  ylim(0, 1) +
  scale_y_continuous(labels = percent,
                     expand = expansion(mult = c(0.0, 0.0))) +
  scale_x_date(date_labels = "%B",
               expand = expansion(mult = c(0.0, 0.0))) +
  labs(title = "Intensidad de lasequía en México",
       x = element_blank(),
       y = element_blank(),
       caption = "Elaboración con datos de CONAGUA, Monitor de Sequía de México. | @javiermtzrd") +
  scale_fill_manual(
    values = rcartocolor::carto_pal(6, "RedOr"),
    breaks = c("0",
               "D0",
               "D1",
               "D2",
               "D3",
               "D4"),
    labels = c("0" = "Sin sequía",
               "D0" = "Anormalmente\nseco",
               "D1" = "Moderada",
               "D2" = "Severa",
               "D3" = "Extrema",
               "D4" = "Excepcional"),
    name = "Intensidad de la sequía",
    guide = guide_legend(override.aes = list(size = 3,
                                             linetype = "solid"),
                         direction = "horizontal",
                         keyheight = unit(3, units = "mm"),
                         keywidth = unit(20, units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 0.5,
                         nrow = 1,
                         byrow = T,
                         label.position = "bottom",
                         order = 2),
    drop = FALSE) +
  theme_jmr(axis.text.y = element_text(size = 6))

walk(c("png", "jpg"),
     ~ggsave(paste0("02_figs/historiq-sequia.", .),
             bg = "transparent",
             width = 200,
             height = 250,
             units = "mm"))
### V2


sequia_por %>%
  filter(anio >= 2011) %>%
  ggplot(aes(x = fecha_n_anio,
             y = percent,
             fill = sequia_id)) +
  geom_area() +
  facet_wrap(anio~ .,
             ncol = 2,
             strip.position = "right") +
  ylim(0, 1) +
  ylim(0, 1) +
  scale_y_continuous(labels = percent,
                     expand = expansion(mult = c(0.0, 0.0))) +
  scale_x_date(date_labels = "%B",
               expand = expansion(mult = c(0.0, 0.0))) +
  labs(x = element_blank(),
       y = element_blank(),
       caption = "Elaboración con datos de CONAGUA, Monitor de Sequía de México. | @javiermtzrd") +
  scale_fill_manual(
    values = rcartocolor::carto_pal(6, "RedOr"),
    breaks = c("0",
               "D0",
               "D1",
               "D2",
               "D3",
               "D4"),
    labels = c("0" = "Sin sequía",
               "D0" = "Anormalmente\nseco",
               "D1" = "Moderada",
               "D2" = "Severa",
               "D3" = "Extrema",
               "D4" = "Excepcional"),
    name = "Intensidad de la sequía",
    drop = FALSE) +
  theme_jmr()

walk(c("png", "jpg"),
     ~ggsave(paste0("02_figs/historiq-sequia-v2.", .),
             bg = "transparent",
             width = 300,
             height = 300,
             units = "mm"))




