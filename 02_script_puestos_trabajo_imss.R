Sys.setlocale("LC_TIME", "es_ES")

# Paquetes ----
if(!require("lubridate")) install.packages("lubridate") & require("lubridate")
if(!require("hot.deck")) install.packages("hot.deck") & require("hot.deck")
if(!require("zoo")) install.packages("zoo") & require("zoo")
if(!require("stringi")) install.packages("stringi") & require("stringi")
if(!require("gganimate")) install.packages("gganimate") & require("gganimate")
if(!require("gridExtra")) install.packages("gridExtra") & require("gridExtra")
if(!require("ggthemes")) install.packages("ggthemes") & require("ggthemes")
if(!require("hrbrthemes")) install.packages("hrbrthemes") & require("hrbrthemes")
if(!require("magick")) install.packages("magick") & require("magick")
if(!require("scales")) install.packages("scales") & require("scales")
if(!require("RColorBrewer")) install.packages("RColorBrewer") & require("RColorBrewer")
if(!require("foreign")) install.packages("foreign") & require("foreign")
if(!require("srvyr")) install.packages("srvyr") & require("srvyr")

# Tidyverse <3
require(tidyverse)

# Colores MCV -----
mcv_discrete <- c(
  "#7D0A4C", "#6E1640", "#9E5E91", "#AA6D9B",
  "#462759", "#30367C", "#AD1E34", "#E7E4E4"
)

# Directorios ----
paste_inp <- function(x){paste0("mcv/puestos_trabajo_imss/01_datos/", x)}
paste_out <- function(x){paste0("mcv/puestos_trabajo_imss/03_productos/", x)}

# Vectores para loop ----
file_vec <- c(
  "asg-2019-12-31.csv",
  "asg-2020-01-31.csv",
  "asg-2020-02-29.csv",
  "asg-2020-03-31_0.csv",
  "asg-2020-04-30.csv",
  "asg-2020-05-31.csv",
  "asg-2020-06-30.csv",
  "asg-2020-07-31.csv",
  "asg-2020-08-31.csv",
  "asg-2020-09-30.csv",
  "asg-2020-10-31.csv",
  "asg-2020-11-30.csv",
  "asg-2020-12-31.csv"
)

dates_vec <- c(
  "2019-12-31",
  "2020-01-31",
  "2020-02-29",
  "2020-03-31",
  "2020-04-30",
  "2020-05-31",
  "2020-06-30",
  "2020-07-31",
  "2020-08-31",
  "2020-09-30",
  "2020-10-31",
  "2020-11-30",
  "2020-12-31"
)

# Loop ----
permanentes <- data.frame()
eventuales <- data.frame()
sexo <- data.frame()
salarios <- data.frame()
for(i in 1:length(file_vec)){
  
  print(
    paste0(
      "Proceso para periodo ", 
      dates_vec[i]
    )
  )
  
  d <- readr::read_delim(
    paste_inp(file_vec[i]), locale = readr::locale(encoding = "latin1"), delim = "|")
  
  tempo <- d %>% 
    mutate(permanentes = tpu + tpc) %>% 
    summarise(tot = sum(permanentes))%>% 
    mutate(
      periodo = dates_vec[i]
    )
  
  permanentes <- bind_rows(permanentes, tempo)
  
  tempo <- d %>% 
    mutate(eventuales = teu + tec) %>% 
    summarise(tot = sum(eventuales))%>% 
    mutate(
      periodo = dates_vec[i]
    )
  
  eventuales <- bind_rows(eventuales, tempo)
  
  tempo <- d %>% 
    mutate(sexo = ifelse(sexo==1,"Hombres","Mujeres")) %>% 
    group_by(sexo) %>% 
    summarise(tot = sum(ta)) %>% 
    mutate(
      periodo = dates_vec[i]
    )
  
  sexo <- bind_rows(sexo, tempo)
  
  tempo <- d %>% 
    drop_na(rango_salarial) %>% 
    group_by(rango_salarial) %>% 
    summarise(tot = sum(ta)) %>% 
    mutate(
      periodo = dates_vec[i]
    )
  
  salarios <- bind_rows(salarios, tempo)
  
  
}

openxlsx::write.xlsx(permanentes, paste_out("permanentes_imss_2020.xlsx"))
openxlsx::write.xlsx(eventuales, paste_out("eventuales_imss_2020.xlsx"))
openxlsx::write.xlsx(sexo, paste_out("sexo_imss_2020.xlsx"))
openxlsx::write.xlsx(salarios, paste_out("salarios_imss_2020.xlsx"))

# Bases armadas ----
permanentes <- readxl::read_excel(paste_out("permanentes_imss_2020.xlsx"))
eventuales <- readxl::read_excel(paste_out("eventuales_imss_2020.xlsx"))
sexo <- readxl::read_excel(paste_out("sexo_imss_2020.xlsx"))
salarios <- readxl::read_excel(paste_out("salarios_imss_2020.xlsx"))

# Plots ----
fiuf <- "Puestos de trabajo afiliados al IMSS"
fiuff <- "Durante 2020"
fiuffi <- "Fuente: elaboración de @MexicoComoVamos con información del @Tu_IMSS | @guzmart_"
ggplot(
  sexo %>% 
    group_by(periodo) %>%
    summarise(tot = sum(tot)),
  aes(
    x = floor_date(as.Date.character(periodo, "%Y-%m-%d"), "month"),
    y = tot,
    label = prettyNum(tot, big.mark = ","),
    group = 1
  )
) + 
  geom_line(col = mcv_discrete[6]) + geom_point(col = mcv_discrete[6]) +
  geom_label(show.legend = F, vjust = -0.5, col = mcv_discrete[6]) +
  scale_y_continuous("", labels = scales::comma, breaks = seq(0,21000000,3000000)) +
  scale_x_date("", date_breaks = "1 month", date_labels = "%b") +
  labs(title= fiuf,
       subtitle = fiuff,
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 25),
        strip.text.x = element_text(size = 15),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Helvetica Neue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 25),
        legend.key.size = unit(4,"line"))
ggsave(filename = paste_out("00_tot_puestos_trabajo_imss.png"), width = 17, height = 12, dpi = 100, bg = "white")


fiuf <- "Puestos de trabajo afiliados al IMSS"
fiuff <- "Desagregación por sexo durante 2020"
fiuffi <- "Fuente: elaboración de @MexicoComoVamos con información del @Tu_IMSS | @guzmart_"
ggplot(
  sexo %>% 
    group_by(sexo) %>% 
    mutate(cambio = (tot - lag(tot))/lag(tot)*100),
  aes(
    x = floor_date(as.Date.character(periodo, "%Y-%m-%d"), "month"),
    y = tot,
    label = prettyNum(tot, big.mark = ","),
    group = sexo,
    col = sexo
  )
) + 
  geom_line() + geom_point() +
  geom_label(show.legend = F, vjust = -0.5) +
  scale_y_continuous("", labels = scales::comma, breaks = seq(0,13000000,2000000)) +
  scale_x_date("", date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual("", values = c(mcv_discrete[6], mcv_discrete[1])) +
  labs(title= fiuf,
       subtitle = fiuff,
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 25),
        strip.text.x = element_text(size = 15),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Helvetica Neue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 25),
        legend.key.size = unit(4,"line"))
ggsave(filename = paste_out("01_tot_puestos_trabajo_imss.png"), width = 17, height = 12, dpi = 100, bg = "white")


fiuf <- "Variación porcentual de trabajo afiliados al IMSS"
fiuff <- "Desagregación por sexo durante 2020"
fiuffi <- "Fuente: elaboración de @MexicoComoVamos con información del @Tu_IMSS | @guzmart_"
ggplot(
  sexo %>% 
    group_by(sexo) %>% 
    mutate(cambio = (tot - lag(tot))/lag(tot)),
  aes(
    x = floor_date(as.Date.character(periodo, "%Y-%m-%d"), "month"),
    y = cambio,
    label = paste0(round(cambio*100,2),"%"),
    group = sexo,
    col = sexo
  )
) + 
  geom_line() + geom_point() +
  ggrepel::geom_label_repel(show.legend = F) +
  scale_y_continuous("", labels = scales::percent, breaks = seq(-0.05,0.05,0.01), limits = c(-0.05,0.05)) +
  scale_x_date("", date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual("", values = c(mcv_discrete[6], mcv_discrete[1])) +
  labs(title= fiuf,
       subtitle = fiuff,
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 25),
        strip.text.x = element_text(size = 15),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Helvetica Neue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 25),
        legend.key.size = unit(4,"line"))
ggsave(filename = paste_out("02_var_porcentual_puestos_trabajo_imss.png"), width = 17, height = 12, dpi = 100, bg = "white")


fiuf <- "Tipo de puestos de trabajo afiliados al IMSS"
fiuff <- "Durante 2020"
fiuffi <- "Fuente: elaboración de @MexicoComoVamos con información del @Tu_IMSS | @guzmart_"
ggplot(
  permanentes %>% 
    mutate(tipo = "Permanentes") %>% 
    bind_rows(
      eventuales %>% 
        mutate(tipo = "Eventuales")
    ),
  aes(
    x = floor_date(as.Date.character(periodo, "%Y-%m-%d"), "month"),
    y = tot,
    label = prettyNum(tot, big.mark = ","),
    group = tipo,
    col = tipo
  )
) + 
  geom_line() + geom_point() +
  geom_label(show.legend = F, vjust = -0.5) +
  scale_y_continuous("", labels = scales::comma, breaks = seq(0,18000000,2000000)) +
  scale_x_date("", date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual("", values = c(mcv_discrete[6], mcv_discrete[1])) +
  labs(title= fiuf,
       subtitle = fiuff,
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 25),
        strip.text.x = element_text(size = 15),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Helvetica Neue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 25),
        legend.key.size = unit(4,"line"))
ggsave(filename = paste_out("03_tipo_puestos_trabajo_imss.png"), width = 17, height = 12, dpi = 100, bg = "white")


fiuf <- "Rango salarial de puestos de trabajo afiliados al IMSS"
fiuff <- "Durante 2020"
fiuffi <- "Fuente: elaboración de @MexicoComoVamos con información del @Tu_IMSS | @guzmart_"
ggplot(
  salarios %>% 
    mutate(rango_salarial = case_when(
      str_ends(rango_salarial, "W3|W4") ~ "W3",
      str_ends(rango_salarial, "W5|W6|W7|W8|W9|W10|W11|W12|W13|W14|W15|W16|W17|W18|W19|W20|W21") ~ "W5",
      T ~ rango_salarial
    )) %>% 
    group_by(rango_salarial, periodo) %>% 
    summarise(tot = sum(tot)) %>% 
    ungroup(),
  aes(
    x = floor_date(as.Date.character(periodo, "%Y-%m-%d"), "month"),
    y = tot,
    label = ifelse(str_detect(periodo,"12"),prettyNum(tot, big.mark = ","),NA),
    group = rango_salarial,
    col = rango_salarial
  )
) + 
  geom_line() + geom_point() +
  ggrepel::geom_label_repel(show.legend = F, size = 4) +
  facet_wrap(~rango_salarial) +
  scale_y_continuous("", labels = scales::comma, breaks = seq(0,15000000,2500000), limits = c(0,15000000)) +
  scale_x_date("", date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual("", values = c(mcv_discrete[6], mcv_discrete[1], mcv_discrete[2], mcv_discrete[3],mcv_discrete[4])) +
  labs(title= fiuf,
       subtitle = fiuff,
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 25),
        strip.text.x = element_text(size = 15),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Helvetica Neue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 25),
        legend.key.size = unit(4,"line"))
ggsave(filename = paste_out("04_rango_salarial_puestos_trabajo_imss.png"), width = 17, height = 12, dpi = 100, bg = "white")
