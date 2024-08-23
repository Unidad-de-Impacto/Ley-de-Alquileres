library(readxl)
library(lubridate)
library(ggplot2)
library(ggtext)

alquileres <- read_excel("/Users/ariquelme/Downloads/alquileresagrupacion_202407.xlsx")

alquileres_amba <- alquileres %>%
  filter(Aglomerado == "AMBA" &
           Inmueble == "Departamento")

alquileres_amba <- alquileres_amba %>%
  mutate(monthly_pct_changes = round((Mediana.por.m2.a.precios.corrientes / lag(Mediana.por.m2.a.precios.corrientes) - 1) * 100, 2))

alquiler_clean <- alquileres_amba 


callout <- paste0(
  "Oferta de departamentos +<b>90.3%</b><br> <b style='color:", "black",
  ";'>2024-01 </b>vs. <b style='color:", "black", ";'>2023-12</b>"
)
#indico el año de corte que divide las tendencias en dos. Previo y posterior a la derogación de la ley de alquileres.
cutoff <- "2023-12-28"

alquileres_amba <- alquileres_amba[alquileres_amba$Mes>="2021-01",]

alquileres_año <- ggplot(alquileres_amba, aes(Mes, Oferta,
                                              fill=factor(ifelse(Mes =="2023-12","Highlighted","Normal")))) + 
  geom_bar(stat = "identity") +
  geom_smooth(aes(group = Mes >= cutoff), 
              method = "lm",
              color = "#E6B861",
              alpha = 0.1) +
  geom_segment(aes(x = 36.5, xend = 36.5, y = 0, yend = 4), 
               linetype = "dashed",
               color = "#E6B861",
               linewidth = 1) +
  geom_label(aes(x = 32, y = 3, 
                 label = "Derogación ley \nde alquileres"),
             color = "black", fill = "white", size = 3.5, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  geom_label(aes(x = 29, y = 2.05, 
                 label = "Oferta de departamentos\n2024-01 vs. 2023-12 +90.3%"),
             color = "black", fill = "white", size = 3.5, 
             family = "sans", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  annotate(
    geom = "curve", x = 34, xend = 37, y = 1.9, yend = 1.5, curvature = .35, 
    angle = 60, color = "#E6B861", linewidth = .4, 
    arrow = arrow(type = "closed", length = unit(.08, "inches"))
  ) +
  # esta es la segunda flecha 
  annotate(
    geom = "curve", x = 38, xend = 42, y = 3.8, yend = 3, curvature = .35, 
    angle = 60, color = "#E6B861", linewidth = .4, 
    arrow = arrow(type = "closed", length = unit(.08, "inches"))
  ) +
  geom_label(aes(x = 36.5, y = 3.9, 
                 label = "Oferta de departamentos\n2024-06 vs. 2023-12 +211.9%"),
             color = "black", fill = "white", size = 3.5, 
             family = "sans", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  scale_x_discrete(breaks = c("2018-01", "2019-01","2020-01","2021-01","2022-01","2023-01","2024-01"), 
                   labels= c("2018","2019","2020","2021","2022","2023","2024")) + 
  scale_fill_manual(name = "año", values=c("#46658B","#46658B")) +
  theme(legend.position = "none", 
        panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        text = element_text(size = 14, family = "montserrat")) + 
  labs(title = "Oferta de alquileres de departamentos en AMBA",
       caption = "Datos hasta julio del 2024\nFuente: Elaboración propia en base a índices elaborados por Mercado Libre y Universidad de San Andrés") +
  ylab("Cantidad de publicaciones") +
  xlab("Mes")

alquileres_año

ggsave('alquileres_año_1.png',
       alquileres_año,
       bg='transparent',
       width = 16, 
       height = 10)









alquileres_precio <- ggplot(alquileres_amba, aes(Mes, Mediana.por.m2.a.precios.constantes,
                                              fill=factor(ifelse(Mes =="2023-12","Highlighted","Normal")))) + 
  geom_bar(stat = "identity") +
  geom_smooth(aes(group = Mes >= cutoff), 
              method = "lm",
              color = "#E6B861",
              alpha = 0.1) +
  geom_segment(aes(x = 36.5, xend = 36.5, y = 0, yend = 370), 
               linetype = "dashed",
               color = "#E6B861",
               linewidth = 1) +
  geom_label(aes(x = 32.5, y = 370, 
                 label = "Derogación ley \nde alquileres"),
             color = "black", fill = "white", size = 3.5, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  geom_label(aes(x = 29, y = 200.05, 
                 label = "Precios departamentos\n2024-01 vs. 2023-12\n-6.7%"),
             color = "black", fill = "white", size = 3.5, 
             family = "sans", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  annotate(
    geom = "curve", x = 33, xend = 37, y = 178.9, yend = 180.5, curvature = .35, 
    angle = 60, color = "#E6B861", linewidth = .4, 
    arrow = arrow(type = "closed", length = unit(.08, "inches"))
  ) +
  # esta es la segunda flecha 
  annotate(
    geom = "curve", x = 38, xend = 42, y = 300, yend = 185, curvature = .35, 
    angle = 60, color = "#E6B861", linewidth = .4, 
    arrow = arrow(type = "closed", length = unit(.08, "inches"))
  ) +
  geom_label(aes(x = 38.8, y = 320.9, 
                 label = "Precios departamentos\n2024-06 vs. 2023-12\n-26.6%"),
             color = "black", fill = "white", size = 3.5, 
             family = "sans", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  scale_x_discrete(breaks = c("2018-01", "2019-01","2020-01","2021-01","2022-01","2023-01","2024-01"), 
                   labels= c("2018","2019","2020","2021","2022","2023","2024")) + 
  scale_fill_manual(name = "año", values=c("#46658B","#46658B")) +
  theme(legend.position = "none", 
        panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        text = element_text(size = 14, family = "montserrat")) + 
  labs(title = "Precios por m2 de departamentos en alquiler en AMBA",
       caption = "Datos hasta julio del 2024\nFuente: Elaboración propia en base a índices elaborados por Mercado Libre y Universidad de San Andrés") +
  ylab("Pesos constantes") +
  xlab("Mes")

alquileres_precio

ggsave('alquileres_año_1.png',
       alquileres_año,
       bg='transparent',
       width = 16, 
       height = 10)

### calculo de variaciones 

base <- read_excel("/Users/ariquelme/Downloads/alquileresagrupacion_202407.xlsx")

base <- base[base$Aglomerado=="AMBA",]
base <- base[base$Inmueble=="Departamento",]
dic2023 <- base[base$Mes=="2023-12",]$Mediana.por.m2.a.precios.constantes
ene2024 <- base[base$Mes=="2024-01",]$Mediana.por.m2.a.precios.constantes

variacion_int <- ((ene2024-dic2023)/dic2023)*100

base <- base[base$Aglomerado=="AMBA",]
base <- base[base$Inmueble=="Departamento",]
dic2023 <- base[base$Mes=="2023-12",]$Oferta
ene2024 <- base[base$Mes=="2024-01",]$Oferta

variacion_int <- ((ene2024-dic2023)/dic2023)*100


base <- base[base$Aglomerado=="AMBA",]
base <- base[base$Inmueble=="Departamento",]
dic2023 <- base[base$Mes=="2023-12",]$Mediana.por.m2.a.precios.constantes
junio2024 <- base[base$Mes=="2024-06",]$Mediana.por.m2.a.precios.constantes

variacion_int <- ((junio2024-dic2023)/dic2023)*100

base <- base[base$Aglomerado=="AMBA",]
base <- base[base$Inmueble=="Departamento",]
dic2023 <- base[base$Mes=="2023-12",]$Oferta
junio2024 <- base[base$Mes=="2024-06",]$Oferta
variacion_int <- ((junio2024-dic2023)/dic2023)*100
