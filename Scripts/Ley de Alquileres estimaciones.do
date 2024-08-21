/*******************************************************************************
						Ley de Alquileres (Decreto 70/2023)
			  Ministerio de Desregulación y Transformación del Estado
					 Secretaria de Simplificación del Estado
								 Martín Rossi
				Autores: Abigail Riquelme y Facundo Gómez García
********************************************************************************/
clear all
set more off

else if  "`c(username)'" == "Usuario" {
	global main "dirección"
	}

*Crear carpetas de "input" (donde estará la base de datos) y "output" (para las tablas):
global input "$main/input"
global output "$main/output"

cd "$main"
*ssc desc numdate

import excel "$input/alquileresagrupacion_202407.xlsx", sheet("Sheet 1") firstrow clear
*Es importante mencionar que la variable de interés, "Oferta", está normalizada utilizando como año base el mes de enero de 2018. Entonces, la variable "Oferta" calcula cambios relativos de la oferta del mes observado con respecto al primer mes de 2018.

*===========================*
*		ESTIMACIONES
*===========================*

*El análisis de las estimaciones se realizará para los alquileres publicados en AMBA y los categorizados como "Departamento":
keep if Aglomerado == "AMBA" & Inmueble == "Departamento"
gen time = monthly(Mes, "YM")
format time %tm

keep Oferta time 

*Creamos la variable de discontinuidad ("t_cen") que mide la distancia entre la fecha de la observación y el punto de corte correspondiente al DNU. Además, generamos la variable de tratamiento específica para este contexto, denominada "post_law". Dado que el DNU entró en vigencia el 29 de diciembre y los datos varían mensualmente, se consideró adecuado establecer como punto de corte el mes de enero de 2024.
gen t_cen = time - ym(2024, 1)
gen post_law =  (time > ym(2024, 1))
gen log_oferta = ln(Oferta)

tsset time
unique time

*Realizamos el tradicional gráfico de RD utilizando un polinomio de grado 1 y un polinomio de grado 2:
rdplot Oferta t_cen, p(1) graph_options(title("RDD - Oferta relativa (Año base 2018)", margin(medium)) xtitle("Variable de discontinuidad", margin(medium)) ytitle("Oferta relativa", margin(medium)) scheme(s1mono) legend(off))
graph export "$output/RDD1_oferta.pdf", replace

rdplot Oferta t_cen, p(2) graph_options(title("RDD -  Oferta relativa (Año base 2018)", margin(medium)) xtitle("Variable de discontinuidad", margin(medium)) ytitle("Oferta relativa", margin(medium)) scheme(s1mono) legend(off))
graph export "$output/RDD2_oferta.pdf", replace

rdplot Oferta t_cen, p(3) graph_options(graphregion(color(white)))


**********************************
*   Regresión Discontinua (RD)   *
**********************************
*Cabe aclarar en este punto que se considera la poca potencia estadística de estas estimaciones por el reducido tamaño de la muestra, pero se presentan estas estimaciones como resultados preliminares para analizar el impacto de la Ley sobre alquileres en el AMBA con datos agrupados. Potencialmente, se replicarían estos resultados para datos más desgregados y de alta frecuencia, como podrían ser reportes diarios:

gen post_law_cen=post_law*t_cen

eststo clear
rdrobust Oferta t_cen, masspoints(on) covs(post_law post_law_cen) p(1) all stdvars(on)
estadd scalar bw = abs(e(h_l))
esttab using "$output/Ley_Alquileres_tabla2_oferta.doc", p se replace label noobs ///
cells(b(fmt(3) star) se(par fmt(3))) ///
stats(bw, fmt(3) labels("Bandwidth")) ///
star(* 0.10 ** 0.05 *** 0.010) ///
addnotes("* 0.10 ** 0.05 *** 0.010") 

eststo clear
rdrobust Oferta t_cen, masspoints(on) covs(post_law post_law_cen) p(2) all stdvars(on) 
estadd scalar bw = abs(e(h_l))
esttab using "$output/Ley_Alquileres_tabla2_oferta.doc", p se append label noobs ///
cells(b(fmt(3) star) se(par fmt(3))) ///
stats(bw, fmt(3) labels("Bandwidth")) ///
star(* 0.10 ** 0.05 *** 0.010) ///
addnotes("* 0.10 ** 0.05 *** 0.010")

eststo clear
rdrobust Oferta t_cen, masspoints(on) covs(post_law post_law_cen) p(3) all stdvars(on) 
esttab using "$output/Ley_Alquileres_tabla2_oferta.doc", p se append label noobs ///
cells(b(fmt(3) star) se(par fmt(3))) ///
stats(bw, fmt(3) labels("Bandwidth")) ///
star(* 0.10 ** 0.05 *** 0.010) ///
addnotes("* 0.10 ** 0.05 *** 0.010")

