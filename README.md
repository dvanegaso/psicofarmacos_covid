## Introducción

En este repositorio se encuentra el código manejado para analizar la evolución del consumo de psicofarmacos durante el desarrollo de la pandemia por COVID en Colombia. 

## Instrucciones

Este código está estructurado de la siguiente manera:

```text
├── bases/
|       └── CÓDIGO_ÚNICO_DE_MEDICAMENTOS_VIGENTES_20261025.csv
|       └── medicamentos_candidatos.csv
|       └── PPED-AreaNac-1950-2017.xlsx
|       └── PPED-AreaSexoEdadNac-2018-2070.xlsx
|       └── principios_activos.csv
│   └── SISMED/
│       └── [codigo WHO de principio activo]_2026-10-26/
│           ├── completa.csv
│           └── ficha.csv
├── diccionarios/
|       └── DDD.csv
|       └── proyecciones_dane_2017_2025.csv
├── salidas/
│       └── base_sismed_final.csv
|       └── base_DHD_final.csv
├── construccion_DHD.R
├── escritura de medicamentos.R
├── lectura_SISMED.R
├── proyecciones_dane.R
├── README.md
└── scraper_ATC_DDD.R
```

En este repositorio solo se adjuntan scripts .R y estructura de carpetas **sin las bases de datos guardadas** debido a los derechos de autoría de los respectivos datos.

## Bases de datos

Para el desarrollo de este proyecto de grado se han utilizado las siguientes bases de datos:

- [Proyecciones poblacionales del DANE](https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion).
- [Retroproyecciones poblacionales del DANE](https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion).
- [Código Único de Medicamentos Vigentes](https://www.datos.gov.co/Salud-y-Protecci-n-Social/C-DIGO-NICO-DE-MEDICAMENTOS-VIGENTES/i7cb-raxc/about_data).
- Base SISMED (no disponible para su descarga directa).

## Datos

Para esta investigación se tomaron medicamentos dentro de la base y cohorte con los siguientes principios activos:

**Principios activos**

- N05A
- N05B
- N05C
- N06A
- N03AE01

Para ello, se consideran los siguientes **medicamentos candidatos**

*Antidepresivos*
- sertraline
- fluoxetine
- escitalopram
- trazodone (no está)
- vortioxetine
- amitriptiline (no está)

*Ansiolíticos benzodiazepínicos*
- alprazolam
- lorazepam
- diazepam
- clobazam

*Hipnóticos y sedantes*
- zolpidem
- zopiclone

*Antipsicóticos*
- quetiapine
- risperidone
- olanzapine

*Control*
- losartan (no está)
- statin (no está)

*Sensibilidad*
- clonazepam

## Limpieza de las bases

Para trabajar los datos a la fecha se ha trabajado con medicamentos cuya presentación es en tabletas, esto debido a que su medición es mas sencilla. 

Para la extracción de los datos se ha seguido el siguiente procedimiento:

- Identificar en la base **SISMED** los medicamentos vendidos por mes entre los años 2017 y 2025.
- Tomar medicamentos cuyo principio activo sea candidato.
- Tomar medicamentos vendidos en tabletas; esto por la existencia de medicamentos cuyo registro en la base CUM se encuentra vencido o no existe forma de rastrear su contenido de principio activo por unidad vendida.
- Revisar las concentraciones de principio activo por cada medicamento (tomando en cuenta unidades, unidades CUM, y cantidad CUM) entre la base CUM y otras bases previamente construidas.
- Hacer `left_join()` de bases mediante el código CUM, esto debido a que existen medicamentos cuyo registro INVIMA corresponde a distintas revisiones o se venden en presentaciones distintas.

## Cinstrucción de variable de interés

La variable $DHD_{t}$ se construyó de la siguiente manera:

$$
DHD_t = \left[ (mg\ vendidos_t / DDD) / (población_t \times días\ del\ mes_t) \right] \times 1000
$$

Para ello se necesitó de la base de **proyecciones y retroproyecciones del DANE** para la población estimada en el país, base **SISMED** para la cantidad de unidades de medicamentos vendidas, y base **CUM** para revisar la cantidad de principio activo contenido dentro de cada medicamento.
