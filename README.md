# MIAPA

**Material de análisis estadístico aplicado a la agronomía con R.**

Autor: Dr. Byron González · [byrong.cc](http://byrong.cc) · 2026

Este repositorio reúne scripts didácticos y conjuntos de datos para explorar diseños experimentales agrícolas, ajustar modelos y revisar sus supuestos. No es un paquete de R ni una aplicación única: cada archivo `.R` es un ejemplo independiente que puede estudiarse y adaptarse a otros datos.

## ¿Qué se puede encontrar?

Los temas están organizados en carpetas según el método o diseño:

| Tema | Contenido y punto de partida |
| --- | --- |
| Arreglo en parcelas divididas | Dos factores aplicados en unidades experimentales de distinto tamaño; se distinguen sus errores experimentales. Ejemplos: `arreglo en parcelas divididas/soya_pd.R` (soya) y `arreglo en parcelas divididas/labranza_control_malezas_mmixto.R` (labranza y control de malezas, también con modelo mixto). |
| Arreglo en parcelas subdivididas | Tres factores asignados en parcelas grandes, medianas y pequeñas; el análisis considera los estratos de error correspondientes. Ejemplo: `arreglo en parcelas subdivididas/parsubdiv_maiz.R` (maíz). |
| Arreglo en franjas y franjas divididas | Factores distribuidos en franjas perpendiculares y, en franjas divididas, otro factor en sus intersecciones. Ejemplos: `arreglo en franjas/franjas_maiz.R` y `arreglo en franjas divididas/strip-split-plot en arroz.R`. |
| Arreglo combinatorio | Análisis factorial de varios factores y sus interacciones. Ejemplo: `arreglo combinatorio/alpacas.R` (raza, ambiente y alimentación). |
| Grupo de experimentos | Análisis conjunto entre localidades, incluyendo ejemplos de modelos mixtos. Ejemplo: `grupo de experimentos/análisis combinado de experimentos.R` (líneas de frijol). |
| Supuestos del ANDEVA | Diagnóstico de residuos en un diseño completamente al azar (DCA) y ejemplo de transformación Box-Cox. Ejemplo: `supuestos del andeva/Andeva_DCA_lec3.R`. |
| Contrastes ortogonales | Comparaciones planificadas entre grupos de tratamientos. Ejemplo: `contrastes ortogonales/contrastes nematicidas.R`. |
| Regresión y polinomios | Regresión múltiple, selección de modelos y respuestas a dosis; se revisan ajuste, residuos y colinealidad. Consulte `análisis de regresión/fitness_reg_mult.R`, `análisis de regresión/polinomial 2 - madera y tensión/madera y tensión.R` y `polinomios ortogonales/eucalipto_polinom.R`. |
| Correlación | Matrices, pruebas y visualizaciones de asociación entre variables. Ejemplo: `análisis de correlación/correlation.salinidad.r.R`. |
| Pruebas no paramétricas | Ejemplos de Kruskal-Wallis, Friedman y Mann-Whitney, con algunas comparaciones posteriores. Consulte la carpeta `pruebas no paramétricas`. |
| Aleatorización | Herramientas interactivas de `FielDHub` para generar arreglos de parcelas subdivididas y franjas divididas. Consulte `arreglo en parcelas subdivididas/aleatorizacion_split_split_plot_maiz.R` y `arreglo en franjas divididas/aleatorizacion_strip_split_plot.R`. |

Los ejemplos incluyen, según el caso, gráficos exploratorios e interacciones, ANDEVA o modelos lineales/mixtos, comparación de medias, análisis de residuos y transformaciones. No todos los scripts siguen exactamente las mismas etapas: revise el código y adapte el análisis al diseño, la pregunta y la estructura de sus datos.

## Datos

Los archivos Excel disponibles están en `data/`. Entre los ejemplos asociados a scripts están:

| Archivo de datos | Ejemplo relacionado |
| --- | --- |
| `soya_pardiv.xlsx` | Parcelas divididas en soya |
| `parsubdivmaiz.xlsx` | Parcelas subdivididas en maíz |
| `peso_alpacas.xlsx` | Arreglo combinatorio de alpacas |
| `frijol.xlsx` | Análisis combinado de experimentos |
| `nem_lec3.xlsx` | Supuestos del ANDEVA y transformación |
| `tabla_nematicidas.xlsx` | Contrastes de tratamientos |
| `polinomioseu.xlsx` | Respuesta polinomial a dosis |
| `salinidad.xlsx`, `salinidad1.xlsx` | Correlación de variables de salinidad |
| `anastrepha.xlsx`, `cacao.xlsx`, `epapa.xlsx` | Ejemplos de pruebas no paramétricas |
| `fitness.xlsx`, `madera.y.tension.xlsx` | Regresión múltiple y polinomial |

La carpeta también contiene otros conjuntos (`barrenador.xlsx`, `cipres.co.xlsx`, entre otros); consulte las hojas y variables de cada archivo antes de elegirlo. Algunos scripts usan conjuntos incluidos en paquetes de R o leen datos desde una URL, por lo que esos ejemplos requieren conexión a Internet.

## Cómo empezar

1. Instale [R](https://www.r-project.org/) y, si lo desea, [RStudio](https://posit.co/download/rstudio-desktop/); abra `miapa.Rproj`.
2. Elija un tema y lea primero el script correspondiente. Revise los nombres de las variables, los factores, la respuesta y la estructura de parcelas o bloques.
3. Abra el archivo de datos indicado y confirme que corresponde al ejemplo. Ejecute el script por secciones, no todos los scripts a la vez.
4. Instale los paquetes que falten cuando R lo solicite y revise los resultados y diagnósticos antes de interpretar las comparaciones.

Para que funcionen las rutas relativas, ejecute los ejemplos desde la carpeta del proyecto. Las rutas de importación no son uniformes en todos los scripts: algunos usan `data/archivo.xlsx` y otros esperan encontrar el archivo en el directorio de trabajo. Si aparece un error de archivo no encontrado, adapte `read_excel()` a la ubicación real, por ejemplo:

```r
readxl::read_excel("data/soya_pardiv.xlsx")
```

Algunos scripts instalan paquetes al ejecutarse, limpian objetos de la sesión o abren ventanas gráficas. Revise esas instrucciones antes de ejecutar código en una sesión con trabajo sin guardar.

## Guía conceptual para analizar un experimento

1. **Identifique el diseño y la unidad experimental.** Determine qué tratamientos se asignaron, a qué unidades y parcelas, y si existen bloques, localidades o mediciones relacionadas. En parcelas divididas y subdivididas, cada factor puede tener un error experimental distinto; un ANDEVA factorial ordinario puede no representar esa estructura.
2. **Explore los datos.** Compruebe tipos, niveles, observaciones faltantes y balance del diseño. Represente la respuesta por tratamiento y examine posibles interacciones.
3. **Ajuste el modelo que corresponde al diseño.** Incluya bloques, factores e interacciones pertinentes; en experimentos combinados o con efectos aleatorios, considere los ejemplos de modelos mixtos.
4. **Revise los supuestos del modelo.** Examine los residuos y gráficos diagnósticos —incluidos normalidad y variación aproximadamente constante— y valore transformaciones solo cuando sean justificadas. Una prueba estadística aislada no sustituye la evaluación del diseño y los gráficos.
5. **Interprete y comunique.** Si corresponde, realice comparaciones de medias o contrastes adecuados al modelo y al error experimental. Presente estimaciones, incertidumbre y unidades, no solo valores *p*.

Los scripts son material de aprendizaje, no una receta universal ni una validación automática del diseño. Para analizar un estudio real, confirme la aleatorización, las unidades experimentales y los supuestos con el protocolo y los datos originales.
