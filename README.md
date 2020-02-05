# Atlas de Espacios Verdes e Índice de Accesibilidad para ciudades argentinas
Código para el relevamiento de espacios verdes en ciudades argentinas, y la cuantificación de su accesibilidad


## Reproducir el entorno de trabajo
Es simplre reproducir el entorno (es decir, instaar automáticamente todos los paquetes de R que utiliza el código) utilizando las funciones del paquete `renv`, que puede ser instalado desde [CRAN](https://cran.r-project.org/web/packages/renv/index.html)

1. Clonar o descargar este repositorio
2. Abrir el proyecto de R desde RStudio (atlas_espacios_verdes.Rproj). De no trabajar con RStudio, simplemente correr los siguientes comandos en una sesión de R lanzada desde la carpeta raíz del proyecto. 3. Usar `renv::init()` para inicializar `renv` 
4. usar `renv::restore()` para instalar automáticamente los paquetes de los que depende el proyecto. La información que utilizar `renv` para reconstruir el ambiente se encuentra en el archivo `renv.lock`

