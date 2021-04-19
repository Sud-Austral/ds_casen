Tablas de Frecuencias
Tabla de frecuencias sin ponderar

Tablas de frecuencias ponderadas

Para producir tablas estadísticas utilizando los datos de la ENAHO, es necesario considerar el factor de expansión, de forma tal que los resultados representen la población objetivo, de acuerdo con el diseño muestral de la encuesta. Ello se hace incluyendo la variable factor de expansión (FACTOR07) en la sintaxis que genera la tabla:

  tabla1.1 <-xtabs(FACTOR07~P102+urbano, data = enaho17_m1)

https://rpubs.com/dsulmont/475708
