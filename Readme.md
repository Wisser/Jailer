Herramienta de base de datos del carcelero
Jailer es una herramienta para subconjuntos de bases de datos y exploración de datos relacionales.

El Subsetter exporta conjuntos de filas coherentes y referencialmente intactos desde bases de datos relacionales, genera conjuntos de datos SQL-DML, DbUnit ordenados topológicamente y XML estructurado jerárquicamente.
El navegador de datos permite la navegación bidireccional a través de la base de datos siguiendo relaciones definidas por el usuario o basadas en claves externas.


Características
Exporta conjuntos de filas coherentes y referencialmente intactos desde su base de datos productiva e importa los datos a su entorno de desarrollo y prueba.
Mejora el rendimiento de la base de datos al eliminar y archivar datos obsoletos sin violar la integridad.
Genera conjuntos de datos SQL-DML ordenados topológicamente, XML estructurado jerárquicamente y DbUnit.
Navegación de datos. Navegue bidireccionalmente a través de la base de datos siguiendo las relaciones definidas por el usuario o basadas en claves externas.
Consola SQL con finalización de código, resaltado de sintaxis y visualización de metadatos de base de datos.
Se incluye una base de datos de demostración con la que puede obtener una primera impresión sin ningún esfuerzo de configuración.
Prerrequisitos
Java JRE 8 (o superior)
Los programas de instalación de Windows y Linux (* .msi y * .deb) ya contienen un JRE. Si los usa, no se necesita más instalación.
Importante: debido a la compatibilidad con gráficos HiDPI, se recomienda encarecidamente Java JRE 11 (o superior).
Controlador JDBC para su RDBMS
Para los sistemas de bases de datos más populares, ya se incluye un controlador.
Noticias
2020-01-01 El motor Jailer se publica en el repositorio de Maven. https://mvnrepository.com/artifact/io.github.wisser/jailer-engine
2019-02-01 La nueva "Herramienta de migración de modelos" le permite buscar y editar fácilmente las asociaciones recién agregadas si el modelo de datos se ha extendido después del último cambio en este modelo de extracción.
2018-04-26 La nueva función "Analizar SQL" analiza declaraciones SQL y propone definiciones de asociación. Esto permite aplicar ingeniería inversa al modelo de datos basándose en consultas SQL existentes.
2018-03-06 Consola SQL con finalización de código, resaltado de sintaxis y visualización de metadatos de base de datos.
2017-05-10 La nueva API proporciona acceso programático a la funcionalidad de exportación e importación de datos. http://jailer.sourceforge.net/api.html
2017-03-30 Gestión de filtros mejorada. Soporte para filtros de importación y filtros literales.
2017-01-27 Los ciclos de referencia ahora se pueden exportar aplazando la inserción de claves externas que aceptan valores NULL.
2016-21-10 Filter Templates le permite definir reglas para asignar filtros a columnas.Los filtros en las columnas de clave primaria se propagarán automáticamente a las columnas de clave externa correspondientes.
2015-12-04 Soporte para la pseudocolumna ROWID de Oracle.
2016-09-08 El nuevo modo "Exportar a" permite exportar filas directamente a un esquema diferente en la misma base de datos.
2015-12-04 Soporte para la pseudocolumna ROWID de Oracle.
2015-10-23 La versión 5.0 presenta la capacidad de recopilar filas en una base de datos integrada separada. Esto le permite exportar datos de bases de datos de solo lectura.
2011-07-20 Implementó la función "Subconjunto por ejemplo": use el navegador de datos para recopilar todas las filas que se extraerán y permita que Jailer cree un modelo para ese subconjunto.
2010-04-15 Se ha introducido un navegador de datos. Navegue bidireccionalmente a través de la base de datos siguiendo las relaciones definidas por el usuario o basadas en claves externas.
2008-12-23 Jailer ahora admite el formato de archivo de conjunto de datos XML plano DbUnit, lo que permite a los usuarios de la famosa extensión JUnit DbUnit utilizar los datos extraídos para pruebas unitarias.
2007-12-05 La versión 2.0 viene con una nueva interfaz gráfica de usuario.
2007-06-05 Tutorial para Jailer ahora disponible.
Instalación
Si no desea instalar Java usted mismo, utilice el archivo de instalación "Jailer-nnn-with-java-JRE.msi" (para Windows) o "jailer-database-tools_10.2.2-x64-with-java-JRE.deb "(para Linux).

De lo contrario, utilice el instalador "Jailer-Install-nnnexe" o descomprima el archivo "jailer_n.nnzip". Ver tambiénhttp://jailer.sourceforge.net/faq.html#multiuser

Para iniciar la herramienta desde el zip descomprimido:

Subcontador de base de datos

En la plataforma Windows, ejecute "Jailer.exe". También puede iniciar "jailerGUI.bat".
En la plataforma Unix / Linux, ejecute el script "jailerGUI.sh" o use "java -jar jailer.jar"
Navegador de datos

En la plataforma Windows, ejecute "jailerDataBrowser.exe" o "jailerDataBrowser.bat"
En la plataforma Unix / Linux, ejecute el script "jailerDataBrowser.sh"
edificio
Clona el repositorio de git:

git clone https://github.com/Wisser/Jailer.git
Para construir la herramienta, puede usar ant: ( https://ant.apache.org )

cd Jailer
ant
Contacto
Inicio: http://jailer.sourceforge.net/ o https://github.com/Wisser/Jailer
Foro: https://sourceforge.net/p/jailer/discussion/
Soporte: rwisser@users.sourceforge.net
Colaboradores
Colaboradores de código
Este proyecto existe gracias a todas las personas que contribuyen. 

Contribuyentes financieros
Conviértete en un contribuyente financiero y ayúdanos a sostener nuestra comunidad. [ Contribuir ]

Individuos


Organizaciones
Apoye este proyecto con su organización. Su logotipo aparecerá aquí con un enlace a su sitio web. [ Contribuir ]

         
