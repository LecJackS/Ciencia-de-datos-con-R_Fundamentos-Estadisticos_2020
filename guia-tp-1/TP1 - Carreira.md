# TP 1: Primeros pasos con R

---

### Alumno: Leandro Carreira
### LU: 669/18

---

## Ejercicio 8 de Guía TP 1

---

<h1>Indice<span class="tocSkip"></span></h1>
<div class="toc"><ul class="toc-item"><li><span><a href="#TP-1:-Primeros-pasos-con-R" data-toc-modified-id="TP-1:-Primeros-pasos-con-R-1">TP 1: Primeros pasos con R</a></span><ul class="toc-item"><li><ul class="toc-item"><li><span><a href="#Alumno:-Leandro-Carreira" data-toc-modified-id="Alumno:-Leandro-Carreira-1.0.1">Alumno: Leandro Carreira</a></span></li><li><span><a href="#LU:-669/18" data-toc-modified-id="LU:-669/18-1.0.2">LU: 669/18</a></span></li></ul></li><li><span><a href="#Ejercicio-8-de-Guía-TP-1" data-toc-modified-id="Ejercicio-8-de-Guía-TP-1-1.1">Ejercicio 8 de Guía TP 1</a></span><ul class="toc-item"><li><span><a href="#Ejercicio-8" data-toc-modified-id="Ejercicio-8-1.1.1">Ejercicio 8</a></span></li><li><span><a href="#Ejercicio-8.a" data-toc-modified-id="Ejercicio-8.a-1.1.2">Ejercicio 8.a</a></span></li><li><span><a href="#Solución-8.a" data-toc-modified-id="Solución-8.a-1.1.3">Solución 8.a</a></span></li><li><span><a href="#Ejercicio-8.b" data-toc-modified-id="Ejercicio-8.b-1.1.4">Ejercicio 8.b</a></span></li><li><span><a href="#Solución-8.b" data-toc-modified-id="Solución-8.b-1.1.5">Solución 8.b</a></span></li><li><span><a href="#Ejercicio-8.c" data-toc-modified-id="Ejercicio-8.c-1.1.6">Ejercicio 8.c</a></span></li><li><span><a href="#Solución-8.c" data-toc-modified-id="Solución-8.c-1.1.7">Solución 8.c</a></span></li></ul></li></ul></li></ul></div>

### Ejercicio 8

Los datos `TITANIC3` del paquete `PASWR2` contienen información sobre los pasajeros del Titanic, incluyendo **clase**, **sexo** y **si sobrevivieron o no**, entre otras caracterı́sticas.

### Ejercicio 8.a

> Determine la **proporción de sobrevivientes por clase**.

### Solución 8.a




```R
# Se necesita para ejercicios 8
install.packages("PASWR2")
```

    Updating HTML index of packages in '.Library'
    Making 'packages.html' ... done



```R
# Importo librería PASWR2
require(PASWR2)
```

    Loading required package: PASWR2
    Loading required package: lattice
    Loading required package: ggplot2
    Registered S3 methods overwritten by 'ggplot2':
      method         from 
      [.quosures     rlang
      c.quosures     rlang
      print.quosures rlang



```R
# Descomentar para verificar que cargue los datos 
#head(TITANIC3)
```

### Ejercicio 8.b

>Calcule la **proporción de sobrevivientes por clase y sexo**.
>
>¿Quién tuvo una tasa **más alta** de supervencia: los **varones de 1ra clase** o las **mujeres de 3ra**?

### Solución 8.b

Filtro pasajeros por

>**varones de 1ra clase** (`sex=="male" & pclass=="1st"`)
>
>o
>
>**mujeres de 3ra** (`sex=="female" & pclass=="3rd"`),

y ademas cuento sobrevivientes en cada uno (`survived==1`), para calcular la tasa:


```R
# Males 1st class
total_males    <- nrow(TITANIC3[TITANIC3$sex=="male" & TITANIC3$pclass=="1st" ,])
survived_males <- nrow(TITANIC3[TITANIC3$sex=="male" & TITANIC3$pclass=="1st" & TITANIC3$survived==1, ])
rate_males     <- survived_males/total_males
# Females 3rd class
total_females    <- nrow(TITANIC3[TITANIC3$sex=="female" & TITANIC3$pclass=="3rd",])
survived_females <- nrow(TITANIC3[TITANIC3$sex=="female" & TITANIC3$pclass=="3rd" & TITANIC3$survived==1, ])
rate_females     <- survived_females/total_females
```


```R
data.frame(rate_males, rate_females)
```


<table>
<thead><tr><th scope=col>rate_males</th><th scope=col>rate_females</th></tr></thead>
<tbody>
	<tr><td>0.3407821</td><td>0.4907407</td></tr>
</tbody>
</table>



> Los datos muestran que los **varones de 1era clase** tuvieron una tasa se supervivencia del $\approx$**34.08%**, mientras que las **mujeres de 3era clase** tuvieron una tasa de supervivencia de $\approx$**49.07%**.
>
> Por lo tanto, la **tasa más alta de supervivencia** la tuvo el grupo de **mujeres de 3era clase**.

### Ejercicio 8.c

> ¿Cuál era la **edad** de la **mujer más grande** que **sobrevivió**?

### Solución 8.c

Dado que hay **muchos valores nulos** (*NA*) entre las edades de los pasajeros en los registros, es necesario hacer explícita la opción `na.rm` del comando `max`, que **calcula el máximo valor *solo* entre los valores no-nulos**.


```R
max(TITANIC3[TITANIC3$sex=="female" & TITANIC3$survived==1 , "age"], na.rm=TRUE)
```


76


> La edad de la mujer más grande que sobrevivió era de **76 años**.
