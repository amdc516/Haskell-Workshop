{-
Corrección: Aprobada.

En general está todo bastante bien.
Si bien no evaluamos performance, te dejé algunos comentarios sobre el uso del where que pueden ayudar tanto a que el código sea más legible como más eficiente.
-}

-- |Punto 1 - Modelo exponencial discreto: la función med modela el avance de una epidemia en el día n, a partir de los datos i0 
-- (infectadxs iniciales) y la tasa b. Lo hace de manera recursiva, ya que si le pedimos la cantidad de infectadxs del día n, lo determina
-- a partir de la cantidad de infectadxs del día anterior (i(n-1)).

med :: Float -> Float -> Int -> Float
med i0 b 0 = i0
med i0 b n = (med i0 b (n-1)) + b * (med i0 b (n-1))  
{-
Ejercicio 1 está bien.
Podrías haber usado el where para med i0 b (n-1), eso no solo lo haría más legible sino que haría más eficiente el código, porque ahora por cada llamado recursivo se ejecuta 2 veces lo mismo, o sea que para el día n se ejecutan 2^n llamados recursivos. Con el where serían solo n. Igualmente no evaluamos performance, está bueno que lo sepan y para probar casos de muchos días la diferencia es poder correrlo o tener que esperar, literalmente, días, meses, años o incluso siglos.
-}


-- |Punto 2 - Modelo logístico discreto: en la función mld suponemos conocida la cantidad total de la población p de manera que p = i_n + s_n
-- siendo i_n la cantidad de infectadxs en el día n, y s_n la cantidad de individuxs sanxs ese día. La cantidad de infectadxs en un determinado
-- día será igual a la cantidad de infectadxs del día anterior i_n-1 más lxs nuevxs infectadxs. Estxs últimos se calculan a partir de lxs 
-- infectadxs del día anterior, b y la proporción de gente sana del día anterior. 

mld :: Float -> Float -> Float -> Int -> Float
mld p i0 b 0 = i0
mld p i0 b n = (mld p i0 b (n-1)) + b * (mld p i0 b (n-1)) * ((p - (mld p i0 b (n-1))) / p)

{-
Ejercicio 2 está bien.
De nuevo el uso del where habría ayudado bastante. 
-}


-- |Punto 3 - Modelo SIR discreto: este modelo separa a la población total en tres grupos: individuxs sanxs, infectadxs y recuperdxs (ver
-- funciones auxiliares st, it y rt). Sir del día n me devolverá una tupla con los valores de cada grupo para ese día, a partir de los 
-- valores iniciales (s0,i0,r0), n y los factores b y g. 

sir :: (Float,Float,Float) -> Float -> Float -> Int -> (Float, Float,Float)
sir (s0,i0,r0) b g 0 = (s0,i0,r0)
sir (s0,i0,r0) b g n = ((st (s0,i0,r0) b g n), (it (s0,i0,r0) b g n), (rt (s0,i0,r0) b g n)) 

{-
Ejercicio 3 está bien.
¿Era necesario acá hacer 3 funciones separadas? ¿Qué nos devuelve el llamado recursivo y cómo podríamos usar eso?
-}

--Funciones auxiliares Punto 3

-- | Función auxiliar 1 - Individxs sanxs: esta función determina lxs individuxs sanxs en un día n a partir de sanxs en el día anterior
-- a los que se le restan lxs nuevxs infectadxs.

st :: (Float,Float,Float) -> Float -> Float -> Int -> Float
st (s0,i0,r0) b g 0 = s0
st (s0,i0,r0) b g n = (st (s0,i0,r0) b g (n - 1)) - ( b * (it (s0,i0,r0) b g (n-1)) * (st (s0,i0,r0) b g (n-1)))
 

-- | Función auxiliar 2 - Individuxs infectadxs: it de un día n determina la cantidad de infectadxs partiendo de lxs infectadxs del día
-- anterior, a lxs cuales se le suman lxs nuevxs infectadxs y se le restan lxs recuperadxs. 

it :: (Float,Float,Float) -> Float -> Float -> Int -> Float
it (s0,i0,r0) b g 0 = i0
it (s0,i0,r0) b g n = (it (s0,i0,r0) b g (n-1)) + b * (it (s0,i0,r0) b g (n-1)) * (st (s0,i0,r0) b g (n-1)) - g * (it (s0,i0,r0) b g (n-1))
  

-- | Función auxliar 3 - Individuxs recuperadxs: esta función determina la cantidad de recuperadxs en el día n partiendo de lxs recuperadxs 
-- en el día anterior, a los cuales se suma lxs nuevxs recuperados (obtenidxs multiplicando la cantidad de infectadxs del día anterior por 
-- un factor g)

rt :: (Float,Float,Float) -> Float -> Float -> Int -> Float 
rt (s0,i0,r0) b g 0 = r0
rt (s0,i0,r0) b g n = (rt (s0,i0,r0) b g (n-1)) + (g * (it (s0,i0,r0) b g (n-1))) 


-- | Punto 4 maxsir: esta función devuelve la cantidad máxima de infectadxs del día con mayor cantidad de infectadxs, hasta el día n. Como
-- caso base toma n = 0, donde la cantidad de infectadxs es dato (i0). Luego, compara la cantidad de infectadxs maxsir en n-1 con la cantidad
-- de infectadxs it en el día n. Si maxsir n-1 es mayor que it n devuelve maxsir n-1; sino devuelve it n.

maxsir :: (Float,Float,Float) -> Float -> Float -> Int -> Float
maxsir (s0,i0,r0) b g 0 = i0
maxsir (s0,i0,r0) b g n | (maxsir (s0,i0,r0) b g (n-1)) > (it (s0,i0,r0) b g n) = (maxsir (s0,i0,r0) b g (n-1))
                        | otherwise = it (s0,i0,r0) b g n

{-
Ejercicio 4 está bien.
Nuevamente el where podría haber ayudado mucho. Fijate que en la primera guarda calculas el maxsir para (n-1) y it para n. Si la comparación da true, vooooolves a calcular maxsir para (n-1), y si da False voooolves a calcular it para n. 
-}