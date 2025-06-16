--------------------------------------------
--     ELIMINAR EL ULTIMO PRODUCTO        --
--------------------------------------------

DELETE FROM PRODUCTO WHERE ID_PRODUCTO=5;

-------------------------------------------- 
--         INSERTAR 5 PRODUCTOS           -- 
--------------------------------------------  

INSERT INTO PRODUCTO
   (
   ID_PRODUCTO, NOMBRE, PRECIO, FECHA_ALTA
   )
VALUES
   (
      4, 'MAGUEL', 915.00, '2025-05-07'                                    
   );

--------------------------------------------
--      ACTUALIZAR EL PRECIO UN 10%       --
--------------------------------------------     

SELECT ID_PRODUCTO
FROM PRODUCTO;

------------------------------------------------------------- 
-- AGREGA LA COLUMNA FABRICANTE . VARCHAR DE 30 POSICIONES -- 
-------------------------------------------------------------                                      

UPDATE PRODUCTO SET PRECIO = PRECIO * 1.10 WHERE ID_PRODUCTO = 1;

-------------------------------------------------------------
--       MUESTRA PRODUCTOS CON PRECIO SUPERIOR A 50        --
-------------------------------------------------------------

ALTER TABLE PRODUCTO ADD FABRICANTE VARCHAR(30);

-------------------------------------------------------------
--       MUESTRA PRODUCTOS CON PRECIO SUPERIOR A 50        --
-------------------------------------------------------------

SELECT *
FROM PRODUCTO
WHERE PRECIO >= 50;

-----------------------------------------------------------------------
-- MUESTRA PRODUCTOS DADOS DE ALTA ENTRE OCTUBRE Y DICIEMBRE DE 2023 --
-----------------------------------------------------------------------

SELECT *
FROM PRODUCTO
WHERE FECHA_ALTA BETWEEN '2023-10-01' AND '2023-12-31';

-------------------------------------------------------------
--       MUESTRA PRODUCTOS DE CIEERTAS CATEGORIAS          --
-------------------------------------------------------------

SELECT *
FROM PRODUCTO
WHERE CATEGORIA IN ('INFORMATICA', 'MOBILIARIO');

-------------------------------------------------------------
--      MUESTRA PRODUCTOS CUYO NOMBRE EMPIECE POR 'S'      --
-------------------------------------------------------------

SELECT *
FROM PRODUCTO
WHERE NOMBRE LIKE 'S%';

------------------------------------------------------------- 
--          MUESTRA PRODUCTOS SIN DESCRIPCION              -- 
------------------------------------------------------------- 

SELECT *
FROM PRODUCTO
WHERE DESCRIPCION IS NULL OR DESCRIPCION = '';

-------------------------------------------------------------
--    MUESTRA PRODUCTOS DE INFORMATICA CON PRECIO > 20.    --
-------------------------------------------------------------

SELECT *
FROM PRODUCTO
WHERE CATEGORIA = 'INFORMATICA' AND PRECIO > 20;

-------------------------------------------------------------
--  MUESTRA PRODUCTOS CON PRECIO < 25 O STOCK MAYOR A 50   --
------------------------------------------------------------- 

SELECT *
FROM PRODUCTO
WHERE STOCK >= 50 OR PRECIO < 25;

---------------------------------------------------------------
-- MUESTRA PRODUCTOS QUE NO SON DE LA CATEGORIA "MOBILIARIO" --
---------------------------------------------------------------

SELECT *
FROM PRODUCTO
WHERE CATEGORIA <> 'MOBILIARIO';

---------------------------------------------------------------
--   MUESTRA LAS 3 PRIMERAS LETRAS DEL NOMBRE DEL PRODUCTO   --
---------------------------------------------------------------

SELECT SUBSTR(NOMBRE, 1, 3)
FROM PRODUCTO;

--------------------------------------------------------------- 
--         MUESTRA CUANTOS PRODUCTOS HAY POR CATEGORIA       -- 
--------------------------------------------------------------- 

SELECT CATEGORIA, COUNT(*) AS CANTIDAD_PRODUCTO
FROM PRODUCTO
GROUP BY CATEGORIA;

---------------------------------------------------------------
--          MUESTRA EL PRECIO PROMEDIO POR CATEGORIA         --
---------------------------------------------------------------       

SELECT CATEGORIA, AVG(PRECIO) AS PRECIO_PROMEDIO
FROM PRODUCTO
GROUP BY CATEGORIA;

--------------------------------------------------------------- 
--        MUESTRA CATEGORIAS CON MAS DE 1 PRODUCTO           -- 
--------------------------------------------------------------- 

SELECT CATEGORIA, COUNT(*) AS TOTAL_PRODUCTOS
FROM PRODUCTO
GROUP BY CATEGORIA
HAVING COUNT(CATEGORIA) > 1
ORDER BY CATEGORIA;

-------------------------------------------------------------------- 
-- MUESTRA CATEGORIAS CON PRODUCTOS DE 2024 Y AL MENOS 1 PRODUCTO -- 
-------------------------------------------------------------------- 

SELECT CATEGORIA, COUNT(*) AS N_CATEGORIAS
FROM PRODUCTO
WHERE YEAR(FECHA_ALTA) = 2024
GROUP BY CATEGORIA
HAVING COUNT(STOCK) > 1
ORDER BY CATEGORIA;                                           
                                                                       
