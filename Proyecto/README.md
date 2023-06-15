# Proyecto Final 

- Fecha de entrega **12 de junio**

## Integrantes :wolf:

- Ortega Garcia Alejandra - `420002495`
- Pedro Mendez Jose Manuel - `315073120`  
- Ramirez Gutierrez Oscar - `419004183`
- Villanueva Garcia Israel - `317052147`
- Aguilera Moreno Adrián - `421005200`
- Gutierrez Medina Sebastian Alejandro - `318287021`

## Para probar, ejecutar ...

Darle los permisos de ejecucion a `proyecto.sh` y `proyecto.rkt`

```
chmod +x proyecto.sh proyecto.rkt
```

Ejecutar el script

```
./proyecto.sh <ruta-archivo>
```

Por ejemplo

```
./proyecto.sh ./pruebas/lp.jly
```

## Ejercicios resueltos

- **Traduccion a codigo Java (7 pts)**

  Se decidio aplicar todos los procesos de las practicas y en caso de que no haya error traducir a Java la representacion **no renombrada**

  ```
  (define (get-rep-java archivo nombre)
    (let* ([entrada (aceptado? archivo)]
           [e-renombrada  (rename-var entrada)]
           [tabla (tipos-programa e-renombrada (make-hash))])
        (type-check e-renombrada tabla)   
        (java-programa entrada nombre)))
  ```

- **Extiende el lenguaje para que admita el ciclo for. (1 pts)**

  Se puede probar con `./pruebas/for`

- **Escribe un script para ejecutar el código compilado al lenguaje objetivo. (2 pts)**

  El script se llama `proyecto.sh`

  

