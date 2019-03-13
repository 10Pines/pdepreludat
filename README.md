# PdePreludat

Pdepreludat es una biblioteca que busca hacer más amigable y didáctico al Prelude de haskell, por ejemplo mejorando ciertos mensajes de error.

La biblioteca exporta un modulo que contiene la mayoría de las funciones existentes en el Prelude, con ciertas modificaciones:
- Fue redefinido el tipo de las funciones que trabajan sobre `Foldable t` para que trabajen sobre `[]`

| Antes | Después |
|-------|---------|
|`> :t length`<br>`length :: Foldable t => t a -> Int`|`> :t length`<br>`length :: [a] -> Int`|

 - Se definió una instancia de `Show` para las funciones
 
| Antes | Después |
|-------|---------|
|`> filter`<br>`<interactive>:2:1: error:`<br>`• No instance for (Show ((a0 -> Bool) -> [a0] -> [a0]))`<br>`arising from a use of ‘print’`<br>`...más errores`|`> filter`<br>`<una función>`|
 
 - Se reemplazó la división entre `Fractional`s (/) para que no sea un error de tipos dividir `Integral`s con `Fractional`s

| Antes | Después |
|-------|---------|
|`> sum [1,2] / length [1,2]`<br>`<interactive>:3:1: error:`<br>`• No instance for (Fractional Int) arising from a use of ‘/’`<br>`• In the expression: sum [1, 2] / length [1, 2]`<br>`...más errores`|`> sum [1,2] / length [1,2]`<br>`1.5`|

- Se modificaron los mensajes de error que aparecen al usar funciones o listas donde se esperaba que haya un número.

| Antes | Después |
|-------|---------|
|`> sum + 5`<br>`<interactive>:9:1: error:`<br>`• Non type-variable argument in the constraint: Num (t a -> a)`<br>`(Use FlexibleContexts to permit this)`<br>`• When checking the inferred type`<br>`...más errores`|`> sum + 5`<br>`<interactive>:9:1: error:`<br>`• Estás usando una función como un número`<br>`• In the expression: sum + 5`<br>`In an equation for ‘it’: it = sum + 5`|



## Para usarlo en un nuevo proyecto

### Instalar stack
Si estás usando Linux, podés correr el siguiente comando:
```
curl -sSL https://get.haskellstack.org/ | sh
```

Si estás usando Windows, podés descargarlo haciendo click [acá](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

Para más información podés ir a la [página oficial de stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

### Preparar proyecto

Correr el siguiente comando que va a crear una carpeta llamada `projecto-test` (o el nombre que le hayan pasado `stack new`) con el proyecto adentro.

```
stack new projecto-test https://raw.githubusercontent.com/10Pines/pdepreludat/master/pdepreludat.hsfiles
```

Una vez creada la carpeta, moverse a la misma y compilar el proyecto con los siguientes comandos:

```
cd projecto-test
stack build
```

### Probarlo

Ahora ya debería haber descargado e instalado todo lo necesario para funcionar, para poder correr el interprete, ejecutar:

```
stack ghci
```

También, se pueden correr los tests usando:

```
stack test
```
