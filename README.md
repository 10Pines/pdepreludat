# PdePreludat

Pdepreludat es una biblioteca que busca hacer más amigable y didáctico al Prelude de haskell, por ejemplo mejorando ciertos mensajes de error.

La biblioteca exporta un modulo que contiene la mayoría de las funciones existentes en el Prelude, con ciertas modificaciones:

## Redefinición Foldable => []

Fue redefinido el tipo de las funciones que trabajan sobre `Foldable t` para que trabajen sobre `[]`

### Antes

```haskell
> :t length
length :: Foldable t => t a -> Int
```

### Después

```haskell
> :t length
length :: [a] -> Int
```

## Mostrar funciones como valor en la consola

Se definió una instancia de `Show` para las funciones

### Antes

```haskell
> filter
<interactive>:2:1: error:
    No instance for (Show ((a0 -> Bool) -> [a0] -> [a0]))
    arising from a use of ‘print’
    // ...más errores
```

### Después

```haskell
> filter
<una función>
```

## División entre fracciones y enteros

Se reemplazó la división entre `Fractional`s (/) para que no sea un error de tipos dividir `Integral`s con `Fractional`s

### Antes

```haskell
> sum [1,2] / length [1,2]
<interactive>:3:1: error:
    No instance for (Fractional Int) arising from a use of ‘/’
    In the expression: sum [1, 2] / length [1, 2]
    // ...más errores
```

### Después

```haskell
> sum [1,2] / length [1,2]
1.5
```

## Mensajes de error al operar funciones o listas

Se modificaron los mensajes de error que aparecen al usar funciones o listas donde se esperaba que haya un número.

### Antes 

```haskell
> sum + 5
<interactive>:9:1: error:
    Non type-variable argument in the constraint: Num (t a -> a)
    (Use FlexibleContexts to permit this)
    When checking the inferred type
    //...más errores
```

### Después

```haskell
> sum + 5
<interactive>:9:1: error:
    Estás usando una función como un número
    In the expression: sum + 5
    In an equation for ‘it’: it = sum + 5
```

## Para usarlo en un nuevo proyecto

### Instalar stack

Si estás usando Linux, podés correr el siguiente comando:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Si estás usando Windows, podés descargarlo haciendo click [acá](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

Para más información podés ir a la [página oficial de stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

### Preparar proyecto

Correr el siguiente comando que va a crear una carpeta llamada `proyecto-test` (o el nombre que le hayan pasado `stack new`) con el proyecto adentro.

```bash
stack new proyecto-test http://bit.ly/preludat
```

Una vez creada la carpeta, moverse a la misma y compilar el proyecto con los siguientes comandos:

```bash
cd proyecto-test
stack build
```

### Probarlo

Ahora ya debería haber descargado e instalado todo lo necesario para funcionar, para poder correr el interprete, ejecutar:

```bash
stack ghci
```

También, se pueden correr los tests usando:

```bash
stack test
```
