# PdePreludat

[![build](https://github.com/10Pines/pdepreludat/actions/workflows/build.yml/badge.svg)](https://github.com/10Pines/pdepreludat/actions/workflows/build.yml)

Pdepreludat es una biblioteca que busca hacer más amigable y didáctico al Prelude de haskell, por ejemplo mejorando ciertos mensajes de error.

La biblioteca exporta un modulo que contiene la mayoría de las funciones existentes en el Prelude, con ciertas modificaciones:

## Simplificado tipo de funciones que recibían Foldable para que usen []

- Fue redefinido el tipo de las funciones que trabajan sobre `Foldable t` para que trabajen sobre `[]`

### Antes

```haskell
> :t any
any :: Foldable t =>  (a -> Bool) -> t a -> Bool
```

### Después

```haskell
> :t any
any :: (a -> Bool) -> [a] -> Bool
```

## Reemplazada jerarquía de tipos de la typeclass Num por un único tipo Number

A efectos prácticos, el tipo Number es lo mismo que un Double, y todas las funciones que usaban cualquier otro tipo de número o algún tipo númerico más general fueron redefinidas para que trabajen con Number.
Este es el cambio más disruptivo o que rompe más la compatibilidad con el resto del mundo de Haskell.

La idea es que ayude en:

### Hacer más simples algunos tipos que se pueden encontrar desde el principio de la cursada

#### Antes

```haskell
> :t 5
5 :: Num p => p
```

#### Después

```haskell
> :t 5
5 :: Number
```

### Simplificar algunos errores

#### Antes 

```haskell
> sum + 5
<interactive>:9:1: error:
    Non type-variable argument in the constraint: Num (t a -> a)
    (Use FlexibleContexts to permit this)
    When checking the inferred type
    //...más errores
```

#### Después

```haskell
> sum + 5
<interactive>:9:1: error:
    • Couldn't match expected type ‘Number’
                  with actual type ‘[Number] -> Number’
    • Probable cause: ‘sum’ is applied to too few arguments
    //...más errores
```

### Hacer innecesarias ciertas conversiones de números:

#### Antes

```haskell
> sum [1,2,3,4] / length [1,2,3,4]
<interactive>:7:1: error:
    • No instance for (Fractional Int) arising from a use of ‘/’
    • In the expression: sum [1, 2, 3, 4] / length [1, 2, 3, 4]
      In an equation for ‘it’:
          it = sum [1, 2, 3, ....] / length [1, 2, 3, ....]
```

#### Después

```haskell
> sum [1,2,3,4] / length [1,2,3,4]
2.5
```

### El trade off más grande que hubo que hacer para permitir esas cosas, es que muchos errores en tiempo de compilación se movieron a tiempo de ejecución, es decir, los números ahora son menos _type safe_.

#### Antes

Esto ni siquiera tipa:

```haskell
> :t take 2.5 [1,2,3,4]

<interactive>:1:6: error:
    • Could not deduce (Fractional Int) arising from the literal ‘2.5’
      from the context: Num a
        bound by the inferred type of it :: Num a => [a]
        at <interactive>:1:1
```

#### Después

Esto tipa

```haskell
> :t take 2.5 [1,2,3,4]
take 2.5 [1,2,3,4] :: [Number]
```

Pero falla en ejecución

```haskell
> take 2.5 [1,2,3,4]
*** Exception: Se esperaba un valor entero pero se pasó uno con decimales
CallStack (from HasCallStack):
  error, called at /home/juan/Development/Proyectos/10pines/pdepreludat/src/Number.hs:11:39 in main:Number
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

## Operaciones entre fracciones y enteros

Se agregó la función `toFloat` para convertir enteros a decimales, ya que creemos que `fromIntegral` puede ser un poco confuso de usar, y se agregaron mejores mensajes de error a las operaciones donde se esperaba un decimal y llegó un entero o viceversa.

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
<interactive>:1:1: error:
    • Estás operando enteros con fraccionales, que son diferentes tipos. Podés convertir el entero en decimal usando toFloat/1 o el decimal en entero usando round/1, floor/1 o ceiling/1.
    • In the expression: sum [1, 2] / length [1, 2]
      In an equation for ‘it’: it = sum [1, 2] / length [1, 2]
```

## Mensajes de error si se trata de usar una función como un comparable o equiparable

### Antes

```haskell
> filter == map

<interactive>:1:1: error:
    • No instance for (Eq ((Bool -> Bool) -> [Bool] -> [Bool]))
        arising from a use of ‘==’
        (maybe you haven't applied a function to enough arguments?)
    • In the expression: filter == map
      In an equation for ‘it’: it = filter == map
```

### Después

```haskell
> filter == map

<interactive>:1:1: error:
    • Las funciones no se pueden ordenar ni comparar.
    • In the expression: filter == map
      In an equation for ‘it’: it = filter == map
```

## Agregada documentación en español de varias funciones

![Ejemplo en el que al pasar el mouse sobre funciones del PdePreludat se ve la documentacion de las mismas](https://user-images.githubusercontent.com/11432672/113488290-6923f680-9493-11eb-8728-ea5fc12b3a28.gif)


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
stack new proyecto-test https://github.com/10Pines/pdepreludat/releases/download/2.1.6/pdepreludat.hsfiles
```

Una vez creada la carpeta, moverse a la misma y compilar el proyecto con los siguientes comandos:

```bash
cd proyecto-test
stack build --test
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

### Para el desarrollador

Podés ver la [wiki](https://github.com/10Pines/pdepreludat/wiki)

