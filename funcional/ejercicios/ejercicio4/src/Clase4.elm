module Clase4 exposing (..)

{-| Ejercicios de Programación Funcional - Clase 4
Este módulo contiene ejercicios para practicar pattern matching y mónadas en Elm
usando árboles binarios como estructura de datos principal.

Temas:
- Pattern Matching con tipos algebraicos
- Mónada Maybe para operaciones opcionales
- Mónada Result para manejo de errores
- Composición monádica con andThen
-}
import Fuzz exposing (maybe)
import Html exposing (a)


-- ============================================================================
-- DEFINICIÓN DEL ÁRBOL BINARIO
-- ============================================================================

type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


-- ============================================================================
-- PARTE 0: CONSTRUCCIÓN DE ÁRBOLES
-- ============================================================================


-- 1. Crear Árboles de Ejemplo


arbolVacio : Tree Int
arbolVacio =
    Empty


arbolHoja : Tree Int
arbolHoja =
    Node 5 Empty Empty


arbolPequeno : Tree Int
arbolPequeno =
    Node 3 (Node 1 Empty Empty) (Node 5 Empty Empty)


arbolMediano : Tree Int
arbolMediano =
    Node 10
        (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))
        (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty))


-- 2. Es Vacío


esVacio : Tree a -> Bool
esVacio arbol =
    case arbol of
        Empty -> True
        Node _ _ _ -> False


-- 3. Es Hoja


esHoja : Tree a -> Bool
esHoja arbol =
    case arbol of
        Node _ Empty Empty -> True
        _ -> False


-- ============================================================================
-- PARTE 1: PATTERN MATCHING CON ÁRBOLES
-- ============================================================================


-- 4. Tamaño del Árbol


tamano : Tree a -> Int
tamano arbol =
    case arbol of
        Empty -> 0
        Node _ izq der -> (tamano izq) + (tamano der) + 1


-- 5. Altura del Árbol


altura : Tree a -> Int
altura arbol =
    case arbol of
        Empty -> 0
        Node _ izq der -> (max (altura izq) (altura der)) + 1


-- 6. Suma de Valores


sumarArbol : Tree Int -> Int
sumarArbol arbol =
    case arbol of
        Empty -> 0
        Node valor izq der -> (sumarArbol izq) + (sumarArbol der) + valor


-- 7. Contiene Valor


contiene : a -> Tree a -> Bool
contiene valor arbol =
    case arbol of 
        Empty -> False
        Node v izq der ->  v == valor || (contiene valor izq) || (contiene valor der)


-- 8. Contar Hojas


contarHojas : Tree a -> Int
contarHojas arbol =
    case arbol of
        Empty -> 0
        Node _ Empty Empty -> 1
        Node _ izq der -> (contarHojas izq) + (contarHojas der)



-- 9. Valor Mínimo (sin Maybe)


minimo : Tree Int -> Int
minimo arbol =
    case arbol of
        Empty -> 0
        Node v Empty Empty -> v
        Node v Empty der -> (min v (minimo der))
        Node v izq Empty -> (min v (minimo izq))
        Node v izq der -> (min v (min (minimo izq) (minimo der)))


-- 10. Valor Máximo (sin Maybe)


maximo : Tree Int -> Int
maximo arbol =
    case arbol of
        Empty -> 0
        Node v Empty Empty -> v
        Node v Empty der -> (max v (minimo der))
        Node v izq Empty -> (max v (minimo izq))
        Node v izq der -> (max v (max (maximo izq) (maximo der)))


-- ============================================================================
-- PARTE 2: INTRODUCCIÓN A MAYBE
-- ============================================================================


-- 11. Buscar Valor

buscarEnLista: a -> List a -> Maybe a
buscarEnLista valor lista = 
    case lista of
        [] -> Nothing
        head :: tail ->
            if head == valor then
                Just head
            else
                buscarEnLista valor tail

buscar : a -> Tree a -> Maybe a
buscar valor arbol =
    case arbol of
        Empty -> Nothing
        Node v izq der ->
            if v == valor then
                Just v
            else
                case buscar valor izq of
                    Just encontrado -> Just encontrado
                    Nothing -> buscar valor der

-- 12. Encontrar Mínimo (con Maybe)

encontrarMinimo : Tree comparable -> Maybe comparable
encontrarMinimo arbol =
    case arbol of
        Empty -> Nothing
        Node v Empty Empty -> Just v
        Node v izq der ->
            case (encontrarMinimo izq, encontrarMinimo der) of
                (Nothing, Nothing) -> Just v
                (Just minIzq, Nothing) -> Just (min v minIzq)
                (Nothing, Just minDer) -> Just (min v minDer)
                (Just minIzq, Just minDer) -> Just (min v (min minIzq minDer))


-- 13. Encontrar Máximo (con Maybe)


encontrarMaximo : Tree comparable -> Maybe comparable
encontrarMaximo arbol =
    case arbol of
        Empty -> Nothing
        Node v Empty Empty -> Just v
        Node v izq der ->
            case (encontrarMaximo izq, encontrarMaximo der) of
                (Nothing, Nothing) -> Just v
                (Just maxIzq, Nothing) -> Just (max v maxIzq)
                (Nothing, Just maxDer) -> Just (max v maxDer)
                (Just maxIzq, Just maxDer) -> Just (max v (max maxIzq maxDer))


-- 14. Buscar Por Predicado


buscarPor : (a -> Bool) -> Tree a -> Maybe a
buscarPor predicado arbol =
    case arbol of
        Empty -> Nothing
        Node v izq der ->
            if predicado v then
                Just v
            else
                case buscarPor predicado izq of
                    Just encontrado -> Just encontrado
                    Nothing -> buscarPor predicado der


-- 15. Obtener Valor de Raíz


raiz : Tree a -> Maybe a
raiz arbol =
    case arbol of
        Empty -> Nothing
        Node v _ _ -> Just v


-- 16. Obtener Hijo Izquierdo


hijoIzquierdo : Tree a -> Maybe (Tree a)
hijoIzquierdo arbol =
    case arbol of
        Empty -> Nothing
        Node _ Empty _  -> Nothing
        --Node _ (Node v _ _) _ -> Just v
        --_ -> Nothing
        Node _ izq _ -> Just izq


hijoDerecho : Tree a -> Maybe (Tree a)
hijoDerecho arbol =
    case arbol of
        Empty -> Nothing
        Node _ _ Empty  -> Nothing
        Node _ _ (Node _ _ _ as right_child) -> Just right_child
        --Node _ _ (Node v _ _) -> (Just (Node v _ _))
        --_ -> Nothing


-- 17. Obtener Nieto


nietoIzquierdoIzquierdo : Tree a -> Maybe (Tree a)
nietoIzquierdoIzquierdo arbol =
    case arbol of
        Empty -> Nothing
        Node _ Empty _ -> Nothing
        Node _ (Node _ Empty _) _ -> Nothing
        Node _ (Node _ izq _) _ -> Just izq

--Con andThen
nietoIzquierdoIzquierdo2 : Tree a -> Maybe (Tree a)
nietoIzquierdoIzquierdo2 arbol =
    Maybe.andThen hijoIzquierdo (Just arbol)
        |> Maybe.andThen hijoIzquierdo
        |> Maybe.andThen hijoIzquierdo

-- 18. Buscar en Profundidad


obtenerSubarbol : a -> Tree a -> Maybe (Tree a)
obtenerSubarbol valor arbol =
    case arbol of
        Empty -> Nothing
        --Node _ Empty Empty -> Nothing
        Node v izq der -> if v == valor then Just arbol else
            case obtenerSubarbol valor izq of
                Just encontrado -> Just encontrado
                Nothing -> obtenerSubarbol valor der



buscarEnSubarbol : a -> a -> Tree a -> Maybe a
buscarEnSubarbol valor1 valor2 arbol =
    case arbol of
        Empty -> Nothing
        Node v izq der -> if v == valor1 then
                Just valor1
            else if v == valor2 then
                Just valor2
            else
                case buscarEnSubarbol valor1 valor2 izq of
                    Just encontrado -> 
                        Just encontrado
                    Nothing -> 
                        buscarEnSubarbol valor1 valor2 der


-- ============================================================================
-- PARTE 3: RESULT PARA VALIDACIONES
-- ============================================================================


-- 19. Validar No Vacío


validarNoVacio : Tree a -> Result String (Tree a)
validarNoVacio arbol =
    case arbol of
        Empty -> Err "El árbol está vacío"
        _ -> Ok arbol


-- 20. Obtener Raíz con Error


obtenerRaiz : Tree a -> Result String a
obtenerRaiz arbol =
    case raiz arbol of
        Nothing -> Err "No se puede obtener la raíz de un árbol vacío"
        Just v -> Ok v


-- 21. Dividir en Valor Raíz y Subárboles


dividir : Tree a -> Result String ( a, Tree a, Tree a )
dividir arbol =
    case arbol of
        Empty -> Err "No se puede dividir un árbol vacío"
        Node v izq der -> Ok (v, izq, der)


-- 22. Obtener Mínimo con Error


obtenerMinimo : Tree comparable -> Result String comparable
obtenerMinimo arbol =
    case encontrarMinimo arbol of
        Nothing -> Err "No hay mínimo en un árbol vacío"
        Just m -> Ok m


-- 23. Verificar si es BST


esBST : Tree comparable -> Bool
esBST arbol =
    let
        ok node lower upper =
            case node of
                Empty -> True
                Node v l r ->
                    let
                        validLower =
                            case lower of
                                Nothing -> True
                                Just lb -> lb < v

                        validUpper =
                            case upper of
                                Nothing -> True
                                Just ub -> v < ub
                    in
                    if validLower && validUpper then
                        ok l lower (Just v) && ok r (Just v) upper
                    else
                        False
    in
    ok arbol Nothing Nothing


-- 24. Insertar en BST


insertarBST : comparable -> Tree comparable -> Result String (Tree comparable)
insertarBST valor arbol =
    case arbol of
        Empty -> Ok (Node valor Empty Empty)
        Node v l r ->
            if valor == v then
                Err "El valor ya existe en el árbol"
            else if valor < v then
                case insertarBST valor l of
                    Err e -> Err e
                    Ok nl -> Ok (Node v nl r)
            else
                case insertarBST valor r of
                    Err e -> Err e
                    Ok nr -> Ok (Node v l nr)


-- 25. Buscar en BST


buscarEnBST : comparable -> Tree comparable -> Result String comparable
buscarEnBST valor arbol =
    case arbol of
        Empty -> Err "El valor no se encuentra en el árbol"
        Node v l r ->
            if valor == v then
                Ok v
            else if valor < v then
                buscarEnBST valor l
            else
                buscarEnBST valor r


-- 26. Validar BST con Result


validarBST : Tree comparable -> Result String (Tree comparable)
validarBST arbol =
    if esBST arbol then
        Ok arbol
    else
        Err "El árbol no es un BST válido"


-- ============================================================================
-- PARTE 4: COMBINANDO MAYBE Y RESULT
-- ============================================================================


-- 27. Maybe a Result


maybeAResult : String -> Maybe a -> Result String a
maybeAResult mensajeError maybe =
    case maybe of
        Nothing -> Err mensajeError
        Just v -> Ok v


-- 28. Result a Maybe


resultAMaybe : Result error value -> Maybe value
resultAMaybe result =
    case result of
        Ok v -> Just v
        Err _ -> Nothing


-- 29. Buscar y Validar


buscarPositivo : Int -> Tree Int -> Result String Int
buscarPositivo valor arbol =
    case buscar valor arbol of
        Nothing -> Err "El valor no se encuentra en el árbol"
        Just v -> if v > 0 then Ok v else Err "El valor no es positivo"


-- 30. Pipeline de Validaciones


validarArbol : Tree Int -> Result String (Tree Int)
validarArbol arbol =
    validarNoVacio arbol
        |> Result.andThen validarResult


-- 31. Encadenar Búsquedas


buscarEnDosArboles : Int -> Tree Int -> Tree Int -> Result String Int
buscarEnDosArboles valor arbol1 arbol2 =
    case buscar valor arbol1 of
        Just v -> Ok v
        Nothing ->
            case buscar valor arbol2 of
                Just v2 -> Ok v2
                Nothing -> Err "Búsqueda fallida"


-- ============================================================================
-- PARTE 5: DESAFÍOS AVANZADOS
-- ============================================================================


-- 32. Recorrido Inorder


inorder : Tree a -> List a
inorder arbol =
    case arbol of
        Empty -> []
        Node v l r -> (inorder l) ++ (v :: inorder r)


-- 33. Recorrido Preorder


preorder : Tree a -> List a
preorder arbol =
    case arbol of
        Empty -> []
        Node v l r -> v :: (preorder l ++ preorder r)


-- 34. Recorrido Postorder


postorder : Tree a -> List a
postorder arbol =
    case arbol of
        Empty -> []
        Node v l r -> (postorder l ++ postorder r) ++ [ v ]


-- 35. Map sobre Árbol


mapArbol : (a -> b) -> Tree a -> Tree b
mapArbol funcion arbol =
    case arbol of
        Empty -> Empty
        Node v l r -> Node (funcion v) (mapArbol funcion l) (mapArbol funcion r)


-- 36. Filter sobre Árbol


filterArbol : (a -> Bool) -> Tree a -> Tree a
filterArbol predicado arbol =
    case arbol of
        Empty -> Empty
        Node v l r ->
            let
                fl = filterArbol predicado l
                fr = filterArbol predicado r
            in
            if predicado v then
                Node v fl fr
            else
                case (fl, fr) of
                    (Empty, _) -> fr
                    (_, Empty) -> fl
                    _ ->
                        let
                            attachRightmost tree t =
                                case tree of
                                    Empty -> t
                                    Node x a b -> Node x a (attachRightmost b t)
                        in
                        attachRightmost fl fr


-- 37. Fold sobre Árbol


foldArbol : (a -> b -> b) -> b -> Tree a -> b
foldArbol funcion acumulador arbol =
    case arbol of
        Empty -> acumulador
        Node v l r ->
            let
                afterLeft = foldArbol funcion acumulador l
                afterV = funcion v afterLeft
            in
            foldArbol funcion afterV r


-- 38. Eliminar de BST


eliminarBST : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarBST valor arbol =
    case arbol of
        Empty -> Err "El valor no existe en el árbol"
        Node v l r ->
            if valor < v then
                case eliminarBST valor l of
                    Err e -> Err e
                    Ok nl -> Ok (Node v nl r)
            else if valor > v then
                case eliminarBST valor r of
                    Err e -> Err e
                    Ok nr -> Ok (Node v l nr)
            else
                case (l, r) of
                    (Empty, _) -> Ok r
                    (_, Empty) -> Ok l
                    _ ->
                        case encontrarMinimo r of
                            Nothing -> Ok l
                            Just minR ->
                                case eliminarBST minR r of
                                    Err e -> Err e
                                    Ok newR -> Ok (Node minR l newR)


-- 39. Construir BST desde Lista


desdeListaBST : List comparable -> Result String (Tree comparable)
desdeListaBST lista =
    List.foldl
        (\nuevo acc ->
            case acc of
                Err e -> Err e
                Ok tree -> insertarBST nuevo tree
        )
        (Ok Empty)
        lista


-- 40. Verificar Balance


estaBalanceado : Tree a -> Bool
estaBalanceado arbol =
    let
        alturaYBalanceado node =
            case node of
                Empty -> (0, True)
                Node _ l r ->
                    let
                        (hl, bl) = alturaYBalanceado l
                        (hr, br) = alturaYBalanceado r
                        h = (max hl hr) + 1
                        balanced = bl && br && (abs (hl - hr) <= 1)
                    in
                    (h, balanced)
    in
    case alturaYBalanceado arbol of
        (_, b) -> b


-- 41. Balancear BST


balancear : Tree comparable -> Tree comparable
balancear arbol =
    let
        lista = inorder arbol
        fromSorted lst =
            case lst of
                [] -> Empty
                _ ->
                    let
                        n = List.length lst
                        mid = n // 2
                        left = List.take mid lst
                        right = List.drop (mid + 1) lst
                        root = List.head (List.drop mid lst)
                    in
                    case root of
                        Nothing -> Empty
                        Just r -> Node r (fromSorted left) (fromSorted right)
    in
    fromSorted lista


-- 42. Camino a un Valor


type Direccion
    = Izquierda
    | Derecha


encontrarCamino : a -> Tree a -> Result String (List Direccion)
encontrarCamino valor arbol =
    let
        helper node =
            case node of
                Empty -> Err "El valor no existe en el árbol"
                Node v l r ->
                    if v == valor then
                        Ok []
                    else
                        case helper l of
                            Ok dirs -> Ok (Izquierda :: dirs)
                            Err _ ->
                                case helper r of
                                    Ok dirs2 -> Ok (Derecha :: dirs2)
                                    Err e -> Err e
    in
    helper arbol


-- 43. Seguir Camino


seguirCamino : List Direccion -> Tree a -> Result String a
seguirCamino camino arbol =
    case camino of
        [] ->
            case arbol of
                Empty -> Err "Camino inválido"
                Node v _ _ -> Ok v
        dir :: rest ->
            case arbol of
                Empty -> Err "Camino inválido"
                Node _ l r ->
                    case dir of
                        Izquierda -> seguirCamino rest l
                        Derecha -> seguirCamino rest r


-- 44. Ancestro Común Más Cercano


ancestroComun : comparable -> comparable -> Tree comparable -> Result String comparable
ancestroComun _ _ _ =
    Err "Uno o ambos valores no existen en el árbol"


-- ============================================================================
-- PARTE 6: DESAFÍO FINAL - SISTEMA COMPLETO
-- ============================================================================


-- 45. Sistema Completo de BST
-- (Las funciones individuales ya están definidas arriba)


-- Operaciones que retornan Bool
esBSTValido : Tree comparable -> Bool
esBSTValido arbol =
    esBST arbol


estaBalanceadoCompleto : Tree comparable -> Bool
estaBalanceadoCompleto arbol =
    estaBalanceado arbol


contieneValor : comparable -> Tree comparable -> Bool
contieneValor valor arbol =
    contiene valor arbol


-- Operaciones que retornan Maybe
buscarMaybe : comparable -> Tree comparable -> Maybe comparable
buscarMaybe valor arbol =
    buscar valor arbol


encontrarMinimoMaybe : Tree comparable -> Maybe comparable
encontrarMinimoMaybe arbol =
    encontrarMinimo arbol


encontrarMaximoMaybe : Tree comparable -> Maybe comparable
encontrarMaximoMaybe arbol =
    encontrarMaximo arbol


-- Operaciones que retornan Result
insertarResult : comparable -> Tree comparable -> Result String (Tree comparable)
insertarResult valor arbol =
    insertarBST valor arbol


eliminarResult : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarResult valor arbol =
    eliminarBST valor arbol


validarResult : Tree comparable -> Result String (Tree comparable)
validarResult arbol =
    validarBST arbol


obtenerEnPosicion : Int -> Tree comparable -> Result String comparable
obtenerEnPosicion posicion arbol =
    let lista = inorder arbol in
    if posicion < 0 || posicion >= List.length lista then
        Err "Posición inválida"
    else
        case List.head (List.drop posicion lista) of
            Nothing -> Err "Posición inválida"
            Just v -> Ok v


-- Operaciones de transformación
map : (a -> b) -> Tree a -> Tree b
map funcion arbol =
    mapArbol funcion arbol


filter : (a -> Bool) -> Tree a -> Tree a
filter predicado arbol =
    filterArbol predicado arbol


fold : (a -> b -> b) -> b -> Tree a -> b
fold funcion acumulador arbol =
    foldArbol funcion acumulador arbol


-- Conversiones
aLista : Tree a -> List a
aLista arbol =
    inorder arbol


desdeListaBalanceada : List comparable -> Tree comparable
desdeListaBalanceada lista =
    let
        fromList lst =
            case lst of
                [] -> Empty
                _ ->
                    let n = List.length lst
                        mid = n // 2
                        left = List.take mid lst
                        right = List.drop (mid + 1) lst
                        root = List.head (List.drop mid lst)
                    in
                    case root of
                        Nothing -> Empty
                        Just r -> Node r (fromList left) (fromList right)
    in
    fromList lista
