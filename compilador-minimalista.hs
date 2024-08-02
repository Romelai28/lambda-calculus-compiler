-- Compilador minimalista
-- Máquina con tres instrucciones: LDI(n) ADD MUL.
-- Permite sumar y multiplicar enteros.
-- Los únicos valores son Int.

type Codigo = [Instruccion] 
type Pila = [Int]


data Instruccion = LDI Int  -- LDI(n) carga n a la pila
                    | ADD     -- Desapila dos elementos, los suma e inserta el resultado de nuevo en la pila.
                    | MUL     -- Desapila dos elementos, los multiplica e inserta el resultado de nuevo en la pila.
                    deriving Show


data MathExp a = Cte a
                | Suma (MathExp a) (MathExp a)
                | Prod (MathExp a) (MathExp a)
                deriving Show

-- Para no tener que escribir MathExp como árbol.
instance Num (MathExp Int) where
    (+) = Suma
    (*) = Prod
    fromInteger = Cte . fromInteger
    abs = error "abs no está implementado para MathExp"
    signum = error "signum no está implementado para MathExp"
    negate (Cte x) = Cte . negate $ x
    negate _ = error "negate solo está implementado para Cte"
    (-) = error "(-) no está implementado para MathExp"


compile :: MathExp Int -> Codigo
compile (Cte n) = [LDI n]
compile (Suma n m) = compile n ++ compile m ++ [ADD]
compile (Prod n m) = compile n ++ compile m ++ [MUL]


interpretar :: Codigo -> Pila -> Int
interpretar []            [x]      = x
interpretar (LDI n : ins) pi       = interpretar ins (n : pi)
interpretar (ADD   : ins) (x:y:pi) = interpretar ins (x+y : pi)
interpretar (MUL   : ins) (x:y:pi) = interpretar ins (x*y : pi)
interpretar [] _        = error "Error: La pila no tiene un solo elemento"
interpretar (ADD : _) _ = error "Error: No hay suficientes elementos en la pila para ADD"
interpretar (MUL : _) _ = error "Error: No hay suficientes elementos en la pila para MUL"


run :: MathExp Int -> Int
run exp = interpretar (compile exp) []


-- Ejemplos:

-- input: compile $ Suma (Prod (Cte 2)(Cte 3)) (Prod (Cte 4)(Cte 5))    // (2*3)+(4*5)
-- output: [LDI 2,LDI 3,MUL,LDI 4,LDI 5,MUL,ADD]

-- input: run $ Suma (Prod (Cte 2)(Cte 3)) (Prod (Cte 4)(Cte 5))        // (2*3)+(4*5)
-- output: 26

-- input: compile $ (2*3)+(4*5)
-- output: [LDI 2,LDI 3,MUL,LDI 4,LDI 5,MUL,ADD]

-- input: run $ (2*3)+(4*5)
-- output: 26

-- input: compile $ 5*3+2+3*12
-- output: [LDI 5,LDI 3,MUL,LDI 2,ADD,LDI 3,LDI 12,MUL,ADD]

-- input: run $ 5*3+2+3*12
-- output: 53

-- input: compile $ 3+(-2)
-- output: [LDI 3,LDI (-2),ADD]

-- input: run $ 3+(-2)
-- output: 1