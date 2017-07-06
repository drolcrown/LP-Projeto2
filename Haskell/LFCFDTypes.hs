module LFCFDTypes where 


-- Os tipos validos sao definidos com o tipo algebrico,
-- que pode ser um tipo inteiro, um tipo booleano e um tipo funcao. 
-- O tipo funcao deve expressar tanto o tipo do argumento
-- quanto o tipo do retorno. 
-- As expressoes, conforme mencionado anteriormente,
-- envolvem tanto valores inteiros quanto booleanos,
-- bem como expressoes binarias (soma, subtracao, etc.),
-- expressoes let, lambda, aplicacao de funcoes e {if-then-else}.


type Id = String

type Gamma= [(Id, Tipo)]

data Tipo = TInt
          | TBool
          | TFuncao Tipo Tipo 
    deriving(Show, Eq)

data Expressao = ValorI Int
               | ValorB Bool
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao 
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao 
               | Let Id Expressao Expressao       
               | Ref Id
               | Lambda (Id, Tipo) Tipo Expressao
               | Aplicacao Expressao Expressao
               | If Expressao Expressao Expressao
    deriving(Show, Eq)


verificarTipos :: Expressao -> Gamma -> Maybe Tipo
verificarTipos (ValorI n)                   _ = return TInt
verificarTipos (ValorB b)                   _ = return TBool
verificarTipos (Lambda (v, t1) t2 exp)  gamma = return (TFuncao t1 t2)
verificarTipos (Soma l r)               gamma = expAlgebricas l r gamma
verificarTipos (Subtracao l r)          gamma = expAlgebricas l r gamma
verificarTipos (Multiplicacao l r)      gamma = expAlgebricas l r gamma
verificarTipos (Divisao l r)            gamma = -- retorna Nothing se o denominador da divisão for um valor zero
    if r == (ValorI 0)
        then Nothing
        else expAlgebricas l r gamma

verificarTipos (Ref v)                  gamma = pesquisar v gamma

verificarTipos (Let v e c)              gamma =
    verificarTipos e gamma  >>= \lt ->
    verificarTipos c gamma' >>= \rt ->
    if lt == rt
        then return rt
        else Nothing
    where
        gamma' = incrementaAmb v (verificarTipos e gamma) gamma

verificarTipos (Aplicacao def arg)      gamma =
    case def of
        (Lambda (v, tId) tExp exp) ->
            verificarTipos arg gamma >>= \a ->
                if a == tId
                    then verificarTipos exp gamma'
                    else Nothing
                where
                    gamma' = [(v, tId)]
        otherwise -> error ("Aplicacao de funcao nao anonima")

verificarTipos (If test pass fail)      gamma =
    verificarTipos test gamma >>= \t ->
    if t == TBool
        then verificarTipos pass gamma >>= \p ->
             verificarTipos fail gamma >>= \f ->
             if p == f
                then return p
                else Nothing
        else Nothing


-- Verifica se os tipos equivalem a TInt.
expAlgebricas :: Expressao -> Expressao -> Gamma -> Maybe Tipo
expAlgebricas l r gamma =
    verificarTipos l gamma >>= \lt ->
    verificarTipos r gamma >>= \rt ->
    if lt == TInt && rt == TInt
        then return TInt
        else Nothing


-- Realiza uma pesquisa por uma determinada variaval em um ambiente.
pesquisar :: Id -> Gamma -> Maybe Tipo
pesquisar v [] = error "Variavel nao declarada."
pesquisar v ((i,e):xs)
    | v == i = return e
    | otherwise = pesquisar v xs


-- Incrementa o ambiente.
incrementaAmb :: Id -> Maybe Tipo -> Gamma -> Gamma
incrementaAmb n Nothing  [] = []
incrementaAmb n (Just v) [] = [(n, v)]
incrementaAmb n (Just v) ((i,e):xs)
    | n == i = []
    | otherwise = incrementaAmb n (return v) xs
