
module VerificaTipos where 


-- Os tipos validos sao definidos com o tipo algebrico, que pode ser um tipo inteiro, um tipo booleano e um tipo funcao. 
-- O tipo funcao deve expressar tanto o tipo do argumento quanto o tipo do retorno. 
-- As expressoes, conforme mencionado anteriormente, envolvem tanto valores inteiros quanto booleanos,
-- bem como expressoes binarias (soma, subtracao, etc.), expressoes let, lambda, aplicacao de funcoes e {if-then-else}.


type Id = String

type Gamma= [(Id, Tipo)]


data Tipo = TInt | TBool | TFuncao Tipo Tipo 
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
verificarTipos (Divisao l r)            gamma = expAlgebricas l r gamma
verificarTipos (Ref v)                  gamma = pesquisar v gamma
verificarTipos (Let v e c)              gamma = undefined

-- Verifica se os tipos equivalem a TInt.
expAlgebricas :: Expressao -> Expressao -> Gamma -> Maybe Tipo
expAlgebricas l r gamma = verificarTipos l gamma >>= \lt -> verificarTipos r gamma >>= \rt -> if lt == TInt && rt == TInt then return TInt else Nothing


-- Realiza uma pesquisa por uma determinada variaval em um ambiente.
pesquisar :: Id -> Gamma -> Maybe Tipo
pesquisar v [] = error "Variavel nao declarada."
pesquisar v ((i,e):xs)
 | v == i = return e
 | otherwise = pesquisar v xs
