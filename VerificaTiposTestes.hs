module VerificaTiposTestes where

import VerificaTipos

import Test.HUnit

v5 = ValorI 5
v2 = ValorI 2

vt = ValorB True
vf = ValorB False

amb = [("x", TInt), ("z", TBool)]

verSoma = (Soma (v2)(v5)) 

verSub = (Subtracao (v2)(v5)) 

verMult = (Multiplicacao (v2)(v5)) 

verDiv = (Divisao (v2)(v5)) 

verSoma1 = (Soma (v2)(Ref "x")) 

verSub1 = (Subtracao (v2)(Ref "x")) 

verMult1 = (Multiplicacao (v2)(Ref "x")) 

verDiv1 = (Divisao (v2)(Ref "x")) 


let1 = Let "x" (ValorI 4) (Soma (Ref "x")(Ref "x"))

let2 = Let "x" (ValorI 4) (Soma (Ref "x")(v2))

let3 = Let "x" (ValorB True) (Ref "x")

let4 = Let "x" (ValorB True) (Soma (ValorI 2) (ValorI 3))

let5 = Let "x" (ValorB True) (Soma (Ref "x") (ValorI 3))



teste = TestCase (assertEqual "verificarTipos 5" (Just TInt) (verificarTipos v5 []))

teste1 = TestCase (assertEqual "verificarTipos True" (Just TBool) (verificarTipos vt []))

teste2 = TestCase (assertEqual "verificarTipos let x = 5 in x + 2" (Just TInt) (verificarTipos let2 []))

teste3 = TestCase (assertEqual "verificarTipos let x = 5 in x + x" (Just TInt) (verificarTipos let1 amb))

teste4 = TestCase (assertEqual "verificarTipos let x = True in x" (Just TBool) (verificarTipos let3 []))

teste5 = TestCase (assertEqual "verificarTipos x + 2" (Just TInt) (verificarTipos verSoma1 amb))

teste6 = TestCase (assertEqual "verificarTipos x - 2" (Just TInt) (verificarTipos verSub1 amb))

teste7 = TestCase (assertEqual "verificarTipos x / 2" (Just TInt) (verificarTipos verDiv1 amb))

teste8 = TestCase (assertEqual "verificarTipos x * 2" (Just TInt) (verificarTipos verMult1 amb))

teste9 = TestCase (assertEqual "verificarTipos 5 + 2" (Just TInt) (verificarTipos verSoma []))

teste10 = TestCase (assertEqual "verificarTipos 5 - 2" (Just TInt) (verificarTipos verSub []))

teste11 = TestCase (assertEqual "verificarTipos 5 / 2" (Just TInt) (verificarTipos verDiv []))

teste12 = TestCase (assertEqual "verificarTipos 5 * 2" (Just TInt) (verificarTipos verMult []))

todosOsTestes = TestList [ teste
                         , teste1
                         , teste2
                         , teste3
                         , teste4
                         , teste5
                         , teste6
                         , teste7
                         , teste8
                         , teste9
                         , teste10
                         , teste11
                         , teste12
                         ]

executarTestes = runTestTT todosOsTestes

