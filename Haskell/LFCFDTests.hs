module LFCFDTests where

import LFCFDTypes

import Test.HUnit

v5 = ValorI 5
v4 = ValorI 4
v3 = ValorI 3
v2 = ValorI 2
v1 = ValorI 1
v0 = ValorI 0

vt = ValorB True
vf = ValorB False

amb = [("x", TInt), ("z", TBool), ("y", TInt)]

verSoma = (Soma (v2) (v5)) 

verSub = (Subtracao (v2) (v5)) 

verMult = (Multiplicacao (v2) (v5)) 

verDiv = (Divisao (v2) (v5)) 

verSoma1 = (Soma (v2) (Ref "x")) 

verSub1 = (Subtracao (v2) (Ref "x")) 

verMult1 = (Multiplicacao (v2) (Ref "x")) 

verDiv1 = (Divisao (v2) (Ref "x"))

verDiv2 = (Divisao (v3) (v0))


let1 = Let "x" (ValorI 4) (Soma (Ref "x")(Ref "x"))

let2 = Let "x" (ValorI 4) (Soma (Ref "x")(v2))

let3 = Let "x" (ValorB True) (Ref "x")

let4 = Let "x" (ValorB True) (Soma (ValorI 2) (ValorI 3))

let5 = Let "x" (ValorB True) (Soma (Ref "x") (ValorI 3))

let6 = Let "x" (ValorB True) ((ValorI 3))

let7 = Let "x" (Soma (ValorB True) (ValorI 3)) ((ValorI 3))


ap1 = Aplicacao (Lambda ("x", TInt) (TBool) (ValorB True)) (v5)

ap2 = Aplicacao (Lambda ("x", TInt) (TInt) (ValorB True)) (ValorB True)

ap3 = Aplicacao (Lambda ("x", TInt) (TBool) (ValorB True)) (ValorB True)

ap4 = Aplicacao (Lambda ("x", TBool) (TInt) (ValorB True)) (ValorB True)

ap5 = Aplicacao (Lambda ("x", TInt) (TInt) (v5)) (v5)

ap6 = Aplicacao (Lambda ("x", TBool) (TBool) (Soma (Ref "x")(v5))) (ValorB True)

ap7 = (Aplicacao (Lambda ("x", TBool ) (TInt) (Soma (ValorI 2)(ValorI 4))) (ValorB True)) 
-- Just Int
ap8 = (Aplicacao (Lambda ("x", TInt ) (TInt) (Soma (ValorI 2)(ValorI 4))) (ValorB True)) 
-- Nothing
ap9 = (Aplicacao (Lambda ("x", TBool) (TBool) (Soma (Ref "x")(ValorI 4))) (ValorB True))

ap10 = (Aplicacao (Lambda ("x", TInt) (TBool) (Soma (Ref "x")(ValorI 4))) (v2))

teste1 = TestCase (assertEqual "verificarTipos 5" (Just TInt) (verificarTipos v5 []))

teste2 = TestCase (assertEqual "verificarTipos True" (Just TBool) (verificarTipos vt []))

teste3 = TestCase (assertEqual "verificarTipos let x = 5 in x + 2" (Just TInt) (verificarTipos let2 []))

teste4 = TestCase (assertEqual "verificarTipos let x = True in x" (Just TBool) (verificarTipos let3 []))

teste5 = TestCase (assertEqual "verificarTipos x + 2" (Just TInt) (verificarTipos verSoma1 amb))

teste6 = TestCase (assertEqual "verificarTipos x - 2" (Just TInt) (verificarTipos verSub1 amb))

teste7 = TestCase (assertEqual "verificarTipos x / 2" (Just TInt) (verificarTipos verDiv1 amb))

teste8 = TestCase (assertEqual "verificarTipos x * 2" (Just TInt) (verificarTipos verMult1 amb))

teste9 = TestCase (assertEqual "verificarTipos 5 + 2" (Just TInt) (verificarTipos verSoma []))

teste10 = TestCase (assertEqual "verificarTipos 5 - 2" (Just TInt) (verificarTipos verSub []))

teste11 = TestCase (assertEqual "verificarTipos 5 / 2" (Just TInt) (verificarTipos verDiv []))

teste12 = TestCase (assertEqual "verificarTipos 5 * 2" (Just TInt) (verificarTipos verMult []))

teste13 = TestCase (assertEqual "verificarTipos 3 / 0" (Nothing) (verificarTipos verDiv2 []))

teste14 = TestCase (assertEqual "verificarTipos if true then true else false" (Just TBool) (verificarTipos (If (vt) (vt) (vf)) []))

teste15 = TestCase (assertEqual "verificarTipos if false then true else false" (Just TBool) (verificarTipos (If (vf) (vt) (vf)) []))

teste16 = TestCase (assertEqual "verificarTipos if true then 5 else 10" (Just TInt) (verificarTipos (If (vt) (ValorI 5) (ValorI 10)) []))

teste17 = TestCase (assertEqual "verificarTipos if 2 then true else false" (Nothing) (verificarTipos (If (ValorI 2) (vt) (vf)) []))

teste18 = TestCase (assertEqual "verificarTipos if 2 then 3 else 4" (Nothing) (verificarTipos (If (ValorI 2) (v3) (v4)) []))

teste19 = TestCase (assertEqual "verificarTipos if true then 2 else false" (Nothing) (verificarTipos (If (vt) (v2) (vf)) []))

teste20 = TestCase (assertEqual "verificarTipos if true then true else 5" (Nothing) (verificarTipos (If (vt) (vt) (v5)) []))

teste21 = TestCase (assertEqual "verificarTipos Aplicacao Funcao Tint Tint 5" (Just TBool) (verificarTipos (ap1) []))

teste22 = TestCase (assertEqual "verificarTipos Aplicacao Funcao Tint Tint True" (Nothing) (verificarTipos (ap2) []))

teste23 = TestCase (assertEqual "verificarTipos Aplicacao Funcao Tint TBool True" (Nothing) (verificarTipos (ap3) []))

teste24 = TestCase (assertEqual "verificarTipos Aplicacao Funcao TBool TInt True" (Just TBool) (verificarTipos (ap4) []))

teste25 = TestCase (assertEqual "verificarTipos Aplicacao Funcao TInt TInt 5" (Just TInt) (verificarTipos (ap5) []))

teste26 = TestCase (assertEqual "Soma = True + 5" (Nothing) (verificarTipos (ap6) []))

teste27 = TestCase (assertEqual "Soma 2 + 4 -- Argumento TBool e tipo TInt = TInt" (Just TInt) (verificarTipos (ap7) []))

teste28 = TestCase (assertEqual "Soma 2 + 4 -- Argumento TBool e tipo TInt" (Nothing) (verificarTipos (ap8) []))

teste29 = TestCase (assertEqual "Soma x + 4 --- x = TBool" (Nothing) (verificarTipos (ap9) []))

teste30 = TestCase (assertEqual "Soma x + 4 --- x = TInt" (Just TInt) (verificarTipos (ap10) []))

teste32 = TestCase (assertEqual "Let x = True in 2" (Just TInt) (verificarTipos (let6) []))

todosOsTestes = TestList [ teste1
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
                         , teste13
                         , teste14
                         , teste15
                         , teste16
                         , teste17
                         , teste18
                         , teste19
                         , teste20
                         , teste21
                         , teste22
                         , teste23
                         , teste24
                         , teste25
                         , teste26
                         , teste27
                         , teste28
                         , teste29
                         , teste30
                         , teste32
                         ]

executarTestes = runTestTT todosOsTestes