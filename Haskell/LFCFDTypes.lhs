\documentclass[12pt]{article}

%include lhs2TeX.fmt

\usepackage[utf8]{inputenc}
\usepackage[brazil]{babel}
\usepackage{color}
\usepackage{fontenc}
\usepackage{biblatex}
\usepackage{csquotes}
\usepackage{verbatim}
\usepackage{listings}
\usepackage{busproofs}
\usepackage{amsfonts}
\usepackage[nottoc]{tocbibind}

\addbibresource{references.bib}

\title{Implementa\c c\~{a}o de Verifica\c c\~{a}o de Tipos em Haskell}

\author{Luisa Sinzker Fantin, 14/0151893\\
        Jo\~{a}o Pedro Silva Sousa, 15/0038381\\
        Rafael Oliveira de Souza, 15/0081537\\
}

\begin{document}

\maketitle

\section{Introdu\c c\~{a}o}

Esse documento apresenta uma implementa\c c\~{a}o,
em \emph{literate Haskell}, do mecanismo de verifica\c c\~{a}o
de tipos de uma linguagem de programa\c c\~{a}o funcional
minimalista.

O objetivo da implementa\c c\~{a}o de tipos consiste em identificar
erros a n\'{i}vel sint\'{a}tico (n\~{a}o erros de sintaxe) na linguagem,
utilizando o interpretador. Algumas computa\c c\~{o}es inv\'{a}lidas podem
ser identificadas antes mesmo de execut\'{a}-las, como por exemplo uma
divis\~{a}o por zero.

Caso tent\'{a}ssemos avaliar uma divis\~{a}o por zero no interpretador,
seria obtida a sa\'{i}a \emph{infinity}, resultado de uma computa\c c\~{a}o
realizada pelo interpretador da linguagem Haskell, n\~{a}o do interpretador
constru\'{i}do. Ou seja, se esper\'{a}ssemos um erro e o GHCi n\~{a}o o
identificasse, ou o GHCi marcasse um erro que n\~{a}o estamos esperando,
o interpretador n\~{a}o seria confi\'{a}vel \cite{Shriram}.


\section{Vis\~{a}o geral da linguagem}

A linguagem LFCF suporta tanto express\~{o}es
identificadas (LET) quanto identificadores e fun\c c\~{o}es de alta ordem
(com o mecanismo de expressoes lambda). O foco \'{e} na
verifica\c c\~{a}o de tipos, ent\~{a}o n\~{a}o est\~{a}o
implementadas fun\c c\~{o}es voltadas para a avalia\c c\~{a}o
das express\~{o}es. 


\section{Defini\c c\~{a}o da \'{A}rvore Sint\'{a}tica Abstrata}

A implementa\c c\~{a}o consiste na defini\c c\~{a}o de
um m\'{o}dulo Haskell mais alguns tipos auxiliares, como
\texttt{Id} (um tipo sin\^{o}nimo para uma \texttt{string}) e
\texttt{Gamma}, que corresponde a um mapeamento de
identificadores em tipos. 

\begin{code}
module LFCFDTypes where

type Id = String

type Gamma= [(Id, Tipo)]

\end{code}

Os tipos v\'{a}lidos s\~{a}o definidos com o
tipo alg\'{e}brico \texttt{Tipo}, que pode
ser um tipo inteiro, um tipo booleano
e um tipo fun\c c\~{a}o. O tipo fun\c c\~{a}o
deve expressar tanto o tipo do argumento quanto
o tipo do retorno. As express\~{o}es, conforme
mencionado anteriormente, envolvem tanto
valores inteiros quanto booleanos, bem
como express\~{o}es bin\'{a}rias (soma,
subtra\c c\~{a}o, etc.), express\~{o}es
\texttt{let}, \texttt{lambda}, aplica\c c\~{a}o
de fun\c c\~{o}es e \texttt{if-then-else} 

\begin{code}

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

\end{code}

A fun\c c\~{a}o que realiza a verifica\c c\~{a}o
de tipos recebe uma express\~{a}o, um ambiente
\texttt{Gamma} e possivelmente retorna um tipo
v\'{a}lido (por isso o retorno \texttt{Maybe Tipo}).
Caso o tipo verificado pelo interpretador seja um tipo
\texttt{a} v\'{a}lido, \'{e} retornado um \texttt{Just a},
caso algum erro ocorra no sistema de tipos,
essa fun\c c\~{a}o deve retornar \texttt{Nothing}.
Isso permite o uso de uma nota\c c\~{a}o
baseada em monadas. 

\begin{code} 

verificarTipos :: Expressao -> Gamma -> Maybe Tipo

\end{code}

Para alguns casos, a verifica\c c\~{a}o de tipos
\'{e} bem trivial, particularmente a verifica\c c\~{a}o
de tipos de express\~{o}es envolvendo valores inteiros,
valores booleanos e express\~{o}es \texttt{lambda}.

\begin{code}

verificarTipos (ValorI n) _ = return TInt

verificarTipos (ValorB b) _ = return TBool

verificarTipos (Lambda (v, t1) t2 exp) g = return (TFuncao t1 t2)

\end{code}

Os casos mostrados s\~{a}o a base dos tipos da linguagem, pois
todas as constru\c c\~{o}es devem ser reduzidas a um dos casos triviais.

Para outros casos, a verifica\c c\~{a}o de tipos
requer um certo grau de indu\c c\~{a}o (seguindo as
regras de deriva\c c\~{a}o vistas em sala de aula). Para
a soma, temos a seguinte regra de deriva\c c\~{a}o: 

\begin{prooftree}
    \AxiomC{$\Gamma\vdash lhs : \texttt{TInt}$}
    \AxiomC{$\Gamma\vdash rhs : \texttt{TInt}$}
    \BinaryInfC{$\Gamma\vdash soma(lhs, rhs) : \texttt{TInt}$}
\end{prooftree}

que pode ser traduzida para Haskell como:

\begin{code}

verificarTipos (Soma l r) gamma  =
    verificarTipos l gamma >>= \lt ->
    verificarTipos r gamma >>= \rt ->
    if lt == TInt && rt == TInt
        then return TInt
        else Nothing

\end{code}

Analogamente, as opera\c c\~{o}es de subtra\c c\~{a}o, multiplica\c c\~{a}o
e divis\~{a}o possuem \'{a}rvores de deriva\c c\~{a}o parecidas:

\begin{itemize}

    \item   Subtra\c c\~{a}o:
        \begin{prooftree}
            \AxiomC{$\Gamma\vdash lhs : \texttt{TInt}$}
            \AxiomC{$\Gamma\vdash rhs : \texttt{TInt}$}
            \BinaryInfC{$\Gamma\vdash subtracao(lhs, rhs) : \texttt{TInt}$}
        \end{prooftree}

    \item   Multiplica\c c\~{a}o:
        \begin{prooftree}
            \AxiomC{$\Gamma\vdash lhs : \texttt{TInt}$}
            \AxiomC{$\Gamma\vdash rhs : \texttt{TInt}$}
            \BinaryInfC{$\Gamma\vdash multiplicacao(lhs, rhs) : \texttt{TInt}$}
        \end{prooftree}

\end{itemize}

Apenas uma mudan\c ca na verifica\c c\~{a}o de tipos da divis\~{a}o:
caso seja identificado um denominador nulo (igual a zero) na divis\~{a}o,
retorna-se \texttt{Nothing}, caso contr\'{a}rio, avalia-se o tipo das
express\~{o}es alg\'{e}bricas normalmente.

\begin{prooftree}
    \AxiomC{$\Gamma\vdash lhs : \texttt{TInt}$}
    \AxiomC{$\Gamma\vdash rhs : \texttt{TInt}$}
    \AxiomC{$rhs \rightarrow \neg (ValorI 0)$}
    \TrinaryInfC{$\Gamma\vdash divisao(lhs, rhs) : \texttt{TInt}$}
\end{prooftree}

Em Haskell:

\begin{code}

verificarTipos (Divisao l r) gamma =
    if r == (ValorI 0)
        then Nothing
        else verificarTipos l gamma >>= \lt ->
             verificarTipos r gamma >>= \rt ->
                if lt == TInt && rt == TInt
                    then return TInt
                    else Nothing

\end{code}

Similarmente, a verifica\c c\~{a}o de express\~{o}es do tipo
\texttt{let} requer um grau de indu\c c\~{a}o. Supondo
uma express\~{a}o \texttt{let v = e in c}, primeiro verificamos
o tipo da express\~{a}o nomeada (\texttt{e}) \'{e} bem tipada com
tipo \texttt{t},
adicionamos uma associa\c c\~{a}o \texttt{(v, t)} no
ambiente \texttt{Gamma} original e computamos o tipo de
\texttt{c} no novo ambiente. Em termos de regras de deriva\c c\~{a}o,
teremos:

\begin{prooftree}
\AxiomC{$\Gamma\vdash e : \tau_1$}
\AxiomC{$(x,\tau_1)\Gamma\vdash c : \tau_2$}
\BinaryInfC{$\Gamma\vdash let(v,e,c) : \tau_2$}
\end{prooftree}

Em Haskell:

\begin{code}

verificarTipos (Let v e c) gamma =
    verificarTipos e gamma  >>= \lt ->
    verificarTipos c gamma' >>= \rt ->
        if lt == rt
            then return rt
            else Nothing
    where
        gamma' = incrementaAmb v (verificarTipos e gamma) gamma

\end{code}

A fun\c c\~{a}o \texttt{incrementaAmb} enriquece o ambiente na mudan\c ca
de escopo das express\~{o}es \texttt{Let}.


A verifica\c c\~{a}o do tipo de uma refer\^{e}ncia \'{e} bastante
simples, requer apenas pesquisar o identificador no ambiente de
mapeamento $\Gamma$, para isso, utilizamos a fun\c c\~{a}o pesquisar:

\begin{code}

verificarTipos (Ref v) gamma = pesquisar v gamma

\end{code}

Caso o identificador n\~{a}o seja encontrado no ambiente de
mapeamento, \'{e} retornado um erro.

Uma express\~{a}o \texttt{If Expressao Expressao Expressao} \'{e}
formada por tr\^{e}s partes: a condi\c c\~{a}o de avalia\c c\~{a}o;
uma cl\'{a}usula \texttt{then} e uma cl\'{a}usula \texttt{else}.
A cl\'{a}usula \texttt{then} \'{e} executada se a express\~{a}o de
condi\c c\~{a}o for avaliada verdadeira e, a cl\'{a}usula \texttt{else},
caso contr\'{a}rio.
A determina\c c\~{a}o do tipo de uma express\~{a}o \texttt{If} \'{e}
um processo em duas etapas:

\begin{enumerate}
    \item Verificar se o tipo da express\~{a}o de condi\c c\~{a}o \'{e}
    um booleano (\texttt{TBool});
    \item Verificar se os tipos das express\~{o}es associadas \`{a}s
    cl\'{a}usulas \texttt{then} e \texttt{else} s\~{a}o iguais.
\end{enumerate}

O interpretador ser\'{a} como apresentado:

\begin{code}

verificarTipos (If test pass fail) gamma =
    verificarTipos test gamma >>= \t ->
    if t == TBool
        then verificarTipos pass gamma >>= \p ->
             verificarTipos fail gamma >>= \f ->
             if p == f
                then return p
                else Nothing
        else Nothing

\end{code}

Conclui-se ent\~{a}o que, a regra para definir uma express\~{a}o
\texttt{If} \'{e}:

\begin{prooftree}
    \AxiomC{$\Gamma\vdash cond : \texttt{boolean}$}
    \AxiomC{$\Gamma\vdash then : \tau$}
    \AxiomC{$\Gamma\vdash else : \tau$}
    \TrinaryInfC{$\Gamma\vdash \{\texttt{if}$ $cond$ $then$ $else\} : \tau$}
\end{prooftree}

A necessidade das cl\'{a}usulas \texttt{then} e \texttt{else} possuirem
o mesmo tipo (no caso da linguagem pode ser inteiro ou booleano) permite
que possamos atribuir um tipo espec\'{i}fico para express\~{o}es \texttt{If}
e que n\~{a}o dependem de uma avalia\c c\~{a}o em tempo de execu\c c\~{a}o.


Para definir o tipo de uma aplica\c c\~{a}o de fun\c c\~{a}o, ser\~{a}o
necess\'{a}rias mais verifica\c c\~{o}es. Uma aplica\c c\~{a}o \'{e}
definida como \texttt{Aplicacao Expressao Expressao}, na qual a primeira
express\~{a}o \'{e} a defini\c c\~{a}o da fun\c c\~{a}o e a segunda
representa o argumento. O processo de verifica\c c\~{a} para uma
\texttt{Aplicacao def arg} \'{e} o seguinte:

\begin{enumerate}

    \item Verificar se a express\~{a}o que define a fun\c c\~{a}o
    (\texttt{def}) \'{e} uma express\~{a}o \textit{lambda}; caso seja,
    ela automaticamente possui o tipo \texttt{TFuncao tId tExp};
    \item Verificar o tipo do argumento passado para a aplica\c c\~{a}o
    (\texttt{arg}) no contexto $\Gamma$ (denominado \texttt{tArg});
    \item Comparar \texttt{tArg} com \texttt{tId}:
    \begin{itemize}
        \item Se forem iguais, ent\~{a}o o par\^{a}metro passado na
        aplica\c c\~{a}o pode ser associado ao identificador da
        express\~{a}o \textit{lambda};
        \item Se forem diferentes, ent\~{a}o o tipo do par\^{a}metro
        difere do tipo do argumento e pode ser retornado \texttt{Nothing};
    \end{itemize}
    \item O tipo da aplica\c c\~{a}o ser\'{a} o tipo da express\~{a}o
    associada na express\~{a}o \textit{lambda} verificado no ambiente
    $\Gamma$' composto pela tupla \texttt{(id, tipo\_do\_id)};

\end{enumerate}

Em Haskell:

\begin{code}

verificarTipos (Aplicacao def arg) gamma =
    case def of
        (Lambda (v, tId) tExp exp) ->
            verificarTipos arg gamma >>= \a ->
                if a == tId
                    then verificarTipos exp gamma'
                    else Nothing
                where
                    gamma' = [(v, tId)]
        otherwise -> error ("Aplicacao de funcao nao anonima")

\end{code}

Conclui-se ent\~{a}o que a regra que define uma aplica\c c\~{a}o de
fun\c c\~{a}o pode ser representada pela \'{a}rvore a seguir:

\begin{prooftree}
    \AxiomC{$\Gamma\vdash def : (\texttt{TFuncao}$ $\tau_{1}$ $\tau_{2})$}
    \AxiomC{$\Gamma\vdash arg : \tau_{1}$}
    \BinaryInfC{$\Gamma\vdash \{$\texttt{Aplicacao }$def$ $arg\} : \tau_{2}$}
\end{prooftree}

\begin{code}

incrementaAmb :: Id -> Maybe Tipo -> Gamma -> Gamma
incrementaAmb n Nothing  [] = []
incrementaAmb n Nothing  ((i,e):xs) = ((i,e):xs)
incrementaAmb n (Just v) [] = [(n, v)]
incrementaAmb n (Just v) ((i,e):xs)
    | n == i = incrementaAmb n (return v) []
    | otherwise = incrementaAmb n (return v) xs


pesquisar :: Id -> Gamma -> Maybe Tipo
pesquisar v [] = error "Variavel nao declarada."
pesquisar v ((i,e):xs)
    | v == i = return e
    | otherwise = pesquisar v xs

\end{code}

\printbibliography


\end{document}
