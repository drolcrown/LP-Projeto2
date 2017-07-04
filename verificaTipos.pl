%%Verificador de tipos em prolog

verificarTipos(_, valor_inteiro(X),tInt) :- integer(X).
verificarTipos(_, valor_booleano(true), tBool) :- !.
verificarTipos(_, valor_booleano(false), tBool) :- !.
%%TODO
%verificarTipos(Gamma, var(_), Res) :-

verificarTipos(_, valor_booleano(_), tErro).

verificarTipos(Gamma, soma(Lhs, Rhs), Res) :-
    verificarTipos(Gamma, Lhs, tInt),!,
    verificarTipos(Gamma, Rhs, tInt),!,
    Res = tInt.
verificarTipos(_, soma(_,_), tErro).

verificarTipos(Gamma, subtracao(Lhs, Rhs), Res) :-
    verificarTipos(Gamma, Lhs, tInt),!,
    verificarTipos(Gamma, Rhs, tInt),!,
    Res = tInt.
verificarTipos(_, subtracao(_,_), tErro).

verificarTipos(Gamma, multiplicacao(Lhs, Rhs), Res) :-
    verificarTipos(Gamma, Lhs, tInt),!,
    verificarTipos(Gamma, Rhs, tInt),!,
    Res = tInt.

verificarTipos(_,multiplicacao(_,_), tErro).

verificarTipos(_, divisao(_, valor_inteiro(0)), tErro):-!.

verificarTipos(Gamma, divisao(Lhs, Rhs), Res) :-
    verificarTipos(Gamma, Lhs, tInt),!,
    verificarTipos(Gamma, Rhs, tInt),!,
    Res = tInt.

verificarTipos(_, divisao(_,_), tErro).

verificarTipos(Gamma, lambda(Arg,Exp),tFuncao(Targ,Texp)) :-
    pesquisarAmbiente(Arg, Gamma, RArg),!,
    verificarTipos(Gamma,RArg, Targ),
    verificarTipos(Gamma,Exp, Texp).

verificarTipos(_,lambda(_,_), tErro).

verificarTipos(Gamma, var(X), Res) :-
    pesquisarAmbiente(X, Gamma, Exp),
    verificarTipos(Gamma, Exp, Res),!.

verificarTipos(Gamma, var(X), variavel_nao_encontrada) :-
    pesquisarAmbiente(X, Gamma, variavel_nao_encontrada),!.

verificarTipos(Gamma, let(X, Exp1, Exp2), T2) :-
    verificarTipos([(X,Exp1)|Gamma], Exp2, T2).

verificarTipos(Gamma,aplicacao(lambda(Arg,Exp),Exp2),tFuncao(Targ,Texp)):- !,
    verificarTipos([(Arg,Exp2)|Gamma], Exp, Texp),
    verificarTipos([(Arg,Exp2)|Gamma], Exp2, Targ).

verificarTipos(Gamma, aplicacao(Exp1,Exp2), Res):-
    pesquisarAmbiente(Exp1, Gamma, lambda(Arg,Exp)),!,
    verificarTipos([(Arg,Exp2)|Gamma], Exp, Res).

verificarTipos(_, aplicacao(_,_), tErro).

pesquisarAmbiente(_, [], variavel_nao_encontrada).
pesquisarAmbiente(Var, [(Var,Exp)|_], Exp) :- !.
pesquisarAmbiente(Var, [_|Tail], Res) :- pesquisarAmbiente(Var, Tail, Res).


