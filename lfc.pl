%% Linguagem com suporte a funcoes de alta.
%
% Esse modulo Prolog ilustra a implementacao de um
% interpretador para uma linguagem de programacao funcional
% com suporte a expressoes lambda, expressoes do tipo let
% e escopo dinamico e estrategia innermost evaluation.
%
% @author: Rodrigo Bonifacio
%

avaliar(_, valor_inteiro(X), valor_inteiro(X)) :- integer(X).

avaliar(_, valor_booleano(true), valor_booleano(true)) :- !.
avaliar(_, valor_booleano(false), valor_booleano(false)) :- !.
avaliar(_, valor_booleano(_), erro_tipo).

avaliar(Gamma, soma(LHS, RHS), Res) :-
    avaliar(Gamma, LHS, valor_inteiro(X)),
    avaliar(Gamma, RHS, valor_inteiro(Y)),!,
    Soma is X + Y,
    Res = valor_inteiro(Soma).

avaliar(_, soma(_, _), erro_tipo).

avaliar(Gamma, subtracao(LHS, RHS), Res) :-
    avaliar(Gamma, LHS, valor_inteiro(X)),
    avaliar(Gamma, RHS, valor_inteiro(Y)),!,
    Subtracao is X - Y,
    Res = valor_inteiro(Subtracao).

avaliar(_, soma(_, _), erro_tipo).

avaliar(Gamma, multiplicacao(LHS, RHS), Res) :-
    avaliar(Gamma, LHS, valor_inteiro(X)),
    avaliar(Gamma, RHS, valor_inteiro(Y)),!,
    Multiplicacao is X * Y,
    Res = valor_inteiro(Multiplicacao).

avaliar(_, multiplicacao(_, _), erro_tipo).

%funções let%
avaliar(Gamma, let(X, Exp1, Exp2), Res) :-
    avaliar(Gamma, Exp1, R),
    avaliar([(X,R)|Gamma], Exp2, Res).

%referencia%
avaliar(Gamma, var(Var), variavel_nao_declarada) :-
    pesquisarAmbiente(Var, Gamma, variavel_nao_declarada),!.

avaliar(Gamma, var(Var), Res) :-
    pesquisarAmbiente(Var, Gamma, Exp),
    avaliar(Gamma, Exp, Res).

%expressão lambda%
avaliar(_, lambda(Arg, Exp), lambda(Arg, Exp)).

%aplicacao%
avaliar(Gamma, aplicacao(Exp1, Exp2), Res) :-
    avaliar(Gamma, Exp1, lambda(Arg, Exp)),!,
    avaliar(Gamma, Exp2, Valor),
    avaliar([(Arg,Valor)|Gamma] , Exp, Res).

%caso Exp1 não seja uma expressao lambda eh feita uma busca em Gamma%
avaliar(Gamma, aplicacao(Exp1,Exp2),Res) :-
    pesquisarAmbiente(Exp1,Gamma,R),
    avaliar(Gamma, R, lambda(Arg, Exp)),!,
    avaliar(Gamma, Exp2, Valor),
    avaliar([(Arg, Valor)|Gamma] , Exp, Res).

avaliar(_, aplicacao(_, _), aplicacao_requer_exp_lambda).

pesquisarAmbiente(_, [], variavel_nao_encontrada).
pesquisarAmbiente(Var, [(Var,Exp)|_], Exp) :- !.
pesquisarAmbiente(Var, [_|Tail], Res) :- pesquisarAmbiente(Var, Tail, Res).









