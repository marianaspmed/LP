%Nome: Mariana Medeiros, Numero: 89503

:- [codigo_comum].

% ------------------------------------------------------------------------------
% soma_lista(Lista, Soma)
% Predicado auxiliar
% Em que Lista eh a lista cujos elementos queremos somar e Soma, a soma desses
% ------------------------------------------------------------------------------
soma_lista([], 0).
soma_lista([H|T], Soma):-
	soma_lista(T, Z),
	Soma is H + Z.

% ------------------------------------------------------------------------------
% combinacoes_soma(N, Els, Soma, Combs)
% Em que N eh um inteiro, Els eh uma lista de inteiros, e Soma um inteiro.
% Combs eh a lista ordenada cujos elementos sao as combinacoes N a N, dos
% elementos de Els cuja soma eh Soma .
% ------------------------------------------------------------------------------

combinacoes_soma(N, Els, Soma, Combs):-
	findall(Comb, (combinacao(N, Els, Comb), soma_lista(Comb, Soma_comb), Soma_comb is Soma), Combs).


% ------------------------------------------------------------------------------
% permutacoes_soma(N, Els, Soma, Perms)
% Em que N eh um inteiro, Els eh uma lista de inteiros, e Soma um inteiro.
% Perms eh a lista ordenada cujos elementos sao as permutacoes das combinacoes
% N a N, dos elementos de Els cuja soma e Soma .
% ------------------------------------------------------------------------------

permutacoes_soma_aux([],[]).
permutacoes_soma_aux([Comb|Combs], [P|Perms]):-
	findall(Perm, permutation(Comb, Perm), P),
	permutacoes_soma_aux(Combs, Perms).

permutacoes_soma(N, Els, Soma, Perms):-
	combinacoes_soma(N, Els, Soma, Combs),
	permutacoes_soma_aux(Combs, Perms1),
	get_lists(Perms1, Perms2),
	sort(Perms2, Perms).

% ------------------------------------------------------------------------------
% get_lists(Lista, Perms)
% Predicado auxiliar: Perms eh a lista de permutacoes Lista no formato correcto
% ------------------------------------------------------------------------------

get_lists_aux([],[]).
get_lists_aux([H|T1],[H|T2]):-
	get_lists_aux(T1,T2).

get_lists([],[]).
get_lists([H|T], Perms):-
	get_lists_aux(H, Res1),
	get_lists( T, Res2),
	append(Res2, Res1, Perms).


% ------------------------------------------------------------------------------
% espaco_fila(Fila, Espacos, H_V)
% Em que Fila eh uma fila (linha ou coluna) de um puzzle e H_V eh um dos atomos
% h ou v, conforme se trate de uma fila horizontal ou vertical, respectivamente,
% significa que Esp eh um espaco de Fila.
% ------------------------------------------------------------------------------

compare(espaco(E, L), espaco(E, L)).

espaco_fila(Fila, Espaco, H_V):-
	espaco_fila(Fila, [[H]|T], H_V, [],[]),
	compare(espaco(H, T), Espaco).

espaco_fila([],Espaco, H_V, Ac,Lista_esp) :-
    length(Ac,Comp),Comp > 1,
    append(Lista_esp,[Ac],Lists),
    espaco_fila([],Espaco, H_V, [],Lists).

espaco_fila([],Espaco, _, _, Lista_esp) :-
    append(Lista_esp,Espaco),
    member(Espaco,Lista_esp).

espaco_fila([[H|T]|Fila],Espaco, H_V, Ac, Lista_esp):-
	is_list([H|T]),
	length(Ac,Comp),Comp > 1,
	append(Lista_esp,[Ac],Lists),
    espaco_fila(Fila,Espaco, H_V, [], Lists).

espaco_fila([[H|T]|Fila],Espaco, H_V, _,Lista_esp) :-
	H_V = 'h',
    is_list([H|T]),!,
    espaco_fila(Fila,Espaco, H_V, [T],Lista_esp).

espaco_fila([[H|T]|Fila],Espaco, H_V, _,Lista_esp) :-
	H_V = 'v',
	is_list([H|T]),!,
	espaco_fila(Fila,Espaco, H_V, [[H]],Lista_esp).

espaco_fila([H|Fila], Espaco, H_V, Ac,Lista_esp) :-
    append(Ac,[H],Esp),
    espaco_fila(Fila,Espaco, H_V, Esp, Lista_esp).


% ------------------------------------------------------------------------------
% espacos_fila(H_V, Fila, Espacos)
% Em que Fila eh uma fila (linha ou coluna) de uma grelha e eh H_V eh um dos
% atomos h ou v.
% Espacos eh a lista de todos os espacos de Fila, da esquerda para a direita.
% ------------------------------------------------------------------------------

espacos_fila(H_V, Fila, []):-
	\+espaco_fila(Fila, _, H_V),!.
espacos_fila(H_V, Fila, Espacos):-
    bagof(Espaco, espaco_fila(Fila,Espaco,H_V),Espacos).


% ------------------------------------------------------------------------------
% espacos_puzzle(Puzzle, Espacos)
% Em que Puzzle eh um puzzle e Espacos eh a lista de espacos de Puzzle.
% ------------------------------------------------------------------------------

espacos_puzzle(Puzzle, Espacos) :-
	espacos_puzzle(Puzzle, Espacos1, [], h),
	mat_transposta(Puzzle, Mat_transp),
	espacos_puzzle(Mat_transp, Espacos2, [], v),
	append(Espacos1, Espacos2, Espacos).
espacos_puzzle([], Espacos, Espacos, _).

espacos_puzzle([Fila|Puzzle], Espacos, Comp, h) :-
	h = h,
    espacos_fila(h, Fila, Espaco),
    append(Comp,Espaco,Novo_comp),
    espacos_puzzle(Puzzle,Espacos,Novo_comp,h).

espacos_puzzle([Fila|Puzzle],Espacos,Comp, v) :-
	v = v,
    espacos_fila(v, Fila, Espaco),
    append(Comp,Espaco,Novo_comp),
    espacos_puzzle(Puzzle,Espacos,Novo_comp,v).


% ------------------------------------------------------------------------------
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% Em que Espacos eh uma lista de espacos e Esp eh um espaco, Esps_com eh a lista
% de espacos com variaveis em comum com Esp, exceptuando Esp, pela mesma ordem
% que aparecem em Espacos.
% ------------------------------------------------------------------------------

get_espaco_list(espaco(_, L), L).
get_espaco_soma(espaco(S, _), S).

membro(P, [Q | _]) :- P == Q ,!.
membro(P, [_ | R]) :- membro(P, R).

posicoes_identicas([EL|_],L2) :-
    membro(EL,L2),!.
posicoes_identicas([_|L1],L2) :-
    posicoes_identicas(L1,L2).

espacos_com_posicoes_comuns([],_,[]).
espacos_com_posicoes_comuns([Espaco|Esps],Esp,[Espaco|Esps_comuns]) :-
	get_espaco_list(Espaco, E1),
	get_espaco_list(Esp, E2),
	Espaco \= Esp,
    posicoes_identicas(E1,E2),!,
    espacos_com_posicoes_comuns(Esps,Esp,Esps_comuns).

espacos_com_posicoes_comuns([_|Esps],Esp,Esps_comuns):-
    espacos_com_posicoes_comuns(Esps,Esp,Esps_comuns).


% ------------------------------------------------------------------------------
% permutacoes_soma_espacos(Espacos, Perms_soma)
% Em que Espacos eh uma lista de espacos, significa que Perms_soma eh a lista de
% listas de 2 elementos: o 1o elemento eh um espaco de Espacos e o 2o e a
% lista ordenada de permutacoes cuja soma eh igual a soma do espaco.
% ------------------------------------------------------------------------------

permutacoes_soma_espacos(Espacos, Perms_soma):-
	permutacoes_soma_espacos_aux(Espacos, Perms_soma).
permutacoes_soma_espacos_aux([], []).
permutacoes_soma_espacos_aux([Esp|Espacos], [[Esp, Perms]|T]):-
	get_espaco_list(Esp, List),
	length(List, N),
	get_espaco_soma(Esp, Soma),
	permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Soma, Perms),
	permutacoes_soma_espacos_aux(Espacos, T).


% ------------------------------------------------------------------------------
% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% Em que Perm eh uma permutacao, Esp eh um espaco, Espacos eh uma lista de
% espacos, e Perms_soma eh uma lista de listas obtida pelo predicado anterior,
% Perm eh uma permutacao possivel para o espaco Esp.
% ------------------------------------------------------------------------------

possivel_aux([], _, _, _).
possivel_aux([El|LT], Perm, Perms_soma, Espacos_coms):-
	nth0(El, Perm, Num), nth0(El, Espacos_coms, Esp_com),
	member(Perms_coms, Perms_soma),
	Perms_coms = [Esp_com, Perms_com],
	append(Perms_com, Nums_com),
	member(Num, Nums_com),
	possivel_aux(LT, Perm, Perms_soma, Espacos_coms).

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma):-
    findall(Y, (member([Esp, Y], Perms_soma)), [X]),
    member(Perm, X),
    espacos_com_posicoes_comuns(Espacos, Esp, Espacos_coms),
    findall(Pos, (member(Esp1, Espacos_coms), nth0(Pos, Espacos_coms, Esp1)), Lista_pos),
    possivel_aux(Lista_pos, Perm, Perms_soma, Espacos_coms).


% ------------------------------------------------------------------------------
% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
% Em que Espacos eh uma lista de espacos, Perms_soma eh uma lista de listas tal
% como obtida pelo predicado permutacoes_soma_espacos, e Esp eh um espaco.
% Perms_poss eh uma lista de 2 elementos em que o primeiro eh a lista de variaveis
% de Esp e o segundo eh a lista ordenada de permutacoes possiveis para espaco Esp.
% ------------------------------------------------------------------------------

permutacoes_possiveis_espaco(Esps, Perms_soma, espaco(E,L), [L, Perms_new]) :-
    bagof(P, Esps^(permutacao_possivel_espaco(P, espaco(E,L), Esps, Perms_soma)), Perms_poss),
    sort(Perms_poss, Perms_new).


% ------------------------------------------------------------------------------
% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% Em que Espacos eh uma lista de espacos, significa que Perms_poss_esps eh a
% lista de permutacoes possiveis.
% ------------------------------------------------------------------------------

permutacoes_possiveis_espacos_aux(_, _, [], []).
permutacoes_possiveis_espacos_aux(Esps, Perms_soma, [E|ET], [P|PT]) :-
    permutacoes_possiveis_espacos_aux(Esps, Perms_soma, ET, PT),
    permutacoes_possiveis_espaco(Esps, Perms_soma, E, P).
permutacoes_possiveis_espacos(Esps, Perms_poss_esps) :-
    permutacoes_soma_espacos(Esps, Perms_soma),
    permutacoes_possiveis_espacos_aux(Esps, Perms_soma, Esps, Perms_poss_esps), !.


% ------------------------------------------------------------------------------
% numeros_comuns(Lst_Perms, Numeros_comuns)
% Em que Lst_Perms eh uma lista de permutacoes, Numeros_comuns eh uma lista de
% pares (pos, numero), em que todas as listas de Lst_Perms contem o numero numero
% na posicao pos.
% ------------------------------------------------------------------------------

intersect_aux(_, [], []).
intersect_aux(Indice, [L | LT], [(Indice, L) | RT]) :-
    NovoIndice is Indice + 1,
    intersect_aux(NovoIndice, LT, RT).

intersect(L, E) :-
    intersect_aux(1, L, E).

interseta2([], _).

interseta2([S | LT], NovoIndice) :-
    interseta2(LT, I),
    intersection(S, I, NovoIndice).

numeros_comuns(Lst_Perms, Numeros_comuns) :-
    maplist(intersect, Lst_Perms, Aux),
    interseta2(Aux, Numeros_comuns),!.

% ------------------------------------------------------------------------------
% atribui_comuns(Perms_Possiveis)
% Em que Perms_Possiveis eh uma lista de permutacoes possiveis, actualiza esta
% lista atribuindo a cada espaco numeros comuns a todas as permutacoes possiveis
% para esse espaco.
% ------------------------------------------------------------------------------

atribui_comuns_aux([], _).
atribui_comuns_aux([Comum|T], Esp):-
    Comum = (Pos, Ind), nth1(Pos, Esp, Ind),
    atribui_comuns_aux(T, Esp).
atribui_comuns([[Esp,Perms|_]|LT]):-
    numeros_comuns(Perms, Comuns),
    atribui_comuns_aux(Comuns, Esp),
    atribui_comuns(LT).
atribui_comuns([]).


% ------------------------------------------------------------------------------
% filterList(Esps, Perms, Novas_Perms)
% Predicado auxiliar que filtra a lista das permutacoes, em que Perms eh o
% resultado de retirar as permutacoes impossiveis de Perms_aux
% ------------------------------------------------------------------------------

filterList(Esps, Perms, Novas_Perms) :-
    include(retira_impossiveis_aux(Esps), Perms, Novas_Perms).

% ------------------------------------------------------------------------------
% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% Em que Perms_Possiveis eh uma lista de permutacoes possiveis, significa que
% Novas_Perms_Possiveis eh o resultado de tirar permutacoes imposseveis de
% Perms_Possiveis.
% ------------------------------------------------------------------------------

retira_impossiveis_aux([], []).
retira_impossiveis_aux([V|VT], [S|Q]):-
    V==S,
    retira_impossiveis_aux(VT, Q).
retira_impossiveis_aux([V|VT], [_|Q]):-
    var(V),
    retira_impossiveis_aux(VT, Q).

retira_impossiveis([], []).
retira_impossiveis([[Esps, Perms_aux]|T], [[Esps, Perms]|Q]):-
    filterList(Esps, Perms_aux, Perms),
    retira_impossiveis(T, Q).