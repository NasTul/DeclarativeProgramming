/* Using recursion, it can exclude not same lenth list */
correspond(E1,[E1|_L1],E2,[E2|_L2]).
correspond(E1,[_HeadL1|TailL1],E2,[_Head2|TailL2]) :-
    correspond(E1,TailL1,E2,TailL2).


interleave(Ls, L) :-
    interleaveHelp(Ls, L, []),
    samelength(Ls).
/* Using help to store combine  call other function,*/
interleaveHelp([], [], []).
interleaveHelp([], [H1|List], [H2|Lists]) :-
    interleaveHelp([H2|Lists], [H1|List], []).
interleaveHelp([[Head|[]]|Lists], [Head|Tail], HelpList) :-
    interleaveHelp(Lists, Tail, HelpList).     
interleaveHelp([[SubHead|SubHeadTail]|InputTail], [SubHead|ListTail], HelpList) :-
    append(HelpList, [SubHeadTail], CombineList),
    interleaveHelp(InputTail, ListTail, CombineList).
/*  Compare the first element of the list with each element after that  */
samelength([]).
samelength([_]).
samelength([Head1,Head2|Tail]) :-
    length(Head1,Len),
    length(Head2,Len),
    samelength([Head1|Tail]).


/*  If the first argument is a number, output it directly  */
partial_eval(Expr0, _Var, _Val, Expr0):-
    number(Expr0).
/*  To determine whether the first element is an atom, if true  
    Determines whether the first element and the variable are equal
    if euqal let expr output the var's value 
    if not let expr output it 
 */
partial_eval(Expr0, Var, Val, Expr) :-
    atom(Expr0),
    ( Expr0 = Var ->
        Expr = Val;
        Expr = Expr0
    ).

/* Decompose an addition expression into two expressions
    And then to figure out what the output is,If the output is a number, 
    add it. if not Direct output expression. Same thing down here.
 */
partial_eval(Expr0_A+Expr0_B, _Var, _Val, Expr):-
    partial_eval(Expr0_A,_Var, _Val,Expr_1),
    partial_eval(Expr0_B,_Var, _Val,Expr_2),
   ( number(Expr_1),number(Expr_2)->
    Expr is Expr_1+Expr_2;
    Expr = 	Expr_1+Expr_2
    ).

partial_eval(Expr0_A*Expr0_B, _Var, _Val, Expr):-
    partial_eval(Expr0_A,_Var, _Val,Expr_1),
    partial_eval(Expr0_B,_Var, _Val,Expr_2),
   ( number(Expr_1),number(Expr_2)->
    Expr is Expr_1*Expr_2;
    Expr = 	Expr_1*Expr_2
    ).

partial_eval(Expr0_A/Expr0_B, _Var, _Val, Expr):-
    partial_eval(Expr0_A,_Var, _Val,Expr_1),
    partial_eval(Expr0_B,_Var, _Val,Expr_2),
   ( number(Expr_1),number(Expr_2)->
    Expr is Expr_1/Expr_2;
    Expr = 	Expr_1/Expr_2
    ).
partial_eval(Expr0_A-Expr0_B, _Var, _Val, Expr):-
    partial_eval(Expr0_A,_Var, _Val,Expr_1),
    partial_eval(Expr0_B,_Var, _Val,Expr_2),
   ( number(Expr_1),number(Expr_2)->
    Expr is Expr_1-Expr_2;
    Expr = 	Expr_1-Expr_2
    ).

partial_eval(Expr0_A//Expr0_B, _Var, _Val, Expr):-
   partial_eval(Expr0_A,_Var, _Val,Expr_1),
    partial_eval(Expr0_B,_Var, _Val,Expr_2),
   ( number(Expr_1),number(Expr_2)->
    Expr is Expr_1//Expr_2;
    Expr = 	Expr_1//Expr_2
    ).
