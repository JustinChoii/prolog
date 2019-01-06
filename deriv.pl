
deriv(A, Y):-
    simplify(A, B),
    d_calc(B, x, D),
    simplify(D, Y).

d_calc(U+V, X, Temp):- 
    d_calc(U, X, DU),                  
    d_calc(V, X, DV),
    Temp = DU + DV.					% Unify instead of Math operation =.
d_calc( U-V, X, Temp):-
    d_calc(U, X, DU),
    d_calc(V, X, DV),
    Temp = DU - DV.
d_calc(U*V, X, Temp):-  
    d_calc(U, X, DU),                 
    d_calc(V, X, DV),
    Temp = U*DV+V*DU.
d_calc(U/V, X, Temp):- 
    d_calc(U, X, DU),               
    d_calc(V, X, DV),
    Temp = (DU*V-DV*U)/(V^2).
d_calc(U^C, X, R):-      
    number(C),
    atom(X),
    d_calc(U, X, DU),
    R = C*DU*U^(C-1).
d_calc(-U, X, -DU):-
    atomic(U),
    d_calc(U, X, DU).
	
d_calc(X, X, 1) :- 
    atom(X).
d_calc(C, X, 0):-
    number(C),
    atom(X).

% simplify/Helper Cases
simplify(A, A):-
    number(A).

simplify(X+0, Y):-
    simplify(X, Y).
simplify(0+X, Y):-
    simplify(X,Y).
simplify(X-0, Y):-
    simplify(X,Y).
simplify(0-A*X^B, -A*X^B).

simplify(0-X, -X):-		% Zero - X will be a negative number.
    atom(X).
simplify(0-A, -A):-
    number(A).
simplify(0-X^2, -X^2).
simplify(X--A, X+A):-
    atomic(X),
    atomic(Y).

simplify(A+-X, A-X).



% Addition
simplify(A+X, A+X):-
    number(A),
    atom(X).
simplify(X+A, X+A):-
    number(A),
    atom(X).
simplify(X+X, 2*X).
simplify(A*X+X, Temp*X):-
    number(A),
    atom(X),
    Temp is A+1.
simplify(A+B, Temp):-
    number(A),
    number(B),
    Temp is A+B.
simplify(A+X+A, Temp*X):-
    number(A),
    atom(X),
    Temp is A+A.
simplify(A+X+B, X+Temp):-
    number(A),
    number(B),
    atom(X),
    Temp is A + B.

simplify(A*X+B*X, Temp*X):-
    number(A),
    number(B),
    Temp is A+B.

simplify(A+B+X, Temp+X):-
    number(A),
    number(B),
    atom(X),
    Temp is A+B.
simplify(A+X+B, Temp+X):-
    number(A),
    number(B),
    atom(X),
    Temp is A+B.
simplify(X+A+B, X+Temp):-
    number(A),
    number(B),
    atom(X),
    Temp is A+B.

simplify(A+B+X, X+Temp):-
    number(A),
    number(B),
    atom(X),
    Temp is A+B).

% Subtraction
simplify(X-A, X-A):-
    number(A).
simplify(A-X, A-X):-
    number(A).
simplify(X-X, 0).
simplify(A-A, 0):-
    number(A).
simplify(A-B, Temp):-
    number(A),
    number(B),
    Temp is A-B.
simplify(A*X-B*X, Temp*X):-
    number(A),
    number(B),
    Temp is A-B.

simplify(A-X+A, -X+Temp):-
    Temp is A+A.

simplify(0+X+B, X+B).


% Multiplication
simplify(0*X, 0).
simplify(X*0, 0).
simplify(X*A, A*X):-
    number(A),
    atom(X).
simplify(A*X, A*X).
simplify(X*1, X).
simplify(1*X, X).
simplify(A*B, Temp):-
    number(A),
    number(B),
    Temp is A*B.

simplify(A*X, A*X):-
    number(A).
simplify(X*A, A*X):-
    number(A).

simplify(X*A*X, Y):-
    number(A),
    simplify(A*X^2, Y).
simplify(A*(B*C), Temp):-
    number(A),
    number(B),
    number(C),
    Temp is A*B*C.

simplify(A*(X*C), Temp*X):-
    number(A),
    number(C),
    Temp is A*C.

simplify(A*(C*X), Temp*X):-
    number(A),
    number(C),
    Temp is A*C.

simplify(X*(A*C), Temp*X):-
    number(A),
    number(C),
    Temp is A*C.

simplify(A*(X+2), A*X+Temp):-
    number(A),
    Temp is A*2.
simplify(X(A+B), Temp*X):-
    number(A),
    number(B),
    Temp is A+B.
simplify(A*B*X, Temp*X):-
    number(A),
    number(B),
    atom(X),
    Temp is A*B.
simplify(A*X*B, Temp*X):-
    number(A),
    number(B),
    atom(X),
    Temp is A*B.
simplify(X*A*B), Temp*X):-
    number(A),
    number(B),
    atom(X),
    Temp is A*B.

simplify(A*X*X, A*X^2):-
    number(A),
    atom(X).

simplify(X*A*X, A*X^2):-
    number(A),
    atom(X).

simplify(X*X*A, A*X^2):-
    number(A),
    atom(X).


simplify(-A*X, -A*X):-
    number(A),
    atom(X).
simplify(A*-X, -A*X):-
    number(A),
    atom(X).
simplify(-A*-X, A*X):-
    number(A),
    atom(X).
simplify(-X*-A, A*X):-
    number(A),
    atom(X).


% Division
simplify(0/X, 0).
simplify(X/1, X).
simplify(X/X, 1).
simplify(A*X/X, A):-
    number(A).
simplify(A*X/1, A*X):-
    number(A).
simplify(A/B, Temp):-
    number(A),
    number(B),
    Temp is A/B.
simplify(X/X^A, 1/X^Temp):-
    number(   A),
    atom(   X),
    Temp is A-1.
simplify(V*X/X^A, 1/X^Temp):-
    number(   A),
    atomic(V),
    atom(   X),
    Temp is A-1.

simplify(-(A*B), -Temp):-
    number(A),
    number(B),
    Temp is A*B.

simplify(X^A/X^B, X^Temp):-
    number(A),
    number(B),
    atom(X),
    A>B,
    Temp is A-B.
simplify(X^A/X^B, X^Temp):-
    number(A),
    number(B),
    atom(X),
    A<B,
    Temp is B-A.

simplify(C*X^A/X^B, C*X^Temp):-
    number(A),
    number(B),
    number(C),
    atom(X),
    A>B,
    Temp is A-B.
simplify(C*X^A/X^B, C*X^Temp):-
    number(A),
    number(B),
    number(C),
    atom(X),
    A<B,
    Temp is B-A.
simplify(-C*X^A/X^B,-C*X^Temp):-
    number(A),
    number(B),
    number(C),
    atom(X),
    A>B,
    Temp is A-B.
simplify(-C*X^A/X^B,-C*X^Temp):-
    number(A),
    number(B),
    number(C),
    atom(X),
    not(A>B),
    Temp is B-A.
simplify(C*X/X^A, C*1/X^Temp):-
    atom(X),
    atomic(C),
    number(A),
    Temp is A-1.
simplify(-C*X/X^A, -C*1/X^Temp):-
    atom(X),
    atomic(C),
    number(A),
    Temp is A-1.
        
% Exponents
simplify(X*X, X^2).
simplify(X^1, X).
simplify(X^0, 1).

simplify(X*(X+A), X^2+A*X):-
    number(A).
simplify(X*(A+X), A*X+X^2):-
    number(A).

simplify(X^A, X^A):-
    number(A).

simplify(A^B, Temp):-
    number(A),
    number(B),
    Temp is A^B.

simplify(X^A/X, X^Temp):-
    number(A),
    Temp is A-1.
simplify(0*X^A, 0).

simplify(A*B*X^C, Temp*X^C):-
    number(A),
    number(B),
    Temp is A*B.

simplify((A*(A*X)), Temp*X):-
   Temp is A*A.

simplify((X^A)^B, X^Temp):-
    number(A),
    number(B),
    Temp is A*B.
simplify(X^A/X^A, 1).

simplify(-A*C*X^B, -Temp*X^B):-
    number(A),
    number(C),
    atom(X),
    Temp is C*A.
simplify(A*C*-X^B, -Temp*X^B):-
    number(A),
    number(C),
    atom(X),
    Temp is C*A.
simplify(-A*C*-X^B, Temp*X^B):-
    number(A),
    number(C),
    atom(X),
    Temp is C*A.
simplify(C*-X^B*-A, Temp*X^B):-
    number(A),
    number(C),
    atom(X),
    Temp is C*A.


% RECURSIVE CASE

simplify(N+M, Temp):-        
   simplify(N, X),
   simplify(M, Y),
   (N \== X; M \== Y), 
   simplify(X+Y, Temp).

simplify(N-M, Temp):-        
   simplify(N, X),
   simplify(M, Y),
   (N \== X; M \== Y),
   simplify(X-Y, Temp).

simplify(N*M, Temp):-       
   simplify(N, X),
   simplify(M, Y),
   (N \== X; M \== Y),
   simplify(X*Y, Temp).

simplify(N/M, Temp):-      
   simplify(N, X),
   simplify(M, Y),
   (N \== X; M \== Y), 
   simplify(X/Y, Temp).

simplify(N^A, Temp) :-
   simplify(A, B),
   simplify(N, X),
   (A \== B; N \== X),
   simplify(X^B, Temp).

% BASE CASE
simplify(X, X).
