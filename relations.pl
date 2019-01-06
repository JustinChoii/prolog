% reverse
reverse([H|T], Newlist):-
    reverser(T, Newlist, [H]).

reverser([], Acc, Acc).
reverser([H|T], Newlist, Acc):-
    reverser(T, Newlist, [H|Acc]).


	
	

% length
length([], 0).
length([_|T], Y):-
    length(T, Temp),
    Y is Temp + 1.


	
	

% subset
subset(R, [], []).
subset(R, [H|T], [H|Newlist]) :-
	Term =.. [R, H], Term,
	subset(R, T, Newlist).

subset(R, [_|T], Newlist):-
	subset(R, T, Newlist).


	
	

% member
member(Item, [Item|_]).  
member(Item, [_|Tail]):-  
  member(Item, Tail).
	

	
	
	

% intersect
% base case #1
% If the second list is empty, there is no intersect.
% Return an empty list, or Newlist.
intersect(_, [], []).

intersect(_, [], Newlist):-
    intersect(_, Newlist, Newlist).

% base case #2
%  there is no intersect.
% Return an empty lisIf the first list is empty,t.
intersect([], _, []).

% recursive case
% Start from the head of list1, which will be a.
% Check if it is a member of list2:
% 	If true:
% 		Append the head to Newlist.
% 		Restart with the tail of the list as the new head.
% 	Else:
% 		Restart with the tail of the list as the new head.
		
intersect([H|T],Y,[H|Newlist]) :-
	member(H, Y),
	intersect(T, Y, Newlist).


intersect([_|T], Y, Newlist) :-
	intersect(T, Y, Newlist).




% compute-change
compute-change(0, 0, 0, 0, 0).
compute-change(Total, Q, D, N, P) :-
    member(P, [0,1,2,3,4]),
    member(N, [0,1]),
    member(D, [0, 1,2]),
    member(Q, [0, 1, 2, 3, 4,5,6,7,8,9,10, 11, 12,13,14,15,16,17,18
                 ,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36
                 ,37,38,39,40,41]),
    Total is 25 * Q + 10 * D + 5 * N + P.

	
	

% compose
% base case 1
compose(L, R, [L,R]).

% base case 2 
compose([], [], []).

% Recursive Case
compose([L|LT], [R|RT], [L,R|A]) :-
    compose(LT, RT, A).

% base case 3
compose(L, [], L).
compose([], R, R).




% palindrome
palindrome([], []).
palindrome(Base, Result):-
    reverse(Base, Temp),
    append(Base, Temp, Result).
