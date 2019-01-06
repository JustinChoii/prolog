door_exists(a,b).
door_exists(a,c).
door_exists(a,d).
door_exists(b,c).
door_exists(b,e).
door_exists(d,c).
door_exists(d,e).
door_exists(e,c).


exists(X, X).
exists(X, Y) :- 						% Check if the rooms are connected
    door_exists(X, Y); 
    door_exists(Y, X).


path_to(X, X, []).					% Base Case



path_to(X, Y, Path) :				                % Initialize
	move_through_house(X, Y, [H|T], [X]), 	  % Recursively look for the path. 
    not(X == H),						                % If X is not the head (Duplicate check).
    reverse([H|T], Path).				          	% Reverse the path and return it into variable: Path

	move_through_house(X, Y, [Y|V], V) :- 		% If X and Y are connected, then append B to Path.
   	     exists(X, Y),					          	% Check if connected, if true ->
   	     not( member(Y, V)).				      	% If Y is a member in Path (Duplicate check).





move_through_house(X, Y, Path, Visited) :-			        % Recursive case to iterate through the path.
     exists(X, Z),       						                    % Find what room is connected to X
    	     not(Z == Y),							                    % IF Z is not the destination,
    	     not(member(Z, Visited)),					            % if Z is not a member of visited,
	     move_through_house(Z, Y, Path, [Z|Visited]). 		% Append the room to Visited
