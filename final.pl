%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  File     	 : fillin.pl
%  Student ID  : 800301
%  User Name   : thakkarj
%  Author   	 : Jigar Thakkar
%  Purpose  	 : Prolog solving fillin crossword puzzles.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Define the solution by specifying a predicate solve_puzzle/3. This
%% program starts with main/3. To run this program, we have to supply
%% puzzle, which is a matrix of # and _ and word list files.  Program
%% produce or maps to file name with filled. The main logic of the program
%% is it reads from blank puzzle file and creates a list of the list where
%% _ found in problem file, _ is replaced by logical, Unknown variable.
%% Also, this matrix is transposed, and an old matrix is appended with the
%% transpose of a matrix.word with lowest similar logical variable slot
%% filled first. If all words are fit one by one into 'blank' spaces and
%% all words are fit then the solution is successful otherwise it will
%% backtrack and try to find another way to fill it. When it found the
%% solution, it checks with filled file and returns true.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(PuzzleFile, WordlistFile, SolutionFile) :-
  	read_file(PuzzleFile, Puzzle),
  	read_file(WordlistFile, Wordlist),
		valid_puzzle(Puzzle),
		solve_puzzle(Puzzle, Wordlist, Solved),
		print_puzzle(SolutionFile, Solved).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% read given file and gives content from that file
read_file(Filename, Content) :-
		open(Filename, read, Stream),
		read_lines(Stream, Content),
		close(Stream).

% read lines from stream
read_lines(Stream, Content) :-
		read_line(Stream, Line, Last),
		(   Last = true
		->  (   Line = []
		    ->  Content = []
		    ;   Content = [Line]
		    )
		;  Content = [Line|Content1],
		    read_lines(Stream, Content1)
	).

%read each line from stream
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;( Char = '_'
 	 ->	Line = [A|Line1]
 	 ;	Line = [Char|Line1]
	 ),
	read_line(Stream, Line1, Last)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% print output puzzle in given file
print_puzzle(SolutionFile, Puzzle) :-
		open(SolutionFile, write, Stream),
		maplist(print_row(Stream), Puzzle),
		close(Stream).

print_row(Stream, Row) :-
		maplist(put_puzzle_char(Stream), Row),
		nl(Stream).

put_puzzle_char(Stream, Char) :-
		(   var(Char)
		->  put_char(Stream, '_')
		;   put_char(Stream, Char)
		).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% checks weather this puzzle in a file is valid or not
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
		maplist(samelength(Row), Rows).

% if puzzle in puzzle row length is same as column it is as counted valid puzzle
samelength([], []).
samelength([_|L1], [_|L2]) :-
		same_length(L1, L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solve_puzzle/3 takes puzzle(Empty), Wordlist which is a list of lists of
% characters, one list per word and fills that empty Puzzle with words,
% new FilledPuzzle is also lists of lists of characters.
solve_puzzle(Puzzle, Wordlist, FilledPuzzle):-
		make_empty_slots(Puzzle,LogicSlots),
		fill_in(Wordlist, LogicSlots),
		FilledPuzzle = Puzzle.

% make_empty_slots/2 takes Puzzle and converts that in to list of all
% possible slots where words can be filled. This Possible slots contains
% "Logical Variable" (Unknown, Unbounded variable).
make_empty_slots(Puzzle,LogicSlots):-
		transpose(Puzzle,TransposePuzzle),
		append(Puzzle, TransposePuzzle, FinalPuzzle),
		logic_variables_slot_list(FinalPuzzle,[],LogicSlots).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for trancepose of matrix
transpose([], []).
transpose([F|Fs], Ts) :-
		transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
		lists_firsts_rests(Ms, Ts, Ms1),
		transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
		lists_firsts_rests(Rest, Fs, Oss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fill all available row in puzzle with logical variables, this will
% converts logic variable included slots which maps words
logic_variables_slot_list([],FinalSlotList,FinalSlotList).
logic_variables_slot_list([P|Ps], SlotList, Slots):-
		logic_variables_row(P, [], SlotList, FinalSlotList),
		logic_variables_slot_list(Ps, FinalSlotList, Slots).

%  Cheks where puzzle have filled solid (#) or there is blank space
logic_variables_row([],[],Slotlist,Slotlist).
logic_variables_row([X|Xs], TempList, Slotlist, Slot):-
	(X == '#'
	-> ( (   samelength(TempList,[])
			 ;   samelength(TempList,[X])
			 )
			 ->  logic_variables_row(Xs, [], Slotlist, Slot)
	     ;   append(Slotlist, [TempList], FinalSlotList),
		       logic_variables_row(Xs, [], FinalSlotList, Slot)
	     )
     	 ;  append(TempList,[X],FinalTempList),
	     (Xs == []
	     ->(samelength(FinalTempList,[X])
	  	   -> logic_variables_row(Xs, [], Slotlist, Slot)
	  	   ;  append(Slotlist, [FinalTempList], FinalSlotList),
		        logic_variables_row(Xs, [], FinalSlotList, Slot)
	       )
	       ; append(TempList, [X], FinalTempList),
		     logic_variables_row(Xs, FinalTempList, Slotlist, Slot)
	   )
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fillin_in/2 takes all Words which needs to fill in to Logical slots and
% fill them in to correct slot
% keysort is inbuilt functions which sorts list of pairs
% select is also provided by prolog, it removes value from actual word
% stack so remaning words can be processed further
% unzip gives us separated value from sorted Key-value (tuple) type list
fill_in([],[]).
fill_in([X|Xs], [Y|Ys]):-
		get_permutation([X|Xs],[Y|Ys],[],KeyValuePairList),
		keysort(KeyValuePairList,[XY|XYs]),
		unzip([_Key|_Keys], [Val|Vals], [XY|XYs]),
		select(Val, [X|Xs], RestWord),
		fill_in(RestWord, Vals).

% get_permutation/4 gives true permutation from all possible solutions
% it uses backtracking untill it gets right result.
% aggregate_all is prolog function which binds Aggregate in Goal according
% to Template. The aggregate_all/3 version performs findall/3 on Goal.
% which creates a list of the instantiations Template gets successively on
% backtracking over Goal and unify the result with Bag. If it not find
% right solution it return empty list.
% In sort aggregate_all gives us count that number of time over predicate
% is true.
% [Count-S] is key-value pair like structure, for example [1-[V,O,L,T]].
% append collect all this key-value typed(like tuple) pair structure in to
% list
get_permutation(_,[],PairList,PairList).
get_permutation([X|Xs], [Y|Ys], Pair, KeyValuePairList):-
		aggregate_all(count, select(Y,[X|Xs],_Restword), Count),
		Count>0,
		append(Pair,[Count-Y],PairList),
		get_permutation([X|Xs], Ys, PairList, KeyValuePairList).

% same function as zip but here is used in reverse contxet so it's name is
% unzip, Other than no difference we can use this for zip aswell
unzip([],[],[]).
unzip([X|Xs], [Y|Ys], [X-Y|XYs]) :-
	  unzip(Xs, Ys, XYs).
