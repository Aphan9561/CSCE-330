:- discontiguous male/1.
:- discontiguous female/1.

%% child(X, Y): X is a child of Y.
child(john,sue).
child(john,sam).
child(jane,sue).
child(jane,sam).
child(sue,george).
child(sue,gina).
child(adam, james).
child(adam, susan).
child(jessie, susan).
child(jessie, jordan).
child(sarah, jordan).
child(sarah, emma).

child(alberto,guido). child(alberto,antonietta).
child(giulia,enrico). child(giulia,annamaria).
child(dante,marco).   child(clara,marco).
child(dante,laura).   child(clara,laura).
child(marco,alberto).   child(marco,giulia).
child(laura,lawrence).  child(laura,julie).
child(emily,lawrence).  child(emily, julie).
child(claire,lawrence). child(claire, julie).
child(sam,emily). child(ben,emily).
child(sam,dave). child(ben,dave).
child(eve,claire). child(annabelle,claire).
child(eve,ed). child(annabelle,ed).
child(giulio,guido). child(donata,giulio).
child(sara,donata). child(marco2,donata).
child(giulio,antonietta). child(gioia,clara).

%% male(X) : X is male.
%% female(X): X is female.
male(james).
male(jordan).
male(guido). male(enrico).
male(marco). male(dante). male(alberto). male(lawrence).
male(sam). male(ben). male(dave). male(ed).
male(giulio). male(marco2).
female(antonietta). female(annamaria).
female(clara).  female(laura).  female(giulia). female(julie).
female(emily). female(claire). female(eve). female(annabelle).
female(donata). female(sara). female(gioia).
female(susan).
female(jessie).
male(adam).
male(john).
female(sue).
male(sam).
female(jane).
male(george).
female(june).
female(sarah).
female(emma).

%% parent(Y,X): Y is parent of X. 
parent(Y,X) :- child(X,Y).

%% father(Y,X): Y is father of X.
father(Y,X) :- child(X,Y), male(Y).

%% opp_sex(X, Y): X is of opposite sex of Y. 
opp_sex(X,Y) :- male(X), female(Y).
opp_sex(Y,X) :- male(X), female(Y).

%% grand_father(X,Z): X is grand_father of Z. 
grand_father(X,Z) :- father(X,Y), parent(Y,Z).

%% mother(X, Y): X is the mother of Y.
%% Problem 1. Define the mother predicate. (1 point) 
mother(X, Y) :- child(Y, X), female(X).

%% grand_parent(X, Y): X is a grand parent of Y.
%% Problem 2. Define the grand_parent predicate. (1 point) 
grand_parent(X, Z) :- parent(X, Y), parent(Y, Z).

%% great_grand_mother(X, Z): X is a great grand mother of Z.
%% Problem 3. Define the great_grand_mother predicate. (1 point) 
great_grand_mother(X, Z) :- mother(X, Y), grand_parent(Y, Z).

%% Full siblings are siblings who have two parents in common.
%% Problem 4. Define the full_sibling predicate. (3 points)
%% Hints: use mother and father predicates.
%% 1 point for the same mother, 1 point for the same
%% father, 1 point for \+ X = Y. 
full_sibling(X, Y) :-
    mother(Z, X), mother(Z, Y), father(U, X), father(U, Y), \+ X = Y. 

%% Half siblings are siblings who have exactly one parent in common. 
%% Problem 5. Define the half_sibling predicate. (4 points)
%% Hints: use mother and father predicates.
%% You get 2 points for sharing the exact same mother but
%% different fathers.
%% You get another 2 points for sharing the exact same father
%% but different mothers. 
half_sibling(X, Y) :- mother(Z, X), mother(Z, Y), father(U, X), father(V, Y), \+ U = V, \+ X = Y.  
half_sibling(X, Y) :- mother(U, X), mother(V, Y), father(Z, X), father(Z, Y), \+ U = V, \+ X = Y.


%% We will use Prolog to find the digits for C, R,O, S, A, D, N, G, E, R such that CROSS + ROADS = DANGER. Note that C , D and R must greater than 0.

%%  CROSS
%%+ ROADS
%% -------
%% DANGER

%% dig(X): X is a digit.
dig(0). dig(1). dig(2). dig(3). dig(4).
dig(5). dig(6). dig(7). dig(8). dig(9).

%% An auxiliary predicate to ensure uniqueness.
uniq(C, R,O, S, A, D, N, G, E) :-
    \+ C = R,
    \+ C = O,
    \+ C = S,
    \+ C = A,
    \+ C = D,
    \+ C = N,
    \+ C = G,
    \+ C = E,

    \+ R = O,
    \+ R = S,
    \+ R = A,
    \+ R = D,
    \+ R = N,
    \+ R = G,
    \+ R = E,

    \+ O = S,
    \+ O = A,
    \+ O = D,
    \+ O = N,
    \+ O = G,
    \+ O = E,

    \+ S = A,
    \+ S = D,
    \+ S = N,
    \+ S = G,
    \+ S = E,

    \+ A = D,
    \+ A = N,
    \+ A = G,
    \+ A = E,

    \+ D = N,
    \+ D = G,
    \+ D = E,

    \+ N = G,
    \+ N = E,
    \+ G = E.
    
%% Problem 6. Define solve_digits predicates. You must use dig/1 and uniq/9.
%% Try to apply the optimization techniques to make
%% your program more efficient. (8 points)
solve_digits(C, R, O, S, A, D, N, G, E) :-

    dig(S),
    % 2 points
    R is (S + S) mod 10,
    R > 0,
    C1 is (S + S) //10,
    dig(D),
    % 2 points
    D > 0,
    E is (S + D + C1) mod 10,
    C2 is (S + D + C1) //10,

    % 1 point
    dig(O), dig(A),
    G is (O + A + C2) mod 10,
    C3 is (O + A + C2) //10,
    % 1 point
    N is (R + O + C3) mod 10,
    C4 is (R + O + C3) // 10,
    dig(C), C > 0,
    % 2 points
    A is (C + R + C4) mod 10, D is (C + R + C4) // 10,
    uniq(C, R,O, S, A, D, N, G, E). 
    

%% A pretty printing predicate to check your solution.
print_solution :-
    solve_digits(C, R,O, S, A, D, N, G, E),
    format("~d~d~d~d~d\n", [C, R, O, S, S]),
    write("+"), nl, 
    format("~d~d~d~d~d\n", [R, O, A, D, S]),
    write("-------------\n"),
    format("~d~d~d~d~d~d\n", [D, A, N, G, E, R]).

%% We will use Prolog to solve the following puzzle.

%% Donna, Danny, David, and Doreen were seated at a table in a restaurant.
%% The men sat across from each other, as did the women. They each ordered a
%% different main course with a different beverage. In addition,
%% – Doreen sat beside the person who ordered steak.
%% – The chicken came with a Coke.
%% – The person with the lasagna sat across from the person with milk.
%% – David never drinks coffee.
%% – Donna only drinks water.
%% – Danny could not afford to order steak.
%% Who ordered the pizza?

people(donna).
people(danny).
people(david).
people(doreen).


%% beside(X, Y): X is sitting beside Y. 
beside(donna, danny).
beside(donna, david).
beside(danny, donna).
beside(david, donna).
beside(doreen, danny).
beside(doreen, david).
beside(danny, doreen).
beside(david, doreen).


%% across(X, Y): X is sitting across Y. 
across(donna, doreen).
across(doreen, donna).
across(david, danny).
across(danny, david).

%% Problem 7. Defined the following solve predicate to solve the puzzle. (10 points)
%% Hints: Use people/1, beside/2, across/2. 
solve(Coffee, Water, Coke, Milk, Steak, Lasagna, Pizza, Chicken) :-
    %% 2 points
    people(Coffee),
    people(Water),
    people(Coke),
    people(Milk),
    people(Steak),
    people(Lasagna),
    people(Pizza),
    people(Chicken),

    % 1 point
    beside(doreen, Steak), 
    % 1 point     
    Chicken = Coke,
    % 1 point
    across(Lasagna, Milk),
    % 1 point 
    \+ Coffee = david,
    % 1 point 
    Water = donna,
    % 1 point
    \+ Steak = danny,

    %% 2 points. 
    \+ Coffee = Water,
    \+ Coffee = Coke,
    \+ Coffee = Milk,
    \+ Water = Coke,
    \+ Water = Milk,
    \+ Coke = Milk,
    \+ Steak = Lasagna,
    \+ Steak = Pizza,
    \+ Steak = Chicken,
    \+ Pizza = Chicken,
    \+ Pizza = Lasagna,
    \+ Chicken = Lasagna.

    






