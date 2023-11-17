mann(ali).
mann(albert).
mann(adolf).
mann(alex).
mann(bruno).
mann(beat).
mann(benno).
mann(bjoern).
mann(caspar).
mann(daniel).

frau(anna).
frau(anita).
frau(agatha).
frau(alessia).
frau(bea).
frau(berta).
frau(claudia).
frau(carla).
frau(deborah).
frau(daisy).

vater(ali, bruno).
vater(ali, beat).
vater(albert, bea).
vater(adolf, benno).
vater(adolf, berta).
vater(alex, bjoern).
vater(beat, claudia).
vater(bjoern, caspar).
vater(bjoern, carla).
vater(caspar, daniel).
vater(caspar, deborah).
vater(caspar, daisy).

mutter(anna, bruno).
mutter(anna, beat).
mutter(anita, bea).
mutter(agatha, benno).
mutter(agatha, berta).
mutter(alessia, bjoern).
mutter(bea, claudia).
mutter(berta, caspar).
mutter(berta, carla).
mutter(claudia, daniel).
mutter(claudia, deborah).
mutter(claudia, daisy).

% elternteil
elternteil(X,Y) :- mutter(X,Y).
elternteil(X,Y) :- vater(X,Y).
% kann auch so gemacht werden in einer Regel: elternteil(X,Y) :- mutter(X,Y) ; vater(X;Y).

% geschwister
geschwister(X,Y) :- elternteil(Z,X), elternteil(Z, Y), X \=Y.

% Grossmutter abfragen 
oma(X,Y) :- mutter(X,Z), elternteil(Z,Y).
% in Worten: X ist Oma von Y <= Wenn X die Mutter von Z ist und Z die Mutter von Y 

% Grossvater abfragen
opa(X,Y) :- vater(X,Z), elternteil(Z,Y).

% Tante abfragen
tante(X,Y) :- geschwister(X,Z), elternteil(Z,Y), frau(X).

% Onkel abfragen
onkel(X,Y) :- geschwister(X,Z), vater(Z,Y), mann(X).

% Urgrossmutter abfragen
uroma(X,Y) :- mutter(X,O),(opa(O,Y);oma(O,Y)).

% Urgrossvater abfragen
uropa(X,Y) :- vater(X,O),(opa(O,Y);oma(O,Y)).

% Schwester abfragen
schwester(X,Y) :- elternteil(Z,X), elternteil(Z,Y), frau(X), X \=Y.

% Bruder abfragen
bruder(X,Y) :- elternteil(Z,X), elternteil(Z,Y), mann(X), X \=Y.

% Sohn abfragen
sohn(X,Y) :- elternteil(Y,X), mann(X).

% Tochter abfragen
tochter(X,Y) :- elternteil(Y,X), frau(X).

% Vorfahre abfragen
vorfahre(X,Y) :-  elternteil(X,Y).
vorfahre(X,Y) :-  elternteil(X,Z), vorfahre(Z,Y).



