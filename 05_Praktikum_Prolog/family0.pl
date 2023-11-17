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
elternteil(X,Y) :- fail.

% geschwister
geschwister(X,Y) :- fail.

% Grossmutter abfragen 
oma(X,Y) :- fail.

% Grossvater abfragen
opa(X,Y) :- fail.

% Tante abfragen
tante(X,Y) :- fail.

% Onkel abfragen
onkel(X,Y) :- fail.

% Urgrossmutter abfragen
uroma(X,Y) :- fail.

% Urgrossvater abfragen
uropa(X,Y) :- fail.

% Schwester abfragen
schwester(X,Y) :- fail.

% Bruder abfragen
bruder(X,Y) :- fail.

% Sohn abfragen
sohn(X,Y) :- fail.

% Tochter abfragen
tochter(X,Y) :- fail.

% Vorfahre abfragen
vorfahre(X,Y) :-  fail.



