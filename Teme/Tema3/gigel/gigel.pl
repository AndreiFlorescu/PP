:- ensure_loaded('chat.pl').

check_compatibility([], []).
check_compatibility([H | T1], [H | T2]) :-
    check_compatibility(T1, T2).

% Returneaza true dacă regula dată ca argument se potriveste cu
% replica data de utilizator. Replica utilizatorului este
% reprezentata ca o lista de tokens. Are nevoie de
% memoria replicilor utilizatorului pentru a deduce emoția/tag-ul
% conversației.
% match_rule/3
% match_rule(_Tokens, _UserMemory, rule(_, _, _, _, _)) :- fail.
match_rule(Tokens, UserMemory, rule(Expr, _, _, Em, Tag)) :-
    check_compatibility(Expr, Tokens),
    get_emotion(UserMemory, UserEm),
    get_tag(UserMemory, UserTag),
    (
       (UserEm == neutru, Em == []) ;
       (UserEm == fericit, Em == [fericit]) ;
       (UserEm == trist, Em == [trist])
    ),
    (
       (UserTag == none, Tag == []) ;
       (UserTag == sport, Tag == [sport]) ;
       (UserTag == film, Tag == [film])
    ).

% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.
% find_matching_rules/4
% find_matching_rules(+Tokens, +Rules, +UserMemory, -MatchingRules)
find_matching_rules(Tokens, Rules, UserMemory, MatchingRules) :-
    findall(
        rule(Expr, RespOp, Ac, Em, Tags),
        (member(Rule, Rules), member(rule(Expr, RespOp, Ac, Em, Tags), Rule), match_rule(Tokens, UserMemory, rule(Expr, RespOp, Ac, Em, Tags))),
        MatchingRules
    ).

% Intoarce in Answer replica lui Gigel. Selecteaza un set de reguli
% (folosind predicatul rules) pentru care cuvintele cheie se afla in
% replica utilizatorului, in ordine; pe setul de reguli foloseste
% find_matching_rules pentru a obtine un set de raspunsuri posibile.
% Dintre acestea selecteaza pe cea mai putin folosita in conversatie.
%
% Replica utilizatorului este primita in Tokens ca lista de tokens.
% Replica lui Gigel va fi intoarsa tot ca lista de tokens.
%
% UserMemory este memoria cu replicile utilizatorului, folosita pentru
% detectarea emotiei / tag-ului.
% BotMemory este memoria cu replicile lui Gigel și va si folosită pentru
% numararea numarului de utilizari ale unei replici.
%
% In Actions se vor intoarce actiunile de realizat de catre Gigel in
% urma replicii (e.g. exit).
%
% Hint: min_score, ord_subset, find_matching_rules

% select_answer/5
% select_answer(+Tokens, +UserMemory, +BotMemory, -Answer, -Actions)
select_answer(Tokens, UserMemory, BotMemory, Answer, Actions) :-
    findall(I, (rules(Q, I), ord_subset(Q, Tokens), !), L),
    find_matching_rules(Tokens, L, UserMemory, [rule(_, Options, Actions, _, _) | _]),
    findall((Cur, Num), (member(Cur, Options), get_answer(Cur, BotMemory, Num)), Ceva),
    min_element(Ceva, Answer).

% Esuează doar daca valoarea exit se afla in lista Actions.
% Altfel, returnează true.
% handle_actions/1
% handle_actions(+Actions)
handle_actions(Actions) :- \+ member(exit, Actions).


% Caută frecvența (numărul de apariți) al fiecarui cuvânt din fiecare
% cheie a memoriei.
% e.g
% ?- find_occurrences(memory{'joc tenis': 3, 'ma uit la box': 2, 'ma uit la un film': 4}, Result).
% Result = count{box:2, film:4, joc:3, la:6, ma:6, tenis:3, uit:6, un:4}.
% Observați ca de exemplu cuvântul tenis are 3 apariți deoarce replica
% din care face parte a fost spusă de 3 ori (are valoarea 3 în memorie).
% Recomandăm pentru usurința să folosiți înca un dicționar în care să tineți
% frecvențele cuvintelor, dar puteți modifica oricum structura, această funcție
% nu este testată direct.

compute_word([], Mem, NewMem) :-
    NewMem = Mem.

compute_word([H | T], Mem, NewMem) :-
    add_answer([H], Mem, Aux),
    compute_word(T, Aux, NewMem).


compute_line(_, 0, Mem, NewMem) :-
    NewMem = Mem, !.

compute_line(WordL, NoTimes, Memory, NewMemory) :-
    compute_word(WordL, Memory, Aux),
    N is NoTimes - 1,
    compute_line(WordL, N, Aux, NewMemory).


compute_key([], _, Cur, Result) :-
    Result = Cur.

compute_key([H | T], UserMemory, Cur, Result) :-
    get_value(UserMemory, H, NoReps),
    words(H, WordL),
    compute_line(WordL, NoReps, Cur, Aux),
    compute_key(T, UserMemory, Aux, Result).


% find_occurrences/2
% find_occurrences(+UserMemory, -Result)
find_occurrences(UserMemory, Result) :-
    dict_keys(UserMemory, Keys),
    compute_key(Keys, UserMemory, count{}, Result).

% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.
% get_happy_score/2
% get_happy_score(+UserMemory, -Score)
get_happy_score(UserMemory, Score) :-
    find_occurrences(UserMemory, WordFreq),
    dict_keys(WordFreq, Words),
    findall(Val, (member(Word, Words), happy(Word), get_value(WordFreq, Word, Val)), TmpFreqs),
    append(TmpFreqs, [0], Freqs),
    sum_list(Freqs, Score).

% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
% get_sad_score/2
% get_sad_score(+UserMemory, -Score)
get_sad_score(UserMemory, Score) :-
    find_occurrences(UserMemory, WordFreq),
    dict_keys(WordFreq, Words),
    findall(Val, (member(Word, Words), sad(Word), get_value(WordFreq, Word, Val)), TmpFreqs),
    append(TmpFreqs, [0], Freqs),
    sum_list(Freqs, Score).

% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
% get_emotion/2
% get_emotion(+UserMemory, -Emotion)
get_emotion(UserMemory, Emotion) :-
    get_happy_score(UserMemory, HappyScore),
    get_sad_score(UserMemory, SadScore),
    Dif = HappyScore - SadScore,
    (Dif < 0, Emotion = trist; (0 < Dif), Emotion = fericit; Dif =:= 0, Emotion = neutru).

% Atribuie un scor pentru un Tag (de cate ori au fost folosit cuvinte din lista tag(Tag, Lista))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să vorbească despre acel subiect.
% get_tag_score/3
% get_tag_score(+Tag, +UserMemory, -Score)
get_tag_score(Tag, UserMemory, Score) :-
    find_occurrences(UserMemory, WordFreq),
    tag(Tag, TagWords),
    findall(Val, (member(Word, TagWords), get_value(WordFreq, Word, Val)), TmpFreqs),
    append(TmpFreqs, [0], Freqs),
    sum_list(Freqs, Score).

% Pentru fiecare tag calculeaza scorul și îl alege pe cel cu scorul maxim.
% Dacă toate scorurile sunt 0 tag-ul va fi none.
% e.g:
% ?- get_tag(memory{'joc fotbal': 2, 'joc box': 3}, Tag).
% Tag = sport.
% get_tag/2
% get_tag(+UserMemory, -Tag)
get_tag(UserMemory, Tag) :-
    get_tag_score(sport, UserMemory, ScoreSport),
    get_tag_score(film, UserMemory, ScoreFilm),
    (ScoreSport > ScoreFilm, Tag = sport; ScoreSport < ScoreFilm, Tag = film; ScoreSport =:= 0, ScoreFilm =:= 0, Tag = none).
