#lang racket/gui
;Ignorați următoarele linii de cod. Conțin import-uri și export-uri necesare checker-ului.

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "random.rkt")
(require "abilities.rkt")
(require "constants.rkt")
;---------------------------------------checker_exports------------------------------------------------
(provide next-state)
(provide next-state-bird)
(provide next-state-bird-onspace)
(provide change)

(provide get-pipes)
(provide get-pipe-x)
(provide next-state-pipes)
(provide add-more-pipes)
(provide clean-pipes)
(provide move-pipes)

(provide invalid-state?)
(provide check-ground-collision)
(provide check-pipe-collisions)

(provide draw-frame)

(provide get-initial-state)
(provide get-bird)
(provide get-bird-y)
(provide get-bird-v-y)

; pipe
(provide get-pipes)
(provide get-pipe-x)

; score25
(provide get-score)

(provide get-abilities)
(provide get-abilities-visible)
(provide get-abilities-active)
; variables
(provide get-variables)
(provide get-variables-gravity)
(provide get-variables-momentum)
(provide get-variables-scroll-speed)

;---------------------------------------checker_exports------------------------------------------------
; Checker-ul contine un numar de teste, fiecare cu numele sau. In acest fisier veti gasi comentarii
; care incep cu TODO %nume_test, unde trebuie sa modificati sau sa implementati o functie, pentru
; a trece testul %nume_test.
;
;Initial state
; Primul pas pe care trebuie sa il facem este sa cream starea initiala a jocului.
; Aceasta va fi salvata in (get-initial-state), si trebuie sa incapsuleze toate informatiile
; necesare jocului, si anume: informatii despre pasare, despre pipes si despre powerups.
; Recomandam ca in pasare, sa retineti, printre altele, informatii despre y-ul curent
; si viteza pe y
; Pe parcursul temei, in state, salvati coordonatele colturilor din stanga sus ale obiectelor.
; Aceasta va face mai usoara atat logica miscarii obiectelor, cat si testarea cerintelor.
; Toate coordonatele oferite in comentarii sau in fisierul constants.rkt, se refera la
; coltul din stanga sus ale obiectelor!
;Inițial state
; Primul pas pe care trebuie să îl facem este să creăm starea inițială a jocului.
; Aceasta va fi salvată în (get-initial-state), și trebuie să incapsuleze toate informațiile
; necesare jocului, și anume: informații despre pasăre, despre pipes și, pentru bonus,
; despre powerups și despre variabilele de mediu.
; Recomandăm ca în pasăre, să rețineți, printre altele, informații despre y-ul curent
; și viteză pe y.
; Pe parcursul temei, în state, salvați coordonatele colțurilor din stânga sus ale obiectelor.
; Aceasta va face mai ușoară atât logică mișcării obiectelor, cât și testarea cerințelor.
; Toate coordonatele oferite în comentarii sau în fișierul variables.rkt se referă la
; colțul din stânga sus ale obiectelor!

;TODO 1
; După ce definiți structurile lui (get-initial-state) și a păsării, introduceți în prima
; pe cea din urmă. Colțul din stânga sus a păsării se va află inițial la:
;    y = bird-inițial-y
; și x = bird-x.
; (get-initial-state) va fi o funcție care va returna starea inițială a jocului.
(define-struct flappy_bird (x y v_y))


;TODO 8
; În starea jocului, trebuie să păstrăm informații despre pipes. Pe parcursul jocului,
; pipe-urile se vor schimba, unele vor fi șterse și vor fi adăugate altele.
; După ce definiți structura pentru pipe și pentru mulțimea de pipes din stare,
; adăugați primul pipe în starea jocului. Acesta se va află inițial în afară ecranului.
; Celelalte pipe-uri vor fi adăugate ulterior, poziționându-le după acest prim pipe.
; Atenție! Fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap.
; Colțul din stânga sus al gap-ului dintre componentele primului pipe se va afla inițial la:
;    y = (+ added-number (random random-threshold)), pentru a da un element de noroc jocului,
; și x = scene-width,
; pentru a-l forța să nu fie inițial pe ecran.
; Atenție! Recomandăm să păstrați în stare colțul din stânga sus al chenarului lipsa
; dintre cele 2 pipe-uri!
(define-struct game_pipe (x gap_y))

;TODO 16
; Vrem o modalitate de a păstra scorul jocului. După ce definiți structura
; acestuia, adăugați scorul inițial, adică 0, în starea inițială a jocului.
; Atenție get-initial-state trebuie sa fie o funcție
; și trebuie apelată în restul codului.
(define-struct abilities_struct (visible active))
(define-struct variables_struct (gravity momentum scroll_speed))

(define-struct game_state (f_bird pipes score abilities variables))

(define (get-initial-state)
  (game_state (flappy_bird bird-x bird-initial-y 0)
              (list (game_pipe scene-width (+ added-number (random random-threshold))))
              0
              (abilities_struct null null)
              (variables_struct initial-gravity initial-momentum initial-scroll-speed)))

;TODO 2
; După aceasta, implementați un getter care extrage din structura voastră
; pasărea, și un al doilea getter care extrage din structura pasăre
; y-ul curent pe care se află această.
(define (get-bird state)
  (game_state-f_bird state))
(define (get-bird-y bird)
  (flappy_bird-y bird))

;TODO 3
; Trebuie să implementăm logică gravitației. next-state-bird va primi drept
; parametri o structură de tip pasăre, și gravitația(un număr real). Aceasta va adaugă
; pozitiei pe y a păsării viteza acesteia pe y, si va adaugă vitezei pe y a păsării,
; gravitația.
(define (next-state-bird bird gravity)
  (struct-copy flappy_bird bird [y (+ (get-bird-y bird) (get-bird-v-y bird))] [v_y (+ (get-bird-v-y bird) gravity)]))

;TODO 4
; După aceasta, implementati un getter care extrage din structura voastră
; viteza pe y a păsării.
(define (get-bird-v-y bird)
  (flappy_bird-v_y bird))

;TODO 6
; Dorim să existe un mod prin care să imprimăm păsării un impuls.
; Definiți funcția next-state-bird-onspace care va primi drept parametri
; o structură de tip pasăre, momentum(un număr real), și va schimba viteza
; pe y a păsării cu -momentum.
(define (next-state-bird-onspace bird momentum)
  (struct-copy flappy_bird bird [v_y (- 0 momentum)]))

; Change
; Change va fi responsabil de input-ul de la tastatură al jocului.
;TODO 7
; Acesta va primi drept parametri o structură de tip stare, și tasta pe
; care am apăsat-o. Aceasta va imprimă păsării momentum-ul, apelând
; funcția next-state-bird-onspace. Pentru orice altă tasta, starea rămâne aceeași.
(define (change current-state pressed-key)
  (match-let ([(game_state f_bird _ _ _ _) current-state])
    (cond [(key=? pressed-key " ")  (struct-copy game_state current-state [f_bird (next-state-bird-onspace f_bird initial-momentum)])]
          [else current-state])))

;TODO 9
; După ce ați definit structurile pentru mulțimea de pipes și pentru un singur pipe,
; implementați getterul get-pipes, care va extrage din starea jocului mulțimea de pipes,
; sub formă de lista.
(define (get-pipes state)
  (game_state-pipes state))

;TODO 10
; Implementați get-pipe-x ce va extrage dintr-o singură structura de tip pipe, x-ul acesteia.
(define (get-pipe-x pipe)
  (game_pipe-x pipe))

;TODO 11
; Trebuie să implementăm logica prin care se mișcă pipes.
; Funcția move-pipes va primi drept parametri mulțimea pipe-urilor din stare
; și scroll-speed(un număr real). Aceasta va scădea din x-ul fiecărui pipe
; scroll-speed-ul dat.
(define (move-pipes pipes scroll-speed)
  (let ([f (lambda(pipe) (struct-copy game_pipe pipe [x (- (get-pipe-x pipe) scroll-speed)]))])
    (map f pipes)))

;TODO 12
; Vom implementa logica prin care pipe-urile vor fi șterse din stare. În momentul
; în care colțul din DREAPTA sus al unui pipe nu se mai află pe ecran, acesta trebuie
; șters.
; Funcția va primi drept parametru mulțimea pipe-urilor din stare.
;
; Hint: cunoaștem lățimea unui pipe, pipe-width
(define (clean-pipes pipes)
  (let ([f (lambda(pipe) (> (+ (get-pipe-x pipe) pipe-width) 0))])
    (filter f pipes)))


;TODO 13
; Vrem să avem un sursa continuă de pipe-uri.
; Implementati funcția add-more-pipes, care va primi drept parametru mulțimea pipe-urilor
; din stare și, dacă avem mai puțin de no-pipes pipe-uri, mai adăugăm una la mulțime,
; având x-ul egal cu pipe-width + pipe-gap + x-ul celui mai îndepărtat pipe, în raport
; cu pasărea.
(define (add-more-pipes pipes)
  (if (< (length pipes) no-pipes)
      (append pipes (list (game_pipe (+ (+ pipe-width pipe-gap) (get-pipe-x (last pipes))) (+ added-number (random random-threshold)))))
      pipes))

;TODO 14
; Vrem ca toate funcțiile implementate anterior legate de pipes să fie apelate
; de către next-state-pipes.
; Aceasta va primi drept parametri mulțimea pipe-urilor și scroll-speed-ul,
; și va apela cele trei funcții implementate anterior, în această ordine:
; move-pipes, urmat de clean-pipes, urmat de add-more pipes.
(define (next-state-pipes pipes scroll-speed)
  (add-more-pipes (clean-pipes (move-pipes pipes scroll-speed))))

;TODO 17
; Creați un getter ce va extrage scorul din starea jocului.
(define (get-score state)
  (game_state-score state))

;TODO 19
; Vrem să creăm logica coliziunii cu pământul.
; Implementati check-ground-collision, care va primi drept parametru
; o structura de tip pasăre, și returnează true dacă aceasta are coliziune
; cu pământul.
;
; Hint: știm înălțimea păsării, bird-height, și y-ul pământului, ground-y.
; Coliziunea ar presupune ca un colț inferior al păsării să aibă y-ul
; mai mare sau egal cu cel al pământului.
(define (check-ground-collision bird)
  (if (< ground-y (+ (get-bird-y bird) bird-height))
      #t
      #f))

; invalid-state?
; invalid-state? îi va spune lui big-bang dacă starea curentă mai este valida,
; sau nu. Aceasta va fi validă atât timp cât nu avem coliziuni cu pământul
; sau cu pipes.
; Aceasta va primi ca parametru starea jocului.

;TODO 20
; Vrem să integrăm verificarea coliziunii cu pământul în invalid-state?.

;TODO 22
; Odată creată logică coliziunilor dintre pasăre și pipes, vrem să integrăm
; funcția nou implementată în invalid-state?.
(define (invalid-state? state)
  (match-let ([(game_state f_bird pipes _ _ _) state])
    (or (check-ground-collision f_bird)
        (check-pipe-collisions f_bird pipes))
    ))

;TODO 21
; Odată ce am creat pasărea, pipe-urile, scor-ul și coliziunea cu pământul,
; următorul pas este verificarea coliziunii dintre pasăre și pipes.
; Implementati funcția check-pipe-collisions care va primi drept parametri
; o structură de tip pasăre, mulțimea de pipes din stare, și va returna
; true dacă există coliziuni, și false în caz contrar. Reiterând,
; fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap. Pot există
; coliziuni doar între pasăre și cele două părți. Dacă pasărea se află în
; chenarul lipsă, nu există coliziune.
;
; Hint: Vă puteți folosi de check-collision-rectangle, care va primi drept parametri
; colțul din stânga sus și cel din dreapta jos ale celor două dreptunghiuri
; pe care vrem să verificăm coliziunea.
(define (check-pipe-collisions bird pipes)
  (match-let ([(flappy_bird x y _) bird])
    (let* ([A1 (make-posn x y)]
           [A2 (make-posn (+ x bird-width) (+ y bird-height))]
           [f (lambda(pipe)
                (match-let ([(game_pipe x gap_y) pipe])
                  (let* ([B11 (make-posn x (- gap_y pipe-height))]
                         [B21 (make-posn (+ x pipe-width) gap_y)]
                         [B12 (make-posn x (+ gap_y pipe-self-gap))]
                         [B22 (make-posn (+ x pipe-width) (+ gap_y pipe-self-gap pipe-height))])
                    (or (check-collision-rectangles A1 A2 B11 B21)
                        (check-collision-rectangles A1 A2 B12 B22)))))])
      (if (null? (filter f pipes))
          #f
          #t))))

(define (check-collision-rectangles A1 A2 B1 B2)
  (match-let ([(posn AX1 AY1) A1]
              [(posn AX2 AY2) A2]
              [(posn BX1 BY1) B1]
              [(posn BX2 BY2) B2])
    (and (< AX1 BX2) (> AX2 BX1) (< AY1 BY2) (> AY2 BY1))))

;Next-state
; Next-state va fi apelat de big-bang la fiecare cadru, pentru a crea efectul de
; animație. Acesta va primi ca parametru o structură de tip stare, și va întoarce
; starea corespunzătoare următorului cadru.

;TODO 5
; Trebuie să integrăm funcția implementată anterior, și anume next-state-bird,
; în next-state.

;TODO 15
; Vrem să implementăm logică legată de mișcarea, ștergerea și adăugarea pipe-urilor
; în next-state. Acesta va apela next-state-pipes pe pipe-urile din starea curentă.

;TODO 18
; Vrem ca next-state să incrementeze scorul cu 0.1 la fiecare cadru.
(define (next-state state)
  (let ([bird (get-bird state)]
        [cur_pipes (get-pipes state)]
        [cur_score (get-score state)]
        [cur_abilities (get-abilities state)]
        [cur_variables (get-variables state)])
    (struct-copy game_state state
                 [f_bird (next-state-bird bird initial-gravity)]
                 [pipes (next-state-pipes cur_pipes (get-variables-scroll-speed cur_variables))]
                 [score (+ cur_score 0.1)]
                 [abilities (next-abilities cur_abilities bird (get-variables-scroll-speed cur_variables))]
                 [variables (next-variables (variables_struct initial-gravity initial-momentum initial-scroll-speed) (get-abilities-active cur_abilities))])))

; draw-frame
; draw-frame va fi apelat de big-bang dupa fiecare apel la next-state, pentru a afisa cadrul curent.
;TODO 23
; Fiecare cadru va fi desenat in urmatorul mod:
; bird peste ground, peste scor, peste pipes, peste empty-scene.
;
; Hint: score-to-image primeste un numar real si intoarce scor-ul sub forma de imagine;
; Scor-ul îl puteți plasa direct la coordonatele date, fără a mai face translatiile menționate mai jos.
; Noi tinem minte coltul din stanga sus al imaginii, insa, la suprapunerea unei imagini A peste o alta imagine,
; coordonatele unde plasam imaginea A reprezinta centrul acesteia. Trebuie facute translatiile de la coltul din stanga
; sus la centrul imaginilor.
; Variabile folosite in aceasta functie:
; bird -> bird-width si bird-height
; ground -> ground-y si ground-height, acesta va acoperi intreaga latime a ecranului
; scor -> text-x si text-y
; pipes -> pipe-width si pipe-height
(define bird-image (rectangle bird-width bird-height  "solid" "yellow"))
(define ground-image (rectangle scene-width ground-height "solid" "brown"))
(define initial-scene (rectangle scene-width scene-height "solid" "white"))

(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))
(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))

(define (draw-ground scene)
  (place-image ground-image
               (quotient scene-width 2)
               (+ (quotient ground-height 2)ground-y)
               scene))

(define (draw-bird bird scene)
  (match-let ([(flappy_bird x y _) bird])
      (place-image bird-image
                   (+ x (quotient bird-width 2))
                   (+ y (quotient bird-height 2))
                   scene)))

(define (draw-score score scene)
  (place-image (score-to-image score)
               text-x
               text-y
               scene))

(define (draw-frame state)
  (match-let ([(game_state f_bird pipes score abilities _) state])
    (draw-bird f_bird (place-active-abilities (get-abilities-active abilities) (draw-ground (draw-score score (place-pipes pipes (place-visible-abilities (get-abilities-visible abilities) initial-scene))))))))

; Folosind `place-image/place-images` va poziționa pipe-urile pe scenă.
(define pipe-image (rectangle pipe-width pipe-height "solid" "green"))

(define (draw-upper-pipe pipe scene)
  (match-let ([(game_pipe x gap_y) pipe])
    (place-image pipe-image
                 (+ x (quotient pipe-width 2))
                 (- gap_y (quotient pipe-height 2))
                 scene)))

(define (draw-lower-pipe pipe scene)
  (match-let ([(game_pipe x gap_y) pipe])
    (place-image pipe-image
                 (+ x (quotient pipe-width 2))
                 (+ gap_y pipe-self-gap (quotient pipe-height 2))
                 scene)))


(define (place-pipes pipes scene)
  (if (null? pipes)
      scene
      (let ([pipe (car pipes)])
        (place-pipes (cdr pipes) (draw-lower-pipe pipe (draw-upper-pipe pipe scene))))))

; Bonus
; Completați abilities.rkt mai întâi, aceste funcții căt, apoi legați
; această funcționalitate la jocul inițial.

; Abilitatea care va încetini timpul va dura 10 de secunde, va avea imaginea (hourglass "mediumseagreen")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = max(5, scroll-speed - 1)
(define (slow-down-scroll variables)
  (struct-copy variables_struct variables [scroll_speed (max 5 (- (get-variables-scroll-speed variables) 1))]))

(define slow-ability (ability_struct (hourglass "mediumseagreen") 10 null slow-down-scroll))

; Abilitatea care va accelera timpul va dura 30 de secunde, va avea imaginea (hourglass "tomato")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = scroll-speed + 1
(define (speed-up-scroll variables)
  (struct-copy variables_struct variables [scroll_speed (+ (get-variables-scroll-speed variables) 1)]))

(define fast-ability (ability_struct (hourglass "tomato") 30 null speed-up-scroll))

; lista cu toate abilităţile posibile în joc
(define ABILITIES (list fast-ability slow-ability))


(define (get-variables state) (game_state-variables state))
(define (get-variables-gravity variable) (variables_struct-gravity variable))
(define (get-variables-momentum variable) (variables_struct-momentum variable))
(define (get-variables-scroll-speed variable) (variables_struct-scroll_speed variable))

; Întoarce abilităţile din stare, cu o reprezentare
; intermediară care trebuie să conțină două liste:
;  - lista abilităţilor vizibile (încarcate în scenă dar nu neaparat vizibile pe ecran).
;  - lista abilităţilor activate (cu care pasărea a avut o coloziune).
(define (get-abilities state) (game_state-abilities state))

; Întoarce abilităţile vizibile din reprezentarea intermediară.
(define (get-abilities-visible abilities) (abilities_struct-visible abilities))

; Întoarce abilităţile active din reprezentarea intermediară.
(define (get-abilities-active abilities) (abilities_struct-active abilities))

; Șterge din reprezentarea abilităţilor vizibile pe cele care nu mai sunt vizibile.
; echivalent cu clean-pipes.
(define (clean-abilities abilities)
  (let ([f (lambda(ability) (> (+ (posn-x (get-ability-pos ability)) 20) 0))])
    (filter f abilities)))


; Muta abilităţile vizibile spre stanga.
; echivalent cu move-pipes.
(define (move-abilities abilities scroll-speed)
  (let ([f (lambda(ability) (struct-copy ability_struct ability [pos (make-posn (- (posn-x (get-ability-pos ability)) scroll-speed) (posn-y (get-ability-pos ability)))]))])
    (map f abilities)))


; Scurge timpul pentru abilităţile activate și le sterge pe cele care au expirat.
; Puteți să va folosiți de variabila globală fps.
(define (time-counter abilities)
  (let ([f (lambda(ability) (> (get-ability-time ability) 0))]
        [g (lambda(ability) (struct-copy ability_struct ability [time (- (get-ability-time ability) (/ 1 fps))]))])
    (filter f (map g abilities))))

; Generează următoarele abilitați vizibile.
; *Atentie* La orice moment pe scena trebuie să fie exact DISPLAYED_ABILITIES
; abilităţi vizibile
; Folosiți funcția fill-abilities din abilities.rkt cât si cele scrise mai sus:
; move-abilities, clean-abilities, time-counter, etc..
(define (next-abilities-visible visible scroll-speed)
  (fill-abilities (clean-abilities (move-abilities visible scroll-speed)) DISPLAYED_ABILITIES ABILITIES))

; Generează structura intermediară cu abilități.
; Observați ca nu există next-abilities-active aceastea sunt acele abilităti
; întoarse next-abilities-visible care au o coliziune cu pasărea.
; Puteti folosi `filer`/`filter-not` ca sa verificați ce abilităti au și abilitați
; nu au coliziuni cu pasărea sau puteti folosi `partition`
(define (ability-colision ability bird)
  (match-let ([(flappy_bird x y _) bird]
              [(posn a_x a_y) (get-ability-pos ability)])
    (let* ([A1 (make-posn x y)]
           [A2 (make-posn (+ x bird-width) (+ y bird-height))]
           [B1 (make-posn (- a_x 20) (- a_y 20))]
           [B2 (make-posn (+ a_x 20) (+ a_y 20))])
      (check-collision-rectangles A1 A2 B1 B2))))

(define (next-abilities abilities bird scroll-speed)
  (let ([f (lambda(ability) (not (ability-colision ability bird)))]
        [g (lambda(ability) (ability-colision ability bird))]
        [cur_visible (next-abilities-visible (get-abilities-visible abilities) scroll-speed)])
    (abilities_struct (filter f cur_visible)
                      (time-counter (append (get-abilities-active abilities) (filter g cur_visible))))))

; Dând-use variabilele actuale și abilitațile calculați care vor
; variabile finale folosite în joc
; Folositi compose-abilities
; Atenție când apelați `next-variables` în next-state dați ca paremetru
; initial-variables și nu variabilele aflate deja în stare
; In felul acesta atunci când
(define (next-variables variables abilities)
  ((compose-abilities abilities) variables))


; Folosind `place-image/place-images` va poziționa abilităţile vizibile la ability pos.
(define (place-cur-ability ability scene)
  (match-let ([(posn x y) (get-ability-pos ability)])
    (place-image (get-ability-image ability) x y scene)))

(define (place-visible-abilities abilities scene)
  (foldr (lambda (ability cur_scene) (place-cur-ability ability cur_scene)) scene abilities))

; Folosind `place-image/place-images` va poziționa abilităţile active
; în partea de sus a ecranului lângă scor.
; Imaginiile vor scalate cu un factor de 0.75 și așezate plecând
; de la ability-posn (constantă globală) cu spații de 50 de px.
; Imaginea cu indexul i va fi așezată la (ability-posn.x - 50*i, ability-posn.y)
(define (place-active-abilities abilities scene)
  (if (null? abilities)
      scene
      (let loop ([ab_list abilities]
             [cntr 0]
             [cur_scene scene])
            (if (null? ab_list)
                cur_scene
                (loop (cdr ab_list) (add1 cntr) (place-image (scale 0.75 (get-ability-image (car ab_list)))
                                                             (- (posn-x abilities-posn) (* 50 cntr))
                                                             (posn-y abilities-posn)
                                                             cur_scene))))))

(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))
