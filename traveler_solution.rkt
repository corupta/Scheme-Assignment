#lang scheme
; 2016400141
; tested using a modified version of the given database 'travelerdb.rkt'
; below is the commented version of that file:
; tests of each functions are commented below as: input => output (input returns output)
;      - - - - -   travelerdb.rkt starts   - - - - -
;(define LOCATIONS
;  '(
;    (venice 150 (milan bologna florence) (concert opera festival))
;    (milan 130 (venice bologna florence turin bern) (fashion concert))
;    (florence 130 (milan bologna venice) (concert opera festival))
;    (bologna 100 (milan florence) (concert festival))
;    (bern 150 (milan zurich) (concert festival theatre science))
;    (zurich 170 (bern munich) (concert theatre))
;    (munich 170 (zurich stuttgart frankfurt salzburg vienna) (concert festival))
;    (stuttgart 130 (munich frankfurt) (concert opera))
;    (frankfurt 130 (stuttgart frankfurt) (concert theatre))
;    (salzburg 130 (munich vienna) (concert opera theatre))
;    (vienna 170 (munich salzburg) (concert opera theatre))
;    (nice 150 (cannes monaco notidentifiedextracity) (opera theatre))
;    (cannes 170 (nice monaco) (festival theatre))
;    (monaco 170 (nice cannes) (casino concert))
;    (barcelona 130 (zaragoza mallorca ibiza) (casino concert theatre football))
;    (zaragoza 90 (barcelona madrid) (football))
;    (madrid 100 (zaragoza valencia) (theatre concert))
;    (valencia 90 (madrid mallorca) (casino concert))
;    (mallorca 170 (valencia barcelona ibiza) (festival concert))
;    (ibiza 170 (mallorca barcelona) (concert festival)))) 
;(define TRAVELERS
;  '(
;    (hans (madrid valencia) (theatre concert opera) frankfurt)
;    (sergio (barcelona milan bologna) (theatre opera) florence)
;    (john (milan barcelona valencia cannes zaragoza) (football concert fashion) monaco)
;    (sisi (madrid munich bern) (concert opera science) vienna)
;    (salma (venice bologna barcelona madrid) (theatre festival) mallorca)
;    (messi (ibiza florence nice vienna milan venice) (concert festival) barcelona)
;    (ronaldo (mallorca ibiza cannes nice) (concert opera) madrid)
;    (medici (madrid zurich) (opera) florence)
;    (james (barcelona florence bologna) (concert opera football) venice)
;    (kate (nice bern ibiza zaragoza) (football festival science) milan)
;    (hagrid (mallorca zaragoza ibiza milan) (football fashion theatre) bologna)
;    (harry (valencia cannes florence) (football concert fashion) bern)
;    (dumbledore (milan bern) (science fashion) zurich)
;    (hermione (salzburg barcelona florence cannes) (football theatre festival) munich)
;    (ron (milan cannes salzburg) (theatre fashion) stuttgart)
;    (dolores (milan cannes vienna) (festival concert) salzburg)
;    (maeve (bern monaco ibiza vienna) (opera concert science) nice)
;    (cercei (vienna barcelona valencia mallorca bologna) (football opera) cannes)
;    (arya (mallorca ibiza frankfurt munich bologna madrid) (opera concert) zaragoza)
;    (sansa (milan barcelona valencia cannes zaragoza) (football concert fashion) valencia))) 
;       - - - - -    travelerdb.rkt ends    - - - - -

(define (get-location city) ; gets the location object from the LOCATIONS, returns a list of (city 0 () ()) if city is not in database.
  (let ((result (filter (lambda (location) (equal? city (car location))) LOCATIONS)))
    (if (equal? result '()) (list city 0 '() '()) (car result))))
; (get-location 'Ankara) => (Ankara 0 () ())
; (get-location 'venice) => (venice 150 (milan bologna florence) (concert opera festival))

(define (get-traveler person) ; gets the traveler object from the TRAVELERS, returns a list of (person () () 'unknown_home_city) if person is not in database.
  (let ((result (filter (lambda (traveler) (equal? person (car traveler))) TRAVELERS)))
    (if (equal? result '()) (list person '() '() 'unknown_home_city) (car result))))
; (get-traveler 'somebody) => (somebody () () unknown_home_city)
; (get-traveler 'john) => (john (milan barcelona valencia cannes zaragoza) (football concert fashion) monaco)

(define (get-city-activities city) ; returns the activities of the given city.
  (cadddr (get-location city)))    ; gets the fourth element of the location object.
; (get-city-activities 'zurich) => (concert theatre)
; (get-city-activities 'berlin) => ()

(define (RAILWAY-CONNECTION city) ; returns the neighbour cities of the given city.
  (caddr (get-location city)))    ; gets the third element of the location object
; (RAILWAY-CONNECTION 'ibiza) => (mallorca barcelona)
; (RAILWAY-CONNECTION 'cannes) => (nice monaco)
; (RAILWAY-CONNECTION 'istanbul) => ()

(define (ACCOMMODATION-COST city) ; returns the accommodation cost of the given city.
  (cadr (get-location city)))     ; gets the second element of the location object
; (ACCOMMODATION-COST 'milan) => 130
; (ACCOMMODATION-COST 'cankiri) => 0

(define (INTERESTED-CITIES person)     ; returns the cities that the given person is interested in.
  (cadr (get-traveler person)))        ; gets the second element of the traveler object
; (INTERESTED-CITIES 'dolores) => (milan cannes vienna)
; (INTERESTED-CITIES 'messi) => (ibiza florence nice vienna milan venice)
; (INTERESTED-CITIES 'jackson) => ()

(define (INTERESTED-ACTIVITIES person) ; return the activities that the given person is interested in.
  (caddr (get-traveler person)))       ; gets the third element of the traveler object
; (INTERESTED-ACTIVITIES 'arya) => (opera concert)
; (INTERESTED-ACTIVITIES 'cercei) => (football opera)
; (INTERESTED-ACTIVITIES 'daenerys) => ()

(define (HOME person)                  ; returns the home city of the given person.
  (cadddr (get-traveler person)))      ; gets the fourth element of the traveler object
; (HOME 'dumbledore) => zurich
; (HOME 'hermione) => munich
; (HOME 'tomriddle) => unknown_home_city

(define (TRAVELER-FROM city)                                   ; returns all people (list of their names) whose home are the given city.
  (filter (lambda (person) (equal? city (HOME person)))        ; filter out travelers whose home is not city
          (map (lambda (traveler) (car traveler)) TRAVELERS))) ; list of all traveler names
; (TRAVELER-FROM 'newyork) => ()
; (TRAVELER-FROM 'venice) => (james)
; (TRAVELER-FROM 'florence) => (sergio medici)

(define (INTERESTED-IN-CITY city)                                    ; returns all people (list of their names) who are interested in the given city.
  (filter (lambda (person) (member city (INTERESTED-CITIES person))) ; filter out travelers who are not interested in the given city.
          (map (lambda (traveler) (car traveler)) TRAVELERS)))       ; list of all traveler names
; (INTERESTED-IN-CITY 'monaco) => (maeve)
; (INTERESTED-IN-CITY 'barcelona) => (sergio john salma james hermione cercei sansa)
; (INTERESTED-IN-CITY 'trabzon) => ()

(define (INTERESTED-IN-ACTIVITY activity)                                    ; returns all people (list of their names) who are interested in the given activity.
  (filter (lambda (person) (member activity (INTERESTED-ACTIVITIES person))) ; filter out travelers who are not interested in the given activity.
          (map (lambda (traveler) (car traveler)) TRAVELERS)))               ; list of all traveler names
; (INTERESTED-IN-ACTIVITY 'football) => (john james kate hagrid harry hermione cercei sansa)
; (INTERESTED-IN-ACTIVITY 'opera) => (hans sergio sisi ronaldo medici james maeve cercei arya)
; (INTERESTED-IN-ACTIVITY 'hockey) => ()

(define (railway-network-helper from-cities result)                                        ; returns all cities in result and all reachable from cities from the from-cities list
  (if (null? from-cities) result                                                           ; base for recursive definition - no more cities left in from cities
      (let ((new-cities (filter (lambda (another-city) (not (member another-city result))) ; let new-cities be those that are not reached yet from the first city of from-cities.
                                (RAILWAY-CONNECTION (car from-cities)))))                  ; (a city is not reached yet if it's not in the result list.)
        (railway-network-helper (append (cdr from-cities) new-cities)                      ; call itself for not checked from-cities (first city of that list is checked) + new cities.
                                (append result new-cities)))))                             ; append new-cities to result so that they won't be re-reached in the next recursive iteration.
; (railway-network-helper '(venice) '(venice)) => (venice milan bologna florence turin bern zurich munich stuttgart frankfurt salzburg vienna)
; (railway-network-helper '(ibiza) '(ibiza)) => (ibiza mallorca barcelona valencia zaragoza madrid)
; (railway-network-helper '(nice) '(nice)) => (nice cannes monaco notidentifiedextracity)
; (railway-network-helper '() '()) => ()
      
(define (RAILWAY-NETWORK city)                                                             ; returns all cities reachable (except from the given city) from the given city.
  (filter (lambda (another-city) (not (equal? city another-city)))                         ; get all reachable cities from the list of the given city: (city) using the above defined
          (railway-network-helper (list city) (list city))))                               ; recursive railway-network-helper function and filter out the given city from its result.
; (RAILWAY-NETWORK 'venice) => (milan bologna florence turin bern zurich munich stuttgart frankfurt salzburg vienna)
; (RAILWAY-NETWORK 'ibiza) => (mallorca barcelona valencia zaragoza madrid)
; (RAILWAY-NETWORK 'nice) => (cannes monaco notidentifiedextracity)
; (RAILWAY-NETWORK 'nowhere) => ()

(define (ACCOMMODATION-EXPENSES person city)                                         ; returns the accommodation expense of the given city for the given person
  (cond ((equal? city (HOME person)) 0)                                              ; if the city is the home of the person, the cost is zero.
        ((let* ((interested-activities (INTERESTED-ACTIVITIES person))               ; let interested-activites be the person's interested activities, and
                (city-activities (get-city-activities city)))                        ; city-activities be the hosted activities on the city.
          (foldl (lambda (activity acc) (or acc (member activity city-activities))) ; if any of the city-activities is a member of the interested-activities
                 #f interested-activities))                                          ; (fold all activities taking or of their membership where base is false) 
         (* 3 (ACCOMMODATION-COST city)))                                            ; then the person stays for 3 nights. the accommodation cost is 3 times the standard cost of the city.
        (else (ACCOMMODATION-COST city))))                                            ; else, the person stays for one night, and the cost is the standard accommodation cost of the city.
; (ACCOMMODATION-EXPENSES 'ronaldo 'cannes) => 170
; (ACCOMMODATION-EXPENSES 'messi 'barcelona) => 0
; (ACCOMMODATION-EXPENSES 'hagrid 'bern) => 450
; (ACCOMMODATION-EXPENSES 'hans 'monaco) => 510

(define (TRAVEL-EXPENSES person city)                          ; returns the travel expense of the given city for the given person
  (cond ((equal? city (HOME person)) 0)                        ; travel cost is 0 if the city is person's home
        ((member city (RAILWAY-NETWORK (HOME person))) 100) ; travel cost is 2 (go & return) * 50 (standard railway cost) if the city can be reached via railway from the person's home.
        (else 200)))                                           ; otherwise, travel cost is 2 (go & return) * 100 (standard airlines cost)
; (TRAVEL-EXPENSES 'ronaldo 'cannes) => 200
; (TRAVEL-EXPENSES 'messi 'barcelona) => 0
; (TRAVEL-EXPENSES 'hagrid 'bern) => 100
; (TRAVEL-EXPENSES 'hans 'monaco) => 200

(define (EXPENSES person city)                                            ; returns the total expense of the given city for the given person
  (+ (ACCOMMODATION-EXPENSES person city) (TRAVEL-EXPENSES person city))) ; total expense is the sum of accommodation and travel expenses.
; (EXPENSES 'ronaldo 'cannes) => 370
; (EXPENSES 'messi 'barcelona) => 0
; (EXPENSES 'hagrid 'bern) => 550
; (EXPENSES 'hans 'monaco) => 710

(define (IN-BETWEEN min-cost max-cost)                                             ; returns the list of cities whose accommodation cost are in between the given min-cost and max-cost.
  (filter (lambda (city) (let ((city-cost (ACCOMMODATION-COST city)))              ; filter to select cities of only those with accommodation cost 
                           (and (>= city-cost min-cost) (<= city-cost max-cost)))) ; greater than or equal to min-cost and less then or equal to max-cost
          (map (lambda (location) (car location)) LOCATIONS)))                     ; from the list of all city names.
; (IN-BETWEEN 10 1000) => (venice milan florence bologna bern zurich munich stuttgart frankfurt salzburg vienna nice cannes monaco barcelona zaragoza madrid valencia mallorca ibiza)
; (IN-BETWEEN 160 170) => (zurich munich vienna cannes monaco mallorca ibiza)
; (IN-BETWEEN 90 100) => (bologna zaragoza madrid valencia)
; (IN-BETWEEN 10 40) => ()
; (IN-BETWEEN 200 400) => ()