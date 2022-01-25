;----------------------------------------------------------------------------
;ABY URUCHOMIC SYSTEM EKSPERCKI:
;file->load->film.clp
;Nastepnie Execution->Reset->'Tak' i potem Execution->Run
;Uzytkownik odpowiada na zadane pytania i otrzymuje dobrany film :)
;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
;ZDEFINIOWANE KLASY SYSTEMU EKSPERCKIEGO
;----------------------------------------------------------------------------

(defclass PERSON
	(is-a USER)
	(role concrete)
	(slot fear)
	(slot fantasy)
	(slot companion)
	(slot explore)
	(slot mood))

(defclass film
	(is-a USER)
	(role concrete)
	(slot studio)
	(slot genre)
	(slot year)
	(slot suggested_film)
	(slot type))
	
;----------------------------------------------------------------------------
; DOMYŚLNE INSTANCJE SYSTEMU EKSPERCKIEGO
;----------------------------------------------------------------------------

(definstances PERSON-INSTANCES
	(client of PERSON))

(definstances film-INSTANCES
	(which_film of film))
	
;----------------------------------------------------------------------------
;WSTĘPNE DANE WEJŚCIOWE ORAZ WYBRANIE TYPU UŻYTKOWNIKA
;----------------------------------------------------------------------------

(deffunction user-input-validation (?question $?valid-input)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?valid-input)) do
      (printout t "Please enter a valid input as mentioned in the question!" crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)
   
(defrule GetCompanion(declare (salience 10))
    =>
    (printout t crlf)
    (printout t "------------------------------------WSZIB------------------------------------" crlf)
    (printout t "------------ SYSTEM EKSPERCKI DO WYBORU ODPOWIEDNIEGO FILMU -----------------" crlf)
    (printout t "--------------------PAWEL MALEC I MILOSZ DARECKY 2020------------------------" crlf)
    (printout t crlf)    
    (send [client] put-companion
    (user-input-validation "Z kim planujesz ogladac film? (rodzina/partner/sam):  "
   		rodzina partner sam)))

;----------------------------------------------------------------------------
;REGUŁY SYSTEMU EKSPERCKIEGO DO WYBORU ODPOWIEDNIEGO FILMU
;----------------------------------------------------------------------------

; rodzina
(defrule watch_with_rodzina
	?ins <- (object (is-a PERSON) (companion rodzina))
	=> 
	(printout t crlf)
	(printout t "Pozwol mi wybrac film odpowiedni do ogladania z twoja rodzina..." crlf crlf)
   	(send [which_film] put-genre
    (user-input-validation "Preferowany gatunek (animacja/fantasy/horror/przygoda):  "
    		animacja fantasy horror przygoda)))
   	 
; sam
(defrule watch_sam
	?ins <- (object (is-a PERSON) (companion sam))
	=> 
	(printout t crlf)
	(printout t "Pozwol, ze wybiore film, ktorym bedziesz mogl cieszyc sie sam..." crlf crlf)
    (send [which_film] put-type
  	(user-input-validation "Czy chcesz ogladnac serial telewizyjny czy film? (tv/film): " 
  		tv film)))
   	 
; partner
(defrule watch_with_partner
	?ins <- (object (is-a PERSON) (companion partner))
	=> 
	(printout t crlf)
	(printout t "Pozwol mi wybrac film odpowiedni do obejrzenia z Twoim partnerem/erka..." crlf crlf)
	(send [which_film] put-year
    (user-input-validation "Czy chesz ogladnac nowy czy stary film? (nowy/stary): "
         nowy stary)))
   	 
; sam tv
(defrule watch_tv
	(and ?ins <- (object (is-a film) (type tv))
	(object (is-a PERSON)(companion sam)))
	=> 
	(printout t crlf)
	(printout t "Pozwol mi wybrac serial relewizyjny, ktory mozesz ogladac sam..." crlf crlf)
	(send [which_film] put-genre
    (user-input-validation "Podaj preferowany gatunek (komedia/akcja/thriller):  "
         komedia akcja thriller)))
   	
; partner nowy film
(defrule watch_nowy_films
	(and ?ins <- (object (is-a film) (year nowy))
	(object (is-a PERSON)(companion partner)))
	=> 
	(printout t crlf)
	(printout t "Pozwol mi wybrac nowy film do obejrzenia z Twoim partnerem..." crlf crlf)
   	(send [which_film] put-genre
   	(user-input-validation "Podaj preferowany gatunek (kryminal/komedia/dramat):  "
         kryminal komedia dramat)))

; rodzina animowany
(defrule watch_animated_films_with_rodzina
	(and ?ins <- (object (is-a PERSON) (companion rodzina))
	(object (is-a film) (genre animacja)))
	=> 
	(printout t crlf)
	(printout t "Pozwol wybrac mi film animowany, ktory mozesz obejrzec z rodzina..." crlf crlf)
   	(send [which_film] put-studio
   	(user-input-validation "Wybierz Twoje ulubione studio (disney/pixar/inne): "
         disney pixar inne)))

; rodzina fantasy
(defrule watch_fantasy_films_with_rodzina
	(and ?ins <- (object (is-a PERSON) (companion rodzina))
	(object (is-a film) (genre fantasy)))
	=> 
	(printout t crlf)
	(printout t "Pozwol mi wybrac film fantasy, ktory mozesz obejrzec z rodzina..." crlf crlf)
   	(send [client] put-fantasy
   	(user-input-validation "Jaki preferujesz typ fantasy? (magia/potwory/duchy):  "
         magia potwory duchy)))
   	 
; rodzina horror
(defrule watch_horror_films_with_rodzina
	(and ?ins <- (object (is-a PERSON) (companion rodzina))
	(object (is-a film) (genre horror)))
	=> 
	(printout t crlf)
	(printout t "Pozwol mi wybrac horror, ktory mozesz obejrzec z rodzina..." crlf crlf)   	(send [client] put-fear
   	(user-input-validation "Czego boisz sie najbardziej? (zabojcy/duchy/zombie):  "
         zabojcy duchy zombie)))
 
; rodzina przygoda
(defrule watch_przygoda_films_with_rodzina
	(and ?ins <- (object (is-a PERSON) (companion rodzina))
	(object (is-a film) (genre przygoda)))
	=> 
	(printout t crlf)
	(printout t "Pozwol mi wybrac film przygodowy, ktory mozesz obejrzec z rodzina..." crlf crlf) 
   	(send [client] put-explore
   	(user-input-validation "Co chcialbys zwiedzic? (dzungla/kosmos/ocean):  "
         dzungla kosmos ocean)))
 
; rodzina disney
(defrule watch_disney_films
	(and ?ins <- (object (is-a film) (studio disney))
	(object (is-a PERSON) (companion rodzina)))
	=> 
	(send ?ins put-suggested_film "Krol Lew 2019"))

; rodzina pixar
(defrule watch_pixar_films
	(and ?ins <- (object (is-a film) (studio pixar))
	(object (is-a PERSON) (companion rodzina)))
	=> 
	(send ?ins put-suggested_film "Toy Story 4 2019"))

; rodzina inne
(defrule watch_inne
	(and ?ins <- (object (is-a film) (studio inne))
	(object (is-a PERSON) (companion rodzina)))
	=> 
	(send ?ins put-suggested_film "Pradawny Lad 1988"))

; magia fantasy rodzina
(defrule watch_fantasy_magia
	(and ?ins <- (object (is-a PERSON) (fantasy magia))
	(object (is-a film) (genre fantasy)))
	=> 
	(send [which_film] put-suggested_film "Fantastyczne zwierzeta i jak je znalezc 2016"))
	
; potwory fantasy rodzina
(defrule watch_fantasy_potwory
	(and ?ins <- (object (is-a PERSON) (fantasy potwory))
	(object (is-a film) (genre fantasy)))
	=> 
	(send [which_film] put-suggested_film "Hotel Transylwania 3 2018"))
	
; duchy fantasy rodzina
(defrule watch_fantasy_duchy 
	(and ?ins <- (object (is-a PERSON) (fantasy duchy))
	(object (is-a film) (genre fantasy)))
	=> 
	(send [which_film] put-suggested_film "Kacper 1995"))

; zabojca horror rodzina
(defrule watch_fear_zabojcy 
	(and ?ins <- (object (is-a PERSON) (fear zabojcy))
	(object (is-a film) (genre horror)))
	=> 
	(send [which_film] put-suggested_film "Teksanska masakra pila mechaniczna 1974"))
	
; duchy horror rodzina
(defrule watch_fear_duchy  
	(and ?ins <- (object (is-a PERSON) (fear duchy))
	(object (is-a film) (genre horror)))
	=> 
	(send [which_film] put-suggested_film "Obecnosc 2013"))
	
; zombie horror rodzina
(defrule watch_fear_zombie  
	(and ?ins <- (object (is-a PERSON) (fear zombie))
	(object (is-a film) (genre horror)))
	=> 
	(send [which_film] put-suggested_film "28 dni pozniej 2002"))

; przygoda dzungla rodzina
(defrule watch_explore_dzungla 
	(and ?ins <- (object (is-a PERSON) (explore dzungla))
	(object (is-a film) (genre przygoda)))
	=> 
	(send [which_film] put-suggested_film "Tarzan: Legenda 2016"))
	
; przygoda kosmos rodzina
(defrule watch_explore_kosmos
	(and ?ins <- (object (is-a PERSON) (explore kosmos))
	(object (is-a film) (genre przygoda)))
	=> 
	(send [which_film] put-suggested_film "WALL-E 2008"))
	
; ocean przygoda rodzina
(defrule watch_explore_ocean
	(and ?ins <- (object (is-a PERSON) (explore ocean))
	(object (is-a film) (genre przygoda)))
	=> 
	(send [which_film] put-suggested_film "Gdzie jest Nemo? 2003"))
	
; stary film partner
(defrule watch_stary_films
	(and ?ins <- (object (is-a film) (year stary))
	(object(is-a PERSON)(companion partner)))
	=> 
	(send ?ins put-suggested_film "Oslawiona 1946") )

; nowy kryminal partner
(defrule watch_nowy_kryminal_film_with_partner
	(and ?ins <- (object (is-a PERSON) (companion partner))
	(object (is-a film) (genre kryminal)(year nowy)))
	=> 
	(send [which_film] put-suggested_film "Mr. & Mrs. Smith 2005"))
	
; nowa komedia partner
(defrule watch_nowy_komedia_film_with_partner
	(and ?ins <- (object (is-a PERSON) (companion partner))
	(object (is-a film) (genre komedia)(year nowy)))
	=> 
	(send [which_film] put-suggested_film "Dziewczyna z sasiedztwa 2004"))
	
; nowy dramatt partner
(defrule watch_nowy_dramat_film_with_partner
	(and ?ins <- (object (is-a PERSON) (companion partner))
	(object (is-a film) (genre dramat)(year nowy)))
	=> 
	(send [which_film] put-suggested_film "Casablanca 1942"))
	
; komedia tv
(defrule watch_tv_komedia
	(and ?ins <- (object (is-a film) (type tv)(genre komedia))
	(object (is-a PERSON) (companion sam)))
	=> 
	(send ?ins put-suggested_film "Przyjaciele 1994-2004"))
	
; tv akcja
(defrule watch_tv_akcja
	(and ?ins <- (object (is-a film) (type tv)(genre akcja))
	(object (is-a PERSON) (companion sam)))
	=> 
	(send ?ins put-suggested_film "Skazany na smierc 2005-2009"))
	
; thriller tv
(defrule watch_tv_thriller
	(and ?ins <- (object (is-a film) (type tv)(genre thriller))
	(object (is-a PERSON) (companion sam)))
	=> 
	(send ?ins put-suggested_film "Dexter 2006-2013"))
	
; film sam
(defrule watch_film_sam
	(and ?ins <- (object (is-a film) (type film))
	(object (is-a PERSON) (companion sam)))
	=> 
	(send ?ins put-suggested_film "Prestiz 2006")
	(printout t crlf)
	(printout t "Pozwol, ze wybiore film, ktorym bedziesz mogl cieszyc sie sam..." crlf)) 
	
;----------------------------------------------------------------------------
;SYSTEM PODAJE SUGEROWANY FILM	
;----------------------------------------------------------------------------

; REGULA
(defrule choose_film (declare (salience -1))
	(object (is-a film) (suggested_film ?mov))
	=>
	(printout t crlf)
	(printout t "-----------------------------------------------------------------------------" crlf)
    (printout t "Zalecany film, ktory najlepiej odpowiada Twoim potrzebom to: " ?mov crlf)
    (printout t "-----------------------------------------------------------------------------" crlf))