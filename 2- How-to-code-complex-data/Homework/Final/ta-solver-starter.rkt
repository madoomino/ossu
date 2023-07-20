;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ta-solver-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ta-solver-starter.rkt



;  PROBLEM 1:
;
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people.
;
;  Design a data definition for Chirper, including a template that is tail recursive and avoids
;  cycles.
;
;  Then design a function called most-followers which determines which user in a Chirper Network is
;  followed by the most people.
;

(define-struct user (name is-verfied? following-list))
;; User is (make-user String Boolean (listof User)
;; interp.  a representation for a user with name, verification status, and list of followings

(define USER1 (make-user "Mado" false empty))
(define USER2 (make-user "Mino" true (list USER1)))
(define USER3 (shared ((-A- (make-user "Complicated" true (list -B- -C-)))
                       (-B- (make-user "veryComplicated" false (list -A-)))
                       (-C- (make-user "veryVeryComplicated" true (list -A-))))
                -A-))

(define (fn-for-chirper u0)
  ;todo: (listof User); acc to be able to visit all users
  ;visited: (listof User); keeps track of the visited users to avoid cycles
  (local [(define (fn-for-u u todo visited)
            (if (member u visited)
                (fn-for-lou todo visited)
                (fn-for-lou (append (user-following-list u) todo)
                            (cons u visited))))
          ;(user-name u)
          ;(user-is-verified? u)
          ;(user-following-list u)

          (define (fn-for-lou todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-u (first todo)
                             (rest todo)
                             visited)]))]
          
    
    (fn-for-u u0 empty empty)))

(define (most-followed u0)
  ;todo: (listof User); acc to be able to visit all users
  ;visited: (listof User); keeps track of the visited users to avoid cycles
  ;uwfsn: (User Natural);  user with their followers number
  ;uwf: (listof uwfsn); a list of users with their followers number
  (local [(define-struct uwfsn (user fsn))
          (define (fn-for-u u todo visited uwf)
            (if (member u visited)
                (fn-for-lou todo visited (add1-to-user u uwf))
                (fn-for-lou (append (user-following-list u) todo)
                            (cons u visited)
                            (cons (make-uwfsn u 0) uwf))))
          ;(user-name u)
          ;(user-is-verified? u)
          ;(user-following-list u)

          (define (fn-for-lou todo visited uwf)
            (cond [(empty? todo) (max uwf)]
                  [else
                   (fn-for-u (first todo)
                             (rest todo)
                             visited)]))
          ;; User (listof uwfsn) -> (listof uwfsn)
          ;; adds 1 to the followers count
          (define (add1-to-user u0 uwf0)
            (local [(define (fn-for-uwf u uwf final)
                      (cond [(empty? uwf) final]
                            [else
                             (if (string=? (user-name (uwfsn-user (first uwf))) (user-name u))
                                 (fn-for-uwf u (cons (make-uwfsn u (add1 (uwfsn-fsn (first uwf))))
                                                     (fn-for-uwf u (rest uwf) (cons (make-uwfsn u (add1 (uwfsn-fsn (first uwf))))
                                                                                    final))))
                                 (fn-for-uwf u (rest uwf) final))]))]
              (fn-for-uwf u0 uwf0 empty)))
          ;; (listof uwfsn) -> String
          ;; produces the name of the uwfsn with max fsn 
          (define (max lou0)
            (local [(define (fn-for-lou lou max-name max-num)
                      (cond [(empty? lou) max-name]
                            [else
                             (if (>= (uwfsn-fsn (first lou)) max-num)
                                 (fn-for-lou (rest lou) (user-name (uwfsn-user (first lou)))
                                             (uwfsn-fsn (first lou)))
                                 (fn-for-lou (rest lou) max-name max-num))]))]
                     
              (fn-for-lou lou0  "" 0)))]
                      
    (fn-for-u u0 empty empty empty)))
