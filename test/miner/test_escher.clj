(ns miner.test-escher
  (:use miner.herbert
        clojure.test))


(deftest schema-conformation
  (is (conforms? '(grammar (list 'grammar pattern (* term pattern)) 
                          term sym 
                          pattern any)
                 '(grammar [person+]
                          name (and str (not (pred clojure.string/blank?)))
                          handle (str "@.+") 	
                          person {:first name :last name :twitter? handle}))))

(deftest quantified
  (let [testfn (conform '(grammar (or (list (or '? '* '+) pat+) 
                                     (list (sym "\\w.*[?*+]") pat*) 
                                     (sym "\\w.*[?*+]"))
                                 pat any))]
    (is (testfn 'int*))
    (is (testfn 'int?))
    (is (testfn 'int+))
    (is (testfn '(* int)))
    (is (testfn '(+ int float)))
    (is (testfn '(? int)))
    (is (not (testfn '(*? int))))
    (is (testfn '(int* 42)))
    (is (not (testfn '(int 42))))))

(deftest simple-seq
  (let [testfn (conform '(grammar form
                                 basic (or literal sym map vec set)
                                 quoted (list 'quote basic)
                                 complex list
                                 form (or basic quoted (list basic (* (or basic complex))))))]
    (is (testfn 'int))
    (is (testfn '(int)))
    (is (testfn '(vec sym (map kw int))))))


;; SEM can't handle recursive rules
;; but should throw an error (with useful message) instead of failing silently
#_ (deftest recursive-form
  (let [testfn (conform '(grammar form
                                 basic (or literal sym map vec set)
                                 quoted (list 'quote any)
                                 form (or basic quoted (list basic (* form)))))]
    (is (testfn 'int))
    (is (testfn '(int)))
    (is (testfn '(vec sym (map kw int))))))



(deftest non-recursive-form
  (let [testfn (conform '(grammar form
                                 basic (or literal sym map vec set list)
                                 quoted (list 'quote any)
                                 simple (or basic quoted)
                                 listed (list basic simple*)
                                 complex (list basic (* (or simple listed)))
                                 form (or simple listed complex)))]
    (is (testfn 'int))
    (is (testfn '(int)))
    (is (testfn '(vec sym (map kw int))))))
