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

(deftest recursive-form
  (let [testfn (conform '(grammar form
                             basic (or sym map vec set)
                             quoted (list 'quote any)
                             form (or literal basic quoted (list basic (* form)))))]
    (is (testfn 'int))
    (is (testfn '(int)))
    (is (not (testfn '(42))))
    (is (testfn '(vec sym (map kw int) (int 42))))))
