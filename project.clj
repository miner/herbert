(defproject com.velisco/herbert "0.7.0-SNAPSHOT"
  :description "A schema for edn"
  :url "https://github.com/miner/herbert"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 ;; [org.clojure/clojure "1.7.0-alpha2"]
                 [org.clojure/clojure "1.6.0"]
                 [com.velisco/tagged "0.3.4"]
                 [com.velicso/re-rand "0.1.2"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [org.clojure/test.check "0.5.9"]
                 [squarepeg "0.6.1"]]
  :profiles {:dev  {:dependencies [[org.clojure/clojure "1.7.0-alpha2"]
                                   [criterium "0.4.3"]]}
             :clj16 {:dependencies [[org.clojure/clojure "1.6.0"]
                                    [criterium "0.4.3"]]}
             ;; end :profiles
             }
  ;; end defproject
  )

