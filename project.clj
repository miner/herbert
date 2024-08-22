(defproject com.velisco/herbert "0.7.1"
  :description "A schema for edn"
  :url "https://github.com/miner/herbert"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :deploy-repositories {"releases" :clojars}
  :dependencies [[org.clojure/clojure "1.11.4"]
                 [com.velisco/tagged "0.5.0"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [org.clojure/test.check "1.1.1"]
                 [com.velisco/strgen "0.2.5"]
                 [squarepeg "0.6.1"]]  )

