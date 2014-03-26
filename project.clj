(defproject com.velisco/herbert "0.6.0-SNAPSHOT"
  :description "A schema for edn"
  :url "https://github.com/miner/herbert"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.velisco/tagged "0.3.4"]
                 [com.gfredericks/re-rand "0.1.1"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [org.clojure/test.check "0.5.7"]
                 [squarepeg "0.6.1"]]
  :profiles {:snapshot {:dependencies [[org.clojure/clojure "1.6.0-master-SNAPSHOT"]]}
             :alpha {:dependencies [[org.clojure/clojure "1.6.0-alpha3"]]}
             }
  :repositories [["sonatype-public" {:url
  "https://oss.sonatype.org/content/groups/public/"}]])

