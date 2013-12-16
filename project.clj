(defproject com.velisco/herbert "0.5.15-SNAPSHOT"
  :description "A schema for edn"
  :url "https://github.com/miner/herbert"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.velisco/tagged "0.3.4"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [reiddraper/simple-check "0.5.3"]
                 [squarepeg "0.6.1"]]
  :profiles {:snapshot {:dependencies [[org.clojure/clojure "1.6.0-master-SNAPSHOT"]]}
             :alpha {:dependencies [[org.clojure/clojure "1.6.0-alpha3"]]}
             :test {:dependencies [[reiddraper/simple-check "0.5.3"]]} }
  :repositories [["sonatype-public" {:url
  "https://oss.sonatype.org/content/groups/public/"}]])

