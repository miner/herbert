(defproject com.velisco/herbert "0.5.7"
  :description "A schema for edn"
  :url "https://github.com/miner/herbert"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [squarepeg "0.6.1"]]
  :profiles {:snapshot {:dependencies [[org.clojure/clojure "1.6.0-master-snapshot"]]}
             :alpha {:dependencies [[org.clojure/clojure "1.6.0-alpha1"]]}
             }
  :repositories [["sonatype-oss-public" {:url "https://oss.sonatype.org/content/groups/public/"}]])

