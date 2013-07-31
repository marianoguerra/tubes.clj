(defproject org.marianoguerra/tubes "0.2.4-SNAPSHOT"
  :description "make ring based web apps by composing small functions"
  :url "http://github.com/marianoguerra/tubes.clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.marianoguerra/pipe "0.1.4"]
                 [org.marianoguerra/rate-meter "0.1.0"]
                 [org.clojure/clojure "1.5.1"]
                 [cheshire "5.2.0"] 
                 [ring-mock "0.1.5"]
                 [ring/ring-core "1.2.0"]
                 [com.cemerick/friend "0.1.5"]
                 [bigml/closchema "0.4"]])
