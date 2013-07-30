(defproject org.marianoguerra/tubes "0.2.3"
  :description "make ring based web apps by composing small functions"
  :url "http://github.com/marianoguerra/tubes.clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.marianoguerra/pipe "0.1.4"]
                 [org.marianoguerra/rate-meter "0.1.0"]
                 [org.clojure/clojure "1.5.1"]
                 [cheshire "5.0.1"] 
                 [ring-mock "0.1.3"]
                 [ring/ring-core "1.1.8"]
                 [com.cemerick/friend "0.1.4"]
                 [bigml/closchema "0.3.1"]])
