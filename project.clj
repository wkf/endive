(defproject endive "0.1.0"
  :description "Selector based CSS templating"
  :url "FIXME: write description"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :test-paths ["src/test"]
  :source-paths ["src/main"]
  :resource-paths ["resources/main"]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [net.sourceforge.cssparser/cssparser "0.9.16"]
                 [garden "1.2.5"]]
  :profiles {:dev
             {:resource-paths ["resources/dev"]}})
