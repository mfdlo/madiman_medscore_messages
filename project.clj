(defproject medscore-msg "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :repositories [["local_repo"]]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [simplenlg-it "1"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [random-seed "1.0.0"]
                 ]
  :source-paths      ["src"];["src/clojure"]
  :main ^:skip-aot medscore-msg.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
