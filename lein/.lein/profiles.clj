{:user {:dependencies
        [[com.cemerick/pomegranate "0.4.0"]
         [io.aviso/pretty "0.1.37"]            ;formatting of exceptions
         ]

        :plugins
        [[luminus/lein-template "4.14"]
         [lein-pprint "1.3.2"]               ;pretty print lein dependencies
         [io.aviso/pretty "0.1.37"]          ;make stack traces pretty
         [lein-ancient "0.6.15"]             ;check dependencies
         [chestnut/lein-template "0.18.0"]
         ]

        :middleware
        [io.aviso.lein-pretty/inject         ;starts pretty during REPL start-up
         ]

        :injections
        [(require '[clojure.pprint :as pp])
         (require '[clojure.reflect :as reflect])
         (require '[clojure.java.javadoc :as jdoc])
         ;colours for stack traces in the REPL
         (require 'io.aviso.repl)
         (defn add-dependency [dep-vec]
           (require 'cemerick.pomegranate)
           ((resolve 'cemerick.pomegranate/add-dependencies)
            :coordinates [dep-vec]))
         (defn jref [o]
           (let [type (if (= (type o) "java.lang.Class") o
                          (type o))]
             (->> (reflect/reflect type)
                  :members (sort-by :name)
                  (pp/print-table [:name :flags :parameter-types :return-type]))))]

        :repositories [["tencent" "https://mirrors.cloud.tencent.com/nexus/repository/maven-public"]]
        :plugin-repositories ^:replace [["tsing-clojars-pl" "https://mirrors.tuna.tsinghua.edu.cn/clojars"
                                         "hw-central-pl" "https://mirrors.huaweicloud.com/repository/maven/"]]
        :mirrors {"central" {:name "hw-central"
                             :url "https://mirrors.huaweicloud.com/repository/maven/"}
                  #"clojars" {:name "tsinghua-clojars"
                              :url "https://mirrors.tuna.tsinghua.edu.cn/clojars"}}
        }}
