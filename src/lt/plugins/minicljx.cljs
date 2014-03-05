(ns lt.plugins.minicljx
  (:require [lt.plugins.clojure :as lt-clj]
            [cljs.reader :as reader]
            [lt.util.cljs :refer [->dottedkw str-contains?]]
            [lt.object :as object]
            [lt.objs.eval :as eval]
            [lt.objs.editor :as ed]
            [lt.objs.clients :as clients])
  (:require-macros [lt.macros :refer [behavior]]))

(defn get-transform [code ruleset]
  (print "transforming " code)
  (-> "(require '[cljx.core] '[cljx.rules]) (cljx.core/transform %CODE% cljx.rules/%RULESET%-rules)"
      (clojure.string/replace "%RULESET%" (name ruleset))
      (clojure.string/replace "%CODE%" (pr-str code))))

(behavior ::on-eval
          :triggers #{:eval :eval.one}
          :reaction (fn [editor]
                      (object/raise minicljx-lang
                                    :eval!
                                    {:origin editor
                                     :info (assoc
                                             (@editor :info)
                                             :code
                                             (ed/->val (:ed @editor)))})))

(behavior ::eval!
          :triggers #{:eval!}
          :reaction (fn [this event]
                      (let [{:keys [info origin]} event
                            command :editor.eval.clj
                            client (eval/get-client! {:command command
                                                      :origin origin
                                                      :info info
                                                      :create lt-clj/try-connect})]
                        (clients/send client command
                                      (assoc info
                                        :code (get-transform (:code info) :clj)
                                        :meta {::type :clj})
                                      :only origin)
                        #_ (clients/send client command
                                      (assoc info
                                        :code (get-transform (:code info) :cljs)
                                        :meta {::type :cljs})
                                      :only origin))))

(behavior ::on-precompile-result
          :triggers #{:editor.eval.clj.result}
          :reaction (fn [obj res]
                      (if-let [result-type (-> res :meta ::type)]
                        (let [editor-tag (->dottedkw :editor result-type)
                              command (->dottedkw :editor.eval result-type)
                              processed-code (->> (:results res)
                                                  (map :result)
                                                  (map reader/read-string)
                                                  (apply str))
                              info (assoc (:info @obj) :code processed-code)]
                          (object/add-tags obj [editor-tag])
                          (clients/send (eval/get-client! {:command command
                                                           :info info
                                                           :origin obj
                                                           :create lt-clj/try-connect})
                                        command info :only obj)
                        #_ (object/remove-tags obj [:editor.clj :editor.cljs])))))

(object/object* ::minicljx-lang
                :tags #{}
                :behaviors [::eval!]
                :triggers #{:eval!})

(def minicljx-lang (object/create ::minicljx-lang))
