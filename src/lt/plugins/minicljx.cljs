(ns lt.plugins.minicljx
  (:require [lt.plugins.clojure :as lt-clj]
            [cljs.reader :as reader]
            [lt.util.cljs :refer [->dottedkw str-contains?]]
            [lt.object :as object]
            [lt.objs.notifos :as notifos]
            [lt.objs.eval :as eval]
            [lt.objs.editor :as ed]
            [lt.objs.clients :as clients])
  (:require-macros [lt.macros :refer [behavior]]))

(defn get-transform [code ruleset]
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

(defn- process-cljx-transform-result
  "Internal helper function to process the result of a cljx transformation"
  [editor result]
  (let [result-type (-> result :meta ::type)
        editor-tag (->dottedkw :editor result-type)
        command (->dottedkw :editor.eval result-type)
        processed-code (->> (:results result)
                            (map :result)
                            (map reader/read-string)
                            (apply str))
        info (assoc (:info @editor) :code processed-code)]
    (clients/send (eval/get-client! {:command command
                                     :info info
                                     :origin editor
                                     :create lt-clj/try-connect})
                  command info :only editor)))

(defn- process-cljx-result
  "Internal helper function to process the result of executing transformed code"
  [editor result]
  (let [result-type (get-in result [:meta :result-type] :inline)]
    (object/raise editor
                  (->dottedkw :editor.eval.cljx.result result-type)
                  result)))

(behavior ::on-precompile-result
          :triggers #{:editor.eval.clj.result}
          :reaction (fn [obj res]
                      (let [process-fn (if (contains? (:meta res) ::type)
                                         process-cljx-transform-result
                                         process-cljx-result)]
                        (process-fn obj res))))

(object/object* ::minicljx-lang
                :tags #{}
                :behaviors [::eval!]
                :triggers #{:eval!})

(def minicljx-lang (object/create ::minicljx-lang))

(behavior ::cljx-result.inline
          :triggers #{:editor.eval.cljx.result.inline}
          :reaction (fn [obj res]
                      (doseq [result (:results res)
                              :let [meta (:meta result)
                                    loc {:line (-> meta :end-line dec)
                                         :ch (-> meta :end-column)
                                         :start-line (-> meta :line dec)}]]
                        (if (contains? result :stack)
                          (object/raise obj :editor.eval.cljx.exception result :passed)
                          (object/raise obj :editor.result (:result result) loc)))))

(behavior ::cljx-result.exception
          :triggers #{:editor.eval.cljx.exception}
          :reaction (fn [obj res passed?]
                      (when-not passed?
                        (notifos/done-working ""))
                      (let [loc {:line (-> res :meta :end-line dec)
                                 :ch (get-in res [:meta :end-column] 0)
                                 :start-line (dec (get-in res [:meta :line] 1))}]
                        (notifos/set-msg! (:result res) {:class "error"})
                        (object/raise obj :editor.exception (:stack res) loc))))
