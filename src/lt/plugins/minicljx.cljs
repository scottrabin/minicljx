(ns lt.plugins.minicljx
  (:require [lt.plugins.clojure :as lt-clj]
            [lt.plugins.watches :as watches]
            [cljs.reader :as reader]
            [lt.util.cljs :refer [->dottedkw str-contains?]]
            [lt.object :as object]
            [lt.objs.notifos :as notifos]
            [lt.objs.eval :as eval]
            [lt.objs.editor :as ed]
            [lt.objs.clients :as clients])
  (:require-macros [lt.macros :refer [behavior]]))

(defn- send-command
  [editor command info]
  "Send the given command to the appropriate evaluation environment.
  This is an internal helper function to ensure that the correct REPL
  is used to evaluate eval'd code"
  (let [client (eval/get-client! {:command command
                                  :info info
                                  :origin editor
                                  :create lt-clj/try-connect})]
    (clients/send client command info :only editor)))

(defn- get-transform [code ruleset]
  (-> "(require '[cljx.core] '[cljx.rules]) (cljx.core/transform %CODE% cljx.rules/%RULESET%-rules)"
      (clojure.string/replace "%RULESET%" (name ruleset))
      (clojure.string/replace "%CODE%" (pr-str code))))

(defn- do-transform
  "Run the transform operations for clj/s"
  [editor info ruleset]
  (send-command editor :editor.eval.clj
                (-> info
                    (assoc :code (get-transform (:code info) ruleset))
                    (assoc-in [:meta ::type] ruleset))))

(defn- process-cljx-transform-result
  "Internal helper function to process the result of a cljx transformation"
  [editor result]
  (let [result-type (-> result :meta ::type)
        command (->dottedkw :editor.eval result-type)
        processed-code (->> (:results result)
                            (map :result)
                            (map reader/read-string)
                            (apply str))
        info (assoc (:info @editor)
               :code processed-code
               :pos (get-in result [:meta ::pos]))]
    (send-command editor command info)))

(defn- process-cljx-result
  "Internal helper function to process the result of executing transformed code"
  [editor result]
  (let [result-type (get-in result [:meta :result-type] :inline)
        command (->dottedkw :editor.eval.cljx.result result-type)]
    (object/raise editor command result)))

(behavior ::on-eval
          :triggers #{:eval}
          :reaction (fn [editor]
                      (let [info (assoc (:info @editor)
                                   :code (ed/->val (:ed @editor)))]
                        (do-transform editor info :clj))))

(behavior ::on-eval.one
          :triggers #{:eval.one}
          :reaction (fn [editor]
                      (let [info (assoc (:info @editor)
                                   :code (ed/->val (:ed @editor))
                                   :meta {::pos (ed/->cursor editor)})]
                      (do-transform editor info :clj))))

(behavior ::cljx-result
          :triggers #{:editor.eval.clj.result}
          :reaction (fn [obj res]
                      (let [process-fn (if (contains? (:meta res) ::type)
                                         process-cljx-transform-result
                                         process-cljx-result)]
                        (process-fn obj res))))

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

(behavior ::cljx-result.no-op
          :triggers #{:editor.eval.clj.no-op}
          :reaction (fn [editor location]
                      (notifos/set-msg! "No form found under cursor" {:class "error"}) ))

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
