(ns lt.plugins.minicljx
  (:require [lt.plugins.minicljx.tmpfile :refer [create-temporary-file! delete-temporary-file!]]
            [lt.plugins.clojure :as lt-clj]
            [cljs.reader :as reader]
            [lt.util.cljs :refer [->dottedkw]]
            [lt.object :as object]
            [lt.objs.notifos :as notifos]
            [lt.objs.eval :as eval]
            [lt.objs.editor :as ed]
            [lt.objs.clients :as clients])
  (:require-macros [lt.macros :refer [behavior]]))

(defn- send-command
  "Send the given command to the appropriate evaluation environment.
  This is an internal helper function to ensure that the correct REPL
  is used to evaluate eval'd code"
  [editor command info]
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
               :path (create-temporary-file!
                      (-> @editor :info :path) result-type processed-code)
               :code processed-code
               :pos (get-in result [:meta ::pos]))]
    (send-command editor command info)))

(defn- process-cljx-result
  "Internal helper function to process the result of executing transformed code"
  [editor result ruleset]
  ; done with the temporary file
  (delete-temporary-file! (get-in @editor [:info :path]) ruleset)
  (let [result-type (get-in result [:meta :result-type] :inline)
        command (->dottedkw :editor.eval.cljx.result result-type)]
    (object/raise editor command result)))

(defn- evaluate-cljx
  [editor info]
  (let [info (assoc info
               :code (ed/->val (:ed @editor)))
        modes [:clj :cljs]]
    (doseq [mode modes]
      (do-transform editor info mode))))

(defn- get-inline-result-loc
  "Transforms the position data from a clj/s evaluation into an inline result structure"
  [{:keys [line end-line end-column] :or {line 1 end-line 1 end-column 0}}]
  {:ch end-column
   :start-line (dec line)
   :line (dec end-line)})

(behavior ::on-eval
          :triggers #{:eval}
          :reaction (fn [editor]
                      (evaluate-cljx editor (:info @editor))))

(behavior ::on-eval.one
          :triggers #{:eval.one}
          :reaction (fn [editor]
                      (evaluate-cljx editor (assoc (:info @editor)
                                              :meta {::pos (ed/->cursor editor)}))))

(behavior ::cljx.precompile-result.clj
          :triggers #{:editor.eval.clj.result}
          :reaction (fn [editor result]
                      (when (contains? (:meta result) ::type)
                        (process-cljx-transform-result editor result))))

(behavior ::cljx.precompile-result.cljs
          :triggers #{:editor.eval.cljs.code}
          :reaction (fn [editor results]
                      (let [command :editor.eval.cljs.exec
                            path (get-in @editor [:info :path])
                            client (eval/get-client! {:command command
                                                      :info {:type "cljs"}
                                                      :key :exec
                                                      :origin editor})
                            processed-result (assoc results
                                               :results (for [result (:results results)]
                                                          (update-in result [:code]
                                                                     #(-> %
                                                                          (eval/pad (-> result :meta :line dec))
                                                                          (eval/append-source-file path)))))]
                        (clients/send client command processed-result :only editor))))

(behavior ::cljx.result.clj
          :triggers #{:editor.eval.clj.result}
          :reaction (fn [editor result]
                      (when-not (contains? (:meta result) ::type)
                        (process-cljx-result editor result :clj))))

(behavior ::cljx.result.cljs
          :triggers #{:editor.eval.cljs.result}
          :reaction (fn [editor result]
                      (process-cljx-result editor {:results (list result)} :cljs)))

(behavior ::cljx.result.clj.no-op
          :triggers #{:editor.eval.clj.no-op}
          :reaction (fn [editor location]
                      (notifos/set-msg! "No clj form found under cursor")))

(behavior ::cljx.result.cljs.no-op
          :triggers #{:editor.eval.cljs.no-op}
          :reaction (fn [editor location]
                      (notifos/set-msg! "No cljs form found under cursor")))

(behavior ::cljx.result.cljs.exception
          :triggers #{:editor.eval.cljs.exception}
          :reaction (fn [editor result]
                      (let [exception (:ex result)
                            loc (get-inline-result-loc (:meta result))]
                        (object/raise editor :editor.exception exception loc))))

(behavior ::cljx.result.inline
          :triggers #{:editor.eval.cljx.result.inline}
          :reaction (fn [editor result]
                      (doseq [res (:results result)]
                        (if (contains? res :stack)
                          (object/raise editor :editor.eval.cljx.exception res :passed)
                          (object/raise editor :editor.result (:result res) (get-inline-result-loc (:meta res)))))))

(behavior ::cljx.result.exception
          :triggers #{:editor.eval.cljx.exception}
          :reaction (fn [editor result passed?]
                      (when-not passed?
                        (notifos/done-working ""))
                      (notifos/set-msg! (:result result) {:class "error"})
                      (object/raise editor :editor.exception (:stack result) (get-inline-result-loc (:meta result)))))
