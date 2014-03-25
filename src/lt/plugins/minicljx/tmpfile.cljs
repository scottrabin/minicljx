(ns lt.plugins.minicljx.tmpfile
  (:require [lt.objs.files :as files]))

(def ^:private tmpdir (.tmpdir (js/require "os")))

(defn get-temporary-filename
  "Get the name of a temporary file associated with a particular editor buffer"
  [path ruleset]
  (let [filename (str (files/basename path ".cljx") "." (name ruleset))
        tmpname (clojure.string/replace
                 (files/join "minicljx" (files/parent path) filename)
                 files/separator
                 "__")]
    (files/join tmpdir tmpname)))

(defn create-temporary-file!
  "Create a temporary file with the given contents, so that the cljs execution
  environment doesn't fail when trying to read the cljs file and encountering a
  (completely valid) #+clj[s]? reader macro in the (ns) declaration. Take the
  source path and the transformed contents, and return the temporary path of
  the file."
  [path ruleset contents]
  (let [tmp-path (get-temporary-filename path ruleset)]
    (files/save tmp-path contents)
    tmp-path))

(defn delete-temporary-file!
  "Delete a temporary file created for a given buffer"
  [path ruleset]
  (let [tmpname (get-temporary-filename path ruleset)]
    (when (files/exists? tmpname)
      (files/delete! (get-temporary-filename path ruleset)))))
