(ns convert-boards
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- file-name [file]
  (last (str/split file #"/")))

(defn- file-dir [file]
  (subs (file-name file) 0 2))

(comment

  (let [files (->> "../site/boards"
                   (clojure.java.io/file)
                   (file-seq)
                   (filter #(.isFile %))
                   (map str))]
    (doseq [file files]
      (let [dir (str "../site/boards/" (file-dir file))]
        (.mkdir (java.io.File. dir))
        (with-open [w (-> (str dir "/" (file-name file) ".gz")
                          clojure.java.io/output-stream
                          java.util.zip.GZIPOutputStream.
                          clojure.java.io/writer)]
          (.write w (slurp file))))))

  )
