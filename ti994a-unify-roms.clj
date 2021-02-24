#!/usr/bin/env bb

(ns ti994a-unify-roms
  (:require [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [clojure.java.io :as io]))

(def cli-opts
  [["-h" "--help"]
   [nil "--system-rom-path PATH" "path to the system rom images"]
   [nil "--rom-path PATH" "path to the cartridge roms"]
   [nil "--output PATH" "path to write the final bin file to"]

   ])

(defn usage [opt-summary]
  (-> "Process timrad2 rom images. Turn separate files into single full memory image for simple loading on MiSTer.

Usage: ./ti994a-unify-roms.clj --system-rom-path=PATH --roms=PATH --output=PATH

Options:
%s
"
      (format opt-summary)
      print))

(def re-system-rom #"994[Aa][Rr][Oo][Mm]\.[Bb][Ii][Nn]")
(def re-system-grom #"994[Aa][Gg][Rr][Oo][Mm]\.[Bb][Ii][Nn]")
(def re-speech-rom #"[Ss][Pp][Cc][Hh][Rr][Oo][Mm]\.[Bb][Ii][Nn]")

(def kilobyte (partial * 1024))

(def size-8k (kilobyte 8))
(def size-16k (kilobyte 16))
(def size-40k (kilobyte 40))
(def size-64k (kilobyte 64))

(defn zero-pad [length]
  (repeat length 0))

(def pad-8k (zero-pad size-8k))
(def pad-16k (zero-pad size-16k))
(def pad-40k (zero-pad size-40k))
(def pad-64k (zero-pad size-64k))

(defn path-is-dir? [path]
  (when-let [f (io/file path)]
    (and (.exists f)
         (.isDirectory f))))

(defn path-is-writable? [path]
  (when-let [f (io/file path)]
    (and (.exists f)
         (.canWrite f))))

(defn list-dir [path]
  (vec (.list (io/file path))))

(defn find-first-match [re filenames]
  (->> filenames
       (map (partial re-matches re))
       (filter identity)
       first))

(defn find-system-roms [path]
  (let [contents (list-dir path)]
    {:rom (find-first-match re-system-rom contents)
     :grom (find-first-match re-system-grom contents)
     :speech (find-first-match re-speech-rom contents)}))

(defn create-romset-file-hash [rom-file-set-re-info]
  (->> rom-file-set-re-info
       (map (fn [[file-name base-name rom-type]]
              [(-> rom-type string/lower-case keyword)
               file-name]))
       (into {})))

(defn create-romset-info-hash [path]
  (->> path
       list-dir
       (map (partial re-matches #"(.+)([CcGgDd])\.[Bb][Ii][Nn]"))
       (group-by second)
       (map (fn [[k vs]] [k (create-romset-file-hash vs)]))
       (into {})))

(defn load-bin [filename]
  (let [f (io/file filename)
        length (.length f)
        data (byte-array length)]
    (.read (io/input-stream f) data)
    (seq data)))

(defn load-bin-files [path hashmap]
  (->> hashmap
       (map (fn [[k filename]]
              (let [f (io/file path filename)]
                [k (when (.exists f) (load-bin (io/file path filename)))])))
       (into {})))

(defn create-rom-data [{:keys [rom grom speech]} {:keys [c g d]}]
  (let [c-data (if c c pad-8k)
        d-data (if d d pad-8k)
        g-data (if g g pad-40k)
        grom-pad (zero-pad (- size-40k (count g-data)))]
    (concat
     ;; 64K module ROM section
     c-data d-data c-data d-data c-data d-data c-data d-data

     ;; 64K GROM section
     grom g-data grom-pad

     ;; 64K ROM section
     pad-40k rom pad-16k

     ;; expansion RAM
     pad-64k

     ;; speech ROM
     (if speech speech []))))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args [args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-opts)]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}

      errors
      {:exit-message (error-msg errors)}

      options
      {:options options}

      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)}
      )
    )
  )

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (let [{:keys [system-rom-path rom-path output]} options]
        (println system-rom-path)
        (assert (path-is-dir? system-rom-path) "--system-rom-path should be a directory")
        (assert (path-is-dir? rom-path) "--roms should be a directory")
        (assert (and (path-is-dir? output)
                     (path-is-writable? output))
                "--output should be a writable directory")

        (let [system-roms (load-bin-files system-rom-path (find-system-roms system-rom-path))
              rom-info (create-romset-info-hash rom-path)
              ]
          (println (format "Found %d roms..." (count rom-info)))
          (doseq [[name info] (sort-by first rom-info)]
            (let [output-filename (str name ".bin")
                  rom-data (create-rom-data system-roms
                                            (load-bin-files rom-path info))]
              (println (format "writing %s ..." output-filename))
              (with-open [w (io/output-stream (io/file output output-filename))]
                (.write w (byte-array rom-data))))))))))

(apply -main *command-line-args*)
