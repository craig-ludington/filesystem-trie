(ns filesystem-trie.core
  #^{:author "Craig Ludington",
     :doc "A Unix filesystem-based implementation of Edward Fredkin's trie data structure.

           Data is stored, indexed by an arbitrary key (a GUUID) which is created by
           this library and returned to the caller.  The caller may use that key to
           retrieve or delete the data.  No updates are permitted so the data may be treated
           as immutable.

           The implementation is entirely in the Unix filesystem.

           cf. http://en.wikipedia.org/wiki/Trie"
     }
  (:import (java.io InputStream FileInputStream FileOutputStream File))
  (:require [clojure.java.io :as io]
            [digest])
  (:use [filesystem-trie.link]))

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(defn- is-file? [p] (.isFile (io/as-file p)))
(defn- is-directory? [p] (.isDirectory (io/as-file p)))

(defn- full-path [root subdir s] (str root "/" subdir "/" (apply str (interpose "/" s))))
(defn- blob-path [path] (str path "/blob"))
(defn- blob-url  [path] (str "file://" path "/blob"))

(defn- ensure-path
  "Create all the directories in path if they don't already exist.
   Return path if successful, throw \"Can't create p\" otherwise"
  [path]
  (if (is-directory? path)
    path
    (do (io/make-parents (str path "/x"))
        (if (is-directory? path)
          path
          (throw (Throwable. (str "Can't create directory " path)))))))

(defn- ensure-key-path    [root key]    (ensure-path (full-path root "key"    key)))
(defn- ensure-digest-path [root digest] (ensure-path (full-path root "digest" digest)))

(defn- mv
  "Return true if existing file was moved to new file."
  [existing new]
  (.renameTo (io/as-file existing) (io/as-file new)))

(defn create
  "Create a new blob (or a new reference to an identical pre-existing blob) and return its key."
  [^String root
   ^InputStream stream]
  (let [key         (uuid) ;; TODO handle collisions
        key-path    (ensure-key-path root key)
        key-file    (blob-path key-path)
        new         (str key-path "/new")
        _           (with-open [f (FileOutputStream. new)] (io/copy stream f))
        digest      (with-open [f (FileInputStream. new)]  (digest/sha-256 f)) 
        digest-path (ensure-digest-path root digest) ;; TODO compare bytes?
        digest-file (blob-path digest-path)]

    (if (is-file? digest-file)
      ;; Existing blob
      (let [x (hard-link digest-file key-file)]
        (cond (= :success x)        (and (io/delete-file new)
                                         key) 
              (= :too-many-links x) (and (io/delete-file digest-file)
                                         (mv new digest-file)
                                         (= :success (hard-link digest-file key-file))
                                         key)))
      ;; Novel blob
      (and (mv new digest-file)
           (= :success (hard-link digest-file key-file))
           key))))

(defn ^InputStream fetch
  "Return the blob for the key."
  [^String root
   ^String key]
  (try (FileInputStream. (blob-path (full-path root "key" key)))
       (catch java.io.FileNotFoundException e
         nil)
       (catch java.io.IOException e
         nil)))

(defn delete
  "Destroy the blob for the key.  There is no 'Are you sure?'."
  [root key]
  (try
    (do (io/delete-file (blob-path (full-path root "key" key)))
        key)
    (catch java.io.FileNotFoundException e
      nil)
    (catch java.io.IOException e
      nil)))
