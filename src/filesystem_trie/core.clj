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
  (:require [clojure.java.io :as io]
            [digest]))

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(defn- relative-path [s]
  "Return the string s with a / inserted after each character."
  (apply str (interpose "/" s)))

(defn- full-path
  "Return a filesystem path starting at root/subdir for the string s."
  ([root subdir s]
     (str root "/" subdir "/" (relative-path s))))

(defn- mkdir-p [root subdir rel-path]
  "Just like the Unix command 'mkdir -p'. True if root/subdir/rel-path was created, false if the path existed."
  (io/make-parents (str root "/" subdir "/" rel-path "/x")))

(defn- blob-path [root subdir key]
  "Return a Unix absolute pathname for root/subdir/relative-path-for-key."
  (str (full-path root subdir key) "/blob"))

(defn- blob-url [root subdir key]
  "Return a file:// url for root/subdir/relative-path-for-key."
  (str "file://" (blob-path root subdir key)))

(defn create [root blob]
  "Create a new blob and return its key."
  (let [key (uuid)
        hash (digest/sha-256 blob)]
    (when  (mkdir-p root "key" (relative-path key))
      (spit (blob-url root "/key" key) blob)
      key)))

(defn fetch [root key]
  "Return the blob for the key."
  (try
    (slurp (blob-url root "key" key))
    (catch java.io.FileNotFoundException e
      nil)
    (catch java.io.IOException e
      nil)))

(defn delete [root key]
  "Destroy the blob for the key.  There is no 'Are you sure?'."
  (try
    (do (io/delete-file (blob-path root "/key" key))
        key)
    (catch java.io.FileNotFoundException e
      nil)
    (catch java.io.IOException e
      nil)))
