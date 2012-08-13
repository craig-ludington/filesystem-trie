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
  (:require [clojure.java.io :as io]))

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(defn- relative-path [s]
  "Return the string s with a / inserted after each character."
  (apply str (interpose "/" s)))

(defn- full-path
  "Return a filesystem path starting at root for the string s."
  ([root s]
     (str root "/" (relative-path s))))

(defn- mkdir-p [root rel-path]
  "Just like the Unix command 'mkdir -p'. True if path p was created, false if p existed."
  (io/make-parents (str root "/" rel-path "/x")))

(defn- blob-path [root key]
  "Return a Unix absolute pathname for key."
  (str (full-path root key) "/blob"))

(defn- blob-url [root key]
  "Return a file:// url for key."
  (str "file://" (blob-path root key)))

(defn create [root blob]
  "Create a new blob and return its key."
  (let [key (uuid)]
    (when  (mkdir-p root (relative-path key))
      (spit (blob-url root key) blob)
      key)))

(defn fetch [root key]
  "Return the blob for the key."
  (try
    (slurp (blob-url root key))
    (catch java.io.FileNotFoundException e
      nil)
    (catch java.io.IOException e
      nil)))

(defn delete [root key]
  "Destroy the blob for the key.  There is no 'Are you sure?'."
  (try
    (do (io/delete-file (blob-path root key))
        key)
    (catch java.io.FileNotFoundException e
      nil)
    (catch java.io.IOException e
      nil)))
