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
  (:import java.io.InputStream)
  (:require [clojure.java.io :as io]
            [digest])
  (:use [filesystem-trie.link]))

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(defn- relative-path
  "Return the string s with a / inserted after each character."
  [s]
  (apply str (interpose "/" s)))

(defn- full-path
  "Return a filesystem path starting at root/subdir for the string s."
  ([root subdir s]
     (str root "/" subdir "/" (relative-path s))))

(defn- mkdir-p
  "Just like the Unix command 'mkdir -p'. Return path if created, nil if the path existed."
  [path]
  (when (io/make-parents (str path "/x"))
    path))

(defn- blob-path
  "Return a Unix absolute pathname for root/subdir/relative-path-for-key."
  [path]
  (str path "/blob"))

(defn- blob-url
  "Return a file:// url for root/subdir/relative-path-for-key."
  [path]
  (str "file://" path "/blob"))

(defn create
  "Create a new blob (or a new reference to an identical pre-existing blob) and return its key."
  [^String root
   ^InputStream blob]
  (let [key             (uuid)
        key-path        (full-path root "key" key)
        key-created?    (mkdir-p key-path)

        tmpfile         (let [x (java.io.File/createTempFile "blobber", ".tmp")]
                          (io/copy blob x)
                          x) 
        digest-path     (full-path root "digest" (digest/sha-256 tmpfile))
        digest-created? (mkdir-p digest-path)]

    (when (not key-created?) 
      (throw (Throwable. (str "Collision for key '" key "'."))))

    (when digest-created?
      (.renameTo tmpfile (io/as-file (blob-path digest-path))))
    
    (hard-link (blob-path digest-path)
               (blob-path key-path))
    key))

(defn fetch
  "Return the blob for the key."
  [root key]
  (try
    (slurp (blob-url (full-path root "key" key) ))
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
