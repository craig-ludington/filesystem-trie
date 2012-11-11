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
  (:import (java.io InputStream FileInputStream File))
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

(defn is-file?
  "True if p is a file."
  [p]
  (.isFile (File. p)))

(defn is-directory?
  "True if p is a directory."
  [p]
  (.isDirectory (File. p)))

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

(defn- ensure-tempfile
  "Return a new temporary file in tmpdir."
  [tmpdir]
  (ensure-path tmpdir)
  (try (java.io.File/createTempFile "blobber" ".tmp" (io/as-file tmpdir))
       (catch java.io.IOException e
         (throw (Throwable. (str e ".  Can't create temp file in '" tmpdir "'."))))))

(defn- blob-path
  "Return a Unix absolute pathname for root/subdir/relative-path-for-key."
  [path]
  (str path "/blob"))

(defn- blob-url
  "Return a file:// url for root/subdir/relative-path-for-key."
  [path]
  (str "file://" path "/blob"))

(defn- mv
  "Return true if existing file was moved to new file."
  [existing new]
  (.renameTo (io/as-file existing) (io/as-file new)))

(defn create
  "Create a new blob (or a new reference to an identical pre-existing blob) and return its key."
  [^String root
   ^InputStream stream]
  (let [key           (uuid)
        key-path      (ensure-path (full-path root "key" key))
        key-file      (blob-path key-path)
        tmp           (ensure-tempfile key-path)
        digest        (do (io/copy stream tmp)
                          (digest/sha-256 tmp))
        identity-path (ensure-path (full-path root "digest" digest))
        identity-file (blob-path identity-path)]

    (if (is-file? identity-file)
      ;; Existing blob
      (let [x (hard-link identity-file key-file)]
        (cond (= :success x)        (and (io/delete-file tmp)
                                         key) 
              (= :too-many-links x) (and (mv identity-file (ensure-tempfile identity-path))
                                         (mv tmp identity-file)
                                         (= :success (hard-link identity-file key-file))
                                         key)))
      ;; Novel blob
      (and (mv tmp identity-file)
           (= :success (hard-link identity-file key-file))
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
