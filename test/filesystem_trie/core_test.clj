(ns filesystem-trie.core-test
  (:require [clojure.java.shell :as sh]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:use clojure.test
        filesystem-trie.core)
  (:import (java.io FileInputStream StringReader)))

(def root "/tmp/blobs")

(defn setup []
  (:exit (sh/sh "rm" "-rf" root)))

(setup)

(deftest uuid-test
  (is (= true  (instance? String  (#'filesystem-trie.core/uuid))))
  (is (= false (instance? Integer (#'filesystem-trie.core/uuid)))))

(deftest full-path-test
  (is (= "/tmp/blobs/key/a/b/c" (#'filesystem-trie.core/full-path root "key" "abc"))))

(deftest ensure-path-test
  (setup)
  (let [path "/tmp/blobs/key/a/b/c"
        result (#'filesystem-trie.core/ensure-path path)]
    (is (= path result))))

(deftest ensure-path-no-permissions-test
  (let [path       "/no/write/permissions/here"
        result     (try (#'filesystem-trie.core/ensure-path path)
                        (catch Throwable e
                          :file-not-created))]
    (is (= result :file-not-created))))

(deftest blob-path-test
  (is (= "/tmp/blobs/key/a/b/c/blob" (#'filesystem-trie.core/blob-path "/tmp/blobs/key/a/b/c"))))

(deftest blob-url-test
  (is (= "file:///tmp/blobs/key/a/b/c/blob" (#'filesystem-trie.core/blob-url "/tmp/blobs/key/a/b/c"))))

(defn make-random-input-stream []
    (let [tmp (.getPath (java.io.File/createTempFile "blobber" ".tmp"))]
      (sh/sh "dd" "if=/dev/urandom" (str "of=" tmp) "bs=1024" "count=16")
      {:path tmp :stream (FileInputStream. tmp)} ))

(deftest binary-create-test
  (let [x       (make-random-input-stream)
        path    (:path x)
        stream  (:stream x)
        key     (create root stream)
        stored  (#'filesystem-trie.core/blob-path (#'filesystem-trie.core/full-path root "key" key))]
    (is (= 0 (:exit (sh/sh "cmp" path stored))))))

(deftest binary-fetch-test
  (let [x           (make-random-input-stream)
        in-path     (:path x)
        key         (create root (:stream x))
        in-stream   (fetch root key)
        out-file    (java.io.File/createTempFile "blobber" ".tmp")
        out-path    (.getPath out-file)]
    (io/copy in-stream out-file)
    (is (= 0 (:exit (sh/sh "cmp" in-path out-path))))))

(deftest nonexistent-blob-delete-test
  (let [key (#'filesystem-trie.core/uuid)]
    (is (= nil (delete root key)))))

(deftest wildcard-delete-test
  (create root (StringReader. "Indigo benelux Aladdin Saudi Arabia jihad Albright csim Soviet Cocaine militia USDOJ e-bomb\n"))
  (is (= nil (delete root "***********************************"))))

(defn os-type
  "Return Darwin or Linux based on the output of uname -a"
  []
  (let [lines  (str/split-lines (:out (sh/sh "uname" "-a")))
        line   (first lines)
        tokens (str/split line  #" ")]
    (first tokens)))

(defn link-count
  "Parse the output of ls to get the link count of a file."
  [path]
  (let [lines  (str/split-lines (:out (sh/sh "ls" "-l" path)))
        line   (first lines)
        tokens (str/split line  #" ")
        offset (let [os (os-type)]
                 (cond (= os "Linux")  1
                       (= os "Darwin") 2))
        token  (when (< offset (count tokens))
                 (nth tokens offset))]
    (when link-count
      (Integer/parseInt token))))

(deftest digest-trie-create-test
  (let [blob-to-store "MIT-LL computer terrorism Guantanamo Kh-11 cybercash LABLINK Attorney General enigma ASPIC espionage\n"
        hash           (digest/sha-256 blob-to-store)
        digest-path    (#'filesystem-trie.core/full-path root "digest" (digest/sha-256 blob-to-store))
        digest-blob    (#'filesystem-trie.core/blob-path digest-path)
        key            (create root (StringReader. blob-to-store))
        key-path       (#'filesystem-trie.core/full-path root "key" key)
        key-blob       (#'filesystem-trie.core/blob-path key-path)]
    (is (= (slurp digest-blob) (slurp key-blob)))
    (is (= 2 (link-count key-blob)))
    (is (= 2 (link-count digest-blob)))))

(defn link-too-many-times
  "Create too many links to original so the next link attempt will fail."
  [original new-dir]
  (dotimes [x 32769]
    (#'filesystem-trie.link/hard-link original (str new-dir "/" x))))

(deftest digest-with-too-many-links-test
  (setup)
  (let [blob-to-store "Too many links, so we had to make a second blob in the digest/ directory.\n"
        hash           (digest/sha-256 blob-to-store)
        digest-path    (#'filesystem-trie.core/full-path root "digest" (digest/sha-256 blob-to-store))
        digest-blob    (#'filesystem-trie.core/blob-path digest-path)
        max-links      (let [os (os-type)]
                         (cond (= os "Linux")  32000    ;; I fear this is valid for ext3 only.
                               (= os "Darwin") 32767))]

    (create root (StringReader. blob-to-store))
    (is (= 2 (link-count digest-blob)))

    (link-too-many-times digest-blob (#'filesystem-trie.core/ensure-path (str root "/work")))
    (is (=  max-links (link-count digest-blob)))

    ;; Next create hits the maximum link count.
    ;; The digest-blob is deleted.
    ;; A new digest-blob is created.
    (create root (StringReader. blob-to-store))
    (is (= 2 (link-count digest-blob)))

    ))
