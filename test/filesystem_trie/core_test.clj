(ns filesystem-trie.core-test
  (:require [conch.core :as sh]
            [clojure.java.io :as io])
  (:use clojure.test
        filesystem-trie.core)
  (:import (java.io FileInputStream StringReader)))

(def root "/tmp/blobs")

(defn setup []
  (sh/proc "rm" "-rf" (str root "/key") (str root "/digest")))

(deftest uuid-test
  (is (= true  (instance? String  (#'filesystem-trie.core/uuid))))
  (is (= false (instance? Integer (#'filesystem-trie.core/uuid)))))

(deftest relative-path-test
  (is (= "a/b/c" (#'filesystem-trie.core/relative-path "abc"))))

(deftest full-path-test
  (is (= "/tmp/blobs/key/a/b/c" (#'filesystem-trie.core/full-path root "key" "abc"))))

(deftest ensure-path-test
  (setup)
  (let [path "/tmp/blobs/key/a/b/c"
        result (#'filesystem-trie.core/ensure-path path)]
    (is (= path result))))

(deftest ensure-path-no-permissions-test
  (setup)
  (let [path       "/no/write/permissions/here"
        result     (try (#'filesystem-trie.core/ensure-path path)
                        (catch Throwable e
                          :file-not-created))]
    (is (= result :file-not-created))))

(deftest blob-path-test
  (is (= "/tmp/blobs/key/a/b/c/blob" (#'filesystem-trie.core/blob-path "/tmp/blobs/key/a/b/c"))))

(deftest blob-url-test
  (is (= "file:///tmp/blobs/key/a/b/c/blob" (#'filesystem-trie.core/blob-url "/tmp/blobs/key/a/b/c"))))

(deftest binary-create-test
  (setup)
  (let [path    (.getPath (java.io.File/createTempFile "blobber" ".tmp"))
        stream  (FileInputStream. (do (sh/proc "dd" "if=/dev/urandom" (str "of=" )  path "bs=1024" "count=16")
                                      (java.io.File/createTempFile "blobber" ".tmp")))
        key     (create root stream)
        stored  (#'filesystem-trie.core/blob-path (#'filesystem-trie.core/full-path root "key" key))
        cmp     (sh/proc "cmp" path stored)]
    (is (= 0 (.exitValue (:process cmp))))))

;; TODO get rid of cmp
(deftest binary-fetch-test
  (setup)
  (let [in-path     (.getPath (java.io.File/createTempFile "blobber" ".tmp"))
        key         (create root (FileInputStream. (do (sh/proc "dd" "if=/dev/urandom" (str "of=" )  in-path "bs=1024" "count=16")
                                                    (java.io.File/createTempFile "blobber" ".tmp"))))
        in-stream   (fetch root key)
        out-file    (java.io.File/createTempFile "blobber" ".tmp")
        out-path    (.getPath out-file)]
    (io/copy in-stream out-file)
    (is (= 0
           (-> (sh/proc "cmp" in-path out-path) :process .exitValue)))))

(deftest nonexistent-blob-delete-test
  (let [key (#'filesystem-trie.core/uuid)]
    (is (= nil (delete root key)))))

(deftest wildcard-delete-test
  (setup)
  (create root "Indigo benelux Aladdin Saudi Arabia jihad Albright csim Soviet Cocaine militia USDOJ e-bomb")
  (is (= nil (delete root "***********************************"))))

(defn link-count
  "Parse the output of ls to get the link count of a file."
  [path]
  (let [tokens (clojure.string/split (sh/read-line (sh/proc "ls" "-l" path) :out) #" ")
        token (when (< 2 (count tokens))
                (nth tokens 2))]
    (when link-count
      (Integer/parseInt token))))

(deftest digest-trie-create-test
  (setup)
  (let [blob-to-store "MIT-LL computer terrorism Guantanamo Kh-11 cybercash LABLINK Attorney General enigma ASPIC espionage"
        hash           (digest/sha-256 blob-to-store)
        digest-path    (#'filesystem-trie.core/full-path root "digest" (digest/sha-256 blob-to-store))
        digest-blob    (#'filesystem-trie.core/blob-path digest-path)
        key            (create root blob-to-store)
        key-path       (#'filesystem-trie.core/full-path root "key" key)
        key-blob       (#'filesystem-trie.core/blob-path key-path)]
    (is (= (slurp digest-blob) (slurp key-blob)))
    (is (= 2 (link-count key-blob)))
    (is (= 2 (link-count digest-blob)))))
