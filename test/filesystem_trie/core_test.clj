(ns filesystem-trie.core-test
  (:require [conch.core :as sh])
  (:use clojure.test
        filesystem-trie.core))

(def root "/tmp/blobs")

(defn setup [] (sh/proc "rm" "-rf" root))

(deftest uuid-test
  (is (= true  (instance? String  (#'filesystem-trie.core/uuid))))
  (is (= false (instance? Integer (#'filesystem-trie.core/uuid)))))

(deftest relative-path-test
  (is (= "a/b/c" (#'filesystem-trie.core/relative-path "abc"))))

(deftest full-path-test
  (is (= "/tmp/blobs/key/a/b/c" (#'filesystem-trie.core/full-path root "key" "abc"))))

(deftest mkdir-p-test
  (setup)
  (let [path "/tmp/blobs/key/a/b/c"
        result (#'filesystem-trie.core/mkdir-p path)]
    (is (= path result))))

(deftest blob-path-test
  (is (= "/tmp/blobs/key/a/b/c/blob" (#'filesystem-trie.core/blob-path "/tmp/blobs/key/a/b/c"))))

(deftest blob-url-test
  (is (= "file:///tmp/blobs/key/a/b/c/blob" (#'filesystem-trie.core/blob-url "/tmp/blobs/key/a/b/c"))))

(deftest create-fetch-test
  (setup)
  (let [blob-to-store   "Mahmoud Ahmadinejad clones Glock lynch covert video USCOI assassination Islam Abduganievich"
        key             (create root blob-to-store)
        blob-retrieved  (fetch  root key)]
    (is (= blob-to-store blob-retrieved))))

(deftest create-delete-test
  (setup)
  (let [blob-to-store   "Mahmoud Ahmadinejad clones Glock lynch covert video USCOI assassination Islam Abduganievich"
        key             (create root blob-to-store)]
    (is (= key (delete root key)))))

(deftest nonexistent-blob-delete-test
  (let [key (#'filesystem-trie.core/uuid)]
    (is (= nil (delete root key)))))

;; This is a Unix pathname injection test:
;;
;;   (blob-path root "***********************************")
;;   ==> "/tmp/blobs/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/blob"
;;
;; In the shell, that would turn into a lot of filename that look something like this:
;; /tmp/blobs/f/a/8/5/0/6/3/6/-/d/b/a/7/-/4/a/9/5/-/9/2/8/c/-/b/0/5/3/c/b/b/e/d/7/5/7/blob
;; 
(deftest wildcard-delete-test
  (setup)
  (let [blob-to-store   "Indigo benelux Aladdin Saudi Arabia jihad Albright csim Soviet Cocaine militia USDOJ e-bomb"
       ignored-key      (create root blob-to-store)
       evil-key         "***********************************"]
    (is (= nil           (delete root evil-key)))
    (is (= blob-to-store (fetch  root ignored-key)))))

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
