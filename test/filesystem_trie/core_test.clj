(ns filesystem-trie.core-test
  (:require [conch.core :as sh])
  (:use clojure.test
        filesystem-trie.core))

(defn setup [] (sh/proc "rm" "-rf" "/tmp/blobs"))

(deftest uuid-test
  (is (= true  (instance? String  (#'filesystem-trie.core/uuid))))
  (is (= false (instance? Integer (#'filesystem-trie.core/uuid)))))

(deftest relative-path-test
  (is (= "a/b/c" (#'filesystem-trie.core/relative-path "abc"))))

(deftest full-path-test
  (is (= "/tmp/blobs/key/a/b/c" (#'filesystem-trie.core/full-path "/tmp/blobs" "key" "abc"))))

(deftest mkdir-p-test
  (setup)
  (is (= true (#'filesystem-trie.core/mkdir-p "/tmp/blobs" "key" "a/b/c")))
  (is (= "contents" (do (spit  "/tmp/blobs/key/a/b/c/stuff" "contents")
                        (slurp "/tmp/blobs/key/a/b/c/stuff")))))

(deftest blob-path-test
  (is (= "/tmp/blobs/key/a/b/c/blob" (#'filesystem-trie.core/blob-path "/tmp/blobs" "key" "abc"))))

(deftest blob-url-test
  (is (= "file:///tmp/blobs/key/a/b/c/blob" (#'filesystem-trie.core/blob-url "/tmp/blobs" "key" "abc"))))

(deftest create-fetch-test
  (setup)
  (let [blob-to-store   "Mahmoud Ahmadinejad clones Glock lynch covert video USCOI assassination Islam Abduganievich"
        key             (create "/tmp/blobs" blob-to-store)
        blob-retrieved  (fetch  "/tmp/blobs" key)]
    (is (= blob-to-store blob-retrieved))))

(deftest create-delete-test
  (setup)
  (let [blob-to-store   "Mahmoud Ahmadinejad clones Glock lynch covert video USCOI assassination Islam Abduganievich"
        key             (create "/tmp/blobs" blob-to-store)]
    (is (= key (delete "/tmp/blobs" key)))))

(deftest nonexistent-blob-delete-test
  (let [key (#'filesystem-trie.core/uuid)]
    (is (= nil (delete "/tmp/blobs" key)))))

;; This is a Unix pathname injection test:
;;
;;   (blob-path "/tmp/blobs" "***********************************")
;;   ==> "/tmp/blobs/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/blob"
;;
;; In the shell, that would turn into a lot of filename that look something like this:
;; /tmp/blobs/f/a/8/5/0/6/3/6/-/d/b/a/7/-/4/a/9/5/-/9/2/8/c/-/b/0/5/3/c/b/b/e/d/7/5/7/blob
;; 
(deftest wildcard-delete-test
  (setup)
  (let [blob-to-store   "Indigo benelux Aladdin Saudi Arabia jihad Albright csim Soviet Cocaine militia USDOJ e-bomb"
       ignored-key      (create "/tmp/blobs" blob-to-store)
       evil-key         "***********************************"]
    (is (= nil           (delete "/tmp/blobs" evil-key)))
    (is (= blob-to-store (fetch  "/tmp/blobs" ignored-key)))))

