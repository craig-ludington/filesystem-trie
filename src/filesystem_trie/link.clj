(ns filesystem-trie.link
  (:import java.nio.file.Path)
  (:import java.nio.file.Files))

(defn- java-path [p]
  "Return a Java Path for the Unix pathname p."
  (java.nio.file.Paths/get (java.net.URI. (str "file://" p))))

(defn hard-link [existing new]
  "Link the Unix pathname new to the Unix pathname existing and return a Java Path for new."
  (java.nio.file.Files/createLink (java-path new) (java-path existing)))
