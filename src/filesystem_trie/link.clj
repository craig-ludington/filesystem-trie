(ns filesystem-trie.link
  (:import java.nio.file.Path)
  (:import java.nio.file.Files))

(defn- java-path [p]
  "Return a Java Path for the Unix pathname p."
  (java.nio.file.Paths/get (java.net.URI. (str "file://" p))))

(defn hard-link [existing new]
  "Link new to existing, returning :success :file-already-exists or :too-many-links."
  (try (do (java.nio.file.Files/createLink (java-path new) (java-path existing))
           :success)
       (catch java.nio.file.FileAlreadyExistsException e
         :file-already-exists)
       (catch java.nio.file.FileSystemException e
         (if (re-find #"Too many links$" (str e))
           :too-many-links
           (throw e)))))