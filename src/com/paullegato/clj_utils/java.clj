(ns com.paullegato.clj-utils.java
  "Utilities for interacting with Java."
  (:require [clojure.reflect :as r]
            [clojure.pprint :refer [print-table]]))



(defn print-methods
  "Prints a table of the methods in the given Java object.
   Based on http://stackoverflow.com/a/5821658/157510"
  [obj]
  (->> (r/reflect obj)
       :members
       (sort-by :name)
       print-table))
