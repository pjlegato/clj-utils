(ns com.paullegato.clj-utils.core
  "General Clojure utility functions"
  (:require [onelog.core :as log])
  (:import [org.joda.time DateTime]))


(defn project-version
  "Returns the Leiningen project version, or nil if no version can be
  discovered."
  []
  (or (System/getProperty "limefog.version")
      ;; The above property isn't set when running from a jarfile, so we fall back on:
      (some-> "project.clj" clojure.java.io/resource slurp read-string (nth 2))))


(defn path-to-filename
  "Given a path, returns only the filename portion of it."
  [long-path]
  (last  (clojure.string/split long-path #"/")))


(defn- milli-time
  "Returns System/nanoTime converted to milliseconds."
  []
  (long (/ (System/nanoTime) 1000000)))


(defn spin-on*
  "Spins at most n times waiting for f to be true. Each spin is timeout
  ms long. Throws an Exception upon timeout with the given tag added to
  its message."
  [f tag n timeout]
  (log/debug "Spinlocking on " tag)
  (loop [n n]
    (if (f)
      (log/debug "Spinlock released for " tag)
      (if (> n 0)
        (do (Thread/sleep timeout)
            (recur (- n 1)))
        (throw (Exception. (str "Spinlock timeout awaiting " tag)))))))


(defmacro spin-on
  "Spins at most n times waiting for f to be true. Each spin is
  timeout ms long.

  Adds the current filename and line to the exception generated upon
  timeout, for easier debugging."
  [f n timeout]
  `(spin-on* ~f
             ~(str (pr-str f) " at " (path-to-filename *file*) ":" (:line (meta &form)))
             ~n
             ~timeout))


(defmacro with-timeout 
  "Runs the given code, aborting it and throwing an exception with the
  given forms in its message after ms milliseconds if the forms have not
  finished executing yet."
  [ms & forms]
  `(let [f# (future ~@forms)]
     (try
       (.get ^java.util.concurrent.Future f# ~ms java.util.concurrent.TimeUnit/MILLISECONDS)
       (catch java.util.concurrent.TimeoutException t#
         ;; Re-throw another exception with a more helpful message containing
         ;; the code that timed out, wrapping the original exception.
         (throw (RuntimeException.
                 (str "Timed out while waiting up to " ~ms " ms for the following code to run: " '~@forms)
                 t#))))))


(defn has-keys? [m keys]
  (every? (partial contains? m) keys))


(defn uuid
  "Generates a random UUID."
  []
  (java.util.UUID/randomUUID))


(defn to-uuid
  "Converts the given string into a UUID if it is not one already."
  [s]
  (assert s)
  (if (instance? java.util.UUID s)
    s
    (java.util.UUID/fromString s)))


(defmacro <!!-timeout
  "Like core.async's <!!, but times out and logs a warning after the
  given number of ms have passed if nothing could be read from port during that time."
  [port ms]
  `(let [timeout-port#  (async/timeout ~ms)
         [val# port#]   (alts!! [~port timeout-port#])]
    (if (= port# timeout-port#)
      (log/warn+ (log/color [:bright :yellow]
                            "<!!-timeout timed out while trying to read " '~port "!")))
    val#))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn add-shutdown-hook!*
  "Causes the given function to be called when the JVM is shut down,
  such as when the user presses Ctrl-C at the console, the code calls
  System.exit(), etc..

  Note that this is on a best-effort basis; it is not guaranteed that the
  function will be called in all cases before the process shuts down."
  [f]
  (.addShutdownHook (Runtime/getRuntime) (Thread. f)))


(defmacro add-shutdown-hook!
  "Runs the given code on a best-effort basis when the JVM is shut down."
  [& forms]
  `(add-shutdown-hook!* (fn [] ~@forms)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn set-uncaught-exception-handler!*
  "Sets a JVM-wide handler for uncaught exceptions. Calls the given
  function with two arguments, the thread that threw the exception, and
  the throwable that wasn't caught."
  [f]
  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (uncaughtException [this thread throwable]
       (f thread throwable)
       (log/error "Default JVM exception logger: got an uncaught exception from " thread "!")
       (log/error throwable)))))


(defmacro set-uncaught-exception-handler!
  "Sets a JVM-wide handler for uncaught exceptions. The given forms will
  be run with 'thread and 'throwable bound appropriately"
  [& forms]
  `(set-uncaught-exception-handler!*
    (fn [~'thread ~'throwable]
      ~@forms)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn sanitize-times
  "Given a map, returns a new map with :updated_at and :created_at
  converted from java.util.GregorianCalendars into Joda DateTimes, if
  present.

  Not only are DateTimes better all around, but GregorianCalendars 
  trigger a bug in pprint (http://dev.clojure.org/jira/browse/CLJ-1390)."
  [a-map]
  (into a-map (for [[k v] (select-keys a-map [:updated-at :created-at])]
                [k (DateTime. v)])))


(defn to-datetime
  "Fixed arity creation of a Joda DateTime with the given source date."
  [source]
  (DateTime. source))


(defn maybe-assoc-apply
  "Like assoc, but replaces the value of key with the result of applying f to its current value.
  Returns the original map unaltered if the map does not have the given key."
  [map key f]
  (if-not (contains? map key)
    map
    (assoc map key (f (get map key)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Thread manipulation
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-all-threads
  "Returns an array of information on all running threads.
   Array elements are instances of java.lang.management.ThreadInfo.
   From http://tech.puredanger.com/2010/05/30/clojure-thread-tricks/"
  []
  (.dumpAllThreads 
   (java.lang.management.ManagementFactory/getThreadMXBean) false false))


(defn get-thread-id
  "Returns a java.lang.management.ThreadInfo object for the given
  thread ID, if possible, else nil."
  [thread-id]
  (first (filter (fn [t] (= thread-id (.getThreadId t)))
                 (get-all-threads))))


(defn get-thread-name
  "Returns a java.lang.management.ThreadInfo object for the given
  thread name, if possible, else nil."
  [thread-name]
  (first (filter (fn [t] (= thread-name (.getThreadName t)))
                  (get-all-threads))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; String parsing
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn maybe-int
  "Given an integer or a string parseable as an integer, returns the
  integer. Otherwise, returns nil."
  [arg]
  (cond
   (string? arg)  (try
                    (Integer/parseInt arg)
                    (catch Throwable t
                      nil))
   (integer? arg) arg
   :else nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Maps
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn assoc-fn
  "Returns the map that results from calling fn on the value of the
  given key and associng the result back to the source map.

  For example:

   > (assoc-fn {:foo \"bar\" :baz \"frog\"} :foo count)
   {:baz \"frog\", :foo 3}

"
  [map key fn]
  (assoc map key (fn (get map key))))
