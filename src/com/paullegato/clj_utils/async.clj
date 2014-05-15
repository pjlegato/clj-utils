(ns com.paullegato.clj-utils.async
  "Core.async utilities"
  (:require [onelog.core :as log]
            [clojure.core.async :as async]))


(defmacro <!!-timeout
  "Like core.async's <!!, but times out and logs a warning after the
  given number of ms have passed if nothing could be read from port during that time."
  [port ms]
  `(let [timeout-port#  (async/timeout ~ms)
         [val# port#]   (async/alts!! [~port timeout-port#])]
    (if (= port# timeout-port#)
      (log/warn+ (log/color [:bright :yellow]
                            "<!!-timeout timed out while trying to read " '~port "!")))
    val#))

(defmacro <!-timeout
  "Like core.async's <!, but times out and logs a warning after the
  given number of ms have passed if nothing could be read from port during that time."
  [port ms]
  `(let [timeout-port#  (async/timeout ~ms)
         [val# port#]   (async/alts! [~port timeout-port#])]
    (if (= port# timeout-port#)
      (log/warn+ (log/color [:bright :yellow]
                            "<!-timeout timed out while trying to read " '~port "!")))
    val#))
