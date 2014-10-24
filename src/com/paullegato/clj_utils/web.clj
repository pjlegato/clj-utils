(ns com.paullegato.clj-utils.web
  "Clojure web utility functions"
  (:require [io.aviso.exception :refer [write-exception format-exception]]
            [onelog.core :as log]))

(defn wrap-exception
  "Returns a Ring handler that catches any exceptions and returns a 500
  internal server error response to the user instead of sending
  exception details.

  Also rewrites the body of any status 500 result to be \"Internal
  Server Error\", to prevent leaks from 3rd party libraries.

  If a custom ring-response is supplied, uses that. Otherwise, returns a
  short and generic 500 \"Internal server error\".

  (Exceptions are already logged by ring.middleware.logger, so we don't
  log them here.)"
  ([f] (wrap-exception f
                       {:status 500
                        :body "Internal server error"}))
  ([f ring-response]
     (fn [request]
       (try (let [result (f request)]
              (if-not (= 500 (:status result))
                result
                (do
                  (log/error "[web] Wrap-exception got a 500 response; rewriting old body " (:body result) "to 'internal server error'")
                  ring-response)))
            (catch Throwable t
              (log/error (format-exception t))
              (log/error "[web] Wrap-exception rewriting exception to 500 Internal Server Error...")
              ring-response)))))

