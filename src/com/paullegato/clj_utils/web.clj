(ns com.paullegato.clj-utils.web
  "Clojure web utility functions"
  (:require [onelog.core :as log]))

(defn wrap-exception
  "Returns a Ring handler that catches any exceptions and
  returns a 500 internal server error to the user instead of exception details.

  Also rewrites the body of any status 500 result to be \"Internal
  Server Error\", to prevent leaks from 3rd party libraries.

  (Exceptions are already logged by ring.middleware.logger, so we don't log them here.)"
  [f]
  (fn [request]
    (try (let [result (f request)]
           (if-not (= 500 (:status result))
             result
             (do
               (log/error "[web] Wrap-exception got a 500 response; rewriting old body " (:body result) "to 'internal server error'")
               {:status 500
                :body "Internal server error"})))
      (catch Throwable t
        (log/error "[web] Wrap-exception rewriting exception to 500 Internal Server Error...")
         {:status 500
          :body "Internal server error"}))))
