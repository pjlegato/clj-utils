(ns com.paullegato.clj-utils.web
  "Clojure web utility functions"
  (:require [io.aviso.exception :refer [write-exception format-exception]]
            [onelog.core :as log]))

(def canned-500-response
  {:status 500
   :body "Internal server error"} )

(defn wrap-exception
  "Returns a Ring handler that catches any exceptions and returns a 500
  internal server error response to the user instead of sending
  exception details.

  Also rewrites the body of any status 500 result to be \"Internal
  Server Error\", to prevent leaks from 3rd party libraries.

  If a custom ring-response is supplied, uses that for HTTP content (or
  when content-type is not set). Otherwise, returns a short and generic
  500 \"Internal server error\".

  (Exceptions are already logged by ring.middleware.logger, so we don't
  log them here.)"
  ([f] (wrap-exception f
                       canned-500-response))
  ([f ring-response]
     (fn [request]
       (try (let [result (f request)]
              (if-not (= 500 (:status result))
                result
                (do
                  (log/error "[web] Wrap-exception got a 500 response; rewriting old body " (:body result) "to 'internal server error'")
                  (let [content-type (get-in result [:headers "Content-Type"])]
                    (if (and content-type
                             (not (= "text/html" content-type)))
                      canned-500-response
                      ring-response)))))
            (catch Throwable t
              (log/error (format-exception t))
              (log/error "[web] Wrap-exception rewriting exception to 500 Internal Server Error...")
              (let [accepted (get-in request [:headers "accept"])]
                (if (and accepted
                         (not (= -1 (.indexOf accepted "text/html"))))
                  ring-response
                  canned-500-response)))))))

