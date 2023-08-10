;; nginx package that has the ssl_preread module compiled in. We use
;; this on tux02 to pass HTTPS to containers without terminating TLS.

(use-modules ((gnu packages web) #:select (nginx))
             (guix gexp)
             (guix packages)
             (guix utils))

(package
  (inherit nginx)
  (arguments
   (substitute-keyword-arguments (package-arguments nginx)
     ((#:configure-flags flags '())
      #~(cons "--with-stream_ssl_preread_module"
              #$flags)))))
