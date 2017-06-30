(require 'hunchentoot)
(require 'jsown)

(load "handlers")

(defparameter *ipt-params*
  '(("-p" . "protocol")
    ("-s" . "source")
    ("-d" . "dest")
    ("-i" . "in")
    ("-o" . "out")
    ("-f" . "fragment")
    ("-m" . "match")
    ("-j" . "target")))


(defparameter *default-routes*
  '(("^/chains/*$"
     (:get . ipt-chains-list)
     (:post . ipt-chain-create))
    ("^/chains/([a-zA-Z0-9_]+)/*$"
     (:get . ipt-chain-details)
     (:put . ipt-chain-set-policy)
     (:delete . ipt-chain-delete))
    ("^/chains/([a-zA-Z0-9_]+)/rules/*$"
     (:get . ipt-rules-list)
     (:post . ipt-rule-create)
     (:put . ipt-rules-batch-insert)
     (:delete . ipt-chain-flush))
    ("^/chains/([a-zA-Z0-9_]+)/rules/([0-9]+)/*$"
     (:get . ipt-rule-show)
     (:put . ipt-rule-replace)
     (:delete . ipt-rule-delete)
     (:patch . ipt-rule-change-number))
    ("^/rules/*$"
     (:get . ipt-rules-save)
     (:put . ipt-rules-restore)
     (:delete . ipt-global-flush))
    ("^/transfers/*$"
     (:post . ipt-rule-move))))


(defclass iptrest-acceptor (hunchentoot:acceptor)
  ((route-table
    :accessor route-table
    :initform nil
    :initarg :route-table))
  (:default-initargs
   :port 8808))

(defmethod hunchentoot:acceptor-dispatch-request ((iptrest iptrest-acceptor) request)
  (let ((static-path (request-static-path request)))
    (if static-path
        (ipt-static-handler static-path)
        (progn
          (mapc (lambda (route)
                  (let ((path (car route))
                        (handlers (cdr route)))
                    (multiple-value-bind (matched groups)
                        (cl-ppcre:scan-to-strings path (hunchentoot:script-name request))
                      (if matched
                          (let ((handler (assoc (hunchentoot:request-method request)
                                                handlers)))
                            (if handler
                                (progn
                                  (setf (hunchentoot:content-type*) "application/json")
                                  (return-from hunchentoot:acceptor-dispatch-request
                                    (apply (cdr handler) (map 'list 'identity groups))))
                                (progn
                                  (setf (hunchentoot:return-code*)
                                        hunchentoot:+http-method-not-allowed+)
                                  (hunchentoot:abort-request-handler))))))))
                (or (route-table iptrest) *default-routes*))
          (setf (hunchentoot:return-code hunchentoot:*reply*) hunchentoot:+http-not-found+)
          (hunchentoot:abort-request-handler)))))

(defmethod request-static-path ((request hunchentoot:request))
  (multiple-value-bind (matched groups)
      (cl-ppcre:scan-to-strings "^/?(index.html|script.js|style.css)?/*$"
                                (hunchentoot:script-name request))
    (if matched (or (svref groups 0) "index.html"))))

(defun ipt-abort-request-handler (http-code error-code error-message)
  (setf (hunchentoot:return-code*) http-code)
  (hunchentoot:abort-request-handler
   (jsown:to-json `(:obj ("code" . ,error-code) ("message" . ,error-message)))))

(defun iptrest-restart ()
  (if (boundp 'ipt-acceptor)
      (hunchentoot:stop ipt-acceptor))
  (defparameter ipt-acceptor (make-instance 'iptrest-acceptor))
  (hunchentoot:start ipt-acceptor))

(defparameter default-server (make-instance 'hunchentoot:acceptor :port 8809))
