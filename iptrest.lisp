(require 'hunchentoot)

(defvar default-routes nil)
(setf default-routes
      '(("^/$" . ipt-root-handler)
        ("^/chains/?$" . ipt-chains-handler)
        ("^/chains/([a-zA-Z0-9_]+)/?$" . ipt-chain-handler)
        ("^/chains/([a-zA-Z0-9_]+)/rules/?$" . ipt-rules-handler)
        ("^/chains/([a-zA-Z0-9_]+)/rules/([0-9]+)/?$" . ipt-rule-handler)))

(defclass iptrest-acceptor (hunchentoot:acceptor)
  ((route-table
    :accessor route-table
    :initform default-routes
    :initarg :route-table)
   (request-class
    :initform 'iptrest-request))
  (:default-initargs
   :port 8808))

(defclass iptrest-request (hunchentoot:request) ())

(defmethod hunchentoot:post-parameters ((request iptrest-request))
  (let ((hunchentoot:*methods-for-post-parameters* '(:post :put :patch)))
    (call-next-method)))

(defmethod hunchentoot:acceptor-dispatch-request ((iptrest iptrest-acceptor) request)
  (hunchentoot:acceptor-log-message iptrest 'info "SCRIPT-NAME: ~S"
                                    (hunchentoot:script-name request))
  (mapc (lambda (route)
          (let* ((re (car route))
                 (handler (cdr route)))
            (multiple-value-bind (match-whole match-groups)
                (cl-ppcre:scan-to-strings
                 re
                 (hunchentoot:script-name request))
              (when match-whole
                (return-from hunchentoot:acceptor-dispatch-request
                  (apply handler request (map 'list 'identity match-groups)))))))
        (route-table iptrest))
  (setf (hunchentoot:return-code hunchentoot:*reply*) hunchentoot:+http-not-found+)
  (hunchentoot:abort-request-handler))

(defun ipt-root-handler (request)
  (format nil "(~S) root (~S) (~S)~%"
          (hunchentoot:request-method request)
          (hunchentoot:get-parameters request)
          (hunchentoot:post-parameters request)))

(defun ipt-chains-handler (request)
  (format nil "(~S) chains~%"
          (hunchentoot:request-method request)))

(defun ipt-chain-handler (request chain-name)
  (format nil "(~S) chain `~a'~%"
          (hunchentoot:request-method request)
          chain-name))

(defun ipt-rules-handler (request chain-name)
  (format nil "(~S) rules (chain `~a')~%"
          (hunchentoot:request-method request)
          chain-name))

(defun ipt-rule-handler (request chain-name rule-num)
  (format nil "(~S) rule ~d (chain `~a')~%"
          (hunchentoot:request-method request)
          rule-num
          chain-name))
