(require 'hunchentoot)
(require 'jsown)


(defvar default-routes nil)
(setf default-routes
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


(defmethod request-static-path ((request hunchentoot:request))
  (multiple-value-bind (matched groups)
      (cl-ppcre:scan-to-strings "^/?(index.html|script.js)?/*$"
                                (hunchentoot:script-name request))
    (if matched (or (svref groups 0) "index.html"))))

(defun ipt-static-handler (path)
  (hunchentoot:handle-static-file (parse-namestring path)))

(defclass iptrest-acceptor (hunchentoot:acceptor)
  ((route-table
    :accessor route-table
    :initform default-routes
    :initarg :route-table))
  (:default-initargs
   :port 8808))

(defclass rule ()
  ((rule-options
    :reader rule-options
    :initform nil
    :initarg :rule-options)
   (rule-matches
    :reader rule-matches
    :initform nil
    :initarg :rule-matches)
   (rule-target
    :reader rule-target
    :initform nil
    :initarg :rule-target)))

;; (defmethod )

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
                (route-table iptrest))
          (setf (hunchentoot:return-code hunchentoot:*reply*) hunchentoot:+http-not-found+)
          (hunchentoot:abort-request-handler)))))

(defun ipt-abort-request-handler (http-code error-code error-message)
  (setf (hunchentoot:return-code*) http-code)
  (hunchentoot:abort-request-handler
   (jsown:to-json `(:obj ("code" . ,error-code) ("message" . ,error-message)))))

(defun execute-iptables-cmd (cmd)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program cmd
                        :output :lines
                        :error-output :string
                        :ignore-error-status t)
    (if (= exit-code 0)
        stdout
        (ipt-abort-request-handler
         500 1
         (format nil "Command '~a' failed with code ~d and error: ~a"
                 cmd exit-code stderr)))))

(defun validate-name (name &optional err-code err-message)
  (if (cl-ppcre:scan "^[a-zA-Z0-9_]+$" name)
      name
      (ipt-abort-request-handler 400 (or err-code 400) (or err-message (format nil "Invalid name: ~a" name)))))

(defun validate-number (number &optional err-code err-message)
  (if (cl-ppcre:scan "^[0-9]+$" number)
      (parse-integer number)
      (ipt-abort-request-handler 400 (or err-code 400) (or err-message (format nil "Invalid number: ~a" number)))))

(defun make-table-arg (table)
  (if (or (not table) (string= table "")) ""
      (format nil "-t ~a" (validate-name table 400 "Invalid table name"))))

(defun grep-lines (handler regex lines)
  (remove nil
          (mapcar
           (lambda (line)
             (multiple-value-bind (matched groups)
                 (cl-ppcre:scan-to-strings regex line)
               (when matched (apply handler matched (map 'list 'identity groups)))))
           lines)))

(defun ipt-chains-list ()
  (let* ((table (hunchentoot:get-parameter "table"))
         (cmd (format nil "sudo iptables ~a -L"
                      (make-table-arg table))))
    (jsown:to-json
     (grep-lines (lambda (ignored-1 chain ignored-2 policy refcount)
                   `(:obj ("chain" . ,chain) ("builtin" . ,(if policy :true :false))
                          ,(if policy
                               `("policy" . ,policy)
                               `("refcount" . ,refcount))))
                 "Chain ([a-zA-Z0-9_]+) \\((policy ([a-zA-Z0-9_]+)|([0-9]+) references)\\)"
                 (execute-iptables-cmd cmd)))))


(defun ipt-chain-create ()
  (let* ((table (hunchentoot:post-parameter "table"))
         (chain (hunchentoot:post-parameter "chain"))
         (cmd (format nil
                      "sudo iptables ~a -N ~a"
                      (make-table-arg table)
                      (validate-name chain))))
    (jsown:to-json (execute-iptables-cmd cmd))))


(defun ipt-chain-details (chain)
  (let* ((table (hunchentoot:get-parameter "table"))
         (cmd (format nil "sudo iptables ~a -L ~a"
                      (make-table-arg table)
                      chain))
         (lines (grep-lines (lambda (ignored-1 chain ignored-2 policy refcount)
                              `(:obj ("builtin" . ,(if policy :true :false))
                                     ,(if policy
                                          `("policy" . ,policy)
                                          `("refcount" . ,refcount))))
                            "Chain ([a-zA-Z0-9_]+) \\((policy ([a-zA-Z0-9_]+)|([0-9]+) references)\\)"
                            (execute-iptables-cmd cmd))))
    (if (not lines)
        (ipt-abort-request-handler 500 500 "Unexpected output by iptables")
        (jsown:to-json (car lines)))))

(defun ipt-chain-set-policy (chain)
  (let* ((table (hunchentoot:post-parameter "table"))
         (policy (hunchentoot:post-parameter "policy"))
         (cmd (format nil "sudo iptables ~a -P ~a ~a"
                      (make-table-arg table)
                      chain
                      (validate-name policy))))
    (jsown:to-json (execute-iptables-cmd cmd))))

(defun ipt-chain-delete (chain)
  (let* ((table (hunchentoot:post-parameter "table"))
         (cmd (format nil "sudo iptables ~a -X ~a"
                      (make-table-arg table)
                      (validate-name chain))))
    (jsown:to-json (execute-iptables-cmd cmd))))

(defun alist-to-json-obj (alist)
  (if (consp alist)
      (mapcar (lambda (c) (cons :obj c)) alist)
      alist))

(defun is-opt (string)
  (and (> (length string) 2) (string= (subseq string 0 2) "--")))

(defun global-opt-match (string)
  (cdr (assoc string
              '(("-p" . "protocol")
                ("-s" . "source")
                ("-d" . "dest")
                ("-m" . "match")
                ("-j" . "target"))
              :test #'string=)))

(defun custom-opt-match (string)
  (if (is-opt string) string))

(defun json-to-hash-table (json-obj)
  (if (and (listp json-obj) (eq (car json-obj) :obj))
      (let ((table (make-hash-table :test #'equal)))
        (mapc (lambda (c)
                (setf (gethash (car c) table)
                      (json-to-hash-table (cdr c))))
              (cdr json-obj))
        table)
      json-obj))

(defun ipt-group-options (options &optional (match-fn #'global-opt-match) grouped)
  (if options
      (let ((opt (funcall match-fn (car options))))
        (if opt
            (if (string= (cadr options) "!")
                (cons (cons opt (cons :neg grouped))
                      (ipt-group-options (cddr options) match-fn))
                (cons (push opt grouped)
                      (ipt-group-options (cdr options) match-fn)))
            (ipt-group-options (cdr options) match-fn (cons (car options) grouped))))))

(defun ipt-rule-list-to-json (lines)
  (jsown:to-json
   (mapcar (lambda (line)
             (let (params
                   matches
                   target-name
                   target-options)
               (loop for item in
                    (ipt-group-options (nreverse (cddr (cl-ppcre:split " +" line))))
                  do
                    (cond
                      ((string= (car item) "target")
                       (setq target-name (cadr item)
                             target-options (cddr item)))
                      ((string= (car item) "match")
                       (format t "~A~%" (cddr item))
                       (push (cons (cadr item)
                                   (cons :obj
                                         (ipt-group-options (nreverse (cddr item))
                                                            #'custom-opt-match)))
                             matches))
                      (t
                       (push (cons (car item) (cadr item)) params))))
               `(:obj
                 ("params" . ,(cons :obj params))
                 ("matches" . ,(cons :obj matches))
                 ("target" . (:obj ("name" . ,target-name) ("options" . ,target-options))))))
           lines)))

(defun ipt-rules-list (chain)
  (let* ((table (hunchentoot:post-parameter "table"))
         (cmd (format nil "sudo iptables ~a -S ~a"
                      (make-table-arg table)
                      (validate-name chain)))
         (lines (cdr (execute-iptables-cmd cmd))))
    (if lines
        (ipt-rule-list-to-json lines)
        (ipt-abort-request-handler 500 500 "Unexpected output from iptables"))))
    

(defun iptrest-restart ()
  (if (boundp 'ipt-acceptor)
      (hunchentoot:stop ipt-acceptor))
  (defparameter ipt-acceptor (make-instance 'iptrest-acceptor))
  (hunchentoot:start ipt-acceptor))

(defparameter default-server (make-instance 'hunchentoot:acceptor :port 8809))
