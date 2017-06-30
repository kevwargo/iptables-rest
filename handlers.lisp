(load "ipt-utils")

(defvar *rules*)

(defun json-safe-val (object key)
  (when (jsown:keyp object key)
    (jsown:val object key)))

(defun ipt-static-handler (path)
  (hunchentoot:handle-static-file (parse-namestring path)))

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

(defun ipt-rules-list (chain)
  (let* ((table (hunchentoot:get-parameter "table"))
         (cmd (format nil "sudo iptables ~a -S ~a"
                      (make-table-arg table)
                      (validate-name chain)))
         (lines (cdr (execute-iptables-cmd cmd))))
    (if lines
        (ipt-rule-list-to-json lines)
        (ipt-abort-request-handler 500 500 "Unexpected output from iptables"))))

(defun ipt-rule-create (chain)
  (let* ((table (hunchentoot:get-parameter "table"))
         (rules (jsown:parse (hunchentoot:raw-post-data :force-text t)))
         (target (json-safe-val rules "target"))
         rules-list)
    (setq *rules* rules)
    (jsown:do-json-keys (name options) (json-safe-val rules "params")
      (setq rules-list
            (append rules-list
                    (list (format nil "~a~a ~a"
                                  (if (json-safe-val options "invert") "! " "")
                                  (car (rassoc name *ipt-params* :test #'string=))
                                  (json-safe-val options "value"))))))
    (dolist (match (json-safe-val rules "matches"))
      (setq rules-list
            (append rules-list
                    (list (format nil "-m ~a" (json-safe-val match "name")))))
      (dolist (option (json-safe-val match "options"))
        (setq rules-list
              (append rules-list
                      (list (format nil "~a~a ~a"
                                    (if (json-safe-val option "invert") "! " "")
                                    (json-safe-val option "name")
                                    (json-safe-val option "value")))))))
    (setq rules-list
          (append rules-list
                  (list (format nil "-j ~a"
                                (json-safe-val target "name")))))
    (dolist (option (json-safe-val target "options"))
      (setq rules-list
            (append rules-list
                    (list (format nil "~a ~a"
                                  (json-safe-val option "name")
                                  (json-safe-val option "value"))))))
    (jsown:to-json
     (execute-iptables-cmd (format nil "sudo iptables ~a -A ~a ~{~a~^ ~}"
                                   (make-table-arg table)
                                   (validate-name chain)
                                   rules-list)))))

(defun ipt-rule-delete (chain rulenum)
  (let* ((table (hunchentoot:get-parameter "table"))
         (cmd (format nil "sudo iptables ~a -D ~a ~a"
                      (make-table-arg table)
                      (validate-name chain)
                      rulenum)))
    (jsown:to-json
     (execute-iptables-cmd cmd))))
