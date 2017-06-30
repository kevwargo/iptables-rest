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

(defun is-opt (string)
  (and (> (length string) 2) (string= (subseq string 0 2) "--")))

(defun global-opt-match (string)
  (cdr (assoc string *ipt-params* :test #'string=)))

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
                (cons (cons opt (cons -1 grouped))
                      (ipt-group-options (cddr options) match-fn))
                (cons (push opt grouped)
                      (ipt-group-options (cdr options) match-fn)))
            (ipt-group-options (cdr options) match-fn (cons (car options) grouped))))))

(defun transform-options (options)
  (mapcar (lambda (opt)
            `(:obj ("name" . ,(car opt)) . ,(if (eq (cadr opt) -1)
                                                `(("value" . ,(format nil "~{~a~^ ~}" (cddr opt)))
                                                  ("invert" . :true))
                                                `(("value" . ,(format nil "~{~a~^ ~}" (cdr opt)))))))
          options))

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
                       (push `(:obj ("name" . ,(cadr item))
                                    ,(cons "options"
                                           (transform-options
                                            (ipt-group-options (nreverse (cddr item))
                                                               #'custom-opt-match))))
                             matches))
                      (t
                       (push item params))))
               `(:obj
                 ("params" . ,(transform-options params))
                 ("matches" . ,matches)
                 ("target" . (:obj ("name" . ,target-name)
                                   ,(cons
                                     "options"
                                     (cons
                                      :obj
                                      (ipt-group-options (nreverse target-options)
                                                         #'custom-opt-match))))))))
           lines)))
