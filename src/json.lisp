;;;; json.lisp — a small JSON reader and writer (from Cadre)
;;;;
;;;; Claude Code's stream-json events and MCP's JSON-RPC messages are JSON.
;;;; In Lisp:
;;;;   object  →  (:object ("key" . value) …)     build one with JOBJ
;;;;   array   →  a vector
;;;;   string  →  a string, number → a number
;;;;   true    →  T, false → :FALSE, null → :NULL
;;;; The writer also writes NIL as null and keywords other than :false and
;;;; :null as strings ("foo" for :foo).

(in-package #:agent-duet)

(define-condition json-error (error)
  ((position :initarg :position :reader json-error-position))
  (:report (lambda (c s) (format s "Malformed JSON at ~d" (json-error-position c)))))

(defun jobj (&rest keys-and-values)
  "A JSON object from alternating keys (strings) and values. Pairs whose
value is :omit are left out."
  (cons :object (loop for (k v) on keys-and-values by #'cddr
                      unless (eq v :omit) collect (cons k v))))

(defun jobject-p (x) (and (consp x) (eq (car x) :object)))

(defun jget (object &rest path)
  "Follow PATH (keys, or indices into arrays) from OBJECT; nil if absent."
  (dolist (key path object)
    (setf object
          (cond ((and (stringp key) (jobject-p object)) (cdr (assoc key (cdr object) :test #'string=)))
                ((and (integerp key) (vectorp object) (not (stringp object)) (< -1 key (length object)))
                 (aref object key))
                (t (return nil))))))

(defun jtrue-p (x)
  "True if X is a JSON truth: not false, null or absent."
  (not (member x '(nil :false :null))))

(defun jlist (x)
  "X, a JSON array, as a list (nil for anything else)."
  (if (and (vectorp x) (not (stringp x))) (coerce x 'list) '()))

;;; Reading

(defun read-json (string &key (start 0))
  "The JSON value in STRING at START, and the position after it."
  (let ((i start) (n (length string)))
    (labels ((fail () (error 'json-error :position i))
             (peek () (if (< i n) (char string i) (fail)))
             (skip () (loop while (and (< i n) (member (char string i) '(#\Space #\Tab #\Newline #\Return)))
                            do (incf i)))
             (expect (word value)
               (if (and (<= (+ i (length word)) n) (string= word string :start2 i :end2 (+ i (length word))))
                   (progn (incf i (length word)) value)
                   (fail)))
             (value ()
               (skip)
               (case (peek)
                 (#\{ (incf i) (object))
                 (#\[ (incf i) (array))
                 (#\" (incf i) (str))
                 (#\t (expect "true" t))
                 (#\f (expect "false" :false))
                 (#\n (expect "null" :null))
                 (t (num))))
             (object ()
               (let ((pairs '()))
                 (skip)
                 (if (char= (peek) #\})
                     (incf i)
                     (loop
                      (skip)
                      (unless (char= (peek) #\") (fail))
                      (incf i)
                      (let ((key (str)))
                        (skip)
                        (unless (char= (peek) #\:) (fail))
                        (incf i)
                        (push (cons key (value)) pairs))
                      (skip)
                      (case (peek)
                        (#\, (incf i))
                        (#\} (incf i) (return))
                        (t (fail)))))
                 (cons :object (nreverse pairs))))
             (array ()
               (let ((items '()))
                 (skip)
                 (if (char= (peek) #\])
                     (incf i)
                     (loop
                      (push (value) items)
                      (skip)
                      (case (peek)
                        (#\, (incf i))
                        (#\] (incf i) (return))
                        (t (fail)))))
                 (coerce (nreverse items) 'vector)))
             (hex4 ()
               (when (> (+ i 4) n) (fail))
               (prog1 (or (parse-integer string :start i :end (+ i 4) :radix 16 :junk-allowed t) (fail))
                 (incf i 4)))
             (str ()
               (with-output-to-string (out)
                 (loop
                  (let ((c (peek)))
                    (incf i)
                    (case c
                      (#\" (return))
                      (#\\ (let ((e (peek)))
                             (incf i)
                             (case e
                               (#\n (write-char #\Newline out))
                               (#\t (write-char #\Tab out))
                               (#\r (write-char #\Return out))
                               (#\b (write-char #\Backspace out))
                               (#\f (write-char #\Page out))
                               (#\u (let ((code (hex4)))
                                      ;; A surrogate pair is one character.
                                      (when (and (<= #xD800 code #xDBFF) (< (+ i 1) n)
                                                 (char= (char string i) #\\) (char= (char string (1+ i)) #\u))
                                                 (incf i 2)
                                                 (let ((low (hex4)))
                                                   (setf code (+ #x10000 (ash (- code #xD800) 10) (- low #xDC00)))))
                                        (write-char (code-char code) out)))
                                (t (write-char e out)))))
                       (t (write-char c out)))))))
               (num ()
                    (let ((s i))
                      (loop while (and (< i n) (find (char string i) "+-0123456789.eE")) do (incf i))
                      (when (= s i) (fail))
                      (let ((text (subseq string s i)))
                        (if (every (lambda (c) (or (digit-char-p c) (char= c #\-))) text)
                            (or (ignore-errors (parse-integer text)) (fail))
                            (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
                              (let ((v (ignore-errors (read-from-string text))))
                                (if (realp v) v (fail)))))))))
             (let ((v (value)))
               (values v i)))))

  (defun parse-json (string)
    "The JSON value STRING holds (and nothing else but space)."
    (multiple-value-bind (v end) (read-json string)
      (unless (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) (subseq string end))
        (error 'json-error :position end))
      v))

  ;;; Writing

  (defun write-json-string (string out)
    (write-char #\" out)
    (loop for c across string
          for code = (char-code c)
          do (case c
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (if (< code 32)
                      (format out "\\u~4,'0x" code)
                      (write-char c out)))))
    (write-char #\" out))

  (defun write-json (value &optional (out *standard-output*))
    (cond ((eq value t) (write-string "true" out))
          ((eq value :false) (write-string "false" out))
          ((member value '(nil :null)) (write-string "null" out))
          ((stringp value) (write-json-string value out))
          ((keywordp value) (write-json-string (string-downcase (symbol-name value)) out))
          ((integerp value) (format out "~d" value))
          ((realp value) (let ((*read-default-float-format* 'double-float))
                           (format out "~f" (coerce value 'double-float))))
          ((jobject-p value)
           (write-char #\{ out)
           (loop for (pair . rest) on (cdr value)
                 do (write-json-string (car pair) out)
                    (write-char #\: out)
                    (write-json (cdr pair) out)
                    (when rest (write-char #\, out)))
           (write-char #\} out))
          ((or (vectorp value) (listp value))
           (write-char #\[ out)
           (let ((first t))
             (map nil (lambda (x)
                        (unless first (write-char #\, out))
                        (setf first nil)
                        (write-json x out))
                  value))
           (write-char #\] out))
          (t (error "Cannot write ~s as JSON" value)))
    value)

  (defun json-string (value)
    (with-output-to-string (out) (write-json value out)))
