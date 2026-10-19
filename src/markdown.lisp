;;;; markdown.lisp — inline Markdown, read into nodes (from Cadre)

(in-package #:agent-integration)

;;; Nodes: (kind start end content-start content-end children . more)
;;;   :text :escape :code :strong :emphasis :strike :autolink
;;;   :link and :image, whose MORE is (url-start url-end url)

(defun md-punctuation-p (c) (and (graphic-char-p c) (not (alphanumericp c)) (char/= c #\Space)))

(defun md-space-p (c) (member c '(#\Space #\Tab #\Newline)))

(defun run-length (string start char)
  (loop for i from start below (length string)
        while (char= (char string i) char)
        count t))

(defun find-code-close (string start ticks end)
  "Where a run of exactly TICKS backticks starts, from START; nil if none."
  (loop with i = start
        while (< i end)
        do (if (char= (char string i) #\`)
               (let ((n (min (run-length string i #\`) (- end i))))
                 (when (= n ticks) (return i))
                 (incf i n))
               (incf i))))

(defun find-bracket-close (string start end)
  "The ] matching the [ before START, or nil."
  (loop with depth = 0
        for i from start below end
        do (case (char string i)
             (#\\ (incf i))
             (#\` (let ((close (find-code-close string (+ i (run-length string i #\`))
                                                (run-length string i #\`) end)))
                    (when close (setf i (+ close (run-length string i #\`) -1)))))
             (#\[ (incf depth))
             (#\] (if (zerop depth) (return i) (decf depth))))))

(defun find-paren-close (string start end)
  (loop with depth = 0
        for i from start below end
        do (case (char string i)
             (#\\ (incf i))
             (#\( (incf depth))
             (#\) (if (zerop depth) (return i) (decf depth)))
             (#\Space (when (and (zerop depth) (< (1+ i) end) (char= (char string (1+ i)) #\"))
                        ;; A title: (url "title")
                        (let ((q (position #\" string :start (+ i 2) :end end)))
                          (when q (setf i q))))))))

(defun left-flanking-p (string i n end)
  "Whether the delimiter run at I (N long) can open emphasis."
  (let ((after (and (< (+ i n) end) (char string (+ i n))))
        (before (and (> i 0) (char string (1- i)))))
    (and after (not (md-space-p after))
         (or (not (md-punctuation-p after)) (null before) (md-space-p before) (md-punctuation-p before)))))

(defun right-flanking-p (string i n start)
  "Whether the delimiter run at I (N long) can close emphasis begun at START."
  (let ((before (and (> i start) (char string (1- i))))
        (after (and (< (+ i n) (length string)) (char string (+ i n)))))
    (and before (not (md-space-p before))
         (or (not (md-punctuation-p before)) (null after) (md-space-p after) (md-punctuation-p after)))))

(defun find-emphasis-close (string start end char n)
  "Where a run of N CHARs closing emphasis begins, from START; nil if none."
  (loop with i = start
        while (< i end)
        do (let ((c (char string i)))
             (cond ((char= c #\\) (incf i 2))
                   ((char= c #\`)
                    (let* ((ticks (run-length string i #\`))
                           (close (find-code-close string (+ i ticks) ticks end)))
                      (setf i (if close (+ close ticks) (+ i ticks)))))
                   ((char= c char)
                    (let ((run (min (run-length string i char) (- end i))))
                      (if (and (>= run n) (> i start)
                               (right-flanking-p string (+ i (- run n)) n start)
                               ;; _ closes only at the end of a word.
                               (or (char/= char #\_)
                                   (let ((after (+ i run)))
                                     (or (>= after (length string)) (not (alphanumericp (char string after)))))))
                          (return (+ i (- run n)))
                          (incf i run))))
                   (t (incf i))))))

(defun url-end (string start end)
  (loop for i from start below end
        while (not (or (md-space-p (char string i)) (member (char string i) '(#\< #\>))))
        finally (return (let ((j i))
                          ;; Trailing punctuation isn't part of the URL.
                          (loop while (and (> j start) (member (char string (1- j)) '(#\. #\, #\; #\: #\! #\? #\) #\' #\")))
                                do (decf j))
                          j))))

(defun markdown-inlines (string &optional (start 0) (end (length string)))
  "The inline nodes of STRING between START and END."
  (let ((nodes '()) (text-start start) (i start))
    (flet ((flush (to) (when (< text-start to) (push (list :text text-start to text-start to nil) nodes)))
           (emit (node next) (push node nodes) (setf i next text-start next)))
      (loop while (< i end)
            do (let ((c (char string i)))
                 (cond
                   ;; \* is a literal *
                   ((and (char= c #\\) (< (1+ i) end) (md-punctuation-p (char string (1+ i))))
                    (flush i)
                    (emit (list :escape i (+ i 2) (1+ i) (+ i 2) nil) (+ i 2)))
                   ;; `code`
                   ((char= c #\`)
                    (let* ((ticks (run-length string i #\`))
                           (close (find-code-close string (+ i ticks) ticks end)))
                      (if close
                          (progn (flush i)
                                 (emit (list :code i (+ close ticks) (+ i ticks) close nil) (+ close ticks)))
                          (incf i ticks))))
                   ;; [text](url) and ![alt](url)
                   ((or (char= c #\[) (and (char= c #\!) (< (1+ i) end) (char= (char string (1+ i)) #\[)))
                    (let* ((open (if (char= c #\!) (1+ i) i))
                           (close (find-bracket-close string (1+ open) end))
                           (paren (and close (< (1+ close) end) (char= (char string (1+ close)) #\()
                                       (find-paren-close string (+ close 2) end))))
                      (if paren
                          (let* ((us (+ close 2))
                                 (ue (or (position #\Space string :start us :end paren) paren))
                                 (url (string-trim "<>" (subseq string us ue))))
                            (flush i)
                            (emit (list* (if (char= c #\!) :image :link) i (1+ paren) (1+ open) close
                                         (markdown-inlines string (1+ open) close)
                                         (list us ue url))
                                  (1+ paren)))
                          (incf i))))
                   ;; <https://…>
                   ((and (char= c #\<) (let ((close (position #\> string :start i :end end)))
                                         (and close (search "://" string :start2 i :end2 close)
                                              (not (find #\Space string :start i :end close)))))
                    (let ((close (position #\> string :start i :end end)))
                      (flush i)
                      (emit (list :autolink i (1+ close) (1+ i) close nil (subseq string (1+ i) close)) (1+ close))))
                   ;; https://… in the text
                   ((and (or (char= c #\h) (char= c #\w))
                         (or (zerop i) (not (alphanumericp (char string (1- i)))))
                         (or (and (< (+ i 8) end) (string= "https://" string :start2 i :end2 (+ i 8)))
                             (and (< (+ i 7) end) (string= "http://" string :start2 i :end2 (+ i 7)))
                             (and (< (+ i 4) end) (string= "www." string :start2 i :end2 (+ i 4)))))
                    (let ((e (url-end string i end)))
                      (flush i)
                      (emit (list :autolink i e i e nil (subseq string i e)) e)))
                   ;; ~~strike~~
                   ((and (char= c #\~) (< (1+ i) end) (char= (char string (1+ i)) #\~)
                         (left-flanking-p string i 2 end))
                    (let ((close (search "~~" string :start2 (+ i 2) :end2 end)))
                      (if (and close (> close (+ i 2)))
                          (progn (flush i)
                                 (emit (list :strike i (+ close 2) (+ i 2) close
                                             (markdown-inlines string (+ i 2) close))
                                       (+ close 2)))
                          (incf i 2))))
                   ;; **strong**, *emphasis*, ***both***, and the same with _
                   ((member c '(#\* #\_))
                    (let ((run (run-length string i c)))
                      (if (and (left-flanking-p string i (min run 3) end)
                               (or (char/= c #\_) (zerop i) (not (alphanumericp (char string (1- i))))))
                          (let* ((n (if (>= run 2) 2 1))
                                 (close (find-emphasis-close string (+ i n) end c n)))
                            (when (and (null close) (= n 2))
                              (setf n 1 close (find-emphasis-close string (+ i 1) end c 1)))
                            (if close
                                (progn (flush i)
                                       (emit (list (if (= n 2) :strong :emphasis) i (+ close n) (+ i n) close
                                                   (markdown-inlines string (+ i n) close))
                                             (+ close n)))
                                (incf i run)))
                          (incf i run))))
                   (t (incf i)))))
      (flush end))
    (nreverse nodes)))

