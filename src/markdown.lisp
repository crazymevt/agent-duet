;;;; markdown.lisp — inline Markdown, read into nodes (from Cadre)

(in-package #:agent-duet)

;;; Nodes: (kind start end content-start content-end children . more)
;;;   :text :escape :code :strong :emphasis :strike :autolink
;;;   :link and :image, whose MORE is (url-start url-end url)
;;;   :directive, whose content is a PUA-delimited artifact directive body

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
                    ;; Codex artifact directives: U+E200 name U+E202 JSON U+E201.
                    ((char= c (code-char #xE200))
                     (let ((close (position (code-char #xE201) string :start (1+ i) :end end)))
                       (if close
                           (progn (flush i)
                             (emit (list :directive i (1+ close) (1+ i) close nil) (1+ close)))
                           (incf i))))
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


    ;;; Blocks (from Cadre): MARKDOWN-BLOCKS reads a whole document into blocks
    ;;; for the preview. Each block ends with its first line in the source.
    ;;;   (:heading level inlines line)  (:paragraph inlines line)
    ;;;   (:code info text line)  (:quote blocks line)  (:rule line)
    ;;;   (:list ordered start ((task blocks) …) line)  — task: nil, :open, :done
    ;;;   (:table aligns header-inlines rows-of-inlines line)

    (defun split-text-lines (text)
      (loop with start = 0
            for nl = (position #\Newline text :start start)
            collect (string-right-trim '(#\Return) (subseq text start (or nl (length text))))
            while nl do (setf start (1+ nl))))

    (defun leading-spaces (line)
      (or (position-if-not (lambda (c) (char= c #\Space)) line) (length line)))


    (defun fence-open (line)
      "If LINE opens a fenced code block: the fence character, its length, and the info string."
      (let ((indent (leading-spaces line)))
        (when (and (< indent 4) (< indent (length line)) (member (char line indent) '(#\` #\~)))
          (let* ((c (char line indent))
                 (n (run-length line indent c))
                 (info (string-trim " " (subseq line (+ indent n)))))
            (when (and (>= n 3) (not (and (char= c #\`) (find #\` info))))
              (values c n info))))))

    (defun fence-close-p (line char count)
      (let ((indent (leading-spaces line)))
        (and (< indent 4) (< indent (length line)) (char= (char line indent) char)
             (let ((n (run-length line indent char)))
               (and (>= n count) (string= "" (string-trim " " (subseq line (+ indent n)))))))))

    (defun heading-level (line)
      "The level of the ATX heading LINE (# … ######), and where its text starts; nil if not one."
      (let ((indent (leading-spaces line)))
        (when (and (< indent 4) (< indent (length line)) (char= (char line indent) #\#))
          (let ((n (run-length line indent #\#)))
            (when (and (<= n 6) (or (= (+ indent n) (length line)) (char= (char line (+ indent n)) #\Space)))
              (values n (min (length line) (+ indent n 1))))))))

    (defun thematic-break-p (line)
      (let ((chars (remove #\Space line)))
        (and (< (leading-spaces line) 4) (>= (length chars) 3)
             (member (char chars 0) '(#\- #\* #\_))
             (every (lambda (c) (char= c (char chars 0))) chars))))

    (defun setext-underline (line)
      "1 for a === line, 2 for ---, else nil."
      (let ((s (string-trim " " line)))
        (and (< (leading-spaces line) 4) (plusp (length s))
             (cond ((every (lambda (c) (char= c #\=)) s) 1)
                   ((every (lambda (c) (char= c #\-)) s) 2)))))

    (defun list-marker (line)
      "If LINE starts a list item: the marker's start and end, where the content
starts, and :ordered with its number or :bullet."
      (let ((indent (leading-spaces line)))
        (when (< indent (length line))
          (let ((c (char line indent)))
            (cond ((and (member c '(#\- #\* #\+))
                        (or (= (1+ indent) (length line)) (char= (char line (1+ indent)) #\Space))
                        (not (thematic-break-p line)))
                   (values indent (1+ indent) (min (length line) (+ indent 2)) :bullet))
                  ((digit-char-p c)
                   (let ((e (or (position-if-not #'digit-char-p line :start indent) (length line))))
                     (when (and (< e (length line)) (<= (- e indent) 9) (member (char line e) '(#\. #\)))
                                (or (= (1+ e) (length line)) (char= (char line (1+ e)) #\Space)))
                       (values indent (1+ e) (min (length line) (+ e 2)) :ordered
                               (parse-integer line :start indent :end e))))))))))

    (defun table-row-p (line) (and (find #\| line) (not (fence-open line))))

                                         (defun table-delimiter-p (line)
                                           (let ((s (string-trim " |" line)))
                                             (and (find #\- s) (find #\| line) (every (lambda (c) (member c '(#\- #\: #\| #\Space))) s))))

                                                                     ;;; Inlines are nodes over the string they came with: (string . nodes).

                                                                     (defun inlines-of (string)
                                                                       (cons string (markdown-inlines string)))

                                                                     (defun strip-indent (line n)
                                                                       "LINE without up to N leading spaces."
                                                                       (subseq line (min (leading-spaces line) n (length line))))

                                                                     (defun blank-line-p (line) (string= "" (string-trim '(#\Space #\Tab) line)))

                                                                     (defun paragraph-text (lines)
                                                                       "LINES joined into a paragraph's text: a line ending in two spaces or \\
breaks the line there; otherwise lines run together."
                                                                       (with-output-to-string (out)
                                                                         (loop for (line . more) on lines
                                                                               do (let* ((trimmed (string-left-trim " " line))
                                                                                         (hard (and more (or (and (>= (length trimmed) 2)
                                                                                                                  (string= "  " trimmed :start2 (- (length trimmed) 2)))
                                                                                                             (and (plusp (length trimmed))
                                                                                                                  (char= #\\ (char trimmed (1- (length trimmed)))))))))
                                                                                    (write-string (string-right-trim " \\" trimmed) out)
                                                                                    (when more (write-char (if hard #\Newline #\Space) out))))))

                                                                     (defun split-table-row (line)
                                                                       (let* ((s (string-trim " " line))
                                                                              (s (if (and (plusp (length s)) (char= (char s 0) #\|)) (subseq s 1) s))
                                                                                                                    (s (if (and (plusp (length s)) (char= (char s (1- (length s))) #\|)
                                                                                                                                                          (or (< (length s) 2) (char/= (char s (- (length s) 2)) #\\)))
                                                                                                                                                          (subseq s 0 (1- (length s))) s)))
                                                                                                                       (loop with cells = '() with start = 0
                                                                                                                             for i from 0 to (length s)
                                                                                                                             do (when (or (= i (length s)) (and (char= (char s i) #\|) (or (zerop i) (char/= (char s (1- i)) #\\))))
                                                                                                                                                                       (push (string-trim " " (subseq s start i)) cells)
                                                                                                                                                                       (setf start (1+ i)))
                                                                                                                                                                finally (return (nreverse cells)))))

                                                                                                                                (defun table-alignments (line)
                                                                                                                                  (mapcar (lambda (cell)
                                                                                                                                            (let ((left (and (plusp (length cell)) (char= (char cell 0) #\:)))
                                                                                                                                                  (right (and (plusp (length cell)) (char= (char cell (1- (length cell))) #\:))))
                                                                                                                                              (cond ((and left right) :center) (right :right) (t :left))))
                                                                                                                                          (split-table-row line)))

                                                                                                                                (defun starts-block-p (line)
                                                                                                                                  "Whether LINE begins a block other than a paragraph."
                                                                                                                                  (or (heading-level line) (fence-open line) (thematic-break-p line) (list-marker line)))

                                                                                                                                (defun quoted-line-p (line)
                                                                                                                                  (let ((indent (leading-spaces line)))
                                                                                                                                    (and (< indent 4) (< indent (length line)) (char= (char line indent) #\>))))

                                                                                                                                (defun unquote-line (line)
                                                                                                                                  "LINE without its > and the space after it, if it has one."
                                                                                                                                  (if (quoted-line-p line)
                                                                                                                                      (let ((rest (subseq line (1+ (leading-spaces line)))))
                                                                                                                                        (if (and (plusp (length rest)) (char= (char rest 0) #\Space)) (subseq rest 1) rest))
                                                                                                                                      line))

                                                                                                                                (defvar *lazy-quotes* '()
                                                                                                                                  "For each quote being parsed, innermost first, a function telling whether a
document line is in it without a >. Such a line can only carry on a paragraph:
where it can't, the innermost quote ends before it (thrown to as LAZY-LINE).")

                                                                                                                                (defun lazy-line-p (n)
                                                                                                                                  (some (lambda (lazy) (funcall lazy n)) *lazy-quotes*))

                                                                                                                                (defun parse-blocks (lines first-line)
                                                                                                                                  "The blocks in LINES (a list of strings), the first being line FIRST-LINE of the document."
                                                                                                                                  (let ((lines (coerce lines 'vector)))
                                                                                                                                    (parse-line-blocks (lambda (k) (aref lines k)) (lambda (k) (< k (length lines))) first-line)))

                                                                                                                                (defun parse-line-blocks (line-function line-p first-line)
                                                                                                                                  "The blocks in the lines LINE-FUNCTION gives for 0 up, as long as LINE-P is
true of their number, the first being line FIRST-LINE of the document."
                                                                                                                                  (let ((blocks '())
                                                                                                                                        (paragraph '())
                                                                                                                                        (paragraph-line nil)
                                                                                                                                        (i 0))
                                                                                                                                    (labels ((line (k) (funcall line-function k))
                                                                                                                                             (more (k) (funcall line-p k))
                                                                                                                                             (end-paragraph ()
                                                                                                                                               (when paragraph
                                                                                                                                                 (push (list :paragraph (inlines-of (paragraph-text (reverse paragraph))) paragraph-line) blocks)
                                                                                                                                                 (setf paragraph '())))
                                                                                                                                             (add (block) (end-paragraph) (push block blocks)))
                                                                                                                                      (loop while (more i)
                                                                                                                                            do (let* ((line (line i))
                                                                                                                                                      (n (+ first-line i)))
                                                                                                                                                 (cond
                                                                                                                                                  ((blank-line-p line) (end-paragraph) (incf i))
                                                                                                                                                  ((lazy-line-p n)
                                                                                                                                                   (unless paragraph (throw 'lazy-line n))
                                                                                                                                                   (push line paragraph)
                                                                                                                                                   (incf i))
                                                                                                                                                  ;; ``` fenced code
                                                                                                                                                  ((fence-open line)
                                                                                                                                                   (multiple-value-bind (char count info) (fence-open line)
                                                                                                                                                     (let* ((indent (leading-spaces line))
                                                                                                                                                            (end (loop for k from (1+ i)
                                                                                                                                                                       while (and (more k) (not (fence-close-p (line k) char count))
                                                                                                                                                                                  (not (lazy-line-p (+ first-line k))))
                                                                                                                                                                       finally (return k)))
                                                                                                                                                            (code (loop for k from (1+ i) below end collect (strip-indent (line k) indent))))
                                                                                                                                                       (add (list :code info (format nil "~{~a~^~%~}" code) n))
                                                                                                                                                       ;; A lazy line ends the quote, and with it the code.
                                                                                                                                                       (setf i (if (and (more end) (not (lazy-line-p (+ first-line end)))) (1+ end) end)))))
                                                                                                                                                  ;; # heading
                                                                                                                                                  ((heading-level line)
                                                                                                                                                   (multiple-value-bind (level start) (heading-level line)
                                                                                                                                                     (let ((text (string-right-trim " #" (subseq line start))))
                                                                                                                                                       (add (list :heading level (inlines-of text) n)))
                                                                                                                                                     (incf i)))
                                                                                                                                                  ;; Text, then === or ---: a heading
                                                                                                                                                  ((and paragraph (setext-underline line))
                                                                                                                                                   (let ((level (setext-underline line))
                                                                                                                                                         (text (paragraph-text (reverse paragraph)))
                                                                                                                                                         (at paragraph-line))
                                                                                                                                                     (setf paragraph '())
                                                                                                                                                     (add (list :heading level (inlines-of text) at))
                                                                                                                                                     (incf i)))
                                                                                                                                                  ((thematic-break-p line) (add (list :rule n)) (incf i))
                                                                                                                                                  ;; > quote
                                                                                                                                                  ((quoted-line-p line)
                                                                                                                                                   ;; The quote can take in the lines up to a blank one or one starting
                                                                                                                                                   ;; another block without a >. The ones without may be more of a
                                                                                                                                                   ;; paragraph in it ("lazy" continuation): parsing decides. Lines
                                                                                                                                                   ;; are checked only as parsing reaches them, as a quote often ends
                                                                                                                                                   ;; early.
                                                                                                                                                   (let* ((start i)
                                                                                                                                                          (end nil)
                                                                                                                                                          (checked i)
                                                                                                                                                          (unquoted (make-hash-table))
                                                                                                                                                          (inner (lambda (k)
                                                                                                                                                                   (let ((k (+ start k)))
                                                                                                                                                                     (or (gethash k unquoted)
                                                                                                                                                                         (setf (gethash k unquoted) (unquote-line (line k)))))))
                                                                                                                                                          (in-quote-p (lambda (k)
                                                                                                                                                                        (let ((k (+ start k)))
                                                                                                                                                                          (loop while (and (null end) (<= checked k))
                                                                                                                                                                                do (if (and (more checked)
                                                                                                                                                                                            (let ((l (line checked)))
                                                                                                                                                                                              (and (not (blank-line-p l))
                                                                                                                                                                                                   (or (quoted-line-p l)
                                                                                                                                                                                                       (not (or (starts-block-p l) (setext-underline l)))))))
                                                                                                                                                                                       (incf checked)
                                                                                                                                                                                       (setf end checked)))
                                                                                                                                                                          (< k checked)))))
                                                                                                                                                     (let ((*lazy-quotes* (cons (lambda (m)
                                                                                                                                                                                  (let ((k (- m first-line start)))
                                                                                                                                                                                    (and (>= k 0) (funcall in-quote-p k)
                                                                                                                                                                                         (not (quoted-line-p (line (+ start k)))))))
                                                                                                                                                                                *lazy-quotes*)))
                                                                                                                                                       (loop (let ((cut (catch 'lazy-line
                                                                                                                                                                          (return (add (list :quote (parse-line-blocks inner in-quote-p (+ first-line start)) n))))))
                                                                                                                                                               ;; The quote ends before line CUT.
                                                                                                                                                               (setf end (- cut first-line) checked end))))
                                                                                                                                                     (setf i end)))
                                                                                                                                                  ;; - item, 1. item
                                                                                                                                                  ((and (list-marker line) (or (null paragraph) (not (blank-line-p (subseq line (nth-value 2 (list-marker line)))))))
                                                                                                                                                   (multiple-value-bind (ms me content kind number) (list-marker line)
                                                                                                                                                     (declare (ignore ms me content))
                                                                                                                                                     (let ((items '()))
                                                                                                                                                       (loop while (and (more i) (eq (nth-value 3 (list-marker (line i))) kind))
                                                                                                                                                             do (multiple-value-bind (ims ime icontent) (list-marker (line i))
                                                                                                                                                                  (declare (ignore ims ime))
                                                                                                                                                                  (let ((item-lines (list (subseq (line i) icontent)))
                                                                                                                                                                        (item-line (+ first-line i)))
                                                                                                                                                                    (incf i)
                                                                                                                                                                    ;; Following lines: indented to the content, blank, or lazy text.
                                                                                                                                                                    (loop while (more i)
                                                                                                                                                                          do (let ((l (line i)))
                                                                                                                                                                               (cond ((blank-line-p l)
                                                                                                                                                                                      (if (and (more (1+ i))
                                                                                                                                                                                               (>= (leading-spaces (line (1+ i))) icontent)
                                                                                                                                                                                               (not (blank-line-p (line (1+ i)))))
                                                                                                                                                                                          (progn (push "" item-lines) (incf i))
                                                                                                                                                                                          (return)))
                                                                                                                                                                                     ((>= (leading-spaces l) icontent)
                                                                                                                                                                                      (push (strip-indent l icontent) item-lines) (incf i))
                                                                                                                                                                                     ((or (list-marker l) (fence-open l) (heading-level l)
                                                                                                                                                                                          (thematic-break-p l)
                                                                                                                                                                                          (blank-line-p (first item-lines)))
                                                                                                                                                                                      (return))
                                                                                                                                                                                     (t (push l item-lines) (incf i)))))
                                                                                                                                                                    (let* ((item-lines (nreverse item-lines))
                                                                                                                                                                           (first (first item-lines))
                                                                                                                                                                           (task (and (>= (length first) 3) (char= (char first 0) #\[)
                                                                                                                                                                                      (char= (char first 2) #\])
                                                                                                                                                                                      (or (= (length first) 3) (char= (char first 3) #\Space))
                                                                                                                                                                                      (case (char first 1)
                                                                                                                                                                                        (#\Space :open) ((#\x #\X) :done)))))
                                                                                                                                                                      (when task
                                                                                                                                                                        (setf (first item-lines) (subseq first (min (length first) 4))))
                                                                                                                                                                      (push (list task (parse-blocks item-lines item-line)) items))))
                                                                                                                                                                ;; A blank line between items keeps the list going.
                                                                                                                                                                (when (and (more (1+ i)) (blank-line-p (line i))
                                                                                                                                                                           (eq (nth-value 3 (list-marker (line (1+ i)))) kind))
                                                                                                                                                                  (incf i)))
                                                                                                                                                       (add (list :list (eq kind :ordered) (or number 1) (nreverse items) n)))))
                                                                                                                                                  ;; | table |
                                                                                                                                                  ((and (table-row-p line) (more (1+ i)) (table-delimiter-p (line (1+ i)))
                                                                                                                                                        (not (lazy-line-p (1+ n))))
                                                                                                                                                   (let ((header (split-table-row line))
                                                                                                                                                         (aligns (table-alignments (line (1+ i))))
                                                                                                                                                         (rows '()))
                                                                                                                                                     (incf i 2)
                                                                                                                                                     (loop while (and (more i) (table-row-p (line i)) (not (blank-line-p (line i)))
                                                                                                                                                                      (not (lazy-line-p (+ first-line i))))
                                                                                                                                                           do (push (mapcar #'inlines-of (split-table-row (line i))) rows)
                                                                                                                                                              (incf i))
                                                                                                                                                     (add (list :table aligns (mapcar #'inlines-of header) (nreverse rows) n))))
                                                                                                                                                  ;; Indented code
                                                                                                                                                  ((and (null paragraph) (>= (leading-spaces line) 4))
                                                                                                                                                   (let ((code '()))
                                                                                                                                                     (loop while (and (more i) (not (lazy-line-p (+ first-line i)))
                                                                                                                                                                      (or (>= (leading-spaces (line i)) 4) (blank-line-p (line i))))
                                                                                                                                                           do (push (strip-indent (line i) 4) code) (incf i))
                                                                                                                                                     (loop while (and code (blank-line-p (first code))) do (pop code))
                                                                                                                                                     (add (list :code "" (format nil "~{~a~^~%~}" (nreverse code)) n))))
                                                                                                                                                  (t (unless paragraph (setf paragraph-line n))
                                                                                                                                                     (push line paragraph)
                                                                                                                                                     (incf i)))))
                                                                                                                                      (end-paragraph))
                                                                                                                                    (nreverse blocks)))

                                                                                                                                (defun markdown-blocks (text)
                                                                                                                                  "The blocks of the Markdown document TEXT."
                                                                                                                                  (parse-blocks (split-text-lines text) 0))

                                                                                                                                (defun inline-plain-text (inlines)
                                                                                                                                  "The text of INLINES ((string . nodes)) without markup."
                                                                                                                                  (let ((string (car inlines)))
                                                                                                                                    (with-output-to-string (out)
                                                                                                                                      (labels ((walk (node)
                                                                                                                                                 (destructuring-bind (kind s e cs ce children &rest more) node
                                                                                                                                                   (declare (ignore s e more))
                                                                                                                                                   (if children (mapc #'walk children) (write-string string out :start cs :end ce))
                                                                                                                                                   (when (and (eq kind :image) (null children)) nil))))
                                                                                                                                        (mapc #'walk (cdr inlines))))))

                                                                                                                                (defun markdown-headings (text)
                                                                                                                                  "The headings of the Markdown document TEXT, as (level title line)."
                                                                                                                                  (let ((headings '()))
                                                                                                                                    (labels ((walk (blocks)
                                                                                                                                               (dolist (b blocks)
                                                                                                                                                 (case (first b)
                                                                                                                                                   (:heading (push (list (second b) (inline-plain-text (third b)) (fourth b)) headings))
                                                                                                                                                   (:quote (walk (second b)))))))
                                                                                                                                      (walk (markdown-blocks text)))
                                                                                                                                    (nreverse headings)))


                                                                                                                                (defun heading-anchor (title)
                                                                                                                                  "The #anchor GitHub gives a heading titled TITLE."
                                                                                                                                  (with-output-to-string (out)
                                                                                                                                    (loop for c across (string-downcase title)
                                                                                                                                          do (cond ((or (alphanumericp c) (char= c #\-) (char= c #\_)) (write-char c out))
                                                                                                                                                   ((char= c #\Space) (write-char #\- out))))))

                                                                                                                                (defun pad-cell (text pad align)
                                                                                                                                  "TEXT with PAD spaces placed for ALIGN (:left, :right or :center)."
                                                                                                                                  (flet ((spaces (n) (make-string (max 0 n) :initial-element #\Space)))
                                                                                                                                    (ecase align
                                                                                                                                      (:left (concatenate 'string text (spaces pad)))
                                                                                                                                      (:right (concatenate 'string (spaces pad) text))
                                                                                                                                      (:center (concatenate 'string (spaces (floor pad 2)) text (spaces (ceiling pad 2)))))))

                                                                                                                                ;;; Finding documents

                                                                                                                                (defparameter *markdown-types* '("md" "markdown" "mdown" "mkd"))

                                                                                                                                (defparameter *skipped-folders* '("node_modules" "target" "build" "dist" "vendor" "__pycache__")
                                                                                                                                  "Folders not searched for documents (besides hidden ones).")

                                                                                                                                (defun markdown-file-p (path)
                                                                                                                                  (and (pathname-name path)
                                                                                                                                       (member (pathname-type path) *markdown-types* :test #'equalp)))

                                                                                                                                (defun markdown-files (directory &key (max-depth 6) (limit 2000))
                                                                                                                                  "The Markdown files under DIRECTORY, as paths relative to it (strings),
sorted: top-level files first, then by path. Hidden and build folders are
skipped."
                                                                                                                                  (let ((root (let ((dir (uiop:ensure-directory-pathname directory)))
                                                                                                                                                ;; As DIRECTORY-FILES names them, so they can be made relative.
                                                                                                                                                (or (ignore-errors (truename dir)) dir)))
                                                                                                                                        (found '())
                                                                                                                                        (count 0))
                                                                                                                                    (labels ((walk (dir depth)
                                                                                                                                               (dolist (file (ignore-errors (uiop:directory-files dir)))
                                                                                                                                                 (when (and (< count limit) (markdown-file-p file))
                                                                                                                                                   (incf count)
                                                                                                                                                   (push (enough-namestring file root) found)))
                                                                                                                                               (when (< depth max-depth)
                                                                                                                                                 (dolist (sub (ignore-errors (uiop:subdirectories dir)))
                                                                                                                                                   (let ((name (car (last (pathname-directory sub)))))
                                                                                                                                                     (unless (or (not (stringp name)) (zerop (length name)) (char= (char name 0) #\.)
                                                                                                                                                                 (member name *skipped-folders* :test #'string=))
                                                                                                                                                       (walk sub (1+ depth))))))))
                                                                                                                                      (walk root 0))
                                                                                                                                    (sort found (lambda (a b)
                                                                                                                                                  (let ((da (count #\/ a)) (db (count #\/ b)))
                                                                                                                                                    (if (= (min da 1) (min db 1)) (string-lessp a b) (< da db)))))))
