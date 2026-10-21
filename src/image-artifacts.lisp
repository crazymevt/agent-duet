;;;; Local image references shared by the headless core and conversation UI.

(in-package #:agent-duet)

(defun image-artifact-decode (string)
  "Decode percent-escaped UTF-8 without treating plus as a space."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
    (loop with i = 0
          while (< i (length string))
          for ch = (char string i)
          do (if (char= ch #\%)
                 (let ((hi (and (< (+ i 2) (length string))
                                (digit-char-p (char string (1+ i)) 16)))
                       (lo (and (< (+ i 2) (length string))
                                (digit-char-p (char string (+ i 2)) 16))))
                   (unless (and hi lo) (error "Invalid percent escape"))
                   (vector-push-extend (+ (* hi 16) lo) bytes)
                   (incf i 2))
                 (loop for byte across (sb-ext:string-to-octets (string ch) :external-format :utf-8)
                       do (vector-push-extend byte bytes)))
             (incf i))
    (sb-ext:octets-to-string bytes :external-format :utf-8)))

(defun image-artifact-uri (pathname)
  "Return an absolute file URI, escaping the UTF-8 bytes of PATHNAME."
  (let ((name (uiop:native-namestring (uiop:ensure-absolute-pathname pathname))))
    (with-output-to-string (out)
      (write-string "file://" out)
      (loop for byte across (sb-ext:string-to-octets name :external-format :utf-8)
            do (if (or (<= 65 byte 90) (<= 97 byte 122) (<= 48 byte 57)
                       (find byte '(45 46 95 126 47)))
                   (write-char (code-char byte) out)
                   (format out "%~2,'0X" byte))))))

(defparameter *image-artifact-extensions*
  '("png" "jpg" "jpeg" "gif" "webp" "svg" "bmp" "tif" "tiff" "ico")
  "Local image artifact formats, independent of agent attachment restrictions.
The UI can offer opening a file even when its thumbnail decoder lacks support.")

(defun resolve-image-artifact (reference directory)
  "Resolve a local image reference against DIRECTORY, or return NIL.
Only file URIs with an empty or localhost authority are local. Plain paths
are tried literally before decoding URL escapes. Prefix relative names containing
an apparent URI scheme (such as screens:home.png) with ./ to make them paths.
Actual image decoding remains the responsibility of the UI."
  (ignore-errors
    (unless (or (stringp reference) (pathnamep reference))
      (return-from resolve-image-artifact nil))
    (let* ((reference (if (pathnamep reference) (uiop:native-namestring reference) reference))
           (colon (position #\: reference))
           (scheme (and colon (> colon 0)
                        (alpha-char-p (char reference 0))
                        (every (lambda (ch) (or (alphanumericp ch) (find ch "+-.")))
                               (subseq reference 0 colon))))
           (file-uri (and scheme (string-equal reference "file" :end1 colon)))
           (path reference))
      (when (zerop (length reference)) (return-from resolve-image-artifact nil))
      (when scheme
        (unless file-uri (return-from resolve-image-artifact nil))
        (setf path (subseq reference (1+ colon)))
        (when (and (>= (length path) 2) (string= path "//" :end1 2))
          (let* ((slash (position #\/ path :start 2))
                 (host (subseq path 2 slash)))
            (unless (and slash (or (string= host "") (string-equal host "localhost")))
              (return-from resolve-image-artifact nil))
            (setf path (subseq path slash))))
        (unless (and (plusp (length path)) (char= (char path 0) #\/))
          (return-from resolve-image-artifact nil))
        (setf path (subseq path 0 (position-if (lambda (ch) (find ch "?#")) path))))
      (labels ((resolve (name)
                 (unless (find #\Null name)
                   (let* ((parsed (uiop:parse-native-namestring name))
                          (absolute (if (uiop:absolute-pathname-p parsed) parsed
                                        (and directory
                                             (merge-pathnames parsed (uiop:ensure-directory-pathname directory)))))
                          (existing (and absolute (probe-file absolute))))
                     (and existing (not (uiop:directory-pathname-p existing))
                          (member (pathname-type existing) *image-artifact-extensions*
                                  :test #'string-equal)
                          existing)))))
        (if file-uri
            (resolve (image-artifact-decode path))
            (or (resolve path) (resolve (image-artifact-decode path))))))))
