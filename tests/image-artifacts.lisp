;;;; Headless local image reference and file URI handling.

(in-package #:agent-duet/tests)

(define-test image-artifacts
    :parent agent-duet
  (with-temp-folder (directory)
    (dolist (name '("mock up.png" "café #1+.JPG" "literal%20.png" "bracket[1].webp"
                    "animation.gif" "photo.jpeg" "mock.svg" "export.BMP" "scan.tif"
                    "scan.tiff" "favicon.ico" "notes.txt"))
      (write-file (merge-pathnames (uiop:parse-native-namestring name) directory) "fixture"))
    (dolist (name '("mock up.png" "café #1+.JPG" "literal%20.png" "bracket[1].webp"
                    "animation.gif" "photo.jpeg" "mock.svg" "export.BMP" "scan.tif"
                    "scan.tiff" "favicon.ico"))
      (let* ((path (truename (merge-pathnames (uiop:parse-native-namestring name) directory)))
             (uri (ai::image-artifact-uri path)))
        (is equal path (ai::resolve-image-artifact name directory))
        (is equal path (ai::resolve-image-artifact path nil))
        (is equal path (ai::resolve-image-artifact (uiop:native-namestring path) nil))
        (is equal path (ai::resolve-image-artifact uri nil))
        (is equal path (ai::resolve-image-artifact
                        (concatenate 'string "file://localhost" (subseq uri 7)) nil))))
    (let ((path (truename (merge-pathnames "mock up.png" directory))))
      (is equal path (ai::resolve-image-artifact "mock%20up.png" directory))
      (is equal path (ai::resolve-image-artifact "./mock up.png" (namestring directory)))
      (is equal path (ai::resolve-image-artifact
                      (concatenate 'string (ai::image-artifact-uri path) "#preview") nil)))
    (true (search "caf%C3%A9%20%231%2B.JPG"
                  (ai::image-artifact-uri (merge-pathnames "café #1+.JPG" directory))))
    (true (search "literal%2520.png"
                  (ai::image-artifact-uri (merge-pathnames "literal%20.png" directory))))
    (dolist (reference '("" "missing.png" "notes.txt" "https://example.com/a.png"
                         "data:image/png;base64,abc" "ftp://host/a.png"
                         "file://remote/tmp/a.png" "file:relative.png" "file://"
                         "bad%.png" "bad%GG.png" "bad%FF.png" "bad%00.png"))
      (false (ai::resolve-image-artifact reference directory) "~s" reference))
    ;; A colon in the first path segment is ambiguous with a URI scheme.
    (let ((path (truename (write-file (merge-pathnames "screens:home.png" directory) "fixture"))))
      (false (ai::resolve-image-artifact "screens:home.png" directory))
      (is equal path (ai::resolve-image-artifact "./screens:home.png" directory)))
    (false (ai::resolve-image-artifact nil directory))
    (false (ai::resolve-image-artifact "mock up.png" nil))
    (ensure-directories-exist (merge-pathnames "folder.png/" directory))
    (false (ai::resolve-image-artifact "folder.png/" directory))))

(define-test visualization-directives
    :parent agent-duet
  (with-temp-folder (directory)
    (write-file (merge-pathnames "mock up.html" directory) "<html></html>")
    (flet ((token (body)
             (format nil "~c~a~c" (code-char #xE200) body (code-char #xE201)))
           (body (name json)
             (format nil "~a~c~a" name (code-char #xE202) json)))
      (let* ((payload (body "visualize" "{\"path\":\"mock up.html\",\"mode\":\"wide\"}"))
             (text (token payload))
             (link (ai::visualization-link payload directory)))
        (is equal '(:directive) (mapcar #'first (ai::markdown-inlines text)))
        (is equal "Open mockup" (first link))
        (is equal (ai::image-artifact-uri (truename (merge-pathnames "mock up.html" directory))) (second link))
        (is equal '(:text :directive :text :directive)
            (mapcar #'first (ai::markdown-inlines (concatenate 'string "Before " text " after " text))))
        (is equal '(:code) (mapcar #'first (ai::markdown-inlines (format nil "`~a`" text))))
        (is equal '(:text) (mapcar #'first (ai::markdown-inlines (subseq text 0 (1- (length text)))))))
      (dolist (json '("{}" "{\"path\":null}" "{\"path\":42}" "{\"path\":\"gone.html\"}" "{\"path\":\"https://example.com/x.html\"}"))
        (is equal '("Mockup (not found)") (ai::visualization-link (body "visualize" json) directory)))
      (is equal '("Mockup (invalid directive)")
          (ai::visualization-link (body "visualize" "{broken}") directory))
      (is equal (truename (merge-pathnames "mock up.html" directory))
          (ai::resolve-image-artifact (namestring (merge-pathnames "mock up.html" directory)) nil :extensions nil))
      (false (ai::resolve-image-artifact "mock up.html" directory))
      (is equal '("Unsupported artifact: other")
          (ai::visualization-link (body "other" "{}") directory)))))
