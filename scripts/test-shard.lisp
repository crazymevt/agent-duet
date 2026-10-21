;;;; Loaded after agent-duet/tests by the parallel test runner.
(let* ((count (parse-integer (uiop:getenv "TEST_SHARDS")))
       (index (parse-integer (uiop:getenv "TEST_SHARD")))
       (root (parachute:find-test 'agent-duet/tests::agent-duet))
       ;; CHILDREN can shuffle for nonserial suites: sort before partitioning
       ;; so every process assigns exactly the same tests to the same worker.
       (children (sort (copy-list (parachute:children root)) #'string<
                       :key (lambda (test) (symbol-name (parachute:name test))))))
  (assert (and (plusp count) (<= 0 index) (< index count)))
  (setf (parachute:children root)
        (loop for child in children for position from 0
              when (= index (mod position count)) collect child))
  (format t "~&Worker ~d/~d: ~d of ~d tests~%"
          (1+ index) count (length (parachute:children root)) (length children))
  (uiop:quit (if (eq :passed (parachute:status (parachute:test root))) 0 1)))
