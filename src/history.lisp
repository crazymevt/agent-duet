;;;; history.lisp — conversations kept as JSON in the user's state directory

(in-package #:agent-duet)

(defvar *history-directory* nil)

(defstruct (history (:constructor %make-history
                                (&key id directory title created updated handles entries
                                      checkpoint abandoned
                                 &aux (folder directory))))
  id folder title created updated handles entries
  checkpoint                            ; the workflow's last checkpoint (WORKFLOW-CHECKPOINT), or nil
  abandoned)                            ; when the user abandoned its interrupted task, or nil

(defun history-directory (&optional history)
  "The storage directory, or HISTORY's working directory when supplied."
  (if history
      (history-folder history)
      (uiop:ensure-directory-pathname
       (or *history-directory*
           (merge-pathnames "agent-duet/history/"
                            (let ((state (uiop:getenv "XDG_STATE_HOME")))
                              (if (and state (plusp (length state)))
                                  (uiop:ensure-directory-pathname state)
                                  (merge-pathnames ".local/state/" (user-homedir-pathname)))))))))

(defparameter *artifact-byte-limit* (* 4 1024 1024))
(defparameter *artifact-count-limit* 128)
(defparameter *artifact-max-age* (* 30 24 60 60))
(defvar *artifact-lock* (sb-thread:make-mutex :name "retained artifacts"))

(defun prune-task-artifacts (directory keep)
  "Keep the newest artifacts, including KEEP; delete expired ones on writes."
  (let* ((cutoff (- (get-universal-time) *artifact-max-age*))
         (files (remove-if-not
                 (lambda (path)
                   (let ((name (pathname-name path)))
                     (or (uiop:string-prefix-p "check-" name)
                         (uiop:string-prefix-p "reply-" name))))
                 (directory (merge-pathnames "*.txt" directory))))
         (others (remove keep files :test #'equal)))
    (setf others (sort others #'> :key (lambda (path) (or (file-write-date path) 0))))
    (loop for path in others for index from 1
          when (or (>= index *artifact-count-limit*)
                   (< (or (file-write-date path) 0) cutoff))
            do (ignore-errors (delete-file path)))))

(defun retain-task-artifact (kind &key text source)
  "Retain a bounded log tail or complete reply; nil on failure or oversized reply.
Prune artifacts on writes by age and count, independently of worktree cleanup."
  (sb-thread:with-mutex (*artifact-lock*)
    (let ((path nil) (complete nil))
      (unwind-protect
           (handler-case
               (let ((bytes (and text (sb-ext:string-to-octets text :external-format :utf-8))))
                 ;; Never silently truncate a reply: use the inline fallback instead.
                 (when (and bytes (> (length bytes) *artifact-byte-limit*))
                   (return-from retain-task-artifact nil))
                 (setf path (merge-pathnames (format nil "artifacts/~a-~a.txt" kind (make-uuid))
                                            (history-directory)))
                 (ensure-directories-exist path)
                 (with-open-file (out path :direction :output :if-exists :error
                                           :element-type '(unsigned-byte 8))
                   (if source
                       (with-open-file (in source :element-type '(unsigned-byte 8))
                         (let ((start (max 0 (- (file-length in) *artifact-byte-limit*)))
                               (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
                           (file-position in start)
                           ;; Drop an incomplete leading UTF-8 character from a cut log.
                           (when (plusp start)
                             (loop for byte = (read-byte in nil nil)
                                   while (and byte (= #x80 (logand byte #xc0)))
                                   finally (when byte (file-position in (1- (file-position in))))))
                           (loop with remaining = *artifact-byte-limit*
                                 for end = (read-sequence buffer in :end (min remaining (length buffer)))
                                 while (plusp end)
                                 do (write-sequence buffer out :end end)
                                    (decf remaining end))))
                       (write-sequence bytes out)))
                 (setf path (truename path) complete t)
                 (ignore-errors (prune-task-artifacts (uiop:pathname-directory-pathname path) path))
                 (namestring (truename path)))
             (error () nil))
        (when (and path (not complete)) (ignore-errors (delete-file path)))))))

(defun make-history (directory)
  "A new, unsaved conversation in DIRECTORY."
  (let ((now (get-universal-time)))
    (%make-history :id (make-uuid) :directory (namestring (uiop:ensure-directory-pathname directory))
                   :created now :updated now :handles (list :claude nil :codex nil))))

(defun history-add-entry (history who kind text)
  "Append an entry and update the conversation's title and timestamp."
  (unless (and (member who '(:claude :codex :middle :user))
               (member kind '(:prompt :text :tool :note :error :activity :activity-error)) (stringp text))
    (error "Invalid history entry."))
  (let ((now (get-universal-time)))
    (setf (history-entries history)
          (nconc (history-entries history) (list (list :who who :kind kind :text text :time now)))
          (history-updated history) now)
    (when (and (eq kind :prompt) (null (history-title history)))
      (let ((line (string-trim '(#\Space #\Tab)
                               (map 'string (lambda (c) (if (find c '(#\Newline #\Return #\Tab)) #\Space c)) text))))
        (setf (history-title history) (subseq line 0 (min 80 (length line)))))))
  history)

(defun history-handle (history agent-id)
  (getf (history-handles history) agent-id))

(defun (setf history-handle) (value history agent-id)
  (unless (and (member agent-id '(:claude :codex)) (or (null value) (stringp value)))
    (error "Invalid conversation handle."))
  (setf (getf (history-handles history) agent-id) value))

;;; The workflow's checkpoint, for recovering an interrupted task (docs/recovery-api.md).
;;; Its shape is the engine's; here it is only kept, and read for display.

(defun json-copy (value)
  "VALUE as it reads back from its JSON: an error if it can't be written."
  (parse-json (json-string value)))

(defun record-history-checkpoint (history checkpoint)
  "Keep CHECKPOINT (a JSON object, or nil) as it will be read back."
  (unless (or (null checkpoint) (jobject-p checkpoint))
    (error "Invalid workflow checkpoint."))
  (setf (history-checkpoint history) (and checkpoint (json-copy checkpoint))
        (history-abandoned history) nil
        (history-updated history) (get-universal-time))
  history)

(defparameter *interrupted-states* '("running" "paused" "interrupted")
  "Checkpoint states that mean the task stopped without the user stopping it.")

(defparameter *unfinished-integration* '("committing" "merging" "interrupted")
  "Integration states that mean merging the parts stopped partway: the task
can be carried on even if the workflow counted itself finished.")

(defun checkpoint-interrupted-p (checkpoint)
  "Whether CHECKPOINT's task stopped without the user stopping it: the app
quit, an agent paused or failed, or merging the parts stopped partway."
  (and (or (member (jget checkpoint "state") *interrupted-states* :test #'equal)
           (and (equal (jget checkpoint "state") "finished")
                (member (jget checkpoint "integration") *unfinished-integration* :test #'equal)))
       t))

(defun history-recoverable-p (history)
  "Whether HISTORY has a task that stopped before finishing, not abandoned."
  (let ((checkpoint (history-checkpoint history)))
    (and checkpoint (null (history-abandoned history))
         (or (equal (jget checkpoint "state") "stopped") (checkpoint-interrupted-p checkpoint))
         t)))

(defun history-interrupted-p (history)
  "Whether HISTORY's task stopped without being stopped, so reopening it
offers recovery unprompted."
  (and (history-recoverable-p history)
       (checkpoint-interrupted-p (history-checkpoint history))))

(defun abandon-history-task (history)
  "Mark HISTORY's task abandoned: kept, but not offered for recovery again."
  (setf (history-abandoned history) (get-universal-time)
        (history-updated history) (history-abandoned history))
  history)

(defun checkpoint-string (value)
  (and (stringp value) (plusp (length value)) value))

(defun checkpoint-agent-name (id)
  (cond ((equal id "claude") "Claude")
        ((equal id "codex") "ChatGPT")
        (t (checkpoint-string id))))

(defun checkpoint-integer (value)
  (and (integerp value) value))

(defun checkpoint-step-description (role agent label)
  "How a step reads, from its saved role, agent and loop label."
  (let ((name (or agent "An agent")))
    (cond ((equal role "propose") (format nil "~a's proposal" name))
          ((equal role "plan") (format nil "~a's plan for splitting the work" name))
          ((equal role "resolve") (format nil "~a's resolving of the merge conflicts" name))
          ((equal role "check") (format nil "the check~@[ on ~a~]" label))
          ((equal role "review") (format nil "~a's review~@[ of ~a~]" name label))
          (t (format nil "~a's work~@[ on ~a~]" name label)))))

(defun step-reason-text (reason)
  "Why a step stopped, from STEP-REASON or its saved string, in a few words."
  (let ((reason (if (stringp reason) (find-symbol (string-upcase reason) :keyword) reason)))
    (case reason
      (:out-of-usage "out of usage")
      (:interrupted "paused or stopped by you")
      (:failed "failed")
      (:disconnected "agent disconnected or exited")
      (:dependency "waiting for prerequisite work")
      (:deadlock "each part is waiting on the other")
      (:prerequisite-changed "prerequisite changed; needs revalidation")
      (:continuation-failed "could not continue after the step finished")
      (:restart "interrupted when the app closed")
      (:integration-unresolved "waiting for the merge conflicts to be resolved")
      (t "paused"))))

(defparameter *step-states* '("paused" "running" "interrupted" "failed")
  "Saved step states that mean it's where the task stopped, most telling first.")

(defun checkpoint-prerequisite-text (checkpoint index)
  "Describe unfinished prerequisites of a saved loop, tolerating older checkpoints."
  (let* ((loops (jlist (jget checkpoint "loops")))
         (part (and (typep index '(integer 0 *)) (nth index loops)))
         (labels (loop for dependency in (jlist (jget part "dependencies"))
                       for prerequisite = (and (typep dependency '(integer 0 *)) (nth dependency loops))
                       for label = (checkpoint-string (jget prerequisite "label"))
                       when (and label (or (not (member (jget prerequisite "state")
                                                       '("approved" "unreviewed") :test #'equal))
                                           (jtrue-p (jget prerequisite "check-failed"))))
                         collect label)))
    (when labels (format nil "waiting for ~{~a~^, ~}" labels))))

(defun checkpoint-loop-waiting-p (checkpoint index)
  "Whether this loop has a saved dependency-related pause."
  (and (typep index '(integer 0 *))
       (not (member (jget checkpoint "loops" index "state")
                    '("approved" "unreviewed" "unresolved" "failed") :test #'equal))
       (some (lambda (step)
               (and (eql index (jget step "loop"))
                    (equal "paused" (jget step "state"))
                    (member (jget step "reason")
                            '("dependency" "prerequisite-changed" "deadlock") :test #'equal)))
             (jlist (jget checkpoint "steps")))))

(defun checkpoint-summary (checkpoint &key (waiting nil waiting-p))
  "What CHECKPOINT (WORKFLOW-CHECKPOINT) says, for the recovery card: a
plist of :version :state :mode :request :lead :partner :agents (:agent
:unavailable-until) :step and :steps (:id :agent :role :description :state
:reason :progress :waiting-for), :loops (:label :author :reviewer :state :round :waiting-for
:unreviewed), :unreviewed (labels), :worktrees (:agent :path :branch) and
:pending-plan (:lead :partner :order :error), the plan waiting for approval, and
:integration (:state :committed :merged). Anything missing or malformed reads as
nil, so older or newer checkpoints still show what they can.
WAITING, when given, is the ids of the steps the live workflow still has
paused: only they count as where it stopped."
  (flet ((str (&rest path) (checkpoint-string (apply #'jget checkpoint path))))
    (let* ((agents (loop for a in (jlist (jget checkpoint "agents"))
                         when (jobject-p a)
                           collect (list :id (checkpoint-string (jget a "id"))
                                         :agent (checkpoint-agent-name (jget a "id"))
                                         :unavailable-until (checkpoint-integer (jget a "unavailable")))))
           (loops (loop for l in (jlist (jget checkpoint "loops"))
                        for index from 0
                        collect (and (jobject-p l)
                                     (list :label (checkpoint-string (jget l "label"))
                                           :author (checkpoint-agent-name (jget l "author"))
                                           :reviewer (checkpoint-agent-name (jget l "reviewer"))
                                           :state (checkpoint-string (jget l "state"))
                                           :round (checkpoint-integer (jget l "round"))
                                           :waiting-for (and (checkpoint-loop-waiting-p checkpoint index)
                                                             (checkpoint-prerequisite-text checkpoint index))
                                           ;; Taken over, or finished without its review.
                                           :unreviewed (or (eq t (jget l "skip-review"))
                                                           (equal "unreviewed" (jget l "state")))))))
           (steps (loop for s in (jlist (jget checkpoint "steps"))
                        when (jobject-p s)
                          collect (let ((agent (checkpoint-agent-name (jget s "agent")))
                                        (loop (let ((index (jget s "loop")))
                                                (and (typep index '(integer 0 *)) (nth index loops)))))
                                    (list :id (checkpoint-string (jget s "id")) :agent agent
                                          :role (checkpoint-string (jget s "role"))
                                          :description (checkpoint-step-description
                                                        (jget s "role") agent (getf loop :label))
                                          :state (checkpoint-string (jget s "state"))
                                          :reason (checkpoint-string (jget s "reason"))
                                          :waiting-for (and (member (jget s "reason")
                                                                    '("dependency" "prerequisite-changed" "deadlock")
                                                                    :test #'equal)
                                                            (checkpoint-prerequisite-text checkpoint (jget s "loop")))
                                          :progress (checkpoint-string (jget s "progress"))))))
           (stopped (remove-if-not (lambda (s)
                                     (if waiting-p
                                         (member (getf s :id) waiting :test #'equal)
                                         (member (getf s :state) *step-states* :test #'equal)))
                                   steps))
           (integration (jget checkpoint "integration"))
           ;; The journal has each part as committed; MERGED-REFS, those merged.
           (committed (loop for entry in (jlist (jget checkpoint "integration-journal"))
                            for path = (and (jobject-p entry) (checkpoint-string (jget entry "path")))
                            when path
                              collect (format nil "~a committed~@[ at ~a~]" path
                                              (short-hash (jget entry "head")))))
           (merged (loop for ref in (jlist (jget checkpoint "merged-refs"))
                         for name = (checkpoint-string ref)
                         when name collect (format nil "~a merged" (short-branch name))))
           (plan (jget checkpoint "pending-plan")))
      (setf loops (remove nil loops))
      (list :version (checkpoint-integer (jget checkpoint "version"))
            :state (str "state") :mode (str "mode") :request (str "request")
            :lead (getf (first agents) :agent) :partner (getf (second agents) :agent)
            :agents agents
            :steps stopped
            :step (first (stable-sort (copy-list stopped) #'<
                               :key (lambda (s) (position (getf s :state) *step-states* :test #'equal))))
            :loops loops
            :unreviewed (loop for l in loops
                              when (getf l :unreviewed)
                                collect (or (getf l :label) (format nil "~@[~a's ~]work" (getf l :author))))
            :worktrees (loop for tree in (jlist (jget checkpoint "trees"))
                             when (and (jobject-p tree) (checkpoint-string (jget tree "path")))
                               collect (list :agent (checkpoint-agent-name (jget tree "agent"))
                                             :path (jget tree "path")
                                             :branch (checkpoint-string (jget tree "branch"))))
            :pending-plan (and (jobject-p plan)
                               (append (list :lead (checkpoint-string (jget plan "lead"))
                                             :partner (checkpoint-string (jget plan "partner")))
                                       (when (checkpoint-string (jget plan "order"))
                                         (list :order (jget plan "order")))
                                       (when (checkpoint-string (jget plan "error"))
                                         (list :error (jget plan "error")))))
            :integration (let ((state (checkpoint-string integration)))
                           (and (or state committed merged)
                                (list :state state :committed committed :merged merged)))))))

;;; What the repository looks like now (INSPECT-WORKFLOW-RECOVERY), against the checkpoint

(defun porcelain-paths (status)
  "The paths in `git status --porcelain=v1 -z` output, each with its status: (\"XY\" . path)."
  (let ((fields (and (stringp status) (uiop:split-string status :separator (string (code-char 0)))))
        (paths '()))
    (loop while fields
          do (let ((field (pop fields)))
               (when (> (length field) 3)
                 (push (cons (subseq field 0 2) (subseq field 3)) paths)
                 ;; A rename or copy is followed by the path it came from.
                 (when (find (char field 0) "RC") (pop fields)))))
    (nreverse paths)))

(defun short-branch (ref)
  (let ((ref (checkpoint-string ref)))
    (cond ((null ref) nil)
          ((uiop:string-prefix-p "refs/heads/" ref) (subseq ref (length "refs/heads/")))
          ((equal ref "HEAD") "detached HEAD")
          (t ref))))

(defun short-hash (hash)
  (let ((hash (checkpoint-string hash)))
    (and hash (subseq hash 0 (min 10 (length hash))))))

(defun inspection-summary (inspection &optional checkpoint)
  "INSPECTION (INSPECT-WORKFLOW-RECOVERY) for the recovery card, each
repository set against how CHECKPOINT saw it: a plist of :changed (true if
anything changed since), :approximate (true if that snapshot may predate the
agents' last edits), :repositories (:path :available :error :branch
:head :files :operation :new-commits :branch-was :changed-since), :agents
(:agent :busy :stopped :unavailable-until), :steps and :loops with prerequisite
labels from CHECKPOINT, and :recovery-refs (parts)."
  (let ((before (loop for r in (jlist (jget checkpoint "repositories"))
                      when (jobject-p r) collect (cons (jget r "path") r)))
        (parts (checkpoint-summary
                (jobj "loops" (jget checkpoint "loops") "steps" (jget inspection "steps")))))
    (list :loops (getf parts :loops)
          :steps (getf parts :steps)
          :changed (eq t (jget inspection "repository-changed"))
          :approximate (eq t (jget inspection "repository-snapshot-pending"))
          :repositories
          (loop for r in (jlist (jget inspection "repositories"))
                when (jobject-p r)
                  collect (let* ((path (checkpoint-string (jget r "path")))
                                 (was (cdr (assoc path before :test #'equal)))
                                 (available (eq t (jget r "available"))))
                            (list :path path :available available
                                  :error (and (not available) (checkpoint-string (jget r "error")))
                                  :branch (short-branch (jget r "branch"))
                                  :head (short-hash (jget r "head"))
                                  :files (porcelain-paths (jget r "status"))
                                  :operation (eq t (jget r "operation"))
                                  :new-commits (and was (checkpoint-string (jget was "head"))
                                                    (not (equal (jget was "head") (jget r "head"))))
                                  :branch-was (and was (checkpoint-string (jget was "branch"))
                                                   (not (equal (jget was "branch") (jget r "branch")))
                                                   (short-branch (jget was "branch")))
                                  :changed-since (and was (not (equalp was r))))))
          :agents (loop for a in (jlist (jget inspection "agents"))
                        when (jobject-p a)
                          collect (list :agent (checkpoint-agent-name (jget a "id"))
                                        :busy (eq t (jget a "busy"))
                                        :stopped (eq t (jget a "shutdown-confirmed"))
                                        :unavailable-until (checkpoint-integer (jget a "unavailable"))))
          :recovery-refs (loop for ref in (jlist (jget inspection "recovery-refs"))
                               for part = (and (jobject-p ref) (checkpoint-string (jget ref "part")))
                               when part collect part))))

(defparameter *recovery-choices*
  '((:review "Review what's there" "Look again at the folder, worktrees and agents as they are now; nothing runs")
    (:restore-worktrees "Restore worktrees"
     "Recreate the missing worktrees from their committed recovery refs; uncommitted edits made in them can't be recovered")
    (:resume "Resume" "Carry on from where it stopped: the agent is shown what's already done before it continues")
    (:revise-order "Revise the order" "Ask the lead to revise the plan using both parts’ progress")
    (:run-independently "Run independently" "Remove prerequisites and carry on with both parts independently")
    (:retry "Redo the step" "Give the step that stopped to its agent again, as it was first given")
    (:hand-off "Hand off" "Have the other agent carry on from what's done so far; its work won't be reviewed")
    (:stop "Stop task" "Stop both agents; the task can still be recovered later")
    (:abandon "Abandon" "Leave the task as it is and stop offering to recover it; nothing is deleted"))
  "Each recovery action (docs/recovery-api.md): its button label and tooltip, in button order.")

(defun select-summary-step (summary id)
  "SUMMARY with the waiting step ID as its :step, the one the card's actions
and details are for; as it was if no waiting step has that ID."
  (let ((step (and id (find id (getf summary :steps) :key (lambda (s) (getf s :id)) :test #'equal))))
    (if step
        (let ((copy (copy-list summary)))
          (setf (getf copy :step) step)
          copy)
        summary)))

(defun missing-worktrees-p (summary inspection)
  "Whether a worktree of SUMMARY's is gone, by INSPECTION, with recovery refs to restore it from."
  (and (getf inspection :recovery-refs)
       (some (lambda (tree)
               (let ((now (find (getf tree :path) (getf inspection :repositories)
                                :key (lambda (r) (getf r :path)) :test #'equal)))
                 (and now (not (getf now :available)))))
             (getf summary :worktrees))
       t))

(defun recovery-actions (summary &optional inspection)
  "The actions the recovery card offers for SUMMARY (CHECKPOINT-SUMMARY), as
RECOVER-WORKFLOW takes them: a step that stopped can be resumed, redone or
handed off (not a check: it is only run again); with none, a plan waiting
for approval can be shown again, and an integration that stopped partway
carried on. Missing worktrees with recovery refs (by INSPECTION,
INSPECTION-SUMMARY's) can be restored. Stopping is for a task not already
stopped; reviewing and abandoning are always there."
  (let* ((step (getf summary :step))
         (role (getf step :role))
         (integration (getf (getf summary :integration) :state)))
    (append '(:review)
            (and (missing-worktrees-p summary inspection) '(:restore-worktrees))
            (cond ((equal (getf step :reason) "deadlock") '(:revise-order :run-independently))
                  ((equal role "check") '(:resume))
                  (step '(:resume :retry :hand-off))
                  ((getf summary :pending-plan) '(:resume))
                  ((member integration (cons "merged" *unfinished-integration*) :test #'equal) '(:resume)))
            (unless (member (getf summary :state) '("stopped" "abandoned" "finished") :test #'equal) '(:stop))
            '(:abandon))))

(defun recovery-choices (actions &key step-role plan integration)
  "The buttons for ACTIONS, in a fixed order: (action label tooltip). A
review handed off finishes unreviewed instead; with no step, resuming shows
the PLAN waiting for approval again, or carries on the INTEGRATION."
  (loop for (action label tooltip) in *recovery-choices*
        when (member action actions)
          collect (cond ((and (eq action :hand-off) (equal step-role "review"))
                         (list action "Finish unreviewed" "Finish this part without a review, and say so"))
                        ((and (eq action :resume) (null step-role) plan)
                         (list action "Show the plan" "Show the plan waiting for approval again, to edit and start it"))
                        ((and (eq action :resume) (null step-role) integration)
                         (list action "Carry on merging"
                               "Carry on bringing the parts together, skipping what's already committed and merged"))
                        (t (list action label tooltip)))))

(defun history-file (id)
  ;; IDs are filenames, never paths supplied by a saved document.
  (unless (and (stringp id) (plusp (length id))
               (every (lambda (c) (or (find c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
                                     (char= c #\-))) id))
    (error "Invalid history ID."))
  (merge-pathnames (make-pathname :name id :type "json") (history-directory)))

(defun history-json (history)
  (jobj "id" (history-id history) "directory" (history-directory history)
        "title" (history-title history) "created" (history-created history)
        "updated" (history-updated history)
        "handles" (jobj "claude" (history-handle history :claude) "codex" (history-handle history :codex))
        ;; Left out when absent, so histories without a task read as before.
        "checkpoint" (or (history-checkpoint history) :omit)
        "abandoned" (or (history-abandoned history) :omit)
        "entries" (coerce (loop for entry in (history-entries history)
                               collect (jobj "who" (getf entry :who) "kind" (getf entry :kind)
                                             "text" (getf entry :text) "time" (getf entry :time))) 'vector)))

(defvar *history-save-lock* (sb-thread:make-mutex :name "history saves"))
(defvar *history-save-worker* nil)
(defvar *history-save-queue* nil)
(defvar *history-summary-cache* (make-hash-table :test #'equal))

(defun copy-history-json (value)
  "Detach mutable JSON containers and strings before crossing threads."
  (typecase value
    (cons (cons (copy-history-json (car value)) (copy-history-json (cdr value))))
    (string (copy-seq value))
    (vector (map 'vector #'copy-history-json value))
    (t value)))

(defun write-history-json (file json)
  "Atomically replace FILE. Preserve the existing best-effort disk policy."
  (ignore-errors
    (let ((temp (merge-pathnames (format nil ".~a-~a.tmp" (pathname-name file) (make-uuid)) file)))
      (ensure-directories-exist file)
      (unwind-protect
           (progn
             (with-open-file (out temp :direction :output :if-exists :error :external-format :utf-8)
               (write-string (json-string json) out)
               (terpri out))
             (uiop:rename-file-overwriting-target temp file))
        (when (probe-file temp) (delete-file temp))))))

(defun flush-history-saves ()
  "Wait until all queued snapshots have reached the writer."
  (loop for worker = (sb-thread:with-mutex (*history-save-lock*) *history-save-worker*)
        while worker do (sb-thread:join-thread worker)))

(defun queue-history-save (history)
  "Queue an immutable snapshot; coalesce pending saves of the same file.
A single writer orders replacements. Call FLUSH-HISTORY-SAVES before exit
or before a synchronous read or destructive operation that needs durability."
  (when (history-entries history)
    (let ((file (history-file (history-id history)))
          (json (copy-history-json (history-json history))))
      (remhash file *history-summary-cache*)
      (sb-thread:with-mutex (*history-save-lock*)
        (setf *history-save-queue*
              (nconc (remove file *history-save-queue* :key #'car :test #'equal)
                     (list (cons file json))))
        (unless *history-save-worker*
          (setf *history-save-worker*
                (sb-thread:make-thread
                 (lambda ()
                   (loop
                     (let ((job (sb-thread:with-mutex (*history-save-lock*)
                                  (unless *history-save-queue*
                                    (setf *history-save-worker* nil)
                                    (return))
                                  (pop *history-save-queue*))))
                       (write-history-json (car job) (cdr job)))))
                 :name "history writer")))))))

(defun save-history (history)
  "Synchronously save HISTORY after earlier queued writes."
  (flush-history-saves)
  (when (history-entries history)
    (let ((file (history-file (history-id history))))
      (remhash file *history-summary-cache*)
      (write-history-json file (history-json history)))))

(defun history-keyword (value choices)
  (or (and (stringp value)
           (find value choices :key (lambda (key) (string-downcase (symbol-name key))) :test #'string=))
      (error "Unknown history keyword.")))

(defun load-history (id)
  "Read a conversation, or nil if missing or corrupt."
  (ignore-errors
    (let* ((json (parse-json (uiop:read-file-string (history-file id) :external-format :utf-8)))
           (entries (jget json "entries")))
      (unless (and (equal id (jget json "id")) (stringp (jget json "directory"))
                   (or (stringp (jget json "title")) (eq :null (jget json "title"))) (integerp (jget json "created"))
                   (integerp (jget json "updated")) (vectorp entries) (not (stringp entries))
                   (jobject-p (jget json "handles")))
        (error "Invalid history."))
      (%make-history
       :id id :directory (jget json "directory") :title (let ((title (jget json "title"))) (and (stringp title) title))
       :created (jget json "created") :updated (jget json "updated")
       :handles (loop for agent in '(:claude :codex)
                      for value = (jget json "handles" (string-downcase (symbol-name agent)))
                      append (list agent (cond ((stringp value) value)
                                               ((member value '(nil :null)) nil)
                                               (t (error "Invalid history handle.")))))
       ;; Older histories have neither; a checkpoint that isn't an object is
       ;; dropped rather than losing the conversation.
       :checkpoint (let ((checkpoint (jget json "checkpoint"))) (and (jobject-p checkpoint) checkpoint))
       :abandoned (let ((abandoned (jget json "abandoned"))) (and (integerp abandoned) abandoned))
       :entries (loop for entry in (jlist entries)
                      collect (progn
                                (unless (and (stringp (jget entry "text")) (integerp (jget entry "time")))
                                  (error "Invalid history entry."))
                                (list :who (history-keyword (jget entry "who") '(:claude :codex :middle :user))
                                      :kind (history-keyword (jget entry "kind") '(:prompt :text :tool :note :error :activity :activity-error))
                                      :text (jget entry "text") :time (jget entry "time"))))))))

(defun history-file-signature (file)
  (ignore-errors
    (let ((stat (sb-posix:stat file)))
      (list (sb-posix:stat-ino stat) (sb-posix:stat-size stat)
            (sb-posix:stat-mtime stat) (sb-posix:stat-ctime stat)))))

(defun cached-history-summary (file)
  (let* ((signature (history-file-signature file))
         (cached (gethash file *history-summary-cache*)))
    (if (and cached (equal signature (car cached)))
        (cdr cached)
        (let* ((history (load-history (pathname-name file)))
               (summary (when history
                          (list :id (history-id history) :title (history-title history)
                                :directory (history-directory history) :updated (history-updated history)
                                :entry-count (length (history-entries history))
                                :interrupted (history-interrupted-p history)))))
          (setf (gethash file *history-summary-cache*) (cons signature summary))
          summary))))

(defun list-histories (&key directory (limit 50))
  "Newest summaries. Parse only files whose metadata changed since last listing."
  (flush-history-saves)
  (let* ((folder (and directory (namestring (uiop:ensure-directory-pathname directory))))
         (files (ignore-errors (uiop:directory-files (history-directory) "*.json")))
         (summaries
           (loop for file in files
                 for summary = (cached-history-summary file)
                 when (and summary (or (null folder)
                                       (equal folder (namestring (uiop:ensure-directory-pathname
                                                                 (getf summary :directory))))))
                   collect (copy-list summary))))
    ;; Bound cache retention when histories or storage directories disappear.
    (maphash (lambda (file value) (declare (ignore value))
               (unless (member file files :test #'equal) (remhash file *history-summary-cache*)))
             *history-summary-cache*)
    (setf summaries (stable-sort summaries #'> :key (lambda (summary) (getf summary :updated))))
    (if limit (subseq summaries 0 (min (max 0 limit) (length summaries))) summaries)))

(defun delete-history (id)
  "Delete a saved conversation, ignoring missing files and disk errors."
  (flush-history-saves)
  (remhash (history-file id) *history-summary-cache*)
  (ignore-errors (delete-file (history-file id))))

(defun prune-histories (&key (keep 200))
  "Delete the oldest files beyond KEEP; use file dates for corrupt histories."
  (flush-history-saves)
  (let ((files (loop for file in (ignore-errors (uiop:directory-files (history-directory) "*.json"))
                     for history = (load-history (pathname-name file))
                     collect (cons file (or (and history (history-updated history))
                                            (ignore-errors (file-write-date file)) 0)))))
    (setf files (stable-sort files #'> :key #'cdr))
    (dolist (entry (nthcdr (max 0 keep) files))
      (ignore-errors (delete-file (car entry))))))
