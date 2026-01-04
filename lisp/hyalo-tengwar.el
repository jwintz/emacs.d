;;; hyalo-tengwar.el --- Render text in Tengwar using overlays -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: faces, overlays

;;; Commentary:

;; Minor mode to render visible text in Tengwar script using overlays.
;; Uses a persistent bun subprocess for fast transliteration.
;;
;; Usage:
;;   M-x hyalo-tengwar-minor-mode
;;   M-x hyalo-tengwar-partial-mode
;;
;; Requirements:
;;   - bun runtime (https://bun.sh)
;;   - Tengwar font installed (e.g., Tengwar Annatar, Tengwar Telcontar)

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'hyalo)

(defgroup hyalo-tengwar nil
  "Render text in Tengwar."
  :group 'hyalo
  :prefix "hyalo-tengwar-")

;;; Customization

(defcustom hyalo-tengwar-bun-command
  (or (executable-find "bun")
      (expand-file-name "~/.bun/bin/bun"))
  "Path to bun executable."
  :type 'string
  :group 'hyalo-tengwar)

(defcustom hyalo-tengwar-script
  (expand-file-name "hyalo-tengwar.ts"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the tengwar transliteration TypeScript script."
  :type 'string
  :group 'hyalo-tengwar)

(defcustom hyalo-tengwar-font-height 1.0
  "Font height for Tengwar display.
Float: relative to underlying text. Integer: absolute in 1/10pt."
  :type 'number
  :group 'hyalo-tengwar)

(defcustom hyalo-tengwar-language "english"
  "Language mode for transliteration (english, sindarin, etc)."
  :type 'string
  :group 'hyalo-tengwar)

(defcustom hyalo-tengwar-use-csur nil
  "If non-nil, use CSUR Unicode font (Tengwar Telcontar).
Otherwise use ASCII Dan Smith font (Tengwar Annatar)."
  :type 'boolean
  :group 'hyalo-tengwar)

(defcustom hyalo-tengwar-mode "general-use"
  "Transcription mode: general-use, classical, or beleriand."
  :type '(choice (const "general-use")
                 (const "classical")
                 (const "beleriand"))
  :group 'hyalo-tengwar)

(defcustom hyalo-tengwar-partial-delimiters '("@@" . "@@")
  "Start and end delimiters for `hyalo-tengwar-partial-mode`."
  :type '(cons string string)
  :group 'hyalo-tengwar)

;;; Internal State

(defvar hyalo-tengwar--process nil
  "The persistent bun subprocess.")

(defvar hyalo-tengwar--cache (make-hash-table :test 'equal)
  "Cache of transliterated words: word -> tengwar.")

(defvar hyalo-tengwar--pending (make-hash-table :test 'equal)
  "Words currently awaiting transliteration: word -> t.")

(defvar hyalo-tengwar--request-id 0
  "Counter for request IDs.")

(defvar hyalo-tengwar--callbacks (make-hash-table :test 'equal)
  "Pending callbacks: request-id -> (word-list . buffer).")

(defvar hyalo-tengwar--output-buffer ""
  "Accumulator for partial JSON lines from subprocess.")

(defvar hyalo-tengwar--ready nil
  "Non-nil when subprocess has signaled ready.")

(defvar hyalo-tengwar--pending-requests nil
  "Queue of requests waiting for subprocess to be ready.")

(defvar-local hyalo-tengwar--update-scheduled nil
  "Unused - kept for compatibility.")

;;; Logging (uses hyalo.el centralized logging)

(defun hyalo-tengwar--log (msg &rest args)
  "Log MSG with ARGS at info level."
  (apply #'hyalo-info 'tengwar msg args))

(defun hyalo-tengwar--trace (msg &rest args)
  "Log MSG with ARGS at trace level."
  (apply #'hyalo-trace 'tengwar msg args))

(defun hyalo-tengwar--debug (msg &rest args)
  "Log MSG with ARGS at debug level."
  (apply #'hyalo-debug 'tengwar msg args))

;;; Subprocess Management

(defun hyalo-tengwar--start-process ()
  "Start the persistent bun subprocess."
  (cond
   ;; Already running
   ((and hyalo-tengwar--process
         (process-live-p hyalo-tengwar--process))
    (hyalo-tengwar--trace "Process already running")
    hyalo-tengwar--process)

   ;; bun not found
   ((not (file-executable-p hyalo-tengwar-bun-command))
    (hyalo-error 'tengwar "bun not found: %s" hyalo-tengwar-bun-command)
    nil)

   ;; Script not found
   ((not (file-exists-p hyalo-tengwar-script))
    (hyalo-error 'tengwar "Script not found: %s" hyalo-tengwar-script)
    nil)

   ;; Start subprocess
   (t
    (hyalo-tengwar--log "Starting subprocess: %s %s --server"
                        hyalo-tengwar-bun-command hyalo-tengwar-script)
    (setq hyalo-tengwar--output-buffer ""
          hyalo-tengwar--ready nil
          hyalo-tengwar--pending-requests nil)
    (let* ((process-environment (cons "NO_COLOR=1" process-environment))
           (proc (make-process
                  :name "hyalo-tengwar"
                  :buffer " *hyalo-tengwar*"
                  :command (list hyalo-tengwar-bun-command
                                 hyalo-tengwar-script
                                 "--server")
                  :connection-type 'pipe
                  :filter #'hyalo-tengwar--filter
                  :sentinel #'hyalo-tengwar--sentinel)))
      (setq hyalo-tengwar--process proc)
      (hyalo-tengwar--trace "Subprocess started (pid %s)" (process-id proc))
      proc))))

(defun hyalo-tengwar--stop-process ()
  "Stop the persistent bun subprocess."
  (when (and hyalo-tengwar--process
             (process-live-p hyalo-tengwar--process))
    (hyalo-tengwar--log "Stopping subprocess")
    (delete-process hyalo-tengwar--process))
  (setq hyalo-tengwar--process nil
        hyalo-tengwar--ready nil
        hyalo-tengwar--output-buffer ""
        hyalo-tengwar--pending-requests nil)
  ;; Clear pending state
  (clrhash hyalo-tengwar--pending)
  (clrhash hyalo-tengwar--callbacks))

(defun hyalo-tengwar--sentinel (process event)
  "Handle PROCESS state changes (EVENT)."
  (hyalo-tengwar--trace "Sentinel: %s" (string-trim event))
  (when (memq (process-status process) '(exit signal))
    (setq hyalo-tengwar--process nil
          hyalo-tengwar--ready nil)
    ;; Clear pending - they won't complete
    (clrhash hyalo-tengwar--pending)
    (clrhash hyalo-tengwar--callbacks)
    (hyalo-tengwar--log "Subprocess exited: %s" (string-trim event))))

(defun hyalo-tengwar--filter (_process output)
  "Handle OUTPUT from subprocess."
  (setq hyalo-tengwar--output-buffer
        (concat hyalo-tengwar--output-buffer output))

  ;; Process complete lines
  (while (string-match "\n" hyalo-tengwar--output-buffer)
    (let* ((line-end (match-end 0))
           (line (substring hyalo-tengwar--output-buffer 0 (1- line-end))))
      (setq hyalo-tengwar--output-buffer
            (substring hyalo-tengwar--output-buffer line-end))
      (hyalo-tengwar--handle-line line))))

(defun hyalo-tengwar--handle-line (line)
  "Parse and handle a JSON LINE from subprocess."
  (hyalo-tengwar--trace "Received: %s" (truncate-string-to-width line 60))
  (condition-case err
      (let ((json (json-read-from-string line)))
        (cond
         ;; Ready signal
         ((assoc 'ready json)
          (hyalo-tengwar--trace "Subprocess ready")
          (setq hyalo-tengwar--ready t)
          ;; Flush pending requests
          (dolist (req (nreverse hyalo-tengwar--pending-requests))
            (hyalo-tengwar--send-request (car req) (cdr req)))
          (setq hyalo-tengwar--pending-requests nil))

         ;; Error response
         ((assoc 'error json)
          (hyalo-warn 'tengwar "Subprocess error: %s"
                      (cdr (assoc 'error json))))

         ;; Normal response with id and results
         ((and (assoc 'id json) (assoc 'results json))
          (let* ((id (cdr (assoc 'id json)))
                 (results (cdr (assoc 'results json)))
                 (callback-data (gethash id hyalo-tengwar--callbacks)))
            (when callback-data
              (remhash id hyalo-tengwar--callbacks)
              (let ((words (car callback-data))
                    (buffer (cdr callback-data)))
                (hyalo-tengwar--handle-results words results buffer)))))))
    (error
     (hyalo-warn 'tengwar "JSON parse error: %s" (error-message-string err)))))

(defun hyalo-tengwar--handle-results (words results buffer)
  "Process RESULTS for WORDS, updating cache and BUFFER."
  (hyalo-tengwar--trace "Received %d results" (length results))

  ;; Update cache
  (cl-loop for word in words
           for trans across results
           do (progn
                (puthash word trans hyalo-tengwar--cache)
                (remhash word hyalo-tengwar--pending)))

  ;; Refresh buffer if still live and mode active
  (when (and (buffer-live-p buffer)
             (or (buffer-local-value 'hyalo-tengwar-minor-mode buffer)
                 (buffer-local-value 'hyalo-tengwar-partial-mode buffer)))
    (with-current-buffer buffer
      (hyalo-tengwar--update-visible))))

;;; Request Handling

(defun hyalo-tengwar--fetch-words (words)
  "Request transliteration for WORDS asynchronously."
  (when words
    (hyalo-tengwar--trace "Fetching %d words" (length words))
    ;; Mark as pending
    (dolist (w words)
      (puthash w t hyalo-tengwar--pending))
    ;; Ensure subprocess running
    (unless (and hyalo-tengwar--process
                 (process-live-p hyalo-tengwar--process))
      (hyalo-tengwar--start-process))
    (if hyalo-tengwar--ready
        (hyalo-tengwar--send-request words (current-buffer))
      ;; Queue until ready
      (push (cons words (current-buffer)) hyalo-tengwar--pending-requests))))

(defun hyalo-tengwar--send-request (words buffer)
  "Send transliteration request for WORDS, storing BUFFER for callback."
  (when (and hyalo-tengwar--process
             (process-live-p hyalo-tengwar--process))
    (let* ((id (format "req-%d" (cl-incf hyalo-tengwar--request-id)))
           (request `((id . ,id)
                      (words . ,(vconcat words))
                      (language . ,hyalo-tengwar-language)
                      (format . ,(if hyalo-tengwar-use-csur "csur" "ascii"))
                      (mode . ,hyalo-tengwar-mode))))
      (puthash id (cons words buffer) hyalo-tengwar--callbacks)
      (process-send-string hyalo-tengwar--process
                           (concat (json-encode request) "\n"))
      (hyalo-tengwar--trace "Sent request %s for %d words" id (length words)))))

;;; Overlay Management

(defun hyalo-tengwar--scan-region (start end collector)
  "Scan region from START to END for words, skipping word at point.
Call COLLECTOR with uncached words found."
  (let ((pt-word-bounds (bounds-of-thing-at-point 'word)))
    (goto-char start)
    (while (re-search-forward "\\({{[^}]+}}\\(\\[[^]]+\\]\\)?\\|{[^}]+}\\(\\[[^]]+\\]\\)?\\|\\*?[a-zA-Z0-9'-]+\\|[[:punct:]]\\)" end t)
      (let* ((word-start (match-beginning 0))
             (word-end (match-end 0))
             (word (match-string-no-properties 0))
             (existing-ov (cl-find-if
                           (lambda (ov) (overlay-get ov 'hyalo-tengwar))
                           (overlays-at word-start)))
             (trans (gethash word hyalo-tengwar--cache))
             ;; Skip if this word intersects with cursor position
             (at-point-p (and pt-word-bounds
                              (>= word-end (car pt-word-bounds))
                              (<= word-start (cdr pt-word-bounds)))))
        (cond
         ;; Skip word at point - clear any existing overlay
         (at-point-p
          (when existing-ov (delete-overlay existing-ov)))
         ;; Cached - apply overlay if not present
         (trans
          (unless existing-ov
            (hyalo-tengwar--apply-overlay word-start word-end trans)))
         ;; Not cached, not pending - queue for fetch
         ((not (gethash word hyalo-tengwar--pending))
          (funcall collector word)))))))

(defun hyalo-tengwar--update-visible ()
  "Scan visible region and apply tengwar overlays."
  (when (and (or (bound-and-true-p hyalo-tengwar-minor-mode)
                 (bound-and-true-p hyalo-tengwar-partial-mode))
             (not (minibufferp)))
    (let* ((start (window-start))
           (end (window-end nil t))
           (pt (point))
           (words-to-fetch nil)
           (collector (lambda (w) (push w words-to-fetch))))
      (save-excursion
        (save-match-data
          (cond
           ;; Full buffer mode
           ((bound-and-true-p hyalo-tengwar-minor-mode)
            (hyalo-tengwar--scan-region start end collector))
           ;; Partial mode with delimiters
           ((bound-and-true-p hyalo-tengwar-partial-mode)
            (hyalo-tengwar--scan-partial-zones start end pt collector)))))
      ;; Fetch uncached words
      (when words-to-fetch
        (setq words-to-fetch (cl-remove-duplicates words-to-fetch :test 'equal))
        (hyalo-tengwar--fetch-words words-to-fetch)))))

(defun hyalo-tengwar--scan-partial-zones (start end pt collector)
  "Scan for delimited zones from START to END, with point at PT.
Call COLLECTOR for uncached zone content in inactive zones."
  (let ((delim-start (car hyalo-tengwar-partial-delimiters))
        (delim-end (cdr hyalo-tengwar-partial-delimiters)))
    (hyalo-tengwar--debug "Partial mode: delimiters='%s'...'%s'" delim-start delim-end)
    (goto-char start)
    (while (search-forward delim-start end t)
      (let* ((d-start-beg (match-beginning 0))
             (d-start-end (match-end 0))
             (region-content-start d-start-end))
        (hyalo-tengwar--trace "Found start delim at %d-%d" d-start-beg d-start-end)
        (when (search-forward delim-end end t)
          (let* ((d-end-beg (match-beginning 0))
                 (d-end-end (match-end 0))
                 (region-content-end d-end-beg)
                 (content (buffer-substring-no-properties region-content-start region-content-end))
                 (is-active (and (>= pt d-start-beg) (<= pt d-end-end))))
            (hyalo-tengwar--trace "Found end delim at %d-%d, content='%s', active=%s"
                                  d-end-beg d-end-end content is-active)
            (if is-active
                ;; Cursor inside: clear overlays to reveal source text
                (remove-overlays d-start-beg d-end-end 'hyalo-tengwar t)
              ;; Cursor outside: transliterate entire zone content as single unit
              (hyalo-tengwar--apply-zone-overlay region-content-start region-content-end content collector)
              (hyalo-tengwar--hide-delimiter d-start-beg d-start-end)
              (hyalo-tengwar--hide-delimiter d-end-beg d-end-end))
            ;; CRITICAL: Skip past end delimiter to prevent reusing it as start
            (goto-char d-end-end)))))))

(defun hyalo-tengwar--apply-zone-overlay (start end content collector)
  "Apply overlay for zone from START to END with CONTENT.
Use cached translation or call COLLECTOR to fetch."
  (let ((existing-ov (cl-find-if
                      (lambda (ov) (and (overlay-get ov 'hyalo-tengwar)
                                        (not (overlay-get ov 'hyalo-tengwar-delimiter))))
                      (overlays-at start)))
        (trans (gethash content hyalo-tengwar--cache)))
    (cond
     ;; Cached - apply overlay if not present
     (trans
      (unless existing-ov
        (hyalo-tengwar--apply-overlay start end trans)))
     ;; Not cached, not pending - queue for fetch
     ((not (gethash content hyalo-tengwar--pending))
      (funcall collector content)))))

(defun hyalo-tengwar--hide-delimiter (start end)
  "Hide delimiter from START to END with invisible overlay."
  (unless (cl-find-if (lambda (ov) (overlay-get ov 'hyalo-tengwar-delimiter))
                      (overlays-at start))
    (let ((ov (make-overlay start end)))
      (overlay-put ov 'hyalo-tengwar t)
      (overlay-put ov 'hyalo-tengwar-delimiter t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'display ""))))

(defun hyalo-tengwar--apply-overlay (start end trans)
  "Apply tengwar overlay from START to END displaying TRANS."
  (let* ((font-family (if hyalo-tengwar-use-csur
                          "Tengwar Telcontar"
                        "Tengwar Annatar"))
         (ov (make-overlay start end))
         (display-string (propertize trans
                                     'face `(:family ,font-family
                                             :height ,hyalo-tengwar-font-height))))
    (overlay-put ov 'hyalo-tengwar t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'display display-string)))

(defun hyalo-tengwar--remove-overlays ()
  "Remove all tengwar overlays in buffer."
  (remove-overlays (point-min) (point-max) 'hyalo-tengwar t))

;;; Hook-Based Updates (no timers)

(defvar-local hyalo-tengwar--last-window-start nil
  "Last window-start position when update was run.")

(defvar-local hyalo-tengwar--last-window-end nil
  "Last window-end position when update was run.")

(defvar-local hyalo-tengwar--last-point nil
  "Last point position when update was run.")

(defvar-local hyalo-tengwar--last-word-bounds nil
  "Bounds (start . end) of word at point during last update.")

(defun hyalo-tengwar--on-post-command ()
  "Post-command hook to update visible overlays."
  (let ((start (window-start))
        (end (window-end nil t))
        (word-bounds (bounds-of-thing-at-point 'word)))
    ;; Update if visible region OR word-at-point changed
    (when (or (not (eq start hyalo-tengwar--last-window-start))
              (not (eq end hyalo-tengwar--last-window-end))
              (not (equal word-bounds hyalo-tengwar--last-word-bounds)))
      ;; Clear overlays from previous word-at-point if it changed
      (when (and hyalo-tengwar--last-word-bounds
                 (not (equal word-bounds hyalo-tengwar--last-word-bounds)))
        (remove-overlays (car hyalo-tengwar--last-word-bounds)
                         (cdr hyalo-tengwar--last-word-bounds)
                         'hyalo-tengwar t))
      (setq hyalo-tengwar--last-window-start start
            hyalo-tengwar--last-window-end end
            hyalo-tengwar--last-word-bounds word-bounds)
      (hyalo-tengwar--update-visible))))

(defun hyalo-tengwar--on-window-scroll (_win _start)
  "Window scroll hook to update visible overlays."
  (hyalo-tengwar--on-post-command))

;;; Minor Mode

(defun hyalo-tengwar--enable ()
  "Enable tengwar mode (start process, hooks)."
  (hyalo-tengwar--log "Enabled in %s" (buffer-name))
  (hyalo-tengwar--start-process)
  (hyalo-tengwar--update-visible)
  (setq hyalo-tengwar--last-window-start nil
        hyalo-tengwar--last-window-end nil
        hyalo-tengwar--last-point nil)
  (add-hook 'post-command-hook #'hyalo-tengwar--on-post-command nil t)
  (add-hook 'window-scroll-functions #'hyalo-tengwar--on-window-scroll nil t))

(defun hyalo-tengwar--disable ()
  "Disable tengwar mode (remove overlays, hooks, maybe stop process)."
  (hyalo-tengwar--log "Disabled in %s" (buffer-name))
  (hyalo-tengwar--remove-overlays)
  (remove-hook 'post-command-hook #'hyalo-tengwar--on-post-command t)
  (remove-hook 'window-scroll-functions #'hyalo-tengwar--on-window-scroll t)
  (unless (cl-some (lambda (buf)
                     (and (not (eq buf (current-buffer)))
                          (or (buffer-local-value 'hyalo-tengwar-minor-mode buf)
                              (buffer-local-value 'hyalo-tengwar-partial-mode buf))))
                   (buffer-list))
    (hyalo-tengwar--stop-process)))

;;; Minor Mode

;;;###autoload
(define-minor-mode hyalo-tengwar-minor-mode
  "Minor mode to render visible text in Tengwar script."
  :lighter " Tengwar"
  :group 'hyalo-tengwar
  (if hyalo-tengwar-minor-mode
      (progn
        ;; Disable partial mode if active (mutex)
        (when (bound-and-true-p hyalo-tengwar-partial-mode)
          (hyalo-tengwar-partial-mode -1))
        (hyalo-tengwar--log "Minor mode enabled (whole buffer)")
        (hyalo-tengwar--enable))
    (hyalo-tengwar--disable)))

;;;###autoload
(define-minor-mode hyalo-tengwar-partial-mode
  "Minor mode to render visible text in Tengwar script between delimiters."
  :lighter " Tengwar-Partial"
  :group 'hyalo-tengwar
  (if hyalo-tengwar-partial-mode
      (progn
        ;; Disable minor mode if active (mutex)
        (when (bound-and-true-p hyalo-tengwar-minor-mode)
          (hyalo-tengwar-minor-mode -1))
        (hyalo-tengwar--log "Partial mode enabled (delimiters: %s ... %s)"
                            (car hyalo-tengwar-partial-delimiters)
                            (cdr hyalo-tengwar-partial-delimiters))
        (hyalo-tengwar--enable))
    (hyalo-tengwar--disable)))

(provide 'hyalo-tengwar)
;;; hyalo-tengwar.el ends here
