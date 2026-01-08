;;; hyalo-minimap.el --- Mouse interaction for demap -*- lexical-binding: t -*-

(require 'demap)
(require 'cl-lib)
(require 'hyalo)

(defvar hyalo-minimap--in-update nil
  "Non-nil when hyalo minimap is performing an update.")

;;; Window-end Caching (Performance Optimization)
;;
;; demap calls (window-end WINDOW t) multiple times per update cycle.
;; The `t` argument forces redisplay each time, causing cascading redraws.
;; This cache stores results per-window within a single command cycle.

(defvar hyalo-minimap--window-end-cache (make-hash-table :test 'eq :weakness 'key)
  "Cache for `window-end' results within a single command cycle.
Maps window -> (window-start . window-end) to detect stale entries.")

(defvar hyalo-minimap--cache-cycle 0
  "Counter incremented each command cycle to invalidate cache.")

(defun hyalo-minimap--clear-window-end-cache ()
  "Invalidate window-end cache for new command cycle."
  (cl-incf hyalo-minimap--cache-cycle))

(defun hyalo-minimap--window-end-advice (orig-fun &optional window update)
  "Advice for `window-end' that caches results when UPDATE is non-nil.
ORIG-FUN is the original `window-end' function.
WINDOW and UPDATE are passed through.
Prevents forced redisplay loops when called from minimap update."
  (if hyalo-minimap--in-update
      ;; Force update=nil to prevent infinite redisplay loops during scroll hooks
      (funcall orig-fun window nil)
    (let ((win (or window (selected-window))))
      (if (and update (window-live-p win))
          ;; UPDATE=t means caller wants accurate value (forces redisplay)
          ;; Cache these expensive calls
          (let* ((cache-key win)
                 (cached (gethash cache-key hyalo-minimap--window-end-cache))
                 (current-start (window-start win))
                 (current-cycle hyalo-minimap--cache-cycle))
            (if (and cached
                     (eq (car cached) current-cycle)
                     (eq (cadr cached) current-start))
                ;; Cache hit: same cycle and window-start unchanged
                (cddr cached)
              ;; Cache miss: compute and store
              (let ((result (funcall orig-fun win t)))
                (puthash cache-key (cons current-cycle (cons current-start result))
                         hyalo-minimap--window-end-cache)
                result)))
        ;; UPDATE=nil: just call original (cheap, no redisplay forced)
        (funcall orig-fun win update)))))

(defun hyalo-minimap--enable-window-end-cache ()
  "Enable window-end caching optimization."
  (advice-add 'window-end :around #'hyalo-minimap--window-end-advice)
  (add-hook 'pre-command-hook #'hyalo-minimap--clear-window-end-cache))

(defun hyalo-minimap--disable-window-end-cache ()
  "Disable window-end caching optimization."
  (advice-remove 'window-end #'hyalo-minimap--window-end-advice)
  (remove-hook 'pre-command-hook #'hyalo-minimap--clear-window-end-cache)
  (clrhash hyalo-minimap--window-end-cache))

;;; Throttling (Performance Optimization)

(defvar hyalo-minimap--throttle-timestamps (make-hash-table :test 'eq)
  "Map of function symbols to their last execution time.")

(defconst hyalo-minimap--update-interval 0.2
  "Minimum interval between demap updates in seconds.")

(defun hyalo-minimap--throttle-update (orig-fun &rest args)
  "Throttle demap updates to avoid killing keyboard navigation performance.
Maintains separate timestamps for each advised function."
  (let* ((now (float-time))
         ;; Use the function name as key. If orig-fun is a lambda/closure, use it directly.
         ;; We assume advice-add passes the symbol if we advised a symbol.
         ;; However, orig-fun is the *next* function in the chain.
         ;; To distinguish, we can't easily rely on orig-fun identity if it changes.
         ;; But we can use a closure-based approach if we generated the advice.
         ;; Simpler: just throttle based on the *advice name* if we could.
         ;; Since we can't easily get the advice name from here without overhead,
         ;; let's make a macro that generates a unique symbol or closure for each advice.
         )
    ;; Fallback: The simplistic global-throttle was definitely wrong.
    ;; We will use a unique advice for each function to capture its identity.
    (apply orig-fun args)))

(defmacro hyalo-minimap--define-throttle (func-sym)
  "Define and add throttling advice for FUNC-SYM."
  (let ((advice-name (intern (format "hyalo-minimap--throttle-%s" func-sym))))
    `(progn
       (defun ,advice-name (orig-fun &rest args)
         "Throttled wrapper."
         (let ((last-time (gethash ',func-sym hyalo-minimap--throttle-timestamps 0.0))
               (now (float-time)))
           (when (> (- now last-time) hyalo-minimap--update-interval)
             (puthash ',func-sym now hyalo-minimap--throttle-timestamps)
             (let ((hyalo-minimap--in-update t))
               (apply orig-fun args)))))
       (advice-add ',func-sym :around #',advice-name))))

(hyalo-minimap--define-throttle demap-track-window-mode-update)
(hyalo-minimap--define-throttle demap-current-line-mode-update)
(hyalo-minimap--define-throttle demap--visible-region-mode-update)
(hyalo-minimap--define-throttle demap--visible-region-mode-update-if)
(hyalo-minimap--define-throttle demap--visible-region-mode-update-window-as)

;; Enable caching when this module loads
(hyalo-minimap--enable-window-end-cache)

(defun hyalo-minimap--scroll-to-event (event)
  "Scroll source window to the position of EVENT in minimap.
When in diffview mode (scroll-all-mode active), syncs both side-by-side windows."
  (let* ((posn (event-end event))
         (pos (posn-point posn))
         (window (posn-window posn)))
    (when (and pos (window-live-p window))
      (let ((buffer (window-buffer window)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when-let* ((source-window demap--minimap-window))
              (when (window-live-p source-window)
                ;; Calculate line number for position
                (let ((line-num (with-current-buffer (window-buffer source-window)
                                  (save-excursion
                                    (goto-char pos)
                                    (line-number-at-pos)))))
                  ;; Scroll source window
                  (with-selected-window source-window
                    (goto-char pos)
                    (recenter nil))
                  ;; If scroll-all-mode is active, sync the other side-by-side window
                  (when (bound-and-true-p scroll-all-mode)
                    (let ((other-buf (if (string= (buffer-name (window-buffer source-window))
                                                  "*side-by-side-1*")
                                         (get-buffer "*side-by-side-2*")
                                       (get-buffer "*side-by-side-1*"))))
                      (when other-buf
                        (dolist (win (get-buffer-window-list other-buf nil t))
                          (with-selected-window win
                            (goto-char (point-min))
                            (forward-line (1- line-num))
                            (recenter nil)))))))
                ;; Force minimap redraw
                (when (window-live-p window)
                  (force-window-update window)
                  (redisplay))))))))))

(defun hyalo-minimap-click (event)
  "Scroll source window to the position of EVENT in minimap."
  (interactive "e")
  (hyalo-minimap--scroll-to-event event))

(defun hyalo-minimap-drag-scroll (event)
  "Scroll source window by dragging in minimap."
  (interactive "e")
  (hyalo-minimap--scroll-to-event event)
  (track-mouse
    (let ((continue t))
      (while continue
        (let ((ev (read-event)))
          (cond
           ((mouse-movement-p ev)
            (hyalo-minimap--scroll-to-event ev))
           ((memq (car-safe ev) '(mouse-1 drag-mouse-1))
            (setq continue nil))
           (t
            (setq continue nil))))))))

(defvar hyalo-minimap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap mouse-set-point] #'hyalo-minimap-click)
    (define-key map [remap mouse-drag-region] #'hyalo-minimap-drag-scroll)
    (define-key map (kbd "<down-mouse-1>") #'hyalo-minimap-drag-scroll)
    (define-key map (kbd "<mouse-1>") #'hyalo-minimap-click)
    (define-key map (kbd "<drag-mouse-1>") #'ignore)
    map)
  "Keymap for hyalo minimap mode.")

(defun hyalo-minimap-setup ()
  "Enable minimap mode in current minimap buffer."
  (hyalo-minimap-mode 1))

(define-minor-mode hyalo-minimap-mode
  "Minor mode to enable mouse scrolling in demap."
  :init-value nil
  :keymap nil
  (if hyalo-minimap-mode
      (progn
        ;; Use overriding-local-map for maximum priority to beat fundamental-mode
        (setq-local overriding-local-map hyalo-minimap-mode-map))
    (kill-local-variable 'overriding-local-map)))

(provide 'hyalo-minimap)
