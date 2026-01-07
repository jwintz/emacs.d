;;; hyalo-minimap.el --- Mouse interaction for demap -*- lexical-binding: t -*-

(require 'demap)
(require 'cl-lib)
(require 'hyalo)

;;; Window-end Caching (Performance Optimization)
;;
;; demap calls (window-end WINDOW t) multiple times per update cycle.
;; The `t` argument forces redisplay each time, causing cascading redraws.
;; This cache stores results per-window within a single command cycle.

(defvar hyalo-minimap--window-end-cache (make-hash-table :test 'eq :weakness 'key)
  "Cache for `window-end' results within a single command cycle.
Maps window -> (window-start . window-end) to detect stale entries.")

(defvar hyalo-minimap--cache-cycle nil
  "Counter incremented each command cycle to invalidate cache.")

(defun hyalo-minimap--clear-window-end-cache ()
  "Invalidate window-end cache for new command cycle."
  (cl-incf hyalo-minimap--cache-cycle))

(defun hyalo-minimap--window-end-advice (orig-fun window &optional update)
  "Advice for `window-end' that caches results when UPDATE is non-nil.
ORIG-FUN is the original `window-end' function.
WINDOW and UPDATE are passed through."
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
      (funcall orig-fun win update))))

(defun hyalo-minimap--enable-window-end-cache ()
  "Enable window-end caching optimization."
  (advice-add 'window-end :around #'hyalo-minimap--window-end-advice)
  (add-hook 'pre-command-hook #'hyalo-minimap--clear-window-end-cache))

(defun hyalo-minimap--disable-window-end-cache ()
  "Disable window-end caching optimization."
  (advice-remove 'window-end #'hyalo-minimap--window-end-advice)
  (remove-hook 'pre-command-hook #'hyalo-minimap--clear-window-end-cache)
  (clrhash hyalo-minimap--window-end-cache))

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
