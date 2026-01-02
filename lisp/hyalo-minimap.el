;;; hyalo-minimap.el --- Mouse interaction for demap -*- lexical-binding: t -*-

(require 'demap)
(require 'cl-lib)
(require 'hyalo)

(defun hyalo-minimap--scroll-to-event (event)
  "Scroll source window to the position of EVENT in minimap."
  (let* ((posn (event-end event))
         (pos (posn-point posn))
         (window (posn-window posn)))
    (when (and pos (window-live-p window))
      (let ((buffer (window-buffer window)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when-let* ((source-window demap--minimap-window))
              (when (window-live-p source-window)
                (with-selected-window source-window
                  (goto-char pos)
                  (recenter nil)
                  (redisplay))
                ;; Force minimap redraw to reflect overlay change immediately
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
