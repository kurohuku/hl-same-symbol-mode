(eval-when-compile (require 'cl))

(defgroup hl-same-symbol nil
  "hl-same-symbol")

(defcustom hl-same-symbol-face 'highlight
  "hl-same-symbol face"
  :group 'hl-same-symbol)

(defcustom hl-same-symbol-delay 0.25
  "Time in seconds to delay before highlighting."
  :type 'number
  :group 'hl-same-symbol)

(defvar hl-same-symbol-idle-timer nil)

(define-minor-mode hl-same-symbol-mode
  "toggle hl-same-symbol mode"
  :global nil
  :group 'hl-same-symbol
  :init-value nil
  :keymap nil
  ;; body
  ;; 古いタイマーを削除する
  (when hl-same-symbol-idle-timer
    (cancel-timer hl-same-symbol-idle-timer))
  (setf hl-same-symbol-idle-timer nil)
  ;; hl-same-symbol-modeが有効なバッファが1つでもあれば
  ;; 新しくタイマーを作成する
  (when (memq t (mapcar (lambda (buffer)
			  (with-current-buffer buffer
			    hl-same-symbol-mode))
			(buffer-list)))
    (hl-same-symbol::unhighlight)
    (setf hl-same-symbol-idle-timer
	  (run-with-idle-timer hl-same-symbol-delay
			       t
			       'hl-same-symbol-function))))

(defun hl-same-symbol-function ()
  (let ((sym (symbol-at-point)))
    (cond
     ((and sym (equal (symbol-name sym) *hl-same-symbol::text*))
      'nothing-to-do)
     (sym
      (hl-same-symbol::unhighlight)
      (hl-same-symbol::highlight (symbol-name sym)))
     (t
      (hl-same-symbol::unhighlight)))
    t))

(defvar *hl-same-symbol::text* "")
(defvar *hl-same-symbol::list* nil)

(defun hl-same-symbol::highlight (text)
  (setf *hl-same-symbol::text* text)
  (let ((len (length text)))
    (save-excursion
      (dolist (win (window-list))
	(with-current-buffer (window-buffer win)
	  (when hl-same-symbol-mode
	    (goto-char (window-start win))
	    (while (let ((pos (search-forward text nil t)))
		     (and pos (< (- pos len) (window-end win))))
	      (hl-same-symbol::highlight-internal text len))))))))

(defun hl-same-symbol::highlight-internal (text len)
  (let ((sym (symbol-at-point)))
    (when (and sym (equal (symbol-name sym) text))
      (let ((overlay (make-overlay (- (point) len) (point))))
	(overlay-put overlay 'face hl-same-symbol-face)
	(push overlay *hl-same-symbol::list*)
	overlay))))

(defun hl-same-symbol::unhighlight ()
  (mapcar 'delete-overlay *hl-same-symbol::list*)
  (setf *hl-same-symbol::list* nil))

(provide 'hl-same-symbol)
