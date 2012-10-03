;;; ematrix.el

;; MIT License, Copyright (C) 2011 liuhui

;;; Commentary:

;; Installation:
;;
;; Put this file in your `load-path' and compile it, then add the
;; following to your ~/.emacs:
;;
;; (autoload 'ematrix "ematrix" "The Matrix" t)
;;
;; Usage:
;;
;; M-x ematrix. Press C-g to quit
;;
;; TODO:
;;
;; * ematrix.el has some problems under text-only terminals
;; * optimizations
;; * hide the cursor

;;; code:

;;; Customizable variables =============================================

(defgroup ematrix nil
  "The Matrix in emacs."
  :group 'games
  :version "1.0")

(defcustom ematrix-timer 0.08
  "Refresh time"
  :type 'number
  :group 'ematrix)

(defcustom ematrix-fgcolor "green4"
  "Foreground color"
  :type 'color
  :group 'ematrix)

(defcustom ematrix-bgcolor "black"
  "Background color"
  :type 'color
  :group 'ematrix)

(defcustom ematrix-boldcolor "green1"
  "Color of bold character"
  :type 'color
  :group 'ematrix)

(defcustom ematrix-hlcolor "white"
  "Color used to highlight character"
  :type 'color
  :group 'ematrix)

(defcustom ematrix-nonblank '(5 . 20)
  "Min and max length of continuous non-blanks per column"
  :type '(cons integer integer)
  :group 'ematrix)

(defcustom ematrix-blank '(5 . 15)
  "Min and max length of continuous blanks per column"
  :type '(cons integer integer)
  :group 'ematrix)

(defcustom ematrix-asynchronous-scroll t
  "Non-nil means asynchronous scroll."
  :type 'boolean
  :group 'ematrix)

(defcustom ematrix-char '(33 . 127)
  "* [a-z]: '(97 . 123)
* [A-Z]: '(65 . 91)
* [0-9]: '(48 . 58)"
  :type '(cons integer integer)
  :group 'ematrix)

;;; Internal variables =================================================

(defvar ematrix-matrix nil
  "An integer matrix whose element describes the state of
corresponding char displayed in emacs:
- 0 means a whitespace character
- 1 means a non-whitespace character
- 2 means a hightlighted non-whitespace character
")

(defvar ematrix-control-array nil
  "An array whose length equals to the width of frame. Its n-th
element should be randomly generated according to
`ematrix-nonblank' and `ematrix-blank'.")

(defvar ematrix-char-length nil)
(defvar ematrix-blank-length nil)
(defvar ematrix-nonblank-length nil)

;;; Funtions ===========================================================

;;;###autoload
(defun ematrix ()
  (interactive)
  (if (or (< (frame-width) 10) (< (frame-height) 10))
      (error "%s" "Frame is too small."))
  (save-window-excursion
    (let ((ematrix-blank-length
           (- (cdr ematrix-blank) (car ematrix-blank)))
          (ematrix-nonblank-length
           (- (cdr ematrix-nonblank) (car ematrix-nonblank)))
          (ematrix-char-length
           (- (cdr ematrix-char) (car ematrix-char)))
          (ematrix-buf (get-buffer-create "*Ematrix*"))
          ;; save state of emacs
          (fg (cdr (assq 'foreground-color (frame-parameters))))
          (bg (cdr (assq 'background-color (frame-parameters))))
          (cc (cdr (assq 'cursor-color (frame-parameters))))
          (scrollbar-p scroll-bar-mode)
          mode-line-format              ; remove mode-line
          menu-p toolbar-p tab-p restore)
      ;; construct restore function
      (setq menu-p (if menu-bar-mode 1 -1)
            toolbar-p (if tool-bar-mode 1 -1)
            restore '((kill-buffer ematrix-buf)
                      (menu-bar-mode menu-p)
                      (tool-bar-mode toolbar-p)
                      (set-scroll-bar-mode scrollbar-p)
                      (set-foreground-color fg)
                      (set-background-color bg)
                      (set-cursor-color cc)))
      (if (fboundp 'tabbar-mode)
          (setq tab-p (if tabbar-mode 1 -1)
                restore (append restore '((tabbar-mode tab-p)))))
      (setq restore `(lambda () ,@restore (setq restore nil)))
      (delete-other-windows)
      (switch-to-buffer ematrix-buf)
      (condition-case nil
          (progn
            ;; Initialize the interface
            (set-foreground-color ematrix-fgcolor)
            (set-cursor-color ematrix-bgcolor)
            (set-background-color ematrix-bgcolor)
            (if (fboundp 'tabbar-mode) (tabbar-mode -1))
            (set-scroll-bar-mode nil)
            (tool-bar-mode -1)
            (menu-bar-mode -1)
            (garbage-collect)
            ;; call main funciton
            (if (input-pending-p) (discard-input))
            (ematrix-main))
        ;; restore everything when error or quit
        (error (funcall restore))
        (quit  (funcall restore)))
      (if restore (funcall restore)))))

(defun ematrix-main ()
  (let* ((fw (frame-width)) (fh (frame-height)) col)
    (setq ematrix-control-array (make-vector fw -1))
    (ematrix-fill-out fh fw)            ; fill out window
    ;; Initialize char matrix
    (setq ematrix-matrix (make-vector fw (make-vector fh 0)))
    (while (not (input-pending-p))
      (setq col 0)
      (while (< col fw)
        ;; only update specified column
        (if ematrix-asynchronous-scroll
            (if (zerop (random 2)) (ematrix-to-column col fh fw))
          (ematrix-to-column col fh fw))
        (setq col (+ 2 col)))
      (goto-char 1)                     ; fix cursor
      (sit-for ematrix-timer))))

(defun ematrix-fill-out (height width)
  "Fill window with whitespace"
  (let ((line (make-string width 32)) (i 0))
    (goto-char 1)
    ;; (erase-buffer)
    (setq line (concat line "\n"))
    (while (< i height)
      (insert line)
      (setq i (1+ i)))
    (goto-char 1)))

(defsubst ematrix-position (i j k)
  "Return the position of char which is at i-th line and j-th
  column when every line has length of k."
  (+ (- j k) (* i k) i))

(defun ematrix-to-column (col height width)
  (let* ((i height) (c (aref ematrix-control-array col))
         (ns 0)     ; number of non-blank at the bottom of this column
         (nb 0)     ; number of blank at the bottom of this column
         (column (copy-sequence (aref ematrix-matrix col))))
    (while (> i 0)
      ;; `ns' and `nb' are only used to decide whether char should be
      ;; inserted at the first line
      (if (zerop (aref column (1- i)))
          (setq ns 0 nb (1+ nb))
        (setq nb 0 ns (1+ ns)))
      (cond
       ;; at the first line
       ((eq i 1)
        (cond
         ;; too much nonblanks -> remove char
         ((and (> ns 0) (>= ns c))
          (goto-char (ematrix-position i col width))
          (delete-char 1) (insert 32)
          (aset column 0 0)
          (aset ematrix-control-array col
                (- (random ematrix-blank-length) (cdr ematrix-blank))))
         ;; too much blanks -> insert random char
         ((and (> nb 0) (>= nb (- c)))
          (goto-char (ematrix-position i col width))
          (delete-char 1)
          (insert (+ (car ematrix-char) (random ematrix-char-length)))
          (aset column 0 1)
          (aset ematrix-control-array col
                (+ (car ematrix-nonblank) (random ematrix-nonblank-length))))))
       ;; at the last line
       ((eq i height)
        (cond
         ;; blank here & non-blank previous line -> insert random char
         ((and (zerop (aref column (1- i)))
               (/= (aref column (- i 2)) 0))
          (goto-char (ematrix-position i col width))
          (delete-char 1)
          (insert (propertize
                   (string (+ (car ematrix-char) (random ematrix-char-length)))
                   'face (list 'bold `((:foreground ,ematrix-hlcolor)))))
          (aset column (1- i) 2))
         ;; non-blank here & blank previous line -> remove char
         ((and (/= (aref column (1- i)) 0)
               (zerop (aref column (- i 2))))
          (goto-char (ematrix-position i col width))
          (delete-char 1) (insert 32)
          (aset column (1- i) 0))
         ;; (non-blank previous line) & highlight -> remove highlight
         ((eq (aref column (1- i)) 2)
          (goto-char (ematrix-position i col width))
          (remove-text-properties (point) (+ 1 (point))
                                  `(face ((:foreground ,ematrix-hlcolor))))
          (aset column (1- i) 1))))
       (t
        (cond
         ;; blank here & nonblank previous -> insert random char
         ((and (zerop (aref column (1- i)))
               (/= (aref column (- i 2)) 0))
          (goto-char (ematrix-position i col width))
          (delete-char 1)
          ;; insert normal, bold or bold & hightlight ?
          (let ((r (random 7))
                (ic (+ (car ematrix-char) (random ematrix-char-length))))
            (cond
             ((> r 3) (insert ic)
              (aset column (1- i) 1))
             ((< r 2)
              (insert (propertize
                       (string ic)
                       'face (list 'bold `((:foreground ,ematrix-hlcolor)))))
              (aset column (1- i) 2))
             (t (insert (propertize
                         (string ic)
                         'face (list 'bold `((:foreground ,ematrix-boldcolor)))))
                (aset column (1- i) 1)))))
         ;; non-blank here
         ((/= (aref column (1- i)) 0)
          (cond
           ;; blank previous -> remove
           ((zerop (aref column (- i 2)))
            (goto-char (ematrix-position i col width))
            (delete-char 1) (insert 32)
            (aset column (1- i) 0))
           ;; highlight & non-blank next line -> remove highlight
           ((and (eq (aref column (1- i)) 2)
                 (/= (aref column i) 0))
            (goto-char (ematrix-position i col width))
            (remove-text-properties (point) (+ 1 (point))
                                    `(face ((:foreground ,ematrix-hlcolor))))
            (aset column (1- i) 1)))))))
      (setq i (1- i)))
    ;; update col-th column of ematrix-matrix
    (aset ematrix-matrix col column)))

(provide 'ematrix)

;;; ematrix.el ends here
