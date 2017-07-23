;;; eg.el --- Norton Guide reader -*- lexical-binding: t -*-
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 0.1
;; Keywords: docs
;; URL: https://github.com/davep/eg.el
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

;; eg.el is free software distributed under the terms of the GNU General
;; Public Licence, version 2 or (at your option) any later version. For
;; details see the file COPYING.

;;; Commentary:
;;
;; eg.el provides code for reading the content of help databases built with
;; Norton Guide and Expert Help. It also provides commands for viewing and
;; navigating the content of Norton Guide files.
;;
;; eg.el is currently a work in progress. Expect it to not work, or work
;; different, or something.

;;; Code:

(require 'cl-lib)

(defconst eg-magic-ng "NG"
  "Magic marker for a guide built with Norton Guide.")

(defconst eg-magic-eh "EH"
  "Magic marker for a guide built with Expert Help.")

(defconst eg-title-length 40
  "Maximum length of a guide title.")

(defconst eg-credit-length 66
  "Maximum length of a line in a guide's credits.")

(defconst eg-prompt-length 128
  "Maximum length of a prompt on a menu.")

(defconst eg-line-length 1024
  "Maximum length of a line in a guide entry.")

(defconst eg-entry-short 0
  "Id of a short entry in a guide.")

(defconst eg-entry-long 1
  "Id of a long entry in a guide.")

(defconst eg-entry-menu 2
  "Id of a menu in a guide.")

(defconst eg-max-see-alsos 20
  "Maximum number of see-also items for a single entry in a guide.

This is the limit published in the Expert Help Compiler manual
and, while this limit isn't really needed in this code, it does
help guard against corrupt guides.")

(defconst eg-rle-marker 255
  "Value of a guide's RLE marker.")

(defvar eg-buffer-name-function (lambda (file) (format " *EG: %s*" file))
  "Function that names a buffer for reading from a Norton guide file.")

(cl-defstruct eg-guide
  ;; Holds details about a guide.
  file
  buffer
  magic
  menu-count
  title
  credits
  menus
  first-entry-pos
  (pos 0))

(cl-defstruct eg-menu
  ;; Holds details about a menu found in a guide.
  title
  prompt-count
  prompts
  offsets)

(cl-defstruct eg-entry
  ;; Holds the details of a guide entry.
  offset
  type
  size
  line-count
  has-see-also
  parent-line
  parent
  parent-menu
  parent-prompt
  previous
  next
  offsets
  lines
  see-also)

(cl-defstruct eg-see-also
  ;; Holds the details of the see-alsos for a guide entry.
  prompt-count
  prompts
  offsets)

(cl-defun eg-skip (guide &optional (bytes 1))
  "Skip BYTES bytes in GUIDE."
  (cl-incf (eg-guide-pos guide) bytes))

(defun eg-goto (guide pos)
  "Move read location in GUIDE to POS."
  (setf (eg-guide-pos guide) pos))

(defun eg-goto-first (guide)
  "Go to the first entry in GUIDE."
  (eg-goto guide (eg-guide-first-entry-pos guide)))

(defmacro eg-save-excursion (guide &rest body)
  "Read from GUIDE and evaluate BODY but leave location unmoved."
  (declare (indent 1))
  (let ((saved-pos (cl-gensym "eg-saved-pos-")))
    `(let ((,saved-pos (eg-guide-pos ,guide)))
       (unwind-protect
           (progn
             ,@body)
         (eg-goto ,guide ,saved-pos)))))

(defmacro eg-with-guide-buffer (guide &rest body)
  "Make GUIDE the current buffer then evaluate BODY."
  (declare (indent 1))
  `(with-current-buffer (eg-guide-buffer ,guide)
     ,@body))

(defun eg-read (guide len)
  "Read bytes from GUIDE.

LEN is the number of bytes to read."
  (eg-with-guide-buffer guide
    (let* ((from (min (+ (point-min) (eg-guide-pos guide)) (point-max)))
           (to   (min (+ from len) (point-max))))
      (eg-skip guide len)
      (buffer-substring-no-properties from to))))

(cl-defun eg-decrypt (n &optional (decrypt t))
  "Decrypt value N if DECRYPT is non-nil."
  (if decrypt (logxor n 26) n))

(defun eg-make-signed-byte (n)
  "Ensure N is a signed byte."
  (if (zerop (logand n #x80))
      n
    (- n #x100)))

(cl-defun eg-read-byte (guide &optional (decrypt t))
  "Read a byte from GUIDE.

If DECRYPT is non-nil, decrypt it.

This has the side-effect of moving `eg-guide-pos'."
  (let ((byte (string-to-char (eg-read guide 1))))
    (eg-make-signed-byte (eg-decrypt byte decrypt))))

(cl-defun eg-peek-byte (guide &optional (decrypt t))
  "Return current byte in GUIDE but don't move position."
  (eg-save-excursion guide
    (eg-read-byte guide decrypt)))

(defun eg-make-signed-word (n)
  "Ensure N is a signed word."
  (if (zerop (logand n #x8000))
      n
    (- n #x10000)))

(cl-defun eg-read-word (guide &optional (decrypt t))
  "Read a word from GUIDE.

If DECRYPT is non-nil, decrypt it.

This has the side-effect of moving `eg-guide-pos'"
  (let ((word (eg-read guide 2)))
    (let ((lo (eg-decrypt (aref word 0) decrypt))
          (hi (eg-decrypt (aref word 1) decrypt)))
      (eg-make-signed-word (+ (lsh hi 8) lo)))))

(cl-defun eg-peek-word (guide &optional (decrypt t))
  "Return current word in GUIDE but don't move position."
  (eg-save-excursion guide
    (eg-read-word guide decrypt)))

(defun eg-make-signed-long (n)
  "Ensure N is a signed long."
  (if (zerop (logand n #x80000000))
      n
    (- n #x100000000)))

(cl-defun eg-read-long (guide &optional (decrypt t))
  "Read a long from GUIDE.

If DECRYPT is non-nil, decrypt it.

This has the side-effect of moving `eg-guide-pos'"
  (let ((long (eg-read guide 4)))
    (let ((lolo (eg-decrypt (aref long 0) decrypt))
          (lohi (eg-decrypt (aref long 1) decrypt))
          (hilo (eg-decrypt (aref long 2) decrypt))
          (hihi (eg-decrypt (aref long 3) decrypt)))
      (eg-make-signed-long
       (+ (lsh (+ (lsh hihi 8) hilo) 16) (+ (lsh lohi 8) lolo))))))

(cl-defun eg-peek-long (guide &optional (decrypt t))
  "Return current long in GUIDE but don't move position."
  (eg-save-excursion guide
    (eg-read-long guide decrypt)))

(cl-defun eg-decrypt-string (s)
  "Decrypt string S."
  (mapconcat #'string (mapcar #'eg-decrypt s) ""))

(defun eg-rle-marker-p (c)
  "Does C look like an RLE marker?"
  (= c eg-rle-marker))

(defun eg-expand-string (s)
  "RLE-expand spaces in string S."
  (apply #'concat
         (cl-loop for c across s
                  with expand = nil
                  if expand collect (if (eg-rle-marker-p c)
                                        " "
                                      (make-string c 32))
                  and do (setq expand nil)
                  else if (eg-rle-marker-p c) do (setq expand t)
                  else collect (string c))))

(cl-defun eg-read-string (guide len &optional (decrypt t))
  "Read a string of LEN characters from GUIDE.

Any trailing NUL characters are removed."
  (let ((s (eg-read guide len)))
    (replace-regexp-in-string "\0[\0-\377[:nonascii:]]*" ""
                              (if decrypt
                                  (eg-decrypt-string s)
                                s))))

(cl-defun eg-read-string-z (guide len &optional (decrypt t))
  "Read string up to LEN characters, stopping if a nul is encountered."
  (let ((s (eg-save-excursion guide
             (eg-read-string guide len decrypt))))
    (eg-skip guide (1+ (length s)))
    s))

(defun eg-skip-entry (guide)
  "Skip an entry/menu in GUIDE."
  (eg-skip guide (+ 22 (eg-read-word guide))))

(defun eg-next-entry (guide)
  "Move GUIDE location to the next entry."
  (unless (eg-eof-p guide)
    (eg-read-word guide)
    (eg-skip-entry guide)))

(defun eg-read-header (guide)
  "Read the header of GUIDE."
  ;; Read the magic "number".
  (setf (eg-guide-magic guide) (eg-read guide 2))
  ;; Skip 4 bytes (I'm not sure what they are for).
  (eg-skip guide 4)
  ;; Get the count of menus.
  (setf (eg-guide-menu-count guide) (eg-read-word guide nil))
  ;; Get the title of the guide.
  (setf (eg-guide-title guide) (eg-read-string guide eg-title-length nil))
  ;; Load the credits for the guide.
  (setf (eg-guide-credits guide)
        (cl-loop for n from 0 to 4
                 collect (eg-read-string guide eg-credit-length nil)))
  guide)

(defun eg-read-menu (guide)
  "Read a menu from GUIDE."
  ;; Skip the byte size of the menu entry.
  (eg-read-word guide)
  (let ((menu (make-eg-menu)))
    ;; Get the number of prompts on the menu.
    (setf (eg-menu-prompt-count menu) (1- (eg-read-word guide)))
    ;; Skip 20 bytes.
    (eg-skip guide 20)
    ;; Get the offsets into the file for each prompt on the menu
    (setf (eg-menu-offsets menu)
          (cl-loop for n from 1 to (eg-menu-prompt-count menu)
                   collect (eg-read-long guide)))
    ;; Skip some unknown values.
    (eg-skip guide (* (1+ (eg-menu-prompt-count menu)) 8))
    ;; Get the menu's title.
    (setf (eg-menu-title menu) (eg-read-string-z guide eg-prompt-length))
    ;; Now load up the prompts.
    (setf (eg-menu-prompts menu)
          (cl-loop for n from 1 to (eg-menu-prompt-count menu)
                   collect (eg-expand-string (eg-read-string-z guide eg-prompt-length))))
    ;; Finally, skip an unknown byte.
    (eg-skip guide)
    menu))

(defun eg-read-menus (guide)
  "Read the menus in GUIDE."
  (let ((i 0))
    (while (< i (eg-guide-menu-count guide))
      (let ((type (eg-read-word guide)))
        (cond ((= type eg-entry-short)
               (eg-skip-entry guide))
              ((= type eg-entry-long)
               (eg-skip-entry guide))
              ((= eg-entry-menu)
               (setf (eg-guide-menus guide)
                     (nconc (eg-guide-menus guide) (list (eg-read-menu guide))))
               (cl-incf i))
              (t
               (setq i (eg-guide-menu-count guide))))))))

(defun eg-entry-short-p (entry)
  "Is ENTRY a short entry?"
  (when entry
    (= (eg-entry-type entry) eg-entry-short)))

(defun eg-entry-long-p (entry)
  "Is ENTRY a long entry?"
  (when entry
    (= (eg-entry-type entry) eg-entry-long)))

(defun eg-entry-type-description (entry)
  "Describe the type of ENTRY."
  (cond ((eg-entry-short-p entry)
         "Short")
        ((eg-entry-long-p entry)
         "Long")
        (t
         "Unknown")))

(defun eg-load-see-alsos (guide)
  "Load a list of see also entries from current position in GUIDE."
  (let ((see-also (make-eg-see-also)))
    (setf (eg-see-also-prompt-count see-also) (min (eg-read-word guide) eg-max-see-alsos))
    (setf (eg-see-also-offsets see-also)
          (cl-loop for n from 1 to (eg-see-also-prompt-count see-also)
                   collect (eg-read-long guide)))
    (setf (eg-see-also-prompts see-also)
          (cl-loop for n from 1 to (eg-see-also-prompt-count see-also)
                   collect (eg-expand-string (eg-read-string-z guide eg-prompt-length))))
    see-also))

(defun eg-read-entry (guide)
  "Read the entry at the current location in GUIDE."
  (let ((entry (make-eg-entry)))
    ;; Load the main "header" information for an entry.
    (setf (eg-entry-offset entry) (eg-guide-pos guide))
    (setf (eg-entry-type   entry) (eg-read-word guide))
    (when (or (eg-entry-short-p entry) (eg-entry-long-p entry))
      (setf (eg-entry-size          entry) (eg-read-word guide))
      (setf (eg-entry-line-count    entry) (eg-read-word guide))
      (setf (eg-entry-has-see-also  entry) (not (zerop (eg-read-word guide))))
      (setf (eg-entry-parent-line   entry) (eg-read-word guide))
      (setf (eg-entry-parent        entry) (eg-read-long guide))
      (setf (eg-entry-parent-menu   entry) (eg-read-word guide))
      (setf (eg-entry-parent-prompt entry) (eg-read-word guide))
      (setf (eg-entry-previous      entry) (eg-read-long guide))
      (setf (eg-entry-next          entry) (eg-read-long guide))
      ;; If it's a short entry...
      (when (eg-entry-short-p entry)
        ;; ...load the offsets associated with each line.
        (setf (eg-entry-offsets entry)
              (cl-loop for n from 1 to (eg-entry-line-count entry)
                       do (eg-read-word guide) ; Skip unknown word.
                       collect (eg-read-long guide))))
      ;; Load the lines for the entry.
      (setf (eg-entry-lines entry)
            (cl-loop for n from 1 to (eg-entry-line-count entry)
                     collect (eg-expand-string (eg-read-string-z guide eg-line-length))))
      ;; If it's a long entry, and it has a see-also list...
      (when (and (eg-entry-long-p entry) (eg-entry-has-see-also entry))
        ;; ...load the see-alsos.
        (setf (eg-entry-see-also entry) (eg-load-see-alsos guide)))
      entry)))

(defun eg-load-entry (guide)
  "Load the current entry from the GUIDE."
  (eg-save-excursion guide
    (eg-read-entry guide)))

(defun eg-entry-text (entry)
  "Get the text of ENTRY as a single string.

New line markers are added at the end of each line."
  (cl-loop for line in (eg-entry-lines entry) concat line concat "\n"))

(defun eg-guide-good-magic-p (guide)
  "Does GUIDE appear to be a Norton Guide file?"
  (memq (eg-guide-magic guide) (list eg-magic-ng eg-magic-eh)))

(defun eg-guide-type (guide)
  "Return a string that describes the type of GUIDE."
  (cond ((string= (eg-guide-magic guide) eg-magic-ng)
         "Norton Guide")
        ((string= (eg-guide-magic guide) eg-magic-eh)
         "Expert Help")
        (t
         "Unknown")))

(defun eg-guide-has-menus-p (guide)
  "Does GUIDE have menus?"
  (> (eg-guide-menu-count guide) 0))

(defun eg-entry-looking-at-short-p (guide)
  "Does it look like GUIDE is positioned on a short entry?"
  (eg-entry-short-p (eg-load-entry guide)))

(defun eg-entry-looking-at-long-p (guide)
  "Does it look like GUIDE is positioned on a long entry?"
  (eg-entry-long-p (eg-load-entry guide)))

(defun eg-entry-has-parent-p (entry)
  "Does ENTRY appear to have a parent entry?"
  (> (eg-entry-parent entry) 0))

(defun eg-entry-has-previous-p (entry)
  "Does ENTRY appear to have a previous entry?"
  (> (eg-entry-previous entry) 0))

(defun eg-entry-has-next-p (entry)
  "Does ENTRY appear to have a next entry?"
  (> (eg-entry-next entry) 0))

(defun eg-entry-has-parent-menu-p (entry)
  "Does ENTRY have a parent menu?"
  (> (eg-entry-parent-menu entry) -1))

(defun eg-entry-has-parent-prompt-p (entry)
  "Does ENTRY know which parent menu prompt it relates to?"
  (> (eg-entry-parent-prompt entry) -1))

(defun eg-eof-p (guide)
  "Do we appear to be at the end of GUIDE?"
  (or
   (eg-with-guide-buffer guide
     (>= (+ (point-min) (eg-guide-pos guide)) (point-max)))
   (not (or (eg-entry-looking-at-short-p guide)
            (eg-entry-looking-at-long-p guide)))))

(defun eg-open (file)
  "Open FILE and return the buffer that'll be used to read it."
  (when (file-exists-p file)
    (let ((guide (eg-read-header
                  (with-current-buffer (generate-new-buffer (funcall eg-buffer-name-function file))
                    (set-buffer-multibyte nil)
                    (let ((coding-system-for-read 'binary))
                      (insert-file-contents-literally file))
                    (make-eg-guide :file file :buffer (current-buffer))))))
      (when (eg-guide-has-menus-p guide)
        (eg-read-menus guide))
      (setf (eg-guide-first-entry-pos guide) (eg-guide-pos guide))
      guide)))

(defun eg-close (guide)
  "Close GUIDE."
  (kill-buffer (eg-guide-buffer guide)))

(defmacro eg-with-guide (guide file &rest body)
  "Open GUIDE from FILE and then evaluate BODY.

`eg-with-guide' handles opening the Norton Guide file and also
ensures that it is closed again after BODY has been evaluated."
  (declare (indent 2))
  `(let ((,guide (eg-open ,file)))
     (unwind-protect
         (progn
           ,@body)
       (eg-close ,guide))))


(defvar eg--current-guide nil
  "The current guide being viewed in an EG buffer.")

(defvar eg--current-entry nil
  "The entry currently being viewed in an EG buffer.")

(defvar eg-mode-map nil
  "Local keymap for Expert Guide.")

(unless eg-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "TAB") #'eg-jump-next-link)
    (define-key map [backtab]   #'eg-jump-prev-link)
    (define-key map ">"         #'eg-goto-next-entry-maybe)
    (define-key map "l"         #'eg-goto-next-entry-maybe)
    (define-key map "d"         #'eg-goto-next-entry-maybe)
    (define-key map "<"         #'eg-goto-prev-entry-maybe)
    (define-key map "h"         #'eg-goto-prev-entry-maybe)
    (define-key map "a"         #'eg-goto-prev-entry-maybe)
    (define-key map "^"         #'eg-goto-parent-entry-maybe)
    (define-key map "k"         #'eg-goto-parent-entry-maybe)
    (define-key map "w"         #'eg-goto-parent-entry-maybe)
    (define-key map "m"         #'eg-view-menu)
    (define-key map "q"         #'eg-quit)
    (define-key map "?"         #'describe-mode)
    (setq eg-mode-map map)))

(defun eg--entry-menu-path (entry)
  "Describe the menu path for ENTRY."
  (if (eg-entry-has-parent-menu-p entry)
      (concat
       (eg-menu-title
        (nth (eg-entry-parent-menu entry)
             (eg-guide-menus eg--current-guide)))
       " >> "
       (nth (eg-entry-parent-prompt entry)
            (eg-menu-prompts
             (nth (eg-entry-parent-menu entry)
                  (eg-guide-menus eg--current-guide)))))
    ""))

(defun eg--header-line ()
  "Return the header line format for an `eg-mode' buffer."
  '(:eval
    (concat
     "Expert Guide | "
     (file-name-nondirectory (eg-guide-file eg--current-guide))
     " | "
     (eg-entry-type-description eg--current-entry)
     " | "
     (eg--entry-menu-path eg--current-entry))))

(put 'eg-mode 'mode-class 'special)

;;;###autoload
(defun eg-mode ()
  "Major mode for viewing Norton Guide database files.

The key bindings for `eg-mode' are:

\\{eg-mode-map}"
  (kill-all-local-variables)
  (use-local-map eg-mode-map)
  (setq major-mode       'eg-mode
        mode-name        "Expert Guide"
        buffer-read-only t
        truncate-lines   t
        header-line-format (eg--header-line))
  (buffer-disable-undo (current-buffer)))

;;;###autoload
(defun eg (file)
  "View FILE as a Norton Guide database."
  (interactive "fGuide: ")
  (let ((buffer (get-buffer-create (format "EG: %s" file))))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (eg-mode)
      (set (make-local-variable 'eg--current-guide) (eg-open file))
      (set (make-local-variable 'eg--current-entry) nil)
      (eg--view-entry))))

(defun eg-jump-next-link ()
  "Jump to the next link in the buffer."
  (interactive)
  (unless (next-button (point))
    (setf (point) (point-min)))
  (forward-button 1))

(defun eg-jump-prev-link ()
  "Jump to the previous link in the buffer."
  (interactive)
  (unless (previous-button (point))
    (setf (point) (point-max)))
  (backward-button 1))

(defun eg-goto-parent-entry-maybe ()
  "Load and view the parent entry, if there is one."
  (interactive)
  (when (eg-entry-has-parent-p eg--current-entry)
    (eg--view-entry (eg-entry-parent eg--current-entry))))

(defun eg-goto-next-entry-maybe ()
  "Load and view the next entry, if there is one."
  (interactive)
  (when (eg-entry-has-next-p eg--current-entry)
    (eg--view-entry (eg-entry-next eg--current-entry))))

(defun eg-goto-prev-entry-maybe ()
  "Load and view the previous entry, if there is one."
  (interactive)
  (when (eg-entry-has-previous-p eg--current-entry)
    (eg--view-entry (eg-entry-previous eg--current-entry))))

;;;###autoload
(defun eg-quit ()
  "Quit the EG buffer."
  (interactive)
  (eg-close eg--current-guide)
  (kill-buffer))

(defvar eg--undosify-map nil
  "Hash table of text translations to try and turn old DOS text
  into something that might display well")

(unless eg--undosify-map
  (setq eg--undosify-map (make-hash-table))
  (puthash 1   "\u263A" eg--undosify-map)
  (puthash 2   "\u263B" eg--undosify-map)
  (puthash 3   "\u2665" eg--undosify-map)
  (puthash 4   "\u2666" eg--undosify-map)
  (puthash 5   "\u2663" eg--undosify-map)
  (puthash 6   "\u2660" eg--undosify-map)
  (puthash 7   "\u2022" eg--undosify-map)
  (puthash 8   "\u25DB" eg--undosify-map)
  (puthash 9   "\u25CB" eg--undosify-map)
  (puthash 10  "\u25D9" eg--undosify-map)
  (puthash 11  "\u2642" eg--undosify-map)
  (puthash 12  "\u2640" eg--undosify-map)
  (puthash 13  "\u266A" eg--undosify-map)
  (puthash 14  "\u266B" eg--undosify-map)
  (puthash 15  "\u263C" eg--undosify-map)
  (puthash 16  "\u25BA" eg--undosify-map)
  (puthash 17  "\u25C4" eg--undosify-map)
  (puthash 18  "\u2195" eg--undosify-map)
  (puthash 19  "\u203C" eg--undosify-map)
  (puthash 20  "\u00B6" eg--undosify-map)
  (puthash 21  "\u00A7" eg--undosify-map)
  (puthash 22  "\u25AC" eg--undosify-map)
  (puthash 23  "\u21A8" eg--undosify-map)
  (puthash 24  "\u2191" eg--undosify-map)
  (puthash 25  "\u2193" eg--undosify-map)
  (puthash 26  "\u2192" eg--undosify-map)
  (puthash 27  "\u2190" eg--undosify-map)
  (puthash 28  "\u221F" eg--undosify-map)
  (puthash 29  "\u2194" eg--undosify-map)
  (puthash 30  "\u25B2" eg--undosify-map)
  (puthash 31  "\u25BC" eg--undosify-map)
  (puthash 127 "\u2302" eg--undosify-map)
  (puthash 128 "\u00C7" eg--undosify-map)
  (puthash 129 "\u00FC" eg--undosify-map)
  (puthash 130 "\u00E9" eg--undosify-map)
  (puthash 131 "\u00E2" eg--undosify-map)
  (puthash 132 "\u00E4" eg--undosify-map)
  (puthash 133 "\u00E0" eg--undosify-map)
  (puthash 134 "\u00E5" eg--undosify-map)
  (puthash 135 "\u00E7" eg--undosify-map)
  (puthash 136 "\u00EA" eg--undosify-map)
  (puthash 137 "\u00EB" eg--undosify-map)
  (puthash 138 "\u00E8" eg--undosify-map)
  (puthash 139 "\u00EF" eg--undosify-map)
  (puthash 140 "\u00EE" eg--undosify-map)
  (puthash 141 "\u00EC" eg--undosify-map)
  (puthash 142 "\u00C4" eg--undosify-map)
  (puthash 143 "\u00C5" eg--undosify-map)
  (puthash 144 "\u00C9" eg--undosify-map)
  (puthash 145 "\u00E6" eg--undosify-map)
  (puthash 146 "\u00C6" eg--undosify-map)
  (puthash 147 "\u00F4" eg--undosify-map)
  (puthash 148 "\u00F6" eg--undosify-map)
  (puthash 149 "\u00F2" eg--undosify-map)
  (puthash 150 "\u00FB" eg--undosify-map)
  (puthash 151 "\u00F9" eg--undosify-map)
  (puthash 152 "\u00FF" eg--undosify-map)
  (puthash 153 "\u00D6" eg--undosify-map)
  (puthash 154 "\u00DC" eg--undosify-map)
  (puthash 155 "\u00A2" eg--undosify-map)
  (puthash 156 "\u00A3" eg--undosify-map)
  (puthash 157 "\u00A5" eg--undosify-map)
  (puthash 158 "\u20A7" eg--undosify-map)
  (puthash 159 "\u0192" eg--undosify-map)
  (puthash 160 "\u00E1" eg--undosify-map)
  (puthash 161 "\u00ED" eg--undosify-map)
  (puthash 162 "\u00F3" eg--undosify-map)
  (puthash 163 "\u00FA" eg--undosify-map)
  (puthash 164 "\u00F1" eg--undosify-map)
  (puthash 165 "\u00D1" eg--undosify-map)
  (puthash 166 "\u00AA" eg--undosify-map)
  (puthash 167 "\u00BA" eg--undosify-map)
  (puthash 168 "\u00BF" eg--undosify-map)
  (puthash 169 "\u2319" eg--undosify-map)
  (puthash 170 "\u00AC" eg--undosify-map)
  (puthash 171 "\u00BD" eg--undosify-map)
  (puthash 172 "\u00BC" eg--undosify-map)
  (puthash 173 "\u00A1" eg--undosify-map)
  (puthash 174 "\u00AB" eg--undosify-map)
  (puthash 175 "\u00BB" eg--undosify-map)
  (puthash 176 "\u2591" eg--undosify-map)
  (puthash 177 "\u2592" eg--undosify-map)
  (puthash 178 "\u2593" eg--undosify-map)
  (puthash 179 "\u2502" eg--undosify-map)
  (puthash 180 "\u2524" eg--undosify-map)
  (puthash 181 "\u2561" eg--undosify-map)
  (puthash 182 "\u2562" eg--undosify-map)
  (puthash 183 "\u2556" eg--undosify-map)
  (puthash 184 "\u2555" eg--undosify-map)
  (puthash 185 "\u2563" eg--undosify-map)
  (puthash 186 "\u2551" eg--undosify-map)
  (puthash 187 "\u2557" eg--undosify-map)
  (puthash 188 "\u255D" eg--undosify-map)
  (puthash 189 "\u255C" eg--undosify-map)
  (puthash 190 "\u255B" eg--undosify-map)
  (puthash 191 "\u2510" eg--undosify-map)
  (puthash 192 "\u2514" eg--undosify-map)
  (puthash 193 "\u2534" eg--undosify-map)
  (puthash 194 "\u252C" eg--undosify-map)
  (puthash 195 "\u251C" eg--undosify-map)
  (puthash 196 "\u2500" eg--undosify-map)
  (puthash 197 "\u253C" eg--undosify-map)
  (puthash 198 "\u255E" eg--undosify-map)
  (puthash 199 "\u255F" eg--undosify-map)
  (puthash 200 "\u255A" eg--undosify-map)
  (puthash 201 "\u2554" eg--undosify-map)
  (puthash 202 "\u2596" eg--undosify-map)
  (puthash 203 "\u2566" eg--undosify-map)
  (puthash 204 "\u2560" eg--undosify-map)
  (puthash 205 "\u2550" eg--undosify-map)
  (puthash 206 "\u256C" eg--undosify-map)
  (puthash 207 "\u2567" eg--undosify-map)
  (puthash 208 "\u2568" eg--undosify-map)
  (puthash 209 "\u2564" eg--undosify-map)
  (puthash 210 "\u2565" eg--undosify-map)
  (puthash 211 "\u2559" eg--undosify-map)
  (puthash 212 "\u2558" eg--undosify-map)
  (puthash 213 "\u2552" eg--undosify-map)
  (puthash 214 "\u2553" eg--undosify-map)
  (puthash 215 "\u256B" eg--undosify-map)
  (puthash 216 "\u256A" eg--undosify-map)
  (puthash 217 "\u251B" eg--undosify-map)
  (puthash 218 "\u250C" eg--undosify-map)
  (puthash 219 "\u2588" eg--undosify-map)
  (puthash 220 "\u2584" eg--undosify-map)
  (puthash 221 "\u258C" eg--undosify-map)
  (puthash 222 "\u2590" eg--undosify-map)
  (puthash 223 "\u2580" eg--undosify-map)
  (puthash 224 "\u03B1" eg--undosify-map)
  (puthash 225 "\u00DF" eg--undosify-map)
  (puthash 226 "\u0393" eg--undosify-map)
  (puthash 227 "\u03C0" eg--undosify-map)
  (puthash 228 "\u03A3" eg--undosify-map)
  (puthash 229 "\u03C3" eg--undosify-map)
  (puthash 230 "\u00B5" eg--undosify-map)
  (puthash 231 "\u03C4" eg--undosify-map)
  (puthash 232 "\u03A6" eg--undosify-map)
  (puthash 233 "\u039B" eg--undosify-map)
  (puthash 234 "\u03A9" eg--undosify-map)
  (puthash 235 "\u03b4" eg--undosify-map)
  (puthash 236 "\u221E" eg--undosify-map)
  (puthash 237 "\u03C6" eg--undosify-map)
  (puthash 238 "\u03B5" eg--undosify-map)
  (puthash 239 "\u2229" eg--undosify-map)
  (puthash 240 "\u2261" eg--undosify-map)
  (puthash 241 "\u00B1" eg--undosify-map)
  (puthash 242 "\u2265" eg--undosify-map)
  (puthash 243 "\u2264" eg--undosify-map)
  (puthash 244 "\u2320" eg--undosify-map)
  (puthash 245 "\u2321" eg--undosify-map)
  (puthash 246 "\u00F7" eg--undosify-map)
  (puthash 248 "\u00B0" eg--undosify-map)
  (puthash 249 "\u2219" eg--undosify-map)
  (puthash 250 "\u00B7" eg--undosify-map)
  (puthash 251 "\u221A" eg--undosify-map)
  (puthash 252 "\u207F" eg--undosify-map)
  (puthash 253 "\u00B2" eg--undosify-map)
  (puthash 254 "\u25A0" eg--undosify-map)
  (puthash 255 "\u00A0" eg--undosify-map))

(defun eg--view-entry (&optional offset)
  "View the entry at OFFSET."
  (when offset
    (eg-goto eg--current-guide offset))
  (setq eg--current-entry
        (eg-load-entry eg--current-guide))
  (eg-view-current-entry))

(defun eg--insert-nav (button test pos help)
  "Insert a navigation button.

BUTTON is the text. TEST is the function used to test if we
should make the button a live link. POS is the function we should
call to find the position to jump to. HELP is the help text to
show for the link."
  (if (funcall test eg--current-entry)
      (insert-text-button button
                          'action (lambda (_)
                                    (eg--view-entry
                                     (funcall pos eg--current-entry)))
                          'help-echo help
                          'follow-link t)
    (insert button)))

(defun eg--insert-see-alsos (entry)
  "Insert any see-also links for ENTRY."
  (when (eg-entry-has-see-also entry)
    (save-excursion
      (setf (point) (point-max))
      (insert (make-string fill-column ?-) "\n")
      (insert "See also: ")
      (cl-loop for see in (eg-see-also-prompts (eg-entry-see-also entry))
               and see-link in (eg-see-also-offsets (eg-entry-see-also entry))
               do (insert-button see
                                 'action `(lambda (_)
                                            (eg--view-entry ,see-link))
                                 'help-echo (format "See also \"%s\"" see)
                                 'follow-link t)
               (insert " ")))))

(defun eg--add-top-nav ()
  "Add navigation links to the top of the buffer."
  (save-excursion
    (setf (point) (point-min))
    (eg--insert-nav "[<< Prev]" #'eg-entry-has-previous-p #'eg-entry-previous "Go to the previous entry")
    (insert " ")
    (eg--insert-nav "[^^ Up ^^]" #'eg-entry-has-parent-p #'eg-entry-parent "Go to the parent entry")
    (insert " ")
    (eg--insert-nav "[Next >>]" #'eg-entry-has-next-p #'eg-entry-next "Go to the next entry")
    (insert "\n\n")))

(defun eg--add-bottom-nav ()
  "Add navigation links to the bottom of the buffer."
  (save-excursion
    (setf (point) (point-max))
    (when (eg-entry-long-p eg--current-entry)
      (eg--insert-see-alsos eg--current-entry))))

(defun eg--decorate-buffer ()
  "Parse tokens, etc, to make the buffer more readable."
  (save-excursion
    (setf (point) (point-min))
    (while (search-forward "^" nil t)
      (let ((token (downcase (buffer-substring-no-properties (point) (1+ (point))))))
        (delete-char -1)
        (cond ((string= token "a")
               (delete-char 3))
              ((string= token "b")
               (delete-char 1))
              ((string= token "c")
               (let ((char (buffer-substring-no-properties (point) (+ (point) 2))))
                 (delete-char 3)
                 (insert (eg--undosify-string (make-string 1 (string-to-number char 16))))))
              ((string= token "n")
               (delete-char 1))
              ((string= token "r")
               (delete-char 1))
              ((string= token "u")
               (delete-char 1))
              ((string= token "^")
               ;; GNDN
               ))))))

(defun eg--undosify-string (s)
  "Try and turn S into something that will look pretty."
  (apply #'concat
         (cl-loop for c across s collect (gethash c eg--undosify-map (string c)))))

(defun eg--insert-entry-text ()
  "Insert the text of the current entry."
  (save-excursion
    (cl-loop for line in (eg-entry-lines eg--current-entry)
             do (insert (eg--undosify-string line) "\n"))))

(defun eg--linkify-entry-text ()
  "Add links to the current buffer text."
  (when (eg-entry-short-p eg--current-entry)
    (save-excursion
      (cl-loop for link in (eg-entry-offsets eg--current-entry)
               do (make-text-button
                   (point-at-bol)
                   (point-at-eol)
                   'action `(lambda (_) (eg--view-entry ,link))
                   'help-echo "View this entry"
                   'follow-link t)
               (forward-line)))))

(defun eg-view-current-entry ()
  "View the current entry."
  (let ((buffer-read-only nil))
    (setf (buffer-string) "")
    (eg--insert-entry-text)
    (eg--linkify-entry-text)
    (eg--decorate-buffer)
    (eg--add-top-nav)
    (eg--add-bottom-nav)))

(defun eg-view-menu ()
  "View the current guide's menu."
  (interactive)
  (let ((buffer-read-only nil))
    (setf (buffer-string) "")
    (save-excursion
      (cl-loop for menu in (eg-guide-menus eg--current-guide)
               do
               (insert (eg-menu-title menu) "\n")
               (cl-loop for prompt in (eg-menu-prompts menu)
                        and link in (eg-menu-offsets menu)
                        do (insert "\t")
                        (insert-text-button
                         prompt
                         'action `(lambda (_)
                                    (eg--view-entry ,link))
                         'help-echo (format "View the \"%s\" entry" prompt)
                         'follow-link t)
                        (insert "\n"))))))

(provide 'eg)

;;; eg.el ends here
