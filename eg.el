;;; eg.el --- Norton Guide reader for GNU Emacs
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; eg.el is free software distributed under the terms of the GNU General
;; Public Licence, version 2 or (at your option) any later version. For
;; details see the file COPYING.

;;; Commentary:
;;
;; eg.el provides code for reading the content of help databases built with
;; Norton Guide and Expert Help.
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
  title
  prompt-count
  prompts
  offsets)

(cl-defstruct eg-entry
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
  prompt-count
  prompts
  offsets)

(cl-defun eg-skip (guide &optional (bytes 1))
  "Skip BYTES bytes in GUIDE."
  (cl-incf (eg-guide-pos guide) bytes))

(defun eg-read (guide len)
  "Read bytes from GUIDE.

LEN is the number of bytes to read."
  (with-current-buffer (eg-guide-buffer guide)
    (let* ((from (+ (point-min) (eg-guide-pos guide)))
           (to   (+ from len)))
      (cl-incf (eg-guide-pos guide) len)
      (buffer-substring-no-properties from to))))

(defun eg-decrypt (n)
  "Decrypt value N."
  (logxor n 26))

(cl-defun eg-read-byte (guide &optional (decrypt t))
  "Read a byte from GUIDE.

If DECRYPT is non-nil, decrypt it."
  (let ((byte (string-to-char (eg-read guide 1))))
    (if decrypt
        (eg-decrypt byte)
      byte)))

(cl-defun eg-read-word (guide &optional (decrypt t))
  "Read a word from GUIDE."
  (let* ((lo (eg-read-byte guide decrypt))
         (hi (eg-read-byte guide decrypt)))
    (+ (lsh hi 8)) lo))

(cl-defun eg-read-long (guide &optional (decrypt t))
  "Read a long from GUIDE."
  (let* ((lo (eg-read-word guide decrypt))
         (hi (eg-read-word guide decrypt)))
    (+ (lsh hi 16) lo)))

(cl-defun eg-decrypt-string (s)
  "Decrypt string S."
  (mapconcat (lambda (c) (make-string 1 c)) (mapcar #'eg-decrypt s) ""))

(defun eg-rle-marker-p (c)
  "Does C look like an RLE marker?"
  (or
   (= c eg-rle-marker)
   (= c (decode-char 'eight-bit eg-rle-marker))))

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
                  else collect (make-string 1 c))))

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
  (let ((pos (eg-guide-pos guide)))
    (let ((s (eg-read-string guide len decrypt)))
      (setf (eg-guide-pos guide) (+ 1 pos (length s)))
      s)))

(defun eg-skip-entry (guide)
  "Skip an entry/menu in GUIDE."
  (eg-skip guide (+ 22 (eg-read-word guide))))

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
  (= (eg-entry-type entry) eg-entry-short))

(defun eg-entry-long-p (entry)
  "Is ENTRY a long entry?"
  (= (eg-entry-type entry) eg-entry-long))

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
    (setf (eg-entry-offset        entry) (eg-guide-pos guide))
    (setf (eg-entry-type          entry) (eg-read-word guide))
    (setf (eg-entry-size          entry) (eg-read-word guide))
    (setf (eg-entry-line-count    entry) (eg-read-word guide))
    (setf (eg-entry-has-see-also  entry) (eg-read-word guide))
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
    entry))

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

(defun eg-test ()
  "Testing helper."
  (let ((guide (eg-open "~/Google Drive/Norton Guides/acebase.ng")))
    (unwind-protect
        (list
         (eg-read-entry guide)
         (eg-read-entry guide)
         (eg-read-entry guide))
      (eg-close guide))))

(defun eg-dump-entry (entry)
  (list
   (cons 'offset (eg-entry-offset entry))
   (cons 'type (eg-entry-type entry))
   (cons 'size(eg-entry-size entry))
   (cons 'count (eg-entry-line-count entry))
   (cons 'see-also (eg-entry-has-see-also entry))
   (cons 'parent-line (eg-entry-parent-line entry))
   (cons 'parent (eg-entry-parent entry))
   (cons 'parent-menu (eg-entry-parent-menu entry))
   (cons 'parent-prompt (eg-entry-parent-prompt entry))
   (cons 'previous (eg-entry-previous entry))
   (cons 'next (eg-entry-next entry))
   (cons 'offsets (eg-entry-offsets entry))
   (cons 'lines (eg-entry-lines entry))
   (cons 'see-also (eg-entry-see-also entry))))

(provide 'eg)

;;; eg.el ends here
