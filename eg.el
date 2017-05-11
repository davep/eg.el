(require 'cl-lib)

(defun eg-debug (&rest args)
  (apply #'message args))

(defconst eg-magic-ng "NG"
  "Magic marker for a guide built with Norton Guide.")

(defconst eg-magic-eh "EH"
  "Magic marker for a guide built with Expert Help.")

(defconst eg-title-length 40
  "Maximum length of a guide title")

(defconst eg-credit-length 66
  "Maximum length of a line in a guide's credits.")

(defconst eg-prompt-length 128
  "Maximum length of a prompt on a menu.")

(defconst eg-entry-short 0
  "Id of a short entry in a guide.")

(defconst eg-entry-long 1
  "Id of a long entry in a guide.")

(defconst eg-entry-menu 2
  "Id of a menu in a guide.")

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

(cl-defun eg-skip (guide &optional (bytes 1))
  "Skip BYTES bytes in GUIDE."
  (cl-incf (eg-guide-pos guide) bytes))

(defun eg-read (guide len)
  "Read LEN bytes from GUIDE."
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
  (let ((byte (aref (eg-read guide 1) 0)))
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
                   collect (eg-read-string-z guide eg-prompt-length)))
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
                    (setq buffer-file-coding-system 'binary)
                    (set-buffer-file-coding-system nil)
                    (insert-file-contents-literally file)
                    (make-eg-guide :file file :buffer (current-buffer))))))
      (when (eg-guide-has-menus-p guide)
        (eg-read-menus guide))
      (setf (eg-guide-first-entry-pos guide) (eg-guide-pos guide))
      guide)))

(defun eg-close (guide)
  "Close GUIDE."
  (kill-buffer (eg-guide-buffer guide)))

(defun eg-test ()
  (let ((guide (eg-open "~/Google Drive/Norton Guides/acebase.ng")))
    (eg-close guide)
    (eg-guide-menus guide)))
