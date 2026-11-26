;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :terminal)

(defvar *terminal-library-file* nil
  "Path this file was loaded from.")

(defvar *terminal-set-nonblock* nil
  "Foreign pointer to nyxt_set_nonblock.")

(defvar *terminal-set-winsize* nil
  "Foreign pointer to nyxt_set_winsize.")

(let ((file (or *load-truename* *compile-file-truename*)))
  (when file
    (setf *terminal-library-file* file)))

(defconstant +transcript-limit+ 262144)
(defconstant +tiocsctty+ #x540E)
(defconstant +tiocgpgrp+ #x540F)
(defconstant +tiocswinsz+ #x5414)
(defconstant +pollin+ #x001)
(defconstant +pollout+ #x004)
(defconstant +pollerr+ #x008)
(defconstant +pollhup+ #x010)
(defconstant +eintr+ 4)
#+linux
(defconstant +eagain+ 11)
#+darwin
(defconstant +posix-spawn-setsigdef+ #x0004)
#+darwin
(defconstant +posix-spawn-setsigmask+ #x0008)
#+darwin
(defconstant +posix-spawn-cloexec-default+ #x4000)

(cffi:defcstruct pollfd
  (fd :int)
  (events :short)
  (revents :short))

(cffi:defcstruct winsize
  (row :unsigned-short)
  (col :unsigned-short)
  (xpixel :unsigned-short)
  (ypixel :unsigned-short))

(-> errno () (values integer &optional))
(defun errno ()
  "Return the current thread's errno."
  (cffi:mem-ref (cffi:foreign-funcall
                 #+linux "__errno_location"
                 #+darwin "__error"
                 :pointer)
                :int))

(-> close-fd (integer) (values integer &optional))
(defun close-fd (fd)
  "Close FD when it is open. Return -1."
  (when (>= fd 0)
    (cffi:foreign-funcall "close" :int fd :int))
  -1)

(defun set-nonblock (fd)
  "Make FD non-blocking."
  #+darwin
  (progn
    (ensure-terminal-dylib)
    (cffi:foreign-funcall-pointer *terminal-set-nonblock* () :int fd :int))
  #-darwin
  (let ((flags (cffi:foreign-funcall "fcntl" :int fd :int 3 :int)))
    (when (>= flags 0)
      (cffi:foreign-funcall "fcntl" :int fd :int 4
                            :int (logior flags #o4000)
                            :int))))

(defun open-pipe ()
  "Return the read and write ends of a non-blocking pipe."
  (cffi:with-foreign-object (fds :int 2)
    (unless (zerop (cffi:foreign-funcall "pipe" :pointer fds :int))
      (error "Could not open a terminal pipe (errno ~a)." (errno)))
    (let ((read-end (cffi:mem-aref fds :int 0))
          (write-end (cffi:mem-aref fds :int 1)))
      (set-nonblock read-end)
      (set-nonblock write-end)
      (values read-end write-end))))

(defun open-fd-limit ()
  "Return the highest open file descriptor, or 256 when it cannot be listed."
  (handler-case
      (let ((numbers (remove nil
                             (mapcar (lambda (path)
                                       (let ((name (file-namestring path)))
                                         (when (and (plusp (length name))
                                                    (every #'digit-char-p name))
                                           (parse-integer name))))
                                     (uiop:directory-files #p"/proc/self/fd/")))))
        (if numbers (reduce #'max numbers) 256))
    (error () 256)))

(defun environment ()
  "Return the process environment with a color terminal type."
  (flet ((prefixed-p (entry prefix)
           (and (>= (length entry) (length prefix))
                (string= entry prefix :end1 (length prefix)))))
    (append (remove-if (lambda (entry)
                         (or (prefixed-p entry "TERM=")
                             (prefixed-p entry "LINES=")
                             (prefixed-p entry "COLUMNS=")))
                       #+sbcl (sb-ext:posix-environ)
                       #-sbcl nil)
            (list "TERM=xterm-256color"))))

(defun foreign-string-array (strings)
  "Allocate a NULL-terminated C string vector for STRINGS."
  (let* ((count (length strings))
         (array (cffi:foreign-alloc :pointer :count (1+ count))))
    (loop for string in strings
          for index from 0
          do (setf (cffi:mem-aref array :pointer index)
                   (cffi:foreign-string-alloc string :encoding :utf-8)))
    (setf (cffi:mem-aref array :pointer count) (cffi:null-pointer))
    array))

(defun free-foreign-string-array (array)
  "Free a vector from `foreign-string-array'."
  (when array
    (loop for index from 0
          for pointer = (cffi:mem-aref array :pointer index)
          until (cffi:null-pointer-p pointer)
          do (cffi:foreign-string-free pointer))
    (cffi:foreign-free array)))

(defun exec-in-child (slave master program argv envp signals limit)
  "Make SLAVE the controlling terminal and replace this process with PROGRAM.
This runs in the forked child. It does not return."
  (declare (ignore master))
  (unwind-protect
       (progn
         (cffi:foreign-funcall "setsid" :int)
         (cffi:foreign-funcall "sigprocmask" :int 2
                               :pointer signals
                               :pointer (cffi:null-pointer)
                               :int)
         (cffi:foreign-funcall "ioctl" :int slave :unsigned-long +tiocsctty+ :int 0 :int)
         (cffi:foreign-funcall "dup2" :int slave :int 0 :int)
         (cffi:foreign-funcall "dup2" :int slave :int 1 :int)
         (cffi:foreign-funcall "dup2" :int slave :int 2 :int)
         (do ((fd 3 (1+ fd)))
             ((> fd limit))
           (cffi:foreign-funcall "close" :int fd :int))
         (cffi:foreign-funcall "execve" :pointer program :pointer argv :pointer envp :int))
    (cffi:foreign-funcall "_exit" :int 127 :void)))

(defun set-winsize (fd rows columns)
  "Set the window size of the terminal on FD."
  #+darwin
  (progn
    (ensure-terminal-dylib)
    (zerop (cffi:foreign-funcall-pointer *terminal-set-winsize* ()
                                         :int fd
                                         :unsigned-short rows
                                         :unsigned-short columns
                                         :int)))
  #-darwin
  (cffi:with-foreign-object (size '(:struct winsize))
    (setf (cffi:foreign-slot-value size '(:struct winsize) 'row) rows
          (cffi:foreign-slot-value size '(:struct winsize) 'col) columns
          (cffi:foreign-slot-value size '(:struct winsize) 'xpixel) 0
          (cffi:foreign-slot-value size '(:struct winsize) 'ypixel) 0)
    (zerop (cffi:foreign-funcall "ioctl" :int fd :unsigned-long +tiocswinsz+
                                 :pointer size :int))))

(defun open-pty (&key (rows 24) (columns 80))
  "Return the master and slave descriptors of a new pseudoterminal."
  (cffi:with-foreign-objects ((master :int)
                              (slave :int)
                              (size '(:struct winsize)))
    (setf (cffi:foreign-slot-value size '(:struct winsize) 'row) rows
          (cffi:foreign-slot-value size '(:struct winsize) 'col) columns
          (cffi:foreign-slot-value size '(:struct winsize) 'xpixel) 0
          (cffi:foreign-slot-value size '(:struct winsize) 'ypixel) 0)
    (unless (zerop (cffi:foreign-funcall "openpty"
                                         :pointer master
                                         :pointer slave
                                         :pointer (cffi:null-pointer)
                                         :pointer (cffi:null-pointer)
                                         :pointer size
                                         :int))
      (error "Could not open a pseudoterminal (errno ~a)." (errno)))
    (values (cffi:mem-ref master :int) (cffi:mem-ref slave :int))))

(define-class pty ()
  ((master -1 :type integer)
   (pid -1 :type integer)
   (wake-read -1 :type integer)
   (wake-write -1 :type integer)
   (thread nil)
   (lock (bt:make-lock "terminal"))
   (pending '() :type list)
   (chunks '() :type list)
   (chunk-bytes 0 :type integer)
   (attached-p nil :type boolean)
   (alive-p t :type boolean)
   (on-output nil :type (or function null))
   (on-exit nil :type (or function null))
   (rows 24 :type integer)
   (columns 80 :type integer))
  (:export-class-name-p t)
  (:predicate-name-transformer #'nclasses:always-dashed-predicate-name-transformer)
  (:documentation "A pseudoterminal and the program running on it."))

(defun process-name (pid)
  "Return the name PID was invoked as."
  (let ((command (format nil "/proc/~d/cmdline" pid))
        (comm (format nil "/proc/~d/comm" pid)))
    (or (and (uiop:file-exists-p command)
             (let* ((text (uiop:read-file-string command))
                    (end (position #\Nul text)))
               (when (and end (plusp end))
                 (file-namestring (subseq text 0 end)))))
        (and (uiop:file-exists-p comm)
             (string-trim '(#\newline #\return)
                          (uiop:read-file-string comm))))))

(export-always 'foreground-name)
(-> foreground-name (pty) (values (or null string) &optional))
(defun foreground-name (pty)
  "Return the name of the program in the foreground of PTY."
  #+linux
  (cffi:with-foreign-object (group :int)
    (when (and (plusp (master pty))
               (zerop (cffi:foreign-funcall "ioctl"
                                            :int (master pty)
                                            :unsigned-long +tiocgpgrp+
                                            :pointer group
                                            :int)))
      (let ((pid (cffi:mem-ref group :int)))
        (when (plusp pid)
          (process-name pid)))))
  #-linux
  (declare (ignore pty))
  #-linux
  nil)

(defun transcript-octets (chunks)
  "Concatenate CHUNKS into one octet vector."
  (let* ((size (reduce #'+ chunks :key #'length :initial-value 0))
         (octets (make-array size :element-type '(unsigned-byte 8)))
         (offset 0))
    (dolist (chunk chunks)
      (replace octets chunk :start1 offset)
      (incf offset (length chunk)))
    octets))

(defun remember-output (pty octets)
  "Store OCTETS and return whether the caller should receive them now."
  (bt:with-lock-held ((lock pty))
    (setf (chunks pty) (nconc (chunks pty) (list octets)))
    (incf (chunk-bytes pty) (length octets))
    (loop while (and (chunks pty)
                     (> (chunk-bytes pty) +transcript-limit+))
          for oldest = (pop (chunks pty))
          do (decf (chunk-bytes pty) (length oldest)))
    (attached-p pty)))

(defun note-output (pty octets)
  "Record OCTETS and deliver them when PTY is attached."
  (when (and (remember-output pty octets) (on-output pty))
    (funcall (on-output pty) octets)))

(export-always 'take-transcript)
(-> take-transcript (pty) (values (array (unsigned-byte 8)) &optional))
(defun take-transcript (pty)
  "Mark PTY attached and return the output buffered so far."
  (bt:with-lock-held ((lock pty))
    (setf (attached-p pty) nil)
    (prog1 (transcript-octets (chunks pty))
      (setf (attached-p pty) t))))

(export-always 'detach)
(-> detach (pty) (values null &optional))
(defun detach (pty)
  "Buffer PTY's output until `take-transcript'."
  (bt:with-lock-held ((lock pty))
    (setf (attached-p pty) nil)))

(export-always 'text)
(-> text (pty) (values string &optional))
(defun text (pty)
  "Return the buffered output of PTY as a string."
  (let ((octets (bt:with-lock-held ((lock pty))
                  (transcript-octets (copy-list (chunks pty))))))
    (if (plusp (length octets))
        (babel:octets-to-string octets :encoding :utf-8 :errorp nil)
        "")))

(defun again-errno-p (err)
  "Return whether ERR means the caller should wait and retry."
  (or (= err +eintr+)
      (= err #+linux +eagain+ #+darwin 35)))

(defun write-fd (fd octets)
  "Write OCTETS to FD. Return how many bytes were written, or NIL on error.
A short count means the descriptor would block."
  (cffi:with-pointer-to-vector-data (pointer octets)
    (loop with total = (length octets)
          with offset = 0
          while (< offset total)
          for wrote = (cffi:foreign-funcall
                       "write" :int fd
                       :pointer (cffi:inc-pointer pointer offset)
                       :unsigned-long (- total offset)
                       :long)
          do (cond
               ((plusp wrote) (incf offset wrote))
               (t (let ((err (errno)))
                    (cond
                      ((= err +eintr+) nil)
                      ((again-errno-p err) (return offset))
                      (t (return nil))))))
          finally (return offset))))

(export-always 'enqueue-input)
(-> enqueue-input (pty (array (unsigned-byte 8))) (values t &optional))
(defun enqueue-input (pty octets)
  "Queue OCTETS to be written to PTY."
  (when (plusp (length octets))
    (bt:with-lock-held ((lock pty))
      (setf (pending pty) (append (pending pty) (list octets))))
    (wake pty)))

(defun wake (pty)
  "Interrupt PTY's I/O thread."
  (when (>= (wake-write pty) 0)
    (cffi:with-foreign-object (byte :unsigned-char)
      (setf (cffi:mem-ref byte :unsigned-char) 1)
      (cffi:foreign-funcall "write" :int (wake-write pty)
                            :pointer byte :unsigned-long 1 :long))))

(defun flush-input (pty)
  "Write queued input until it would block."
  (loop while (alive-p pty)
        for chunk = (bt:with-lock-held ((lock pty))
                      (pop (pending pty)))
        unless chunk
          return t
        do (let ((wrote (write-fd (master pty) chunk)))
             (cond
               ((null wrote) (return nil))
               ((< wrote (length chunk))
                (bt:with-lock-held ((lock pty))
                  (push (subseq chunk wrote) (pending pty)))
                (return t))))))

(defun read-master (pty scratch)
  "Read from PTY's master into a fresh vector, or :EOF."
  (let ((count (cffi:foreign-funcall "read" :int (master pty)
                                     :pointer scratch :unsigned-long 8192 :long)))
    (cond
      ((plusp count)
       (let ((octets (make-array count :element-type '(unsigned-byte 8))))
         (dotimes (index count)
           (setf (aref octets index) (cffi:mem-aref scratch :unsigned-char index)))
         octets))
      ((zerop count) :eof)
      (t (if (again-errno-p (errno))
             nil
             :eof)))))

(defun read-available (pty scratch)
  "Deliver everything currently readable on PTY. Return :EOF when it closes."
  (loop for chunk = (read-master pty scratch)
        while (vectorp chunk)
        do (note-output pty chunk)
        finally (return chunk)))

(defun drain-wake (fd)
  "Discard wake-up bytes."
  (cffi:with-foreign-object (scratch :unsigned-char 64)
    (loop for count = (cffi:foreign-funcall "read" :int fd
                                             :pointer scratch :unsigned-long 64 :long)
          while (plusp count))))

(defun set-pollfd (pfds index fd events)
  (let ((entry (cffi:mem-aptr pfds '(:struct pollfd) index)))
    (setf (cffi:foreign-slot-value entry '(:struct pollfd) 'fd) fd
          (cffi:foreign-slot-value entry '(:struct pollfd) 'events) events
          (cffi:foreign-slot-value entry '(:struct pollfd) 'revents) 0)))

(defun poll-revents (pfds index)
  (cffi:foreign-slot-value (cffi:mem-aptr pfds '(:struct pollfd) index)
                           '(:struct pollfd)
                           'revents))

(defun pty-exited (pty)
  "Stop PTY because the program closed its end."
  (let ((notify nil))
    (bt:with-lock-held ((lock pty))
      (when (alive-p pty)
        (setf (alive-p pty) nil
              notify t)))
    (when (and notify (on-exit pty))
      (funcall (on-exit pty)))))

(defun pty-loop (pty)
  "Read program output and write queued input until PTY stops."
  (cffi:with-foreign-objects ((pfds '(:struct pollfd) 2)
                              (scratch :unsigned-char 8192))
    (loop while (alive-p pty)
          for events = (if (bt:with-lock-held ((lock pty)) (pending pty))
                           (logior +pollin+ +pollout+)
                           +pollin+)
          do (set-pollfd pfds 0 (master pty) events)
             (set-pollfd pfds 1 (wake-read pty) +pollin+)
             (when (plusp (cffi:foreign-funcall "poll" :pointer pfds :int 2 :int 500 :int))
               (when (logtest (poll-revents pfds 1) +pollin+)
                 (drain-wake (wake-read pty)))
               (when (or (logtest (poll-revents pfds 0) +pollout+)
                         (logtest (poll-revents pfds 1) +pollin+))
                 (unless (flush-input pty)
                   (pty-exited pty)))
               (when (and (alive-p pty)
                          (logtest (poll-revents pfds 0)
                                   (logior +pollin+ +pollhup+ +pollerr+)))
                 (when (eq :eof (read-available pty scratch))
                   (pty-exited pty)))))))

#+darwin
(cffi:defcfun ("posix_spawn" %posix-spawn) :int
  (pid (:pointer :int))
  (path :string)
  (file-actions :pointer)
  (attr :pointer)
  (argv :pointer)
  (envp :pointer))

#+darwin
(cffi:defcfun ("posix_spawn_file_actions_init" %spawn-actions-init) :int
  (actions :pointer))

#+darwin
(cffi:defcfun ("posix_spawn_file_actions_destroy" %spawn-actions-destroy) :int
  (actions :pointer))

#+darwin
(cffi:defcfun ("posix_spawn_file_actions_adddup2" %spawn-adddup2) :int
  (actions :pointer)
  (fd :int)
  (newfd :int))

#+darwin
(cffi:defcfun ("posix_spawnattr_init" %spawnattr-init) :int
  (attr :pointer))

#+darwin
(cffi:defcfun ("posix_spawnattr_destroy" %spawnattr-destroy) :int
  (attr :pointer))

#+darwin
(cffi:defcfun ("posix_spawnattr_setflags" %spawnattr-setflags) :int
  (attr :pointer)
  (flags :short))

#+darwin
(cffi:defcfun ("posix_spawnattr_setsigmask" %spawnattr-setsigmask) :int
  (attr :pointer)
  (mask :pointer))

#+darwin
(cffi:defcfun ("posix_spawnattr_setsigdefault" %spawnattr-setsigdefault) :int
  (attr :pointer)
  (mask :pointer))

#+darwin
(cffi:defcfun ("sigemptyset" %sigemptyset) :int
  (set :pointer))

#+darwin
(cffi:defcfun ("sigfillset" %sigfillset) :int
  (set :pointer))

#+darwin
(defun library-sibling (name type)
  "Return NAME.TYPE beside the loaded library file."
  (when *terminal-library-file*
    (probe-file (make-pathname :name name
                               :type type
                               :defaults *terminal-library-file*))))

#+darwin
(defun login-exec-source ()
  "Return the login-exec source, or NIL."
  (or (library-sibling "login-exec" "c")
      (ignore-errors
       (probe-file (asdf:system-relative-pathname :nyxt/terminal "login-exec.c")))))

#+darwin
(defun bundled-login-exec ()
  "Return a nyxt-login-exec beside this executable, or NIL."
  (let ((argv0 (uiop:argv0)))
    (when (and (stringp argv0) (plusp (length argv0)))
      (probe-file
       (uiop:merge-pathnames*
        "nyxt-login-exec"
        (uiop:pathname-directory-pathname
         (uiop:ensure-pathname argv0 :namestring :native)))))))

#+darwin
(defun login-exec-destination ()
  "Return the cached nyxt-login-exec path."
  (uiop:merge-pathnames*
   "nyxt/nyxt-login-exec"
   (uiop:ensure-directory-pathname (uiop:xdg-cache-home))))

#+darwin
(defun fresh-login-exec-p (program source)
  "Return whether PROGRAM and its library are at least as new as SOURCE."
  (let ((library (helper-dylib program)))
    (and (probe-file program)
         (probe-file library)
         (or (null source)
             (and (>= (file-write-date program) (file-write-date source))
                  (>= (file-write-date library) (file-write-date source)))))))

#+darwin
(defun compile-c (source output &rest flags)
  "Compile SOURCE to OUTPUT. Return OUTPUT."
  (ensure-directories-exist output)
  (let ((temporary (make-pathname :name (format nil "~a-~a"
                                                (pathname-name output)
                                                (random 1000000))
                                  :type (pathname-type output)
                                  :defaults output)))
    (unwind-protect
         (multiple-value-bind (stdout stderr exit-code)
             (uiop:run-program
              (append (list "/usr/bin/cc" "-O2")
                      flags
                      (list "-o" (uiop:native-namestring temporary)
                            (uiop:native-namestring source)))
              :output :string
              :error-output :string
              :ignore-error-status t
              :force-shell nil)
           (declare (ignore stdout))
           (unless (and exit-code (zerop exit-code) (probe-file temporary))
             (error "Could not build the terminal program: ~a" stderr))
           (uiop:rename-file-overwriting-target temporary output))
      (uiop:delete-file-if-exists temporary))
    output))

#+darwin
(defun helper-dylib (program)
  "Return the library that belongs beside PROGRAM."
  (make-pathname :name "libnyxt-terminal"
                 :type "dylib"
                 :defaults program))

#+darwin
(defun build-login-exec (source destination)
  "Compile SOURCE to DESTINATION and its library. Return DESTINATION."
  (compile-c source destination)
  (compile-c source (helper-dylib destination) "-dynamiclib")
  destination)

#+darwin
(defun login-exec-program ()
  "Return the program that makes a spawned shell own its terminal."
  (let ((source (login-exec-source)))
    (uiop:native-namestring
     (or (bundled-login-exec)
         (let ((cached (login-exec-destination)))
           (if (fresh-login-exec-p cached source)
               cached
               (if source
                   (build-login-exec source cached)
                   (error "Could not find the terminal program."))))))))

#+darwin
(defun ensure-terminal-dylib ()
  "Load the library that sets the terminal size and non-blocking mode."
  (unless (and *terminal-set-nonblock* *terminal-set-winsize*)
    (cffi:load-foreign-library (helper-dylib (login-exec-program)))
    (setf *terminal-set-nonblock* (cffi:foreign-symbol-pointer "nyxt_set_nonblock")
          *terminal-set-winsize* (cffi:foreign-symbol-pointer "nyxt_set_winsize"))
    (unless (and *terminal-set-nonblock* *terminal-set-winsize*)
      (error "Could not load the terminal library."))))

#+darwin
(defun spawn-status (step status)
  "Signal when STATUS is a non-zero errno from STEP."
  (unless (zerop status)
    (error "Could not start a terminal (~a, errno ~a)." step status)))

#+darwin
(defun spawn (slave master argv)
  "Run ARGV with SLAVE as its controlling terminal. Return the child pid."
  (declare (ignore master))
  (let ((helper (login-exec-program))
        (argv-pointer nil)
        (envp nil)
        (actions-ready nil)
        (attr-ready nil)
        (pid -1))
    (unwind-protect
         (cffi:with-foreign-objects ((actions :pointer)
                                     (attr :pointer)
                                     (pid-slot :int)
                                     (blocked :unsigned-char 8)
                                     (defaults :unsigned-char 8))
           (setf argv-pointer (foreign-string-array (cons helper argv))
                 envp (foreign-string-array (environment)))
           (unwind-protect
                (progn
                  (spawn-status "file actions" (%spawn-actions-init actions))
                  (setf actions-ready t)
                  (spawn-status "attributes" (%spawnattr-init attr))
                  (setf attr-ready t)
                  (spawn-status "stdin" (%spawn-adddup2 actions slave 0))
                  (spawn-status "stdout" (%spawn-adddup2 actions slave 1))
                  (spawn-status "stderr" (%spawn-adddup2 actions slave 2))
                  (spawn-status "signals" (%sigemptyset blocked))
                  (spawn-status "signals" (%sigfillset defaults))
                  (spawn-status "flags"
                                (%spawnattr-setflags
                                 attr
                                 (logior +posix-spawn-setsigdef+
                                         +posix-spawn-setsigmask+
                                         +posix-spawn-cloexec-default+)))
                  (spawn-status "mask" (%spawnattr-setsigmask attr blocked))
                  (spawn-status "defaults" (%spawnattr-setsigdefault attr defaults))
                  (spawn-status "spawn"
                                (%posix-spawn pid-slot helper actions attr
                                              argv-pointer envp))
                  (setf pid (cffi:mem-ref pid-slot :int)))
             (when actions-ready
               (%spawn-actions-destroy actions))
             (when attr-ready
               (%spawnattr-destroy attr))))
      (when argv-pointer
        (free-foreign-string-array argv-pointer))
      (when envp
        (free-foreign-string-array envp)))
    pid))

#-darwin
(defun spawn (slave master argv)
  "Fork and exec ARGV with SLAVE as its terminal. Return the child pid."
  (let ((program nil)
        (argv-pointer nil)
        (envp nil)
        (signals nil)
        (pid -1))
    (unwind-protect
         (progn
           (setf program (cffi:foreign-string-alloc (first argv) :encoding :utf-8)
                 argv-pointer (foreign-string-array argv)
                 envp (foreign-string-array (environment))
                 signals (cffi:foreign-alloc :unsigned-char :count 128))
           (dotimes (index 128)
             (setf (cffi:mem-aref signals :unsigned-char index) 0))
           (let ((limit (open-fd-limit)))
             (setf pid (cffi:foreign-funcall "fork" :int))
             (cond
               ((zerop pid)
                (exec-in-child slave master program argv-pointer envp signals limit))
               ((minusp pid)
                (error "Could not start a terminal (errno ~a)." (errno))))))
      (when program (cffi:foreign-string-free program))
      (when argv-pointer (free-foreign-string-array argv-pointer))
      (when envp (free-foreign-string-array envp))
      (when signals (cffi:foreign-free signals)))
    pid))

(export-always 'start)
(-> start (list
           &key (:on-output (or function null)) (:on-exit (or function null)))
    (values pty &optional))
(defun start (argv &key on-output on-exit)
  "Run ARGV on a new pseudoterminal and return it.
ON-OUTPUT is called with each chunk of output once the terminal is attached.
ON-EXIT is called when the program closes the terminal."
  (check-type argv (cons string))
  (multiple-value-bind (master slave) (open-pty)
    (set-nonblock master)
    (let ((pid (spawn slave master argv)))
      (close-fd slave)
      (multiple-value-bind (wake-read wake-write) (open-pipe)
        (let ((pty (make-instance 'pty
                                  :master master
                                  :pid pid
                                  :wake-read wake-read
                                  :wake-write wake-write
                                  :on-output on-output
                                  :on-exit on-exit)))
          (setf (thread pty)
                (bt:make-thread (lambda () (pty-loop pty)) :name "terminal"))
          pty)))))

(defun reap (pid)
  "Signal PID's process group and wait for PID to exit."
  (cffi:with-foreign-object (status :int)
    (cffi:foreign-funcall "kill" :int (- pid) :int 1 :int)
    (loop repeat 50
          for result = (cffi:foreign-funcall "waitpid" :int pid
                                              :pointer status :int 1 :int)
          when (plusp result)
            return nil
          do (sleep 0.01)
          finally (cffi:foreign-funcall "kill" :int (- pid) :int 9 :int)
                  (cffi:foreign-funcall "waitpid" :int pid
                                        :pointer status :int 0 :int))))

(export-always 'stop)
(-> stop ((or pty null)) (values t &optional))
(defun stop (pty)
  "Close PTY and reap its program."
  (when pty
    (let ((thread nil)
          (pid -1))
      (bt:with-lock-held ((lock pty))
        (setf (alive-p pty) nil
              thread (thread pty)
              pid (pid pty))
        (setf (thread pty) nil
              (pid pty) -1))
      (wake pty)
      (setf (master pty) (close-fd (master pty)))
      (when (and thread
                 (bt:thread-alive-p thread)
                 (not (eq thread (bt:current-thread))))
        (bt:join-thread thread))
      (when (plusp pid)
        (reap pid))
      (setf (wake-read pty) (close-fd (wake-read pty))
            (wake-write pty) (close-fd (wake-write pty))))))

(export-always 'resize)
(-> resize (pty integer integer) (values boolean &optional))
(defun resize (pty rows columns)
  "Tell PTY's program that the window is ROWS by COLUMNS."
  (setf (rows pty) rows
        (columns pty) columns)
  (set-winsize (master pty) rows columns))
