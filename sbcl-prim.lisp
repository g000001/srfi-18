(in-package :sb-thread)

(defstruct (srfi-19-thread
            (:include thread))
  specific
  function)

;;; borrowed from SBCL: target-thread.lisp
(defun srfi-19-thread-start! (thread
                              &aux arguments
                                   (function (srfi-19-thread-function thread)))
  #+sb-doc
  "Create a new thread of NAME that runs FUNCTION with the argument
list designator provided (defaults to no argument). When the function
returns the thread exits. The return values of FUNCTION are kept
around and can be retrieved by JOIN-THREAD."
  #-sb-thread (declare (ignore function name arguments))
  #-sb-thread (error "Not supported in unithread builds.")
  #+sb-thread (assert (or (atom arguments)
                           (null (cdr (last arguments))))
                       (arguments)
                       "Argument passed to ~S, ~S, is an improper list."
                       'make-thread arguments)
  #+sb-thread
  (let* ((setup-sem (make-semaphore :name "Thread setup semaphore"))
         (real-function (coerce function 'function))
         (arguments     (if (listp arguments)
                            arguments
                            (list arguments)))
         (initial-function
          (named-lambda initial-thread-function ()
            ;; In time we'll move some of the binding presently done in C
            ;; here too.
            ;;
            ;; KLUDGE: Here we have a magic list of variables that are
            ;; not thread-safe for one reason or another.  As people
            ;; report problems with the thread safety of certain
            ;; variables, (e.g. "*print-case* in multiple threads
            ;; broken", sbcl-devel 2006-07-14), we add a few more
            ;; bindings here.  The Right Thing is probably some variant
            ;; of Allegro's *cl-default-special-bindings*, as that is at
            ;; least accessible to users to secure their own libraries.
            ;;   --njf, 2006-07-15
            ;;
            ;; As it is, this lambda must not cons until we are ready
            ;; to run GC. Be very careful.
            (let* ((*current-thread* thread)
                   (*restart-clusters* nil)
                   (*handler-clusters* (sb-kernel::initial-handler-clusters))
                   (*condition-restarts* nil)
                   (sb-impl::*deadline* nil)
                   (sb-impl::*deadline-seconds* nil)
                   (sb-impl::*step-out* nil)
                   ;; internal printer variables
                   (sb-impl::*previous-case* nil)
                   (sb-impl::*previous-readtable-case* nil)
                   (sb-impl::*internal-symbol-output-fun* nil)
                   (sb-impl::*descriptor-handlers* nil)) ; serve-event
              ;; Binding from C
              (setf sb-vm:*alloc-signal* *default-alloc-signal*)
              (setf (thread-os-thread thread) (current-thread-os-thread))
              (with-mutex ((thread-result-lock thread))
                (with-all-threads-lock
                  (push thread *all-threads*))
                (with-session-lock (*session*)
                  (push thread (session-threads *session*)))
                (setf (thread-%alive-p thread) t)
                (signal-semaphore setup-sem)
                ;; can't use handling-end-of-the-world, because that flushes
                ;; output streams, and we don't necessarily have any (or we
                ;; could be sharing them)
                (catch 'sb-impl::toplevel-catcher
                  (catch 'sb-impl::%end-of-the-world
                    (with-simple-restart
                        (terminate-thread
                         (format nil
                                 "~~@<Terminate this thread (~A)~~@:>"
                                 *current-thread*))
                      (without-interrupts
                        (unwind-protect
                             (with-local-interrupts
                               ;; Now that most things have a chance
                               ;; to work properly without messing up
                               ;; other threads, it's time to enable
                               ;; signals.
                               (sb-unix::unblock-deferrable-signals)
                               (setf (thread-result thread)
                                     (cons t
                                           (multiple-value-list
                                            (apply real-function arguments))))
                               ;; Try to block deferrables. An
                               ;; interrupt may unwind it, but for a
                               ;; normal exit it prevents interrupt
                               ;; loss.
                               (block-deferrable-signals))
                          ;; We're going down, can't handle interrupts
                          ;; sanely anymore. GC remains enabled.
                          (block-deferrable-signals)
                          ;; We don't want to run interrupts in a dead
                          ;; thread when we leave WITHOUT-INTERRUPTS.
                          ;; This potentially causes important
                          ;; interupts to be lost: SIGINT comes to
                          ;; mind.
                          (setq *interrupt-pending* nil)
                          (handle-thread-exit thread))))))))
            (values))))
    ;; If the starting thread is stopped for gc before it signals the
    ;; semaphore then we'd be stuck.
    (assert (not *gc-inhibit*))
    ;; Keep INITIAL-FUNCTION pinned until the child thread is
    ;; initialized properly. Wrap the whole thing in
    ;; WITHOUT-INTERRUPTS because we pass INITIAL-FUNCTION to another
    ;; thread.
    (without-interrupts
      (with-pinned-objects (initial-function)
        (let ((os-thread
               (%create-thread
                (get-lisp-obj-address initial-function))))
          (when (zerop os-thread)
            (error "Can't create a new thread"))
          (wait-on-semaphore setup-sem)
          thread)))))
