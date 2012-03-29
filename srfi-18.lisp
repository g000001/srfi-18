;;;; srfi-18.lisp

(cl:in-package :srfi-18.internal)

(def-suite srfi-18)

(in-suite srfi-18)

(defun not-implemented ()
  (error "not-implemented"))

;; current-thread
(defun current-thread ()
  #+sbcl ;; (gethash sb-thread:*current-thread* *threads*)
  sb-thread:*current-thread*
  #-(or sbcl) (not-implemented))

;; thread?
(defun thread? (obj)
  #+sbcl (sb-thread::thread-p obj)
  #-(or sbcl) (not-implemented))

;; make-thread
(defun make-thread (thunk &optional name)
  ;; (*make-thread thunk name)
  (sb-thread::make-srfi-19-thread :name name :function thunk))

;; thread-name
(defun thread-name (thread)
  (sb-thread:thread-name thread))

;; thread-specific
(defun thread-specific (thread)
  (and (sb-thread::srfi-19-thread-p thread)
       (sb-thread::srfi-19-thread-specific thread)))

;; thread-specific-set!
(defun thread-specific-set! (thread obj)
  (and (sb-thread::srfi-19-thread-p thread)
       (setf (sb-thread::srfi-19-thread-specific thread)
             obj)))

;; thread-start!
(defun thread-start! (thread)
  (and (sb-thread::srfi-19-thread-p thread)
       (sb-thread::srfi-19-thread-start! thread)))

;; thread-yield!
(defun thread-yield! ()
  (sb-thread:thread-yield))

;; thread-sleep!
(defun thread-sleep! (timeout)
  (sb-thread:interrupt-thread sb-thread:*current-thread*
                              (lambda () (sleep timeout))))

;; thread-terminate!
(defun thread-terminate! (thread)
  (sb-thread:terminate-thread thread))

;; thread-join!
(defun thread-join! (thread &optional timeout timeout-val)
  (if timeout
      (handler-case (sb-ext:with-timeout timeout
                      (sb-thread:join-thread thread))
        (sb-ext:timeout (c)
          (declare (ignore c))
          (or timeout-val
              (error "join timeout exception"))))
      (sb-thread:join-thread thread)))


(defstruct (mutex
            (:include sb-thread:mutex)
            (:conc-name "SRFI-18-%MUTEX-")
            ;; make-mutex
            (:constructor make-mutex
                          (&optional nam
                                     &aux (name (and nam (string nam))))))
  specific)

;; mutex-specific
(declaim (inline mutex-specific))
(defun mutex-specific (mutex)
  (srfi-18-%mutex-specific mutex))

;; mutex-name
(declaim (inline mutex-name))
(defun mutex-name (mutex)
  (srfi-18-%mutex-name mutex))

;; mutex?
(defun mutex? (obj)
  (sb-thread::mutex-p obj))

;; mutex-specific-set!
(defun mutex-specific-set! (mutex value)
  (setf (srfi-18-%mutex-specific mutex) value))

;; mutex-state
;; thread T: the mutex is in the locked/owned state and thread T is
;; the owner of the mutex
;; symbol not-owned: the mutex is in the locked/not-owned state
;; (not-implemented) symbol abandoned: the mutex is in the unlocked/abandoned state
;; (not-implemented) symbol not-abandoned: the mutex is in the unlocked/not-abandoned state

;;; FIXME
(defun mutex-state (mutex)
  (or (sb-thread:mutex-owner mutex)
      :not-owned))

;; mutex-lock!
(defun mutex-lock! (mutex &optional timeout (thread sb-thread:*current-thread*))
  (let ((sb-thread:*current-thread* thread))
    (sb-thread:grab-mutex mutex :timeout timeout)))

;; mutex-unlock!
(defun mutex-unlock! (mutex &optional condition-variable timeout)
  (if condition-variable
      (progn
        (sb-thread:condition-wait condition-variable
                                  mutex
                                  :timeout timeout)
        (sb-thread:release-mutex mutex))
      (sb-thread:release-mutex mutex)))

;; condition-variable?
;; make-condition-variable
;; condition-variable-name
;; condition-variable-specific
(defstruct (condition-variable
            (:include sb-thread:waitqueue)
            (:constructor make-condition-variable (&optional name))
            (:predicate condition-variable?))
  specific)

;; condition-variable-specific-set!
(defun condition-variable-specific-set! (cv value)
  (setf (condition-variable-specific cv) value))

;; condition-variable-signal!
(defun condition-variable-signal! (cv)
  (sb-thread:condition-notify cv))

;; condition-variable-broadcast!
(defun condition-variable-broadcast! (cv)
  (sb-thread:condition-broadcast cv))

;; current-time :: srfi-19
;; time? :: srfi-19

;; time->seconds
(defun time->seconds (time)
  (+ (srfi-19:time-second time)
     (* (srfi-19:time-nanosecond time) 1d-10)))

;; seconds->time
(defun seconds->time (sec)
  (multiple-value-bind (s ns)
                       (floor sec)
    (srfi-19:make-time 'srfi-19:time-utc (floor ns 1d-10) s)))

;; current-exception-handler

;; with-exception-handler :: srfi-34

;; raise :: srfi-34

;; join-timeout-exception? (not-implemented)
;; abandoned-mutex-exception? (not-implemented)
;; terminated-thread-exception? (not-implemented)
;; uncaught-exception? (not-implemented)
;; uncaught-exception-reason (not-implemented)

;;; eof
