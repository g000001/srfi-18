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

#|(let ((th (thread-start! (make-thread (lambda () (princ 'a #.*standard-output*))))))
  (princ 'b)
  (thread-join! th))|#

#|(let ((th (make-thread (lambda () :foo) "foo")))
  (thread-start! th))|#

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

#|(cl:loop :for x :from 0
      :do (thread-sleep! 1)
          (princ x #.*standard-output*))|#

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

#|(let ((th (thread-start! (make-thread (lambda () (sleep 8) (expt 2 100))))))
  ;; (do-something-else)
  (thread-join! th 1  )
  )|#


;; make-mutex ::
;; mutex-name ::
;; mutex-specific ::
(defstruct (mutex
            (:include sb-thread:mutex)
            (:constructor make-mutex
                          (&optional nam
                                     &aux (name (and nam (string nam))))))
  specific)

;; mutex?
(defun mutex? (obj)
  (sb-thread::mutex-p obj))

;; mutex-specific-set!
(defun mutex-specific-set! (mutex value)
  (setf (mutex-specific mutex) value))

#|(let ((m (make-mutex)))
  (mutex-specific-set! m :foo)
  (mutex-specific m))|#

;; mutex-state
;; thread T: the mutex is in the locked/owned state and thread T is
;; the owner of the mutex
;; symbol not-owned: the mutex is in the locked/not-owned state
;; symbol abandoned: the mutex is in the unlocked/abandoned state
;; symbol not-abandoned: the mutex is in the unlocked/not-abandoned state

;;; FIXME
(defun mutex-state (mutex)
  (or (sb-thread:mutex-owner mutex)
      :not-owned))

#|(let ((m (make-mutex)))
  (sb-thread:with-mutex (m)
    (mutex-state m))
  (mutex-state m))|#

;; mutex-lock!
(defun mutex-lock! (mutex &optional timeout (thread sb-thread:*current-thread*))
  (let ((sb-thread:*current-thread* thread))
    (sb-thread:grab-mutex mutex :timeout timeout)))

#|(let ((m (make-mutex)))
  (mutex-lock! m)
  (mutex-state m))|#

;; mutex-unlock!
(defun mutex-unlock! (mutex)
  (sb-thread:release-mutex mutex))

;; condition-variable?
;; make-condition-variable
;; condition-variable-name
;; condition-variable-specific
;; condition-variable-specific-set!
;; condition-variable-signal!
;; condition-variable-broadcast!

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

;; join-timeout-exception?
;; abandoned-mutex-exception?
;; terminated-thread-exception?
;; uncaught-exception?
;; uncaught-exception-reason
