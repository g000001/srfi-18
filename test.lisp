(cl:in-package :srfi-18.internal)

(def-suite srfi-18)

(in-suite srfi-18)


#|(let ((th (thread-start! (make-thread (lambda () (princ 'a #.*standard-output*))))))
  (princ 'b)
  (thread-join! th))|#

#|(let ((th (make-thread (lambda () :foo) "foo")))
  (thread-start! th))|#

#|(cl:loop :for x :from 0
      :do (thread-sleep! 1)
          (princ x #.*standard-output*))|#


#|(let ((th (thread-start! (make-thread (lambda () (sleep 8) (expt 2 100))))))
  ;; (do-something-else)
  (thread-join! th 1  )
  )|#



#|(let ((m (make-mutex)))
  (mutex-specific-set! m :foo)
  (mutex-specific m))|#


#|(let ((m (make-mutex)))
  (sb-thread:with-mutex (m)
    (mutex-state m))
  (mutex-state m))|#


#|(let ((m (make-mutex)))
  (mutex-lock! m)
  (mutex-state m))|#


;;; an implementation of a mailbox object of depth one; this
;;; implementation does not behave well in the presence of forced
;;; thread terminations using thread-terminate! (deadlock can occur
;;; if a thread is terminated in the middle of a put! or get! operation)

#|(defun make-empty-mailbox ()
  (let ((put-mutex (make-mutex)) ; allow put! operation
        (get-mutex (make-mutex))
        (cell 'NIL) )
    (sb-thread:with-recursive-lock (put-mutex)
      (sb-thread:with-recursive-lock (get-mutex)
        (labels ((put! (obj)
               (mutex-lock! put-mutex 'NIL 'NIL) ; prevent put! operation
               (setq cell obj)
               (mutex-unlock! get-mutex) ) ; allow get! operation

             (get! ()
               (mutex-lock! get-mutex 'NIL 'NIL) ; wait until object in mailbox
               (let ((result cell))
                 (setq cell 'NIL) ; prevent space leaks
                 (mutex-unlock! put-mutex) ; allow put! operation
                 result )))

      #|(mutex-lock! get-mutex 'NIL 'NIL)|# ; prevent get! operation

      (lambda (msg)
        (case msg
          ((put!) #'put!)
          ((get!) #'get!)
          (otherwise (error "unknown message")) )))))))|#

#|(defun mailbox-put! (m obj) (funcall (funcall m 'put!) obj))|#
#|(defun mailbox-get! (m) (funcall (funcall m 'get!)))|#

#|(handler-case (sb-ext:with-timeout 2
                (let ((mb (make-empty-mailbox)))
                  (mailbox-put! mb 'foo)
                  (mailbox-get! mb)
                  (mailbox-put! mb 'bar)))
  (sb-ext:timeout () nil) )|#



 ; an implementation of a mailbox object of depth one; this
    ; implementation behaves gracefully when threads are forcibly
    ; terminated using thread-terminate! (the "abandoned mutex"
    ; exception will be raised when a put! or get! operation is attempted
    ; after a thread is terminated in the middle of a put! or get!
    ; operation)

(defun make-empty-mailbox ()
  (let ((mutex (make-mutex))
        (put-condvar (make-condition-variable))
        (get-condvar (make-condition-variable))
        (full? nil)
        (cell nil) )
    (labels ((put! (obj)
               (mutex-lock! mutex)
               (if full?
                   (progn
                     (mutex-unlock! mutex put-condvar)
                     (put! obj) )
                   (progn
                     (setq cell obj)
                     (setq full? 'T)
                     (condition-variable-signal! get-condvar)
                     (mutex-unlock! mutex) )))
             (get! ()
               (mutex-lock! mutex)
               (if (not full?)
                   (progn
                     (mutex-unlock! mutex get-condvar)
                     (get!) )
                   (let ((result cell))
                     (setq cell nil) ; avoid space leaks
                     (setq full? nil)
                     (condition-variable-signal! put-condvar)
                     (mutex-unlock! mutex) ))))
      (lambda (msg)
        (case msg
          ((put!) #'put!)
          ((get!) #'get!)
          (otherwise (error "unknown message")) )))))

(defun mailbox-put! (m obj) (funcall (funcall m 'put!) obj))
(defun mailbox-get! (m) (funcall (funcall m 'get!)))

(test mailbox
  (is-true (handler-case (sb-ext:with-timeout 2
                           (let ((mb (make-empty-mailbox)))
                             (mailbox-put! mb 'foo)
                             (mailbox-get! mb)
                             (mailbox-put! mb 'bar)
                             t ))
             (sb-ext:timeout () nil) )))
