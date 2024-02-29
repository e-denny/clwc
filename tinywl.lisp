;;; -*- mode: lisp -*-

(in-package :clwc)

;;; --------------------------------------------------------------------------------

(defparameter *listener-alist* '())

(defparameter *xcursor-size* 24)
(defparameter *xcursor-theme* nil)
(defparameter *server* nil)

(defclass cw-server ()
  ((wl-display :accessor wl-display :initarg :wl-display)
   (backend :accessor backend :initarg :backend)
   (wlr-renderer :accessor wlr-renderer :initarg :wlr-renderer)
   (wlr-allocator :accessor wlr-allocator :initarg :wlr-allocator)
   (wlr-scene :accessor wlr-scene :initarg :wlr-scene)

   (wlr-scene-output-layout :accessor wlr-scene-output-layout :initarg :wlr-scene-output-layout)
   (wlr-output-layout :accessor wlr-output-layout :initarg :wlr-output-layout)
   (output-list :accessor output-list :initform nil)

   (seat :accessor seat :initarg :seat)
   (cursor :accessor cursor :initarg :cursor)

   (toplevel-list :accessor toplevel-list :initform nil)

   (button-state :accessor button-state :initarg :released)
   (grabbed-toplevel :accessor grabbed-toplevel :initarg :grabbed-toplevel)
   (grabbed-x :accessor grabbed-x :initarg :grabbed-x)
   (grabbed-y :accessor grabbed-y :initarg :grabbed-y)
   (grabbed-box :accessor grabbed-box :initarg :grabbed-box)
   (resize-edges :accessor resize-edges :initarg :resize-edges)

   (xdg-shell :accessor xdg-shell :initarg :xdg-shell)))


(defclass cw-xdg-shell ()
  ((wlr-xdg-shell :accessor wlr-xdg-shell :initarg :wlr-xdg-shell)
   (new-xdg-surface-listener :accessor new-xdg-surface-listener :initform (%make-listener))))

(defclass cw-backend ()
  ((wlr-backend :accessor wlr-backend :initarg :wlr-backend)
   (new-output-listener :accessor new-output-listener :initform (%make-listener))
   (new-input-listener :accessor new-input-listener :initform (%make-listener))))


(defclass cw-seat ()
  ((wlr-seat :accessor wlr-seat :initarg :wlr-seat)
   (keyboard-list :accessor keyboard-list :initform nil)
   (request-set-selection-listener :accessor request-set-selection-listener :initform (%make-listener))
   (request-cursor-listener :accessor request-cursor-listener :initform (%make-listener))))


(defclass cw-keyboard ()
  ((wlr-keyboard :accessor wlr-keyboard :initarg :wlr-keyboard)
   (modifier-listener :accessor modifier-listener :initform (%make-listener))
   (key-listener :accessor key-listener :initform (%make-listener))
   (destroy-listener :accessor destroy-listener :initform (%make-listener))))


(defclass cw-output ()
  ((wlr-output :accessor wlr-output :initarg :wlr-output)
   (frame-listener :accessor frame-listener :initform (%make-listener))
   (request-state-listener :accessor request-state-listener :initform (%make-listener))
   (destroy-listener :accessor destroy-listener :initform (%make-listener))))


(defclass cw-toplevel ()
  ((wlr-xdg-toplevel :accessor wlr-xdg-toplevel :initarg :wlr-xdg-toplevel)
   (wlr-scene-tree :accessor wlr-scene-tree :initarg :wlr-scene-tree)
   (surface-map-listener :accessor surface-map-listener :initform (%make-listener))
   (surface-unmap-listener :accessor surface-unmap-listener :initform (%make-listener))
   (surface-destroy-listener :accessor surface-destroy-listener :initform (%make-listener))
   (request-move-listener :accessor request-move-listener :initform (%make-listener))
   (request-resize-listener :accessor request-resize-listener :initform (%make-listener))
   (request-maximize-listener :accessor request-maximize-listener :initform (%make-listener))
   (request-fullscreen-listener :accessor request-fullscreen-listener :initform (%make-listener))))


(defclass cw-cursor ()
  ((wlr-cursor :accessor wlr-cursor :initarg :wlr-cursor)
   (wlr-xcursor-mgr :accessor wlr-xcursor-mgr :initarg :wlr-xcursor-mgr)
   (cursor-mode :accessor cursor-mode :initform :cursor-passthrough)

   (cursor-motion-listener :accessor cursor-motion-listener :initform (%make-listener))
   (cursor-motion-absolute-listener :accessor cursor-motion-absolute-listener
                                    :initform (%make-listener))
   (cursor-frame-listener :accessor cursor-frame-listener :initform (%make-listener))
   (cursor-axis-listener :accessor cursor-axis-listener :initform (%make-listener))
   (cursor-button-listener :accessor cursor-button-listener :initform (%make-listener))))



;;; --------------------------------------------------------------------------------
;;; listener
;;; --------------------------------------------------------------------------------

(defun listener->class-instance (listener)
  (cdr (assoc listener *listener-alist* :test #'cffi:pointer-eq)))

(defun remove-listener (listener)
  (remove listener *listener-alist* :key 'car :test #'cffi:pointer-eq)
  (cffi:foreign-free listener))

;;; --------------------------------------------------------------------------------


(defun toplevel= (toplevel-a toplevel-b)
  (cffi:pointer-eq (wlr-xdg-toplevel toplevel-a) (wlr-xdg-toplevel toplevel-b)))

(defun focus-toplevel (toplevel wlr-surface)
  "Provide keyboard focus to a toplevel."
  (when toplevel
    (let* ((wlr-seat (wlr-seat (seat *server*)))
           (prev-wlr-surface (%get-seat-focused-surface wlr-seat))
           (wlr-keyboard (%wlr-seat-get-keyboard wlr-seat)))
      ;; Don't re-focus the existing focused surface
      (when (and wlr-surface (cffi:pointer-eq wlr-surface prev-wlr-surface))
        (return-from focus-toplevel t))
      ;; If there is a previously focused surface, let the client know that
      ;; it is no longer focused.
      (when (not (cffi:null-pointer-p prev-wlr-surface))
        (let ((prev-wlr-xdg-surface (%wlr-xdg-surface-try-from-wlr-surface
                                     (%get-seat-focused-surface wlr-seat))))
          (when (not (= (%xdg-surface-role prev-wlr-xdg-surface)
                        (cffi:foreign-enum-value 'wlr-xdg-surface-role :toplevel)))
            (error "Surface is not toplevel"))
          (activate-toplevel (%xdg-surface-toplevel prev-wlr-xdg-surface) nil)))
      ;; move to toplevel to the front
      (%wlr-scene-node-raise-to-top (wlr-scene-tree toplevel))
      (format t "into: focus-toplevel: ~a, ~a~%" toplevel (toplevel-list *server*))
      (setf (toplevel-list *server*) (cons toplevel (remove toplevel (toplevel-list *server*) :test #'toplevel=)))
      ;; activate new surface
      (activate-toplevel (wlr-xdg-toplevel toplevel) t)
      (%seat-keyboard-notify-enter wlr-seat (wlr-xdg-toplevel toplevel) wlr-keyboard))))


;; -------------------- commands --------------------

(defparameter *key-bindings-hash* (make-hash-table :test #'equal))


(defun command-quit ()
  "Quit the compositor."
  (%wl-display-destroy-clients (wl-display *server*))
  (%wl-display-terminate (wl-display *server*))
  (%wl-display-destroy (wl-display *server*)))


(defun command-next-toplevel ()
  ;; The active toplevel is the front of the list.
  (focus-toplevel (second (toplevel-list *server*)) nil))

(defun command-terminal ()
  (uiop:launch-program "alacritty"))

(defun setup-keybindings ()
  (let ((key-bindings '(("M-q" . command-quit)
                        ("M-t" . command-terminal)
                        ("M-s" . command-next-toplevel))))
    (loop for (key . command) in key-bindings
          do (setf (gethash key *key-bindings-hash*) command))))

;; -------------------- keyboard --------------------

(defun keyboard-p (dev-type)
  (= dev-type (cffi:foreign-enum-value 'wlr-input-device-type :keyboard)))


(defun keyboard-delete (keyboard)
  ;; TODO: do I also need to free the wlr_keyboard itself?
  (remove-listener (key-listener keyboard))
  (remove-listener (modifier-listener keyboard))
  (remove-listener (destroy-listener keyboard)))


(defun get-keys (keyboard event-keyboard-key)
  ;; FIXME: seems to only work for keys and not symbols.
  ;; Something wrong about the key map / key codes ?
  (let* ((keys-ptr (%get-keyboard-keys keyboard event-keyboard-key))
         (keys-str (cffi:foreign-string-to-lisp keys-ptr)))
    (cffi:foreign-free keys-ptr)
    keys-str))


(defun get-modifiers (keyboard)
  ;; FIXME: replace with a 'with-' macro?
  (let* ((modifiers-ptr (%get-keyboard-modifiers keyboard))
         (modifiers-str (cffi:foreign-string-to-lisp modifiers-ptr)))
    (cffi:foreign-free modifiers-ptr)
    modifiers-str))


(defun lookup-key-binding (modifiers keys)
  (let ((key-binding (concatenate 'string modifiers keys)))
    (gethash key-binding *key-bindings-hash*)))


(defcallback handle-keyboard-modifiers :void ((listener :pointer) (event :pointer))
  "Handle modifier key events. The event is raised when when a modifier key such as
Shift or Alt is pressed. The event is simply communicated to the toplevel."
  (declare (ignore event))
  (format t "into: handle-keyboard-modifiers: ~a~%" listener)
  (let* ((keyboard-instance (listener->class-instance listener))
         (keyboard (wlr-keyboard keyboard-instance))
         (seat (wlr-seat (seat *server*))))
    ;; Set the keyboard for the seat
    (%wlr-seat-set-keyboard seat keyboard)
    ;; Send modifiers to the toplevel.
    (%seat-keyboard-notify-modifiers seat keyboard))
  (format t "leaving: handle-keyboard-modifiers~%"))


(defcallback handle-keyboard-key :void ((listener :pointer) (keyboard-key-event :pointer))
  ;; TODO: should we be comparing key syms rather than characters here?
  ;; i.e. convert the key bindings into key syms and compare them instead?
  (format t "into: handle-keyboard-key: ~a, ~a~%" listener keyboard-key-event)
  (let* ((keyboard-instance (listener->class-instance listener))
         (keyboard (wlr-keyboard keyboard-instance))
         (handled nil))
    ;; FIXM: should really be clear that this is a 'pressed' event
    (when (eq 0 (%keyboard-key-state keyboard-key-event))
      (let* ((keys (get-keys keyboard keyboard-key-event))
             (modifiers (get-modifiers keyboard))
             (command (lookup-key-binding modifiers keys)))
        (when command
          (setf handled t)
          (funcall command))))
    (when (not handled)
      (%seat-keyboard-notify-key (wlr-seat (seat *server*))
                                 keyboard
                                 keyboard-key-event)))
  (format t "leaving: handle-keyboard-key~%"))


(defcallback handle-keyboard-destroy :void ((listener :pointer) (event :pointer))
  "Handle signal to destory keyboard. The keyboard will no longer raise events."
  (declare (ignore event))
  (format t "into: handle-keyboard-destroy: ~a~%" listener)
  (let* ((keyboard (listener->class-instance listener)))
    (keyboard-delete keyboard))
  (format t "leaving: handle-keyboard-destroy~%"))


(defun cw-create-keyboard (wlr-input-device)
  (let* ((wlr-keyboard (%wlr-keyboard-from-input-device wlr-input-device))
         (keyboard (make-instance 'cw-keyboard :wlr-keyboard wlr-keyboard))
         (seat (wlr-seat (seat *server*))))
    (%setup-keyboard wlr-keyboard)
    ;; set the new keyboard as the active keyboard for the seat
    (%wlr-seat-set-keyboard seat wlr-keyboard)

    (with-slots (key-listener modifier-listener destroy-listener)
        keyboard
      (push (cons key-listener keyboard) *listener-alist*)
      (%signal-add-keyboard-key key-listener
                                wlr-keyboard
                                (cffi:callback handle-keyboard-key))

      (push (cons modifier-listener keyboard) *listener-alist*)
      (%signal-add-keyboard-modifiers modifier-listener
                                      wlr-keyboard
                                      (cffi:callback handle-keyboard-modifiers))

      (push (cons destroy-listener keyboard) *listener-alist*)
      (%signal-add-keyboard-destroy destroy-listener
                                    ;; NOTE: this is the input-device and not the keyboard
                                    wlr-input-device
                                    (cffi:callback handle-keyboard-destroy)))
    (push keyboard (keyboard-list (seat *server*)))))



;; -------------------- pointer --------------------


(defun create-new-pointer (wlr-input-device)
  ;; Just attach the pointer device to the cursor.
  ;; All pointer handling is proxied through wlr_cursor.
  (%wlr-cursor-attach-input-device (wlr-cursor (cursor *server*)) wlr-input-device))


(defun pointer-p (dev-type)
  (= dev-type (cffi:foreign-enum-value 'wlr-input-device-type :pointer-device)))

;; -------------------- input --------------------


(defcallback handle-new-input :void ((listener :pointer) (wlr-input-device :pointer))
  ;; This function is called for each connected input device (keyboard, mouse, touch screen - TBD),
  ;; and each time a new input device is connected.
  (declare (ignore  listener))
  (format t "into: handle-new-input: ~a~%" wlr-input-device)
  (let ((dev-type (%device-type wlr-input-device)))
    (cond ((keyboard-p dev-type)
           (cw-create-keyboard wlr-input-device))
          ((pointer-p dev-type)
           (create-new-pointer wlr-input-device))
          (t
           nil)))
  ;; Set the seat capabilites. We always have a pointer
  ;; (even if there are no pointer devices)
  (%set-seat-capabilities (wlr-seat (seat *server*))
                          (if (not (eql (keyboard-list (seat *server*)) nil))
                              1
                              0))
  (format t "leaving: handle-new-input~%"))


;; -------------------- cursor --------------------


(cffi:defcallback handle-seat-request-cursor :void ((listener :pointer) (event :pointer))
  ;; This event is raised when a client provides a cursor image.
  (declare (ignore listener))
  (format t "into: handle-seat-request-cursor: ~a~%" event)
  (let ((focused-client (%get-seat-focused-client (wlr-seat (seat *server*))))
        (seat-client (%get-seat-client event)))
    ;; Check that the client has pointer focus
    (when (cffi:pointer-eq focused-client seat-client)
      (%cursor-set-surface (wlr-cursor (cursor *server*)) event)))
  (format t "leaving: handle-seat-request-cursor~%"))


(cffi:defcallback handle-seat-request-set-selection :void ((listener :pointer) (event :pointer))
  ;; This event is raised by the seat when a toplevel wants to set the selection,
  ;; usually when the user copies something.
  (declare (ignore listener))
  (format t "into: handle-seat-request-set-selection ~a~%" event)
  (%set-seat-selection (wlr-seat (seat *server*)) event)
  (format t "leaving: handle-seat-request-set-selection~%"))


(defun xdg-toplevel-toplevel= (wlr-xdg-toplevel toplevel)
  (cffi:pointer-eq wlr-xdg-toplevel (wlr-xdg-toplevel toplevel)))

(defun xdg-toplevel->toplevel (wlr-xdg-toplevel)
  (find wlr-xdg-toplevel (toplevel-list *server*) :test #'xdg-toplevel-toplevel=))


(defun desktop-toplevel-at (cursor)
  (multiple-value-bind (wlr-scene-node sx sy)
      (scene-node-at (wlr-scene *server*) cursor)
    (when (cffi:null-pointer-p wlr-scene-node)
      (return-from desktop-toplevel-at (values nil nil nil nil)))
    (when (not (equal (%get-node-type wlr-scene-node)
                      (cffi:foreign-enum-value 'wlr-scene-node-type :buffer)))
      (return-from desktop-toplevel-at (values nil nil nil nil)))
    (let* ((scene-buffer (%wlr-scene-buffer-from-node wlr-scene-node))
           (scene-surface (%wlr-scene-surface-try-from-buffer scene-buffer)))
      (when (not scene-surface)
        (return-from desktop-toplevel-at (values nil nil nil nil)))
      (let ((wlr-surface (%get-scene-surface-wlr-surface scene-surface))
            (toplevel (xdg-toplevel->toplevel (%get-root-toplevel wlr-scene-node))))
        (values toplevel wlr-surface sx sy)))))


(defun reset-cursor-mode ()
  (setf (cursor-mode (cursor *server*)) :cursor-passthrough)
  (setf (grabbed-toplevel *server*) nil))


(defun process-cursor-move (time)
  ;; Move the grabbed-toplevel the the new position.
  (declare (ignore time))
  (with-slots (grabbed-toplevel grabbed-x grabbed-y cursor) *server*
    (with-slots (wlr-scene-tree) grabbed-toplevel
      (multiple-value-bind (cx cy) (cursor-coords (wlr-cursor cursor))
        ;; Assign new values to the toplevel class instance.
        (%scene-tree-set-position wlr-scene-tree
                                  (floor (- cx grabbed-x))
                                  (floor (- cy grabbed-y)))))))


(defun process-cursor-resize (time)
  (declare (ignore time))
  (with-slots (grabbed-toplevel grabbed-x grabbed-y grabbed-box resize-edges cursor) *server*
    (destructuring-bind (gbx gby gbwidth gbheight)
        grabbed-box
      (multiple-value-bind (cur-x cur-y)
          (cursor-coords (wlr-cursor cursor))
        (let ((border-x (- cur-x grabbed-x))
              (border-y (- cur-y grabbed-y))
              (new-left gbx)
              (new-right (+ gbx gbwidth))
              (new-top gby)
              (new-bottom (+ gby gbheight)))
          (destructuring-bind (edge-top-p edge-bottom-p edge-left-p edge-right-p)
              resize-edges
            (cond (edge-top-p
                   (setf new-top border-y)
                   (when (>= new-top new-bottom)
                     (setf new-top (- new-bottom 1))))
                  (edge-bottom-p
                   (setf new-bottom border-y)
                   (when (<= new-bottom new-top)
                     (setf new-bottom (+ new-top 1)))))
            (cond (edge-left-p
                   (setf new-left border-x)
                   (when (>= new-left new-right)
                     (setf new-left (- new-right 1))))
                  (edge-right-p
                   (setf new-right border-x)
                   (when (<= new-right new-left)
                     (setf new-right (+ new-left 1))))))
          (with-slots (wlr-scene-tree wlr-xdg-toplevel) grabbed-toplevel
            (multiple-value-bind (surf-x surf-y)
                (xdg-surface-get-geometry wlr-xdg-toplevel)
              (let ((new-width (ceiling (- new-right new-left)))
                    (new-height (ceiling (- new-bottom new-top))))
                (%scene-tree-set-position wlr-scene-tree
                                          (floor (- new-left surf-x))
                                          (floor (- new-top surf-y)))
                (%wlr-xdg-toplevel-set-size wlr-xdg-toplevel
                                            new-width
                                            new-height)))))))))


(defun process-cursor-motion (time)
  (format t "process-cursor-motion: cursor-mode: ~a~%" (cursor-mode (cursor *server*)))
  (cond
    ((equal (cursor-mode (cursor *server*)) :cursor-move)
     (process-cursor-move time))
    ((equal (cursor-mode (cursor *server*)) :cursor-resize)
     (process-cursor-resize time))
    (t
     ;; Find the toplevel and under the pointer and send it the event
     (multiple-value-bind (toplevel wlr-surface sx sy)
         (desktop-toplevel-at (wlr-cursor (cursor *server*)))
       (when (not toplevel)
         ;; There is no toplevel under the cursor. Set the cursor to the default
         (%wlr-cursor-set-xcursor (wlr-cursor (cursor *server*))
                                  (wlr-xcursor-mgr (cursor *server*))
                                  "default"))
       (let ((seat (wlr-seat (seat *server*))))
         (if wlr-surface
             (progn
               ;; Let the surface know that the cursor has entered it.
               (%wlr-seat-pointer-notify-enter seat wlr-surface sx sy)
               ;; Notify a motion if the focus did not change.
               (%wlr-seat-pointer-notify-motion seat time sx sy))
             ;; Clear pointer focus so future events are not sent the the last toplevel
             ;; that has the cursor over it.
             (%wlr-seat-pointer-clear-focus seat)))))))


(cffi:defcallback handle-cursor-motion :void ((listener :pointer) (event :pointer))
  ;; The event is forwarded by the cursor when a pointer emits a relative
  ;; pointer motion event (i.e. a delta)
  (declare (ignore listener))
  (format t "into: handle-cursor-motion: ~a~%" event)
  (let ((time (%get-time-event-pointer-motion event)))
    (%cursor-move (wlr-cursor (cursor *server*)) event)
    (process-cursor-motion time))
  (format t "leaving: handle-cursor-motion~%"))


(cffi:defcallback handle-cursor-motion-absolute :void ((listener :pointer) (event :pointer))
  ;; The event is forwarded by the cursor when a pointer emits an absolute
  ;; pointer motion event.
  (declare (ignore listener))
  (format t "into: handle-cursor-motion-absolute: ~a~%" event)
  (let ((time (%get-time-event-pointer-motion-absolute event)))
    (%cursor-warp-absolute (wlr-cursor (cursor *server*)) event)
    (process-cursor-motion time))
  (format t "leaving: handle-cursor-motion-absolute~%"))


(cffi:defcallback handle-cursor-button :void ((listener :pointer) (event :pointer))
  ;; The event is forwarded by the cursor when a pointer emits a button event.
  (declare (ignore listener))
  (format t "into: handle-cursor-button: ~a~%" event)
  (%seat-pointer-notify-button (wlr-seat (seat *server*)) event)
  (multiple-value-bind (toplevel wlr-surface)
      (desktop-toplevel-at (wlr-cursor (cursor *server*)))
    (if (= (%get-button-state event) (cffi:foreign-enum-value 'wlr-button-state :released))
        (progn
          (setf (button-state *server*) :released)
          ;; if any button is released, exit interactive move/resize
          (format t "handle-cursor-button: button released~%")
          (reset-cursor-mode)
          (format t "handle-cursor-button: cursor-mode: ~a~%" (cursor-mode (cursor *server*)))
          )
        ;; else, focus the toplevel if the button was pressed (not released)
        (progn
          (setf (button-state *server*) :pressed)
          (focus-toplevel toplevel wlr-surface))))
  (format t "leaving: handle-cursor-button~%"))


(cffi:defcallback handle-cursor-axis :void ((listener :pointer) (axis-event :pointer))
  ;; The event is forwarded by the cursor when a pointer emits an axis event,
  ;; such as a scroll wheel movement.
  ;; Notify the toplevel with pointer focus of the axis event.
  (declare (ignore listener))
  (format t "into: handle-cursor-axis: ~a~%" axis-event)
  (%seat-pointer-notify-axis (wlr-seat (seat *server*)) axis-event)
  (format t "leaving: handle-cursor-axis~%"))


(cffi:defcallback handle-cursor-frame :void ((listener :pointer) (pointer-frame-event :pointer))
  ;; The event is forwarded by the cursor when a pointer emits a frame event.
  ;; Frame events are sent after regular pointer events to group multiple events
  ;; together.
  ;; Notify the toplevel with pointer focus of the frame event.
  (declare (ignore listener))
  (declare (ignore pointer-frame-event))
  (format t "into: handle-cursor-frame~%")
  (%wlr-seat-pointer-notify-frame (wlr-seat (seat *server*)))
  (format t "leaving: handle-cursor-frame~%"))


;; -------------------- output --------------------


(defun output-delete (output)
  (remove-listener (frame-listener output))
  (remove-listener (destroy-listener output))
  (remove-listener (request-state-listener output))
  ;; TODO: do I also need to delete and pointers contained within the output-instance
  ;; i.e. (wlr-output output) Actually, I don't think so.
  )


(cffi:defcallback handle-output-frame :void ((listener :pointer) (data :pointer))
  ;; Called every time an output is ready to display a frame, usually at
  ;; the output's refresh rate.
  (declare (ignore data))
  (format t "into: handle-output-frame: ~a~%" listener)
  (let* ((output (listener->class-instance listener))
         (scene-output (%wlr-scene-get-scene-output (wlr-scene *server*)
                                                    (wlr-output output))))
    (%wlr-scene-output-commit scene-output (cffi:null-pointer))
    (multiple-value-bind (sec nsec) (osicat-posix:clock-gettime osicat-posix:clock-monotonic)
      (%scene-output-send-frame-done scene-output sec nsec)))
  (format t "into: handle-output-frame~%"))


(cffi:defcallback handle-output-destroy :void ((listener :pointer) (data :pointer))
  (declare (ignore data))
  (format t "into: handle-output-destroy: ~a~%" listener)
  (let* ((output (listener->class-instance listener)))
    (output-delete output))
  (format t "leaving: handle-output-destroy~%"))

(cffi:defcallback handle-output-request-state :void ((listener :pointer) (event :pointer))
  (format t "into: handle-output-request-state: ~a~%" listener)
  (let* ((output (listener->class-instance listener)))
    (output-commit-state (wlr-output output) event))
  (format t "leaving: handle-output-request-state~%"))

(cffi:defcallback handle-new-output :void ((listener :pointer) (wlr-output :pointer))
  ;; This function is called for each connected output device (screen/monitor), and each time a
  ;; new output device is connected.
  (declare (ignore listener))
  (format t "into: handle-new-output: ~a~%" wlr-output)

  ;; configure the output created by the backend
  (%wlr-output-init-render wlr-output (wlr-allocator *server*) (wlr-renderer *server*))

  ;; The output may be disabled, switch it on.
  (with-foreign-object (state :pointer)
    (%wlr-output-state-init state)
    (%wlr-output-state-set-enabled state t)

    (let ((wlr-output-mode (%wlr-output-preferred-mode wlr-output)))
      (when (not (cffi:null-pointer-p wlr-output-mode))
        (%wlr-output-state-set-mode state wlr-output-mode))

      (%wlr-output-commit-state wlr-output state)
      (%wlr-output-state-finish state)))

  (let ((new-output (make-instance 'cw-output :wlr-output wlr-output)))
    (push new-output (output-list *server*))

    (push (cons (frame-listener new-output) new-output) *listener-alist*)
    (%signal-add-output-frame (frame-listener new-output)
                              (wlr-output new-output)
                              (cffi:callback handle-output-frame))

    (push (cons (request-state-listener new-output) new-output) *listener-alist*)
    (%signal-add-output-request-state (request-state-listener new-output)
                                      (wlr-output new-output)
                                      (cffi:callback handle-output-request-state))

    (push (cons (destroy-listener new-output) new-output) *listener-alist*)
    (%signal-add-output-destroy (destroy-listener new-output)
                                (wlr-output new-output)
                                (cffi:callback handle-output-destroy))

    ;; add the output the output layout
    (let* ((wlr-output-layout-output (%wlr-output-layout-add-auto (wlr-output-layout *server*)
                                                                  (wlr-output new-output)))
           (wlr-scene-output (%wlr-scene-output-create (wlr-scene *server*)
                                                       (wlr-output new-output))))
      (%wlr-scene-output-layout-add-output (wlr-scene-output-layout *server*)
                                           wlr-output-layout-output
                                           wlr-scene-output)))
  (format t "leaving: handle-new-output~%"))

;; -------------------- toplevel --------------------


(cffi:defcallback handle-xdg-toplevel-map :void ((listener :pointer) (data :pointer))
  ;; Called when the surface is mapped, or ready to display on-screen.
  (declare (ignore data))
  (format t "into: handle-xdg-toplevel-map: ~a~%" listener)
  (let ((toplevel (listener->class-instance listener)))
    (push toplevel (toplevel-list *server*))
    (focus-toplevel toplevel (%xdg-toplevel-get-surface (wlr-xdg-toplevel toplevel))))
  (format t "leaving: handle-xdg-toplevel-map~%"))


(cffi:defcallback handle-xdg-toplevel-unmap :void ((listener :pointer) (data :pointer))
  ;; Called when the surface is unmapped, and should no longer be shown.
  (declare (ignore data))
  (format t "into: handle-xdg-toplevel-unmap: ~a~%" listener)
  (let ((toplevel (listener->class-instance listener)))
    (format t "toplevel=: ~a, ~a~%" toplevel (grabbed-toplevel *server*))
    (when (grabbed-toplevel *server*)
      (when (toplevel= toplevel (grabbed-toplevel *server*))
        (reset-cursor-mode)))
    (format t "toplevel=: ~a, ~a~%" toplevel (toplevel-list *server*))
    (setf (toplevel-list *server*) (remove toplevel (toplevel-list *server*) :test #'toplevel=)))
  (format t "leaving: handle-xdg-toplevel-unmap~%"))


(cffi:defcallback handle-xdg-toplevel-destroy :void ((listener :pointer) (data :pointer))
  ;; Called when the surface is destroyed, and should not be shown again.
  (declare (ignore data))
  (format t "into: handle-xdg-toplevel-destroy: ~a~%" listener)
  (let ((toplevel (listener->class-instance listener)))
    ;; FIXME: do I also need to delete any other C objects like wlr-xdg-surface?
    ;; TODO: also remove every listener from the lookup alist
    (remove-listener (surface-map-listener toplevel))
    (remove-listener (surface-unmap-listener toplevel))
    (remove-listener (surface-destroy-listener toplevel))
    (remove-listener (request-move-listener toplevel))
    (remove-listener (request-resize-listener toplevel))
    (remove-listener (request-maximize-listener toplevel))
    (remove-listener (request-fullscreen-listener toplevel))
    ;; (cffi:foreign-free (wlr-toplevel toplevel))
    )
  (format t "leaving: handle-xdg-toplevel-destroy~%"))


(defun begin-interactive (toplevel mode edges)
  ;; Set up interactive move or resize operation. The compositor stops
  ;; propagating pointer events to toplevels and instead consumes them itself
  ;; to move or resize windows.
  (let* ((wlr-seat (wlr-seat (seat *server*)))
         (focused-surface (%get-seat-focused-surface wlr-seat))
         (root-surface (%wlr-surface-get-root-surface focused-surface))
         (toplevel-surface (%xdg-toplevel-get-surface (wlr-xdg-toplevel toplevel)))
         border-x
         border-y)
    (when (not (cffi:pointer-eq toplevel-surface root-surface))
      ;; ignore move/resize requests from unfocused toplevels.
      (return-from begin-interactive))

    (setf (grabbed-toplevel *server*) toplevel)
    (setf (cursor-mode (cursor *server*)) mode)

    (multiple-value-bind (cur-x cur-y)
        (cursor-coords (wlr-cursor (cursor *server*)))
      (let ((node-x (%get-scene-tree-node-x (wlr-scene-tree toplevel)))
            (node-y (%get-scene-tree-node-y (wlr-scene-tree toplevel))))
        (cond ((equal mode :cursor-move)
               (setf (grabbed-x *server*) (- cur-x node-x))
               (setf (grabbed-y *server*) (- cur-y node-y)))
              ((equal mode :cursor-resize)
               (multiple-value-bind (box-x box-y box-width box-height)
                   (xdg-surface-get-geometry (wlr-xdg-toplevel toplevel))
                 (destructuring-bind (edge-top-p edge-bottom-p edge-left-p edge-right-p)
                     edges
                   (declare (ignore edge-top-p))
                   (declare (ignore edge-left-p))
                   (setf border-x (+ node-x box-x (if edge-right-p box-width 0)))
                   (setf border-y (+ node-y box-y (if edge-bottom-p box-height 0))))
                 (setf (grabbed-x *server*) (- cur-x border-x))
                 (setf (grabbed-y *server*) (- cur-y border-y))
                 (setf (grabbed-box *server*)
                       (list (+ box-x node-x)
                             (+ box-y node-y)
                             box-width
                             box-height))
                 (setf (resize-edges *server*) edges))))))))


(cffi:defcallback handle-xdg-toplevel-request-move :void ((listener :pointer) (data :pointer))
  ;; Event is raised when a toplevel would like to begin an interactive move, typically
  ;; when the toplevel-side decorations are clicked.
  (declare (ignore data))
  (format t "into: handle-xdg-toplevel-request-move: ~a~%" listener)
  (let ((toplevel (listener->class-instance listener)))
    (when (equal (button-state *server*) :pressed)
      (begin-interactive toplevel :cursor-move '(nil nil nil nil))))
  (format t "leaving: handle-xdg-toplevel-request-move:~%"))


(defun calc-edges (edges-num)
  (list
   (= (logand (ash 1 0) edges-num) (ash 1 0))
   (= (logand (ash 1 1) edges-num) (ash 1 1))
   (= (logand (ash 1 2) edges-num) (ash 1 2))
   (= (logand (ash 1 3) edges-num) (ash 1 3))))

(cffi:defcallback handle-xdg-toplevel-request-resize :void ((listener :pointer) (event :pointer))
  ;; Event is raised when a toplevel would like to begin an interactive resize, typically
  ;; when the toplevel-side decorations are clicked.
  (format t "into: handle-xdg-toplevel-request-resize: ~a, ~a~%" listener (button-state *server*))
  (let* ((toplevel (listener->class-instance listener))
         (edges-num (%get-edges event))
         (edges (calc-edges edges-num)))
    (when (equal (button-state *server*) :pressed)
      (begin-interactive toplevel :cursor-resize edges)))
  (format t "leaving: handle-xdg-toplevel-request-resize~%"))

(cffi:defcallback handle-xdg-toplevel-request-maximize :void ((listener :pointer) (event :pointer))
  ;; Send empty reply.
  (declare (ignore event))
  (format t "into: handle-xdg-toplevel-request-maximize: ~a~%" listener)
  (let* ((toplevel (listener->class-instance listener)))
    (%xdg-surface-schedule-configure (wlr-xdg-toplevel toplevel)))
  (format t "leaving: handle-xdg-toplevel-request-maximize~%"))

(cffi:defcallback handle-xdg-toplevel-request-fullscreen :void ((listener :pointer) (event :pointer))
  ;; Send empty reply.
  (declare (ignore event))
  (format t "into: handle-xdg-toplevel-request-fullscreen: ~a~%" listener)
  (let* ((toplevel (listener->class-instance listener)))
    (%xdg-surface-schedule-configure (wlr-xdg-toplevel toplevel)))
  (format t "leaving: handle-xdg-toplevel-request-fullscreen~%"))

(cffi:defcallback handle-new-xdg-surface :void ((listener :pointer) (xdg-surface :pointer))
  (declare (ignore listener))
  (format t "into: handle-new-xdg-surface: ~a~%" xdg-surface)
  (block leave
    (when (= (%xdg-surface-role xdg-surface) (cffi:foreign-enum-value 'wlr-xdg-surface-role :popup))
      ;; We must add xdg popups to the scene graph so they get rendered. The
      ;; wlroots scene graph provides a helper for this, but to use it we must
      ;; provide the proper parent scene node of the xdg popup. To enable this,
      ;; we always set the user data field of xdg_surfaces to the corresponding
      ;; scene node.
      (%add-popup-scene-graph xdg-surface)
      (return-from leave))

    (when (not (= (%xdg-surface-role xdg-surface) (cffi:foreign-enum-value 'wlr-xdg-surface-role :toplevel)))
      (error "Surface is not a toplevel"))

    (let* ((wlr-xdg-toplevel (%xdg-surface-toplevel xdg-surface))
           (toplevel (make-instance 'cw-toplevel
                                    :wlr-xdg-toplevel wlr-xdg-toplevel
                                    :wlr-scene-tree (%scene-xdg-surface-create
                                                     (wlr-scene *server*) wlr-xdg-toplevel))))

      (%set-xdg-surface-data xdg-surface (wlr-scene-tree toplevel))
      ;; put the xdg-toplevel in the node data of the tree-toplevel
      ;; we'll use this to lookup the toplevel from the xdg-toplevel
      (%set-scene-tree-node-data (wlr-scene-tree toplevel) wlr-xdg-toplevel)

      ;; The new toplevel is not added to the list of toplevels here, but when it is mapped.

      (push (cons (surface-map-listener toplevel) toplevel) *listener-alist*)
      (%signal-add-xdg-surface-map (surface-map-listener toplevel)
                                   xdg-surface
                                   (cffi:callback handle-xdg-toplevel-map))

      (push (cons (surface-unmap-listener toplevel) toplevel) *listener-alist*)
      (%signal-add-xdg-surface-unmap (surface-unmap-listener toplevel)
                                     xdg-surface
                                     (cffi:callback handle-xdg-toplevel-unmap))

      (push (cons (surface-destroy-listener toplevel) toplevel) *listener-alist*)
      (%signal-add-xdg-surface-destroy (surface-destroy-listener toplevel)
                                       xdg-surface
                                       (cffi:callback handle-xdg-toplevel-destroy))

      ;; TODO: need to add maximize and fullscreen.
      (push (cons (request-move-listener toplevel) toplevel) *listener-alist*)
      (%signal-add-toplevel-request-move (request-move-listener toplevel)
                                         wlr-xdg-toplevel
                                         (cffi:callback handle-xdg-toplevel-request-move))

      (push (cons (request-resize-listener toplevel) toplevel) *listener-alist*)
      (%signal-add-toplevel-request-resize (request-resize-listener toplevel)
                                           wlr-xdg-toplevel
                                           (cffi:callback handle-xdg-toplevel-request-resize))))
  (format t "leaving: handle-new-xdg-surface~%"))

;; -------------------- main --------------------

(defun init-backend (backend)
  (push (cons (new-input-listener backend) backend) *listener-alist*)
  (%signal-add-new-input (new-input-listener backend)
                         (wlr-backend backend)
                         (cffi:callback handle-new-input))

  (push (cons (new-output-listener backend) backend) *listener-alist*)
  (%signal-add-new-output (new-output-listener backend)
                          (wlr-backend backend)
                          (cffi:callback handle-new-output)))

(defun init-seat (seat)
  (push (cons (request-set-selection-listener seat) seat) *listener-alist*)
  (%signal-add-request-set-selection (request-set-selection-listener seat)
                                     (wlr-seat seat)
                                     (cffi:callback handle-seat-request-set-selection))
  (push (cons (request-cursor-listener seat) seat) *listener-alist*)
  (%signal-add-request-cursor (request-cursor-listener seat)
                              (wlr-seat seat)
                              (cffi:callback handle-seat-request-cursor)))

(defun init-cursor (cursor)
  (with-slots (cursor-motion-listener
               cursor-motion-absolute-listener
               cursor-frame-listener
               cursor-axis-listener
               cursor-button-listener
               wlr-cursor
               wlr-cursor-mgr) cursor
    (push (cons cursor-motion-listener cursor) *listener-alist*)
    (%signal-add-cursor-motion cursor-motion-listener
                               wlr-cursor
                               (cffi:callback handle-cursor-motion))
    (push (cons cursor-motion-absolute-listener cursor) *listener-alist*)
    (%signal-add-cursor-motion-absolute cursor-motion-absolute-listener
                                        wlr-cursor
                                        (cffi:callback handle-cursor-motion-absolute))
    (push (cons cursor-frame-listener cursor) *listener-alist*)
    (%signal-add-cursor-frame cursor-frame-listener
                              wlr-cursor
                              (cffi:callback handle-cursor-frame))
    (push (cons cursor-axis-listener cursor) *listener-alist*)
    (%signal-add-cursor-axis cursor-axis-listener
                             wlr-cursor
                             (cffi:callback handle-cursor-axis))
    (push (cons cursor-button-listener cursor) *listener-alist*)
    (%signal-add-cursor-button cursor-button-listener
                               wlr-cursor
                               (cffi:callback handle-cursor-button))))


(defun init-xdg-shell (xdg-shell)
  (push (cons (new-xdg-surface-listener xdg-shell) xdg-shell) *listener-alist*)
  (%signal-add-new-xdg-surface (new-xdg-surface-listener xdg-shell)
                               (wlr-xdg-shell xdg-shell)
                               (cffi:callback handle-new-xdg-surface)))


(defun main ()
  (let* ((wl-display (%wlr-display-create))
         (wlr-backend (backend-autocreate wl-display))
         (wlr-renderer (renderer-autocreate wlr-backend wl-display))
         (wlr-allocator (allocator-autocreate wlr-backend wlr-renderer))
         (wlr-output-layout (%wlr-output-layout-create)))
    (format t "before make-instance cw-server~%")
    (setf *server* (make-instance
                    'cw-server
                    :wl-display wl-display
                    :backend (make-instance 'cw-backend
                                            :wlr-backend wlr-backend)
                    :wlr-renderer wlr-renderer
                    :wlr-allocator wlr-allocator
                    :wlr-output-layout wlr-output-layout
                    :wlr-scene (%wlr-scene-create)
                    :xdg-shell (make-instance 'cw-xdg-shell
                                              :wlr-xdg-shell (%wlr-xdg-shell-create wl-display 3))
                    :cursor (make-instance 'cw-cursor
                                           :wlr-cursor (cursor-create-attach wlr-output-layout)
                                           :wlr-xcursor-mgr (%wlr-xcursor-manager-create
                                                             (cffi:null-pointer)
                                                             24))
                    :seat (make-instance 'cw-seat
                                         :wlr-seat (%wlr-seat-create wl-display "seat0"))))
    (format t "after make-instance cw-server~%")
    (%wlr-compositor-create wl-display 5 wlr-renderer)
    (format t "check 1~%")

    (%wlr-subcompositor-create wl-display)
    (format t "check 2~%")
    (%wlr-data-device-manager-create wl-display)
    (format t "check 3~%")
    (setf (wlr-scene-output-layout *server*)
          (%wlr-scene-attach-output-layout (wlr-scene *server*)
                                           (wlr-output-layout *server*)))

    (init-backend (backend *server*))
    (format t "check 4~%")
    (init-seat (seat *server*))
    (format t "check 5~%")
    (init-cursor (cursor *server*))
    (format t "check 6~%")
    (init-xdg-shell (xdg-shell *server*))
    (format t "check 7~%")

    (setup-keybindings)
    (format t "check 8~%")

    (let ((socket (add-socket-auto wl-display wlr-backend)))
      (format t "check 9~%")
      (start-backend (wlr-backend (backend *server*)) wl-display)
      (format t "check 10~%")
      (format t "socket: ~a~%" socket)
      (osicat-posix:setenv "WAYLAND_DISPLAY" socket))
    (format t "check 11~%")

    (%wl-display-run (wl-display *server*))
    (format t "check 12~%")

    ;; Once wl-display-run returns, we shut down the server.

    (%wl-display-destroy-clients (wl-display *server*))
    (%scene-node-destroy (wlr-scene *server*))
    (%wlr-xcursor-manager-destroy (wlr-xcursor-mgr *server*))
    (%wlr-output-layout-destroy (wlr-output-layout *server*))
    (%wl-display-destroy (wl-display *server*))))
