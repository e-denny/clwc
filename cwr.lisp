;;; -*- Mode: Lisp -*-

(in-package :clwc)

(pushnew #P"/home/edgar/common-lisp/clwc/lib/" *foreign-library-directories*
         :test #'equal)

(define-foreign-library clwc
    (:unix (:or "clwc.so"))
  (t (:default "clwc")))

(use-foreign-library clwc)

;; --------------------------------------------------

(defcfun ("make_listener" %make-listener) :pointer)

;; --------------------------------------------------
;; enums
;; --------------------------------------------------

(defcenum wlr-output-transform
  :normal
  :transform-90
  :transform-180
  )

(defcenum wlr-input-device-type
  :keyboard
  :pointer-device
  :touch
  :tool
  :pad
  :switch
  )

(defcenum wlr-scene-node-type
  :tree
  :rect
  :buffer)

(defcenum wlr-xdg-surface-role
  :none
  :toplevel
  :popup)

(defcenum wlr-button-state
  :released
  :pressed)

;; --------------------------------------------------
;; input
;; --------------------------------------------------

(defcfun ("device_type" %device-type) :int
  (device :pointer))

(defcfun "set_seat_capabilities" :void
  (seat :pointer)
  (device-type :int))

(defcfun ("signal_add_new_input" %signal-add-new-input) :void
  (wl-listener :pointer)
  (wlr-backend :pointer)
  (handler-func :pointer))



;; --------------------------------------------------
;; output
;; --------------------------------------------------

(defcfun ("wlr_output_layout_destroy" %wlr-output-layout-destroy) :void
  (wlr-output-layout :pointer))

(defcfun "render_output" :void
  (wlr-output :pointer)
  (wlr-renderer :pointer)
  (color :pointer))

(defcfun ("signal_add_output_frame" %signal-add-output-frame) :void
  (wl-listener :pointer)
  (wlr-output :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_output_destroy" %signal-add-output-destroy) :void
  (wl-listener :pointer)
  (wlr-output :pointer)
  (handler-func :pointer))

(defcfun "set_output_rules" :void
  (wlr-output :pointer)
  (scale :float)
  (transform :int))

(defcfun "enable_output" :void
  (wlr-output :pointer)
  (wlr-allocator :pointer)
  (wlr-render :pointer)
  (wlr-output-layout :pointer))

(defcfun ("signal_add_new_output" %signal-add-new-output) :void
  (wl-listener :pointer)
  (wlr-backend :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_output_request_state" %signal-add-output-request-state) :void
  (wl-listener :pointer)
  (wlr-backend :pointer)
  (handler-func :pointer))

(defcfun ("wlr_output_init_render" %wlr-output-init-render) :bool
  (wlr-output :pointer)
  (wlr-allocator :pointer)
  (wlr-renderer :pointer))

(defcfun ("has_output_modes" %has-output-modes) :int
  (wlr-output :pointer))

(defun has-output-modes (wlr-output)
  (if (= (%has-output-modes wlr-output) 0)
      t
      nil))

(defcfun ("wlr_output_preferred_mode" %wlr-output-preferred-mode) :pointer
  (wlr-output :pointer))

(defcfun ("wlr_output_set_mode" %wlr-output-set-mode) :void
  (wlr-output :pointer)
  (wlr-output-mode :pointer))

(defcfun ("wlr_output_enable" %wlr-output-enable) :void
  (wlr-output :pointer)
  (enable :boolean))

(defcfun ("wlr_output_commit" %wlr-output-commit) :boolean
  (wlr-output :pointer))

(defcfun ("wlr_output_layout_add_auto" %wlr-output-layout-add-auto) :pointer
  (wlr-output-layout :pointer)
  (wlr-output :pointer))

(defcfun ("output_commit_state" output-commit-state) :void
  (wlr-output :pointer)
  (event :pointer))

(defcfun ("wlr_output_state_init" %wlr-output-state-init) :void
  (state :pointer))

(defcfun ("wlr_output_state_set_enabled" %wlr-output-state-set-enabled) :void
  (state :pointer)
  (enabled :boolean))

(defcfun ("wlr_output_state_set_mode" %wlr-output-state-set-mode) :void
  (wlr-output-state :pointer)
  (wlr-output-mode :pointer))

(defcfun ("wlr_output_commit_state" %wlr-output-commit-state) :boolean
  (wlr-output :pointer)
  (wlr-output-state :pointer))

(defcfun ("wlr_output_state_finish" %wlr-output-state-finish) :void
  (wlr-output-state :pointer))

;; --------------------------------------------------
;; keyboard
;; --------------------------------------------------

(defcfun ("keyboard_key_state" %keyboard-key-state) :int
  (wlr-event-keyboard-key :pointer))

(defcfun ("get_keyboard_modifiers" %get-keyboard-modifiers) :pointer
  (wlr_keyboard :pointer))

(defcfun ("get_keyboard_keys" %get-keyboard-keys) :pointer
  (wlr_keyboard :pointer)
  (wlr_event-keyboard-key :pointer))

(defcfun ("seat_keyboard_notify_key" %seat-keyboard-notify-key) :void
  (wlr-seat :pointer)
  (wlr-keyboard :pointer)
  (wlr-event-keyboard-key :pointer))

(defcfun ("seat_keyboard_notify_modifiers" %seat-keyboard-notify-modifiers) :void
  (wlr-seat :pointer)
  (wlr-keyboard :pointer))

(defcfun ("wlr_keyboard_from_input_device" %wlr-keyboard-from-input-device) :pointer
  (wlr-input-device :pointer))

(defcfun ("setup_keyboard" %setup-keyboard) :void
  (wlr-keyboard :pointer))

(defcfun ("signal_add_keyboard_key" %signal-add-keyboard-key) :void
  (wl-listener :pointer)
  (wlr-keyboard :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_keyboard_modifiers" %signal-add-keyboard-modifiers) :void
  (wl-listener :pointer)
  (wlr-keyboard :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_keyboard_destroy" %signal-add-keyboard-destroy) :void
  (wl-listener :pointer)
  (wlr-input-device :pointer)
  (handler-func :pointer))


;; --------------------------------------------------
;; cursor
;; --------------------------------------------------


(defcfun ("wlr_xcursor_manager_destroy" %wlr-xcursor-manager-destroy) :void
  (wlr-cursor-manager :pointer))

(defcfun ("wlr_cursor_create" %wlr-cursor-create) :pointer)

(defcfun ("wlr_xcursor_manager_create" %wlr-xcursor-manager-create) :pointer
  (name :pointer)
  (size :uint32))

(defcfun ("wlr_cursor_attach_input_device" %wlr-cursor-attach-input-device) :void
  (wlr-cursor :pointer)
  (wlr-input-device :pointer))

(defcfun ("signal_add_cursor_motion" %signal-add-cursor-motion) :void
  (wl-listener :pointer)
  (wlr-cursor :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_cursor_motion_absolute" %signal-add-cursor-motion-absolute) :void
  (wl-listener :pointer)
  (wlr-cursor :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_cursor_frame" %signal-add-cursor-frame) :void
  (wl-listener :pointer)
  (wlr-cursor :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_cursor_button" %signal-add-cursor-button) :void
  (wl-listener :pointer)
  (wlr-cursor :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_cursor_axis" %signal-add-cursor-axis) :void
  (wl-listener :pointer)
  (wlr-cursor :pointer)
  (handler-func :pointer))

(defcfun ("cursor_move" %cursor-move) :void
  (wlr-cursor :pointer)
  (wlr-pointer-motion-event :pointer))

(defcfun ("cursor_warp_absolute" %cursor-warp-absolute) :void
  (wlr-cursor :pointer)
  (wlr-pointer-motion-absolute-event :pointer))


(defcfun ("output_at_cursor" %output-at-cursor) :pointer
  (wlr-output-layout :pointer)
  (wlr-cursor :pointer))

(defcfun ("warp_cursor_coordinates" %warp-cursor-coordinates) :void
  (wlr-cursor :pointer))


(defcfun ("wlr_cursor_set_xcursor" %wlr-cursor-set-xcursor) :pointer
  (cursor :pointer)
  (wlr-xcursor-manager :pointer)
  (name :string))

(defcfun ("wlr_cursor_attach_output_layout" %wlr-cursor-attach-output-layout) :void
  (wlr-cursor :pointer)
  (wlr-output-layout :pointer))

(defcfun ("cursor_set_surface" %cursor-set-surface) :pointer
  (wlr-cursor :pointer)
  (wlr-seat-pointer-request-set-cursor-event :pointer))

(defun cursor-coords (cursor)
  (let ((x (%get-cursor-x cursor))
        (y (%get-cursor-y cursor)))
    (values x y)))

(defcfun ("get_cursor_x" %get-cursor-x) :double
  (wlr-cursor :pointer))

(defcfun ("get_cursor_y" %get-cursor-y)   :double
  (wlr-cursor :pointer))

(defcfun ("get_time_event_pointer_motion" %get-time-event-pointer-motion) :uint32
  (wlr-pointer-motion-event :pointer))

(defcfun ("get_time_event_pointer_motion_absolute" %get-time-event-pointer-motion-absolute) :uint32
  (wlr-pointer-motion-absolute-event :pointer))

(defcfun ("get_button_state" %get-button-state) :int
  (wlr-pointer-button-event :pointer))

(defun cursor-create-attach (wlr-output-layout)
  (let ((wlr-cursor (%wlr-cursor-create)))
    (%wlr-cursor-attach-output-layout wlr-cursor wlr-output-layout)
    wlr-cursor))

(defcfun ("scene_node_destroy" %scene-node-destroy) :void
  (wlr-scene :pointer))

;; --------------------------------------------------
;; wlr-surface
;; --------------------------------------------------

(defcfun ("wlr_surface_get_root_surface" %wlr-surface-get-root-surface) :pointer
  (wlr-surface :pointer))

;; --------------------------------------------------
;; seat
;; --------------------------------------------------

(defcfun ("seat_pointer_notify_button" %seat-pointer-notify-button) :void
  (wlr-seat :pointer)
  (wlr-pointer-button-event :pointer))

(defcfun ("seat_pointer_notify_axis" %seat-pointer-notify-axis) :void
  (wlr-seat :pointer)
  (wlr-pointer-axis-event :pointer))

(defcfun ("get_seat_focused_surface" %get-seat-focused-surface) :pointer
  (wlr-seat :pointer))

(defcfun ("get_seat_focused_client" %get-seat-focused-client) :pointer
  (wlr-seat-client :pointer))

(defcfun ("get_seat_client" %get-seat-client) :pointer
  (wlr-seat-client :pointer))

(defcfun ("wlr_seat_get_keyboard" %wlr-seat-get-keyboard) :pointer
  (wlr-seat :pointer))

(defcfun ("seat_keyboard_notify_enter" %seat-keyboard-notify-enter) :void
  (wlr-seat :pointer)
  (wlr-xdg-toplevel :pointer)
  (wlr-keyboard :pointer))

(defcfun ("wlr_seat_pointer_notify_motion" %wlr-seat-pointer-notify-motion) :void
  (wlr-seat :pointer)
  (time :uint32)
  (sx :double)
  (sy :double))

(defcfun ("wlr_seat_pointer_notify_enter" %wlr-seat-pointer-notify-enter) :void
  (wlr-seat :pointer)
  (wlr-surface :pointer)
  (sx :double)
  (sy :double))

(defcfun ("wlr_seat_set_keyboard" %wlr-seat-set-keyboard) :void
  (wlr-seat :pointer)
  (wlr-keyboard :pointer))

(defcfun ("set_seat_capabilities" %set-seat-capabilities) :void
  (wlr-seat :pointer)
  (has-keyboard :int))

(defcfun ("wlr_seat_pointer_notify_frame" %wlr-seat-pointer-notify-frame) :void
  (wlr-seat :pointer))

(defcfun ("set_seat_selection" %set-seat-selection) :void
  (wlr-seat :pointer)
  (wlr-seat-request-set-selection-event :pointer))

(defcfun ("signal_add_request_set_selection" %signal-add-request-set-selection) :void
  (wl-listener :pointer)
  (wlr-seat :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_request_cursor" %signal-add-request-cursor) :void
  (wl-listener :pointer)
  (wlr-seat :pointer)
  (handler-func :pointer))

(defcfun ("wlr_seat_pointer_clear_focus" %wlr-seat-pointer-clear-focus) :void
  (wlr-sear :pointer))

;; --------------------------------------------------
;; box
;; --------------------------------------------------

(cffi:defcstruct wlr-box
  "A box representing a rectangle region in a 2D space.

The x and y coordinates are inclusive, and the width and height lengths are
exclusive. In other words, the box starts from the coordinates (x, y), and
goes up to but not including (x + width, y + height)."
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(cobj:define-cobject-class (wlr-box (:struct wlr-box)))

(defun box-coords (wlr-box)
  (let ((x (%get-box-x wlr-box))
        (y (%get-box-y wlr-box))
        (width (%get-box-width wlr-box))
        (height (%get-box-height wlr-box)))
    (values x y width height)))

(defcfun ("get_box_x" %get-box-x) :int
  (wlr-box :pointer))

(defcfun ("get_box_y" %get-box-y) :int
  (wlr-box :pointer))

(defcfun ("get_box_width" %get-box-width) :int
  (wlr-box :pointer))

(defcfun ("get_box_height" %get-box-height) :int
  (wlr-box :pointer))

(defcfun ("make_box" %make-box) :pointer)

;; --------------------------------------------------
;; scene
;; --------------------------------------------------

(defun scene-node-at (scene cursor)
  (cffi:with-foreign-objects ((sx :double)
                              (sy :double))
    (let ((node (%scene-node-at scene cursor sx sy)))
      (values node (cffi:mem-ref sx :double) (cffi:mem-ref sy :double)))))

(defcfun ("scene_node_at" %scene-node-at) :pointer
  (wlr-scene :pointer)
  (wlr-cursor :pointer)
  (sx :pointer)
  (sy :pointer))

(defcfun ("get_node_type" %get-node-type) :int
  (wlr-scene-node :pointer))

(defcfun ("wlr_scene_buffer_from_node" %wlr-scene-buffer-from-node) :pointer
  (wlr-scene-node :pointer))

(defcfun ("wlr_scene_surface_try_from_buffer" %wlr-scene-surface-try-from-buffer) :pointer
  (wlr-scene-buffer :pointer))

(defcfun ("get_scene_surface_wlr_surface" %get-scene-surface-wlr-surface) :pointer
  (wlr-scene-surface :pointer))

(defcfun ("get_root_toplevel" %get-root-toplevel) :pointer
  (wlr-scene-node :pointer))

(defcfun ("scene_tree_set_position" %scene-tree-set-position) :void
  (wlr-scene-tree :pointer)
  (x :int)
  (y :int))

(defcfun ("wlr_scene_node_raise_to_top" %wlr-scene-node-raise-to-top) :void
  (wlr-scene-node :pointer))

(defcfun ("wlr_scene_get_scene_output" %wlr-scene-get-scene-output) :pointer
  (wlr-scene :pointer)
  (wlr-output :pointer))

(defcfun ("wlr_scene_output_commit" %wlr-scene-output-commit) :void
  (wlr-scene-output :pointer)
  (wlr-scene-output-state-options :pointer))

(defcfun ("wlr_scene_output_send_frame_done" %wlr-secen-output-send-frame-done) :void
  (wlr-scene-node :pointer)
  (wlr-scene-output :pointer))

(defcfun ("scene_output_send_frame_done" %scene-output-send-frame-done) :void
  (wlr-scene-output :pointer)
  (sec :long)
  (nsec :long))

(defcfun ("scene_xdg_surface_create" %scene-xdg-surface-create) :pointer
  (wlr-scene-tree :pointer)
  (wlr-xdg-toplevel :pointer))

(defcfun ("set_scene_tree_node_data" %set-scene-tree-node-data) :void
  (wlr-scene-tree :pointer)
  (wlr-xdg-toplevel :pointer))

(defcfun ("wlr_scene_create" %wlr-scene-create) :pointer)

(defcfun ("wlr_scene_attach_output_layout" %wlr-scene-attach-output-layout) :pointer
  (wlr-scene :pointer)
  (wlr-output-layout :pointer))

(defcfun ("wlr_scene_output_create" %wlr-scene-output-create) :pointer
  (wlr-scene :pointer)
  (wlr-output :pointer))

(defcfun ("wlr_scene_output_layout_add_output" %wlr-scene-output-layout-add-output) :pointer
  (wlr-scene-output-layout :pointer)
  (wlr-output-layout-output :pointer)
  (wlr-scene-output :pointer))

;; --------------------------------------------------
;; xdg_surface
;; --------------------------------------------------

(defcfun ("wlr_xdg_surface_try_from_wlr_surface" %wlr-xdg-surface-try-from-wlr-surface) :pointer
  (wlr-seat :pointer))


(defun xdg-surface-get-geometry (wlr-xdg-toplevel)
  (let ((wlr-box (%make-box)))
    (%xdg-surface-get-geometry wlr-xdg-toplevel wlr-box)
    (let ((x (%get-box-x wlr-box))
          (y (%get-box-y wlr-box))
          (width (%get-box-width wlr-box))
          (height (%get-box-height wlr-box)))
      (cffi:foreign-free wlr-box)
      (values x y width height))))

(defcfun ("xdg_surface_get_geometry" %xdg-surface-get-geometry) :pointer
  (wlr-xdg-toplevel :pointer)
  (wlr-box :pointer))

(defcfun ("xdg_surface_role" %xdg-surface-role) :int
  (wlr-xdg-surface :pointer))

(defcfun ("set_xdg_surface_data" %set-xdg-surface-data) :void
  (wlr-xdg-surface :pointer)
  (wlr-scene-tree :pointer))

(defcfun ("signal_add_new_xdg_surface" %signal-add-new-xdg-surface) :void
  (wl-listener :pointer)
  (wlr-xdg-shell :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_xdg_surface_map" %signal-add-xdg-surface-map) :void
  (wl-listener :pointer)
  (wlr-xdg-surface :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_xdg_surface_unmap" %signal-add-xdg-surface-unmap) :void
  (wl-listener :pointer)
  (wlr-xdg-surface :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_xdg_surface_destroy" %signal-add-xdg-surface-destroy) :void
  (wl-listener :pointer)
  (wlr-xdg-surface :pointer)
  (handler-func :pointer))

;; --------------------------------------------------
;; xdg_toplevel
;; --------------------------------------------------

;; An xdg-shell surface is a wl_surface wrapped twice - once
;; in a xdg_surface and then again in a xdg_toplevel or xdg_popup

;; TODO: create a bool type and an unsigned int
(defcfun ("wlr_xdg_toplevel_activated" %wlr-xdg-toplevel-activated) :uint32
  (wlr-xdg-toplevel :pointer)
  (activated :bool))

(defcfun ("wlr_xdg_toplevel_set_size" %wlr-xdg-toplevel-set-size) :uint32
  (wlr-xdg-toplevel :pointer)
  (width :uint32)
  (heigth :uint32))

(defcfun ("wlr_xdg_toplevel_set_activated" %wlr-xdg-toplevel-set-activated) :pointer
  (wlr-xdg-surface :pointer)
  (activated :bool))

(defun activate-toplevel (wlr-xdg-surface activate-p)
  (%wlr-xdg-toplevel-set-activated wlr-xdg-surface (convert-to-foreign activate-p :boolean)))

(defcfun ("xdg_toplevel_get_surface" %xdg-toplevel-get-surface) :pointer
  (wlr-xdg_toplevel :pointer))


(defcfun ("add_popup_scene_graph" %add-popup-scene-graph) :void
  (wlr-xdg-surface :pointer))

(defcfun ("xdg_surface_toplevel" %xdg-surface-toplevel) :pointer
  (wlr-xdg-surface :pointer))

(defcfun ("signal_add_toplevel_request_move" %signal-add-toplevel-request-move) :void
  (wl-listener :pointer)
  (wlr-xdg-toplevel :pointer)
  (handler-func :pointer))

(defcfun ("signal_add_toplevel_request_resize" %signal-add-toplevel-request-resize) :void
  (wl-listener :pointer)
  (wlr-xdg-toplevel :pointer)
  (handler-func :pointer))

(defcfun ("get_scene_tree_node_x" %get-scene-tree-node-x) :int
  (wlr-scene-tree :pointer))

(defcfun ("get_scene_tree_node_y" %get-scene-tree-node-y) :int
  (wlr-scene-tree :pointer))

(defcfun ("xdg_surface_schedule_configure" %xdg-surface-schedule-configure) :uint32
  (wlr-xdg-toplevel :pointer))

;; --------------------------------------------------
;; resize edges
;; --------------------------------------------------

(defcfun ("get_edges" %get-edges) :int
  (wlr-xdg-toplevel-resize-event :pointer))

(defun get-resize-event-edges (event)
  (let ((resize-array (%get-resize-event-edges event))
        result)
    (setf result
          (loop for i from 0 below 4
                collect (if (= (cffi:mem-ref resize-array :int i) 1)
                            t
                            nil)))
    (cffi:foreign-free resize-array)
    result))

(defcfun ("get_resize_event_edges" %get-resize-event-edges) :pointer
  (wlr-xdg-toplevel-resize-event :pointer))

;; --------------------------------------------------
;; server
;; --------------------------------------------------

(defcfun ("wl_display_create" %wlr-display-create) :pointer)


(defcfun ("wlr_backend_autocreate" %wlr-backend-autocreate) :pointer
  (wl-display :pointer)
  (session-pointer :pointer))

(defun backend-autocreate (wl-display)
  (let ((wlr-backend (%wlr-backend-autocreate wl-display (cffi:null-pointer))))
    (when (cffi:null-pointer-p wlr-backend)
      (error "Could not create: backend"))
    wlr-backend))

(defcfun ("wlr_renderer_autocreate" %wlr-renderer-autocreate) :pointer
  (wlr-renderer :pointer))

(defcfun ("wlr_renderer_init_wl_display" %wlr-renderer-init-wl-display) :void
  (wlr-renderer :pointer)
  (wl-display :pointer))

(defun renderer-autocreate (wlr-backend wl-display)
  (let ((wlr-renderer (%wlr-renderer-autocreate wlr-backend)))
    (when (cffi:null-pointer-p wlr-renderer)
      (error "Could not create: renderer"))
    (%wlr-renderer-init-wl-display wlr-renderer wl-display)
    wlr-renderer))

(defcfun ("wlr_allocator_autocreate" %wlr-allocator-autocreate) :pointer
  (wlr-backend :pointer)
  (wlr-renderer :pointer))

(defun allocator-autocreate (wlr-backend wlr-renderer)
  (let ((wlr-allocator (%wlr-allocator-autocreate wlr-backend wlr-renderer)))
    (when (cffi:null-pointer-p wlr-allocator)
      (error "Could not create: allocator"))
    wlr-allocator))


(defcfun ("wlr_seat_create" %wlr-seat-create) :pointer
  (display :pointer)
  (name :string))


(defcfun ("wlr_output_layout_create" %wlr-output-layout-create) :pointer)

(defcfun ("wlr_compositor_create" %wlr-compositor-create) :pointer
  (wl-display :pointer)
  (version :uint32)
  (wlr-renderer :pointer))

(defcfun ("wlr_subcompositor_create" %wlr-subcompositor-create) :pointer
  (wl-display :pointer))

(defcfun ("wlr_data_device_manager_create" %wlr-data-device-manager-create) :pointer
  (wl-display :pointer))

(defcfun ("wlr_xdg_shell_create" %wlr-xdg-shell-create) :pointer
  (wl-display :pointer)
  (version :uint32))

(defcfun ("wlr_backend_destroy" %wlr-backend-destroy) :pointer
  (wlr-backend :pointer))

(defcfun ("wl_display_add_socket_auto" %wl-display-add-socket-auto) :pointer
  (wl-display :pointer))

(defun add-socket-auto (wl-display wlr-backend)
  (let ((socket (%wl-display-add-socket-auto wl-display)))
    (when (cffi:null-pointer-p socket)
      (%wlr-backend-destroy wlr-backend)
      (error "Could not create: socket"))
    socket))

(defcfun ("wlr_backend_start" %wlr-backend-start) :bool
  (wlr-backend :pointer))

(defcfun ("wl_display_destroy" %wl-display-destroy) :void
  (wl-display :pointer))

(defun start-backend (wlr-backend wl-display)
  (unless (%wlr-backend-start wlr-backend)
    (%wlr-backend-destroy wlr-backend)
    (%wl-display-destroy wl-display)
    (error "Could not start: backend")))

(defcfun ("wl_display_run" %wl-display-run) :void
  (display :pointer))

(defcfun ("wl_display_destroy_clients" %wl-display-destroy-clients) :void
  (wl-display :pointer))

(defcfun ("wl_display_terminate" %wl-display-terminate) :void
  (wl-display :pointer))
