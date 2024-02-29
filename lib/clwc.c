/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <getopt.h>
#include <linux/input-event-codes.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/render/allocator.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_data_control_v1.h>
#include <wlr/types/wlr_export_dmabuf_v1.h>
#include <wlr/types/wlr_gamma_control_v1.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_matrix.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_primary_selection.h>
#include <wlr/types/wlr_primary_selection_v1.h>
#include <wlr/types/wlr_screencopy_v1.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_subcompositor.h>
#include <wlr/types/wlr_viewporter.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_output_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>

// My functions --------------

void run(char *command);
// void setup_cursor(struct wlr_cursor *cursor, struct wlr_xcursor_manager *cursor_mgr);
int device_type(struct wlr_input_device *device);
void set_seat_capabilities(struct wlr_seat *seat, int has_keyboard);
void create_pointer(struct wlr_input_device *device);
void create_keyboard(struct wlr_input_device *device);
void render_output(struct wlr_output *output, struct wlr_renderer *renderer, float *color);
void sigchld(int unused);
struct wlr_output *get_output_at_point(struct wlr_output_layout *output_layout,
                                       struct wlr_cursor *cursor);
void setup_keyboard(struct wlr_keyboard *keyboard);
void set_output_rules(struct wlr_output *output, float scale, int transform);
void enable_output(struct wlr_output *output, struct wlr_allocator *alloc,
                   struct wlr_renderer *renderer, struct wlr_output_layout *output_layout);
void setup_init(struct wl_display *display, struct wlr_cursor *cursor,
                struct wlr_output_layout *layout);
void signal_add_new_input(struct wl_listener *listener,
                           struct wlr_backend *backend,
                           void (*handler_func)(struct wl_listener*, void*));
void signal_add_new_output(struct wl_listener *listener,
                           struct wlr_backend *backend,
                           void (*handler_func)(struct wl_listener*, void*));
void signal_add_output_frame(struct wl_listener *listener,
                             struct wlr_output *output,
                             void (*handler_func)(struct wl_listener*, void*));
void signal_add_output_destroy(struct wl_listener *listener,
                               struct wlr_output *output,
                               void (*handler_func)(struct wl_listener*, void*));
void signal_add_output_request_state(struct wl_listener *listener,
                                     struct wlr_output *output,
                                     void (*handler_func)(struct wl_listener*, void*));
void signal_add_keyboard_key(struct wl_listener *listener,
                             struct wlr_keyboard *keyboard,
                             void (*handler_func)(struct wl_listener*, void*));
void signal_add_keyboard_destroy(struct wl_listener *listener,
                                 struct wlr_input_device *device,
                                 void (*handler_func)(struct wl_listener*, void*));
void signal_add_keyboard_modifiers(struct wl_listener *listener,
                                   struct wlr_keyboard *keyboard,
                                   void (*handler_func)(struct wl_listener*, void*));

void signal_add_request_set_selection(struct wl_listener *listener,
                                      struct wlr_seat *seat,
                                      void (*handler_func)(struct wl_listener*, void*));
void signal_add_request_cursor(struct wl_listener *listener,
                               struct wlr_seat *seat,
                               void (*handler_func)(struct wl_listener*, void*));

void signal_add_cursor_frame(struct wl_listener *listener,
                             struct wlr_cursor *cursor,
                             void (*handler_func)(struct wl_listener*, void*));
void signal_add_cursor_motion(struct wl_listener *listener,
                              struct wlr_cursor *cursor,
                              void (*handler_func)(struct wl_listener*, void*));
void signal_add_cursor_motion_absolute(struct wl_listener *listener,
                                       struct wlr_cursor *cursor,
                                       void (*handler_func)(struct wl_listener*, void*));
void signal_add_cursor_button(struct wl_listener *listener,
                              struct wlr_cursor *cursor,
                              void (*handler_func)(struct wl_listener*, void*));
void signal_add_cursor_axis(struct wl_listener *listener,
                            struct wlr_cursor *cursor,
                            void (*handler_func)(struct wl_listener*, void*));

void cursor_move(struct wlr_cursor *cursor, struct wlr_pointer_motion_event *event);
void cursor_warp_absolute(struct wlr_cursor *cursor, struct wlr_pointer_motion_absolute_event *event);

struct wl_listener *make_listener();
int backend_start(struct wlr_backend *backend);

int keyboard_key_state(struct wlr_keyboard_key_event *event);
char *get_keyboard_modifiers(struct wlr_keyboard *keyboard);
char *get_keyboard_keys(struct wlr_keyboard *device, struct wlr_keyboard_key_event *event);

struct wlr_output *output_at_cursor(struct wlr_output_layout *output_layout,
                                    struct wlr_cursor *cursor);

void warp_cursor_coordinates(struct wlr_cursor *cursor);
void cursor_set_surface(struct wlr_cursor *cursor,
                        struct wlr_seat_pointer_request_set_cursor_event *event);

void seat_pointer_notify_button(struct wlr_seat *seat, struct wlr_pointer_button_event *event);
void seat_pointer_notify_axis(struct wlr_seat *seat, struct wlr_pointer_axis_event *event);



struct wlr_surface *get_seat_focused_surface(struct wlr_seat *seat);
struct wlr_seat_client *get_seat_focused_client(struct wlr_seat *seat);
struct wlr_seat_client *get_seat_client(struct wlr_seat_pointer_request_set_cursor_event *event);

void seat_keyboard_notify_enter(struct wlr_seat *seat, struct wlr_xdg_toplevel *toplevel,
                                struct wlr_keyboard *keyboard);

void seat_keyboad_notify_modifiers(struct wlr_seat *seat,
                                    struct wlr_keyboard *keyboard);
void seat_keyboard_notify_key(struct wlr_seat *seat, struct wlr_keyboard *keyboard,
                             struct wlr_keyboard_key_event *event);
void set_seat_selection(struct wlr_seat *seat,
                        struct wlr_seat_request_set_selection_event *event);

struct wlr_scene_node *scene_node_at(struct wlr_scene *scene,
                                     struct wlr_cursor *cursor, double *sx, double *sy);
int get_node_type(struct wlr_scene_node *node);
struct wlr_surface *get_scene_surface_wlr_surface(struct wlr_scene_surface *scene_surface);
struct wlr_xdg_toplevel *get_root_toplevel(struct wlr_scene_node *node);
void scene_tree_set_position(struct wlr_scene_tree *scene_tree, int x, int y);
double get_cursor_x(struct wlr_cursor *cursor);
double get_cursor_y(struct wlr_cursor *cursor);

int get_box_x(struct wlr_box *box);
int get_box_y(struct wlr_box *box);
int get_box_width(struct wlr_box *box);
int get_box_height(struct wlr_box *box);
struct wlr_box *make_box();

uint32_t get_time_event_pointer_motion (struct wlr_pointer_motion_event *event);
uint32_t get_time_event_pointer_motion_absolute (struct wlr_pointer_motion_absolute_event *event);
int get_button_state(struct wlr_pointer_button_event *event);
void scene_output_send_frame_done(struct wlr_scene_output *scene_output, long sec, long nsec);
void xdg_surface_get_geometry(struct wlr_xdg_toplevel *xdg_toplevel, struct wlr_box *box);
int has_output_modes(struct wlr_output *output);
struct wlr_surface *xdg_toplevel_get_surface(struct wlr_xdg_toplevel *toplevel);
int *get_resize_event_edges(struct wlr_xdg_toplevel_resize_event *event);
int xdg_surface_role(struct wlr_xdg_surface *surface);
void add_popup_scene_graph(struct wlr_xdg_surface *surface);
struct wlr_scene_tree *scene_xdg_surface_create(struct wlr_scene *scene, struct wlr_xdg_toplevel *toplevel);
void set_scene_tree_node_data(struct wlr_scene_tree *scene_tree, struct wlr_xdg_toplevel *toplevel);
void set_xdg_surface_data(struct wlr_xdg_surface *surface, struct wlr_scene_tree *scene_tree);
struct wlr_xdg_toplevel *xdg_surface_toplevel(struct wlr_xdg_surface *surface);

void signal_add_new_xdg_surface(struct wl_listener *listener,
                                struct wlr_xdg_shell *xdg_shell,
                                void (*handler_func)(struct wl_listener*, void*));

void signal_add_xdg_surface_map(struct wl_listener *listener,
                                struct wlr_xdg_surface *xdg_surface,
                                void (*handler_func)(struct wl_listener*, void*));
void signal_add_xdg_surface_unmap(struct wl_listener *listener,
                                  struct wlr_xdg_surface *xdg_surface,
                                  void (*handler_func)(struct wl_listener*, void*));
void signal_add_xdg_surface_destroy(struct wl_listener *listener,
                                    struct wlr_xdg_surface *xdg_surface,
                                    void (*handler_func)(struct wl_listener*, void*));


void signal_add_toplevel_request_move(struct wl_listener *listener,
                                      struct wlr_xdg_toplevel *toplevel,
                                      void (*handler_func)(struct wl_listener*, void*));
void signal_add_toplevel_request_resize(struct wl_listener *listener,
                                        struct wlr_xdg_toplevel *toplevel,
                                        void (*handler_func)(struct wl_listener*, void*));
int get_edges(struct wlr_xdg_toplevel_resize_event *event);
void output_commit_state(struct wlr_output *output, struct wlr_output_event_request_state *event);
int get_scene_tree_node_x(struct wlr_scene_tree *scene_tree);
int get_scene_tree_node_y(struct wlr_scene_tree *scene_tree);
uint32_t xdg_surface_schedule_configure(struct wlr_xdg_toplevel *xdg_toplevel);
void scene_node_destroy(struct wlr_scene *scene);

// ----------------------------------------------------------------------

/* macros */
#define ERROR(fmt, ...)                            \
    do {                                           \
        fprintf(stderr, fmt "\n", ##__VA_ARGS__);  \
        exit(EXIT_FAILURE);                        \
    } while (0)
#define EERROR(fmt, ...) ERROR(fmt ": %s", ##__VA_ARGS__, strerror(errno))

// ----------------------------------------------------------------------
// xdg_toplevel
// ----------------------------------------------------------------------


void xdg_surface_get_geometry(struct wlr_xdg_toplevel *xdg_toplevel, struct wlr_box *box)
{
    wlr_xdg_surface_get_geometry(xdg_toplevel->base, box);
}

int xdg_surface_role(struct wlr_xdg_surface *surface)
{
    return surface->role;
}

void scene_node_destroy(struct wlr_scene *scene)
{
    wlr_scene_node_destroy(&scene->tree.node);
}

void add_popup_scene_graph(struct wlr_xdg_surface *surface)
{
    struct wlr_xdg_surface *parent = wlr_xdg_surface_try_from_wlr_surface(surface->popup->parent);
    struct wlr_scene_tree *parent_tree;
    assert(parent != NULL);
    parent_tree = parent->data;
    surface->data = wlr_scene_xdg_surface_create(parent_tree, surface);
}

void set_xdg_surface_data(struct wlr_xdg_surface *surface, struct wlr_scene_tree *scene_tree)
{
    surface->data = scene_tree;
}


void signal_add_xdg_surface_map(struct wl_listener *listener,
                                struct wlr_xdg_surface *xdg_surface,
                                void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&xdg_surface->surface->events.map, listener);
}

void signal_add_xdg_surface_unmap(struct wl_listener *listener,
                                struct wlr_xdg_surface *xdg_surface,
                                void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&xdg_surface->surface->events.unmap, listener);
}

void signal_add_xdg_surface_destroy(struct wl_listener *listener,
                                    struct wlr_xdg_surface *xdg_surface,
                                    void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&xdg_surface->events.destroy, listener);
}

void signal_add_toplevel_request_move(struct wl_listener *listener,
                                      struct wlr_xdg_toplevel *toplevel,
                                      void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&toplevel->events.request_move, listener);
}

void signal_add_toplevel_request_resize(struct wl_listener *listener,
                                        struct wlr_xdg_toplevel *toplevel,
                                        void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&toplevel->events.request_resize, listener);
}

void signal_add_new_xdg_surface(struct wl_listener *listener,
                                struct wlr_xdg_shell *xdg_shell,
                                void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&xdg_shell->events.new_surface, listener);
}

// ----------------------------------------------------------------------
// seat
// ----------------------------------------------------------------------

void seat_pointer_notify_button(struct wlr_seat *seat,
                                struct wlr_pointer_button_event *event)
{
    wlr_seat_pointer_notify_button(seat, event->time_msec, event->button, event->state);
}

void seat_pointer_notify_axis(struct wlr_seat *seat,
                                struct wlr_pointer_axis_event *event)
{
    wlr_seat_pointer_notify_axis(seat, event->time_msec, event->orientation,
                                 event->delta, event->delta_discrete, event->source);
}

struct wlr_surface *get_seat_focused_surface(struct wlr_seat *seat)
{
    return seat->keyboard_state.focused_surface;
}

struct wlr_seat_client *get_seat_focused_client(struct wlr_seat *seat)
{
    return seat->pointer_state.focused_client;
}

struct wlr_seat_client *get_seat_client(struct wlr_seat_pointer_request_set_cursor_event *event)
{
    return event->seat_client;
}

void set_seat_selection(struct wlr_seat *seat,
                        struct wlr_seat_request_set_selection_event *event) {
    wlr_seat_set_selection(seat, event->source, event->serial);
}

void signal_add_request_set_selection(struct wl_listener *listener,
                                      struct wlr_seat *seat,
                                      void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&seat->events.request_set_selection, listener);
}

void signal_add_request_cursor(struct wl_listener *listener,
                               struct wlr_seat *seat,
                               void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&seat->events.request_set_cursor, listener);
}

void signal_add_output_request_state(struct wl_listener *listener,
                                     struct wlr_output *output,
                                     void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&output->events.request_state, listener);

}
// ----------------------------------------------------------------------
// scene
// ----------------------------------------------------------------------

struct wlr_scene_node *scene_node_at(struct wlr_scene *scene,
                                     struct wlr_cursor *cursor, double *sx,
                                     double *sy)
{
    return wlr_scene_node_at(&scene->tree.node, cursor->x, cursor->y, sx, sy);
}

int get_node_type(struct wlr_scene_node *node)
{
    return node->type;
}

int get_scene_tree_node_x(struct wlr_scene_tree *scene_tree)
{
    return scene_tree->node.x;
}

int get_scene_tree_node_y(struct wlr_scene_tree *scene_tree)
{
    return scene_tree->node.y;
}

struct wlr_surface *get_scene_surface_wlr_surface(struct wlr_scene_surface *scene_surface)
{
    return scene_surface->surface;
}

void scene_tree_set_position(struct wlr_scene_tree *scene_tree, int x, int y)
{
    wlr_scene_node_set_position(&scene_tree->node, x, y);
}

void scene_output_send_frame_done(struct wlr_scene_output *scene_output, long sec, long nsec)
{
    struct timespec now;
    now.tv_sec = sec;
    now.tv_nsec = nsec;
    wlr_scene_output_send_frame_done(scene_output, &now);
}

struct wlr_scene_tree *scene_xdg_surface_create(struct wlr_scene *scene,
                                                struct wlr_xdg_toplevel *toplevel)
{
    return wlr_scene_xdg_surface_create(&scene->tree, toplevel->base);
}

void set_scene_tree_node_data(struct wlr_scene_tree *scene_tree, struct wlr_xdg_toplevel *toplevel)
{
    scene_tree->node.data = toplevel;
}

struct wlr_xdg_toplevel *get_root_toplevel(struct wlr_scene_node *node)
{
    /* this assumes we have added a 'cw_view pointer' to the 'data' element of
       the root node. */
    struct wlr_scene_tree *tree = node->parent;
    while (tree != NULL && tree->node.data == NULL) {
      tree = tree->node.parent;
    }
    return tree->node.data;
}

uint32_t xdg_surface_schedule_configure(struct wlr_xdg_toplevel *toplevel)
{
    return wlr_xdg_surface_schedule_configure(toplevel->base);
}

// ----------------------------------------------------------------------
// output
// ----------------------------------------------------------------------

void set_output_rules(struct wlr_output *output,
                      float scale,
                      int transform)
{
    /* The mode is a tuple of (width, height, refresh rate), and each
     * output supports only a specific set of modes. We just pick the
     * output's preferred mode; a more sophisticated compositor would let
     * the user configure it. */
    wlr_output_set_mode(output, wlr_output_preferred_mode(output));
    wlr_output_enable_adaptive_sync(output, 1);
    wlr_output_set_scale(output, scale);
    wlr_output_set_transform(output, transform);
}



void enable_output(struct wlr_output *output,
                   struct wlr_allocator *alloc,
                   struct wlr_renderer *renderer,
                   struct wlr_output_layout *output_layout)
{
    wlr_output_init_render(output, alloc, renderer);
    wlr_output_enable(output, 1);
    if (!wlr_output_commit(output))
        return;

    /* Adds this to the output layout. The add_auto function arranges outputs
     * from left-to-right in the order they appear. A more sophisticated
     * compositor would let the user configure the arrangement of outputs in the
     * layout.
     *
     * The output layout utility automatically adds a wl_output global to the
     * display, which Wayland clients can see to find out information about the
     * output (such as DPI, scale factor, manufacturer, etc).
     */
    wlr_output_layout_add_auto(output_layout, output);
}

void render_output(struct wlr_output *output, struct wlr_renderer *renderer, float *color)
{
    if (!wlr_output_attach_render(output, NULL))
        return;

    wlr_renderer_begin(renderer, output->width, output->height);
    wlr_renderer_clear(renderer, color);
    wlr_output_render_software_cursors(output, NULL);
    wlr_renderer_end(renderer);
    wlr_output_commit(output);
}

struct wlr_output *get_output_at_point(struct wlr_output_layout *output_layout,
                                       struct wlr_cursor *cursor)
{
    struct wlr_output *o = wlr_output_layout_output_at(output_layout, cursor->x, cursor->y);
    return o;
}

int has_output_modes(struct wlr_output *output)
{
    if (!wl_list_empty(&output->modes))
        return 0;
    return 1;
}


void sigchld(int unused)
{
    if (signal(SIGCHLD, sigchld) == SIG_ERR)
        EERROR("can't install SIGCHLD handler");
    while (0 < waitpid(-1, NULL, WNOHANG))
        ;
}

void output_commit_state(struct wlr_output *output, struct wlr_output_event_request_state *event)
{
    wlr_output_commit_state(output, event->state);
}


// ----------------------------------------------------------------------
// input
// ----------------------------------------------------------------------

int device_type(struct wlr_input_device *device)
{
    // FIXME: check this type
    return device->type;
}

void set_seat_capabilities(struct wlr_seat *seat, int has_keyboard)
{
    /* Always have a cursor, even if
     * there are no pointer devices, so we always include that capability. */
    uint32_t caps = WL_SEAT_CAPABILITY_POINTER;

    if (has_keyboard == 1)
        caps |= WL_SEAT_CAPABILITY_KEYBOARD;
    wlr_seat_set_capabilities(seat, caps);
}

struct wlr_output *output_at_cursor(struct wlr_output_layout *output_layout,
                                    struct wlr_cursor *cursor)
{
    return wlr_output_layout_output_at(output_layout, cursor->x, cursor->y);
}

void warp_cursor_coordinates(struct wlr_cursor *cursor)
{
    wlr_cursor_warp_closest(cursor, NULL, cursor->x, cursor->y);
}

void setup_init(struct wl_display *display, struct wlr_cursor *cursor, struct wlr_output_layout *layout)
{
    wlr_export_dmabuf_manager_v1_create(display);
    wlr_screencopy_manager_v1_create(display);
    wlr_data_device_manager_create(display);
    wlr_data_control_manager_v1_create(display);
    wlr_gamma_control_manager_v1_create(display);
    wlr_primary_selection_v1_device_manager_create(display);
    wlr_viewporter_create(display);

    wlr_xdg_output_manager_v1_create(display, layout);
    wlr_cursor_attach_output_layout(cursor, layout);
}

// ----------------------------------------------------------------------
// box
// ----------------------------------------------------------------------

int get_box_x(struct wlr_box *box) { return box->x; }
int get_box_y(struct wlr_box *box) { return box->y; }
int get_box_width(struct wlr_box *box) { return box->width; }
int get_box_height(struct wlr_box *box) { return box->height; }

// FIXME: Need a destroy_box as well
struct wlr_box *make_box()
{
    struct wlr_box *b = calloc(1, sizeof(*b));
    return b;
}

// ----------------------------------------------------------------------
// resize edges
// ----------------------------------------------------------------------

int get_edges(struct wlr_xdg_toplevel_resize_event *event)
{
    return event->edges;
}

int *get_resize_event_edges(struct wlr_xdg_toplevel_resize_event *event)
{
    int *resize_array = calloc(4, sizeof(*resize_array));

    if ((event->edges & WLR_EDGE_TOP) == WLR_EDGE_TOP) {
        resize_array[0] = 1;
    } else {
        resize_array[0] = 0;
    }
    if ((event->edges & WLR_EDGE_BOTTOM) == WLR_EDGE_BOTTOM) {
        resize_array[1] = 1;
    } else {
        resize_array[1] = 0;
    }
    if ((event->edges & WLR_EDGE_LEFT) == WLR_EDGE_LEFT) {
        resize_array[2] = 1;
    } else {
        resize_array[2] = 0;
    }
    if ((event->edges & WLR_EDGE_RIGHT) == WLR_EDGE_RIGHT) {
        resize_array[3] = 1;
    } else {
        resize_array[3] = 0;
    }
    return resize_array;
}

// ----------------------------------------------------------------------
// cursor
// ----------------------------------------------------------------------

double get_cursor_x(struct wlr_cursor *cursor) { return cursor->x; }
double get_cursor_y(struct wlr_cursor *cursor) { return cursor->y; }

void cursor_set_surface(struct wlr_cursor *cursor,
                        struct wlr_seat_pointer_request_set_cursor_event *event)
{
  wlr_cursor_set_surface(cursor, event->surface, event->hotspot_x, event->hotspot_y);
}

void cursor_warp_absolute(struct wlr_cursor *cursor, struct wlr_pointer_motion_absolute_event *event)
{
    wlr_cursor_warp_absolute(cursor, &event->pointer->base, event->x, event->y);
}

uint32_t get_time_event_pointer_motion (struct wlr_pointer_motion_event *event)
{
    return event->time_msec;
}

uint32_t get_time_event_pointer_motion_absolute (struct wlr_pointer_motion_absolute_event *event)
{
    return event->time_msec;
}

int get_button_state(struct wlr_pointer_button_event *event)
{
    return event->state;
}


void cursor_move(struct wlr_cursor *cursor, struct wlr_pointer_motion_event *event)
{
    wlr_cursor_move(cursor, &event->pointer->base, event->delta_x, event->delta_y);
}

void signal_add_cursor_frame(struct wl_listener *listener,
                             struct wlr_cursor *cursor,
                             void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&cursor->events.frame, listener);
}

void signal_add_cursor_motion(struct wl_listener *listener,
                             struct wlr_cursor *cursor,
                             void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&cursor->events.motion, listener);
}

void signal_add_cursor_motion_absolute(struct wl_listener *listener,
                                       struct wlr_cursor *cursor,
                                       void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&cursor->events.motion_absolute, listener);
}

void signal_add_cursor_axis(struct wl_listener *listener,
                            struct wlr_cursor *cursor,
                            void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&cursor->events.axis, listener);
}

void signal_add_cursor_button(struct wl_listener *listener,
                              struct wlr_cursor *cursor,
                              void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&cursor->events.button, listener);
}

// ----------------------------------------------------------------------

void signal_add_new_input(struct wl_listener *listener,
                           struct wlr_backend *backend,
                           void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&backend->events.new_input, listener);
}

void signal_add_new_output(struct wl_listener *listener,
                           struct wlr_backend *backend,
                           void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&backend->events.new_output, listener);
}

void signal_add_output_frame(struct wl_listener *listener,
                             struct wlr_output *output,
                             void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&output->events.frame, listener);
}



void signal_add_output_destroy(struct wl_listener *listener,
                               struct wlr_output *output,
                               void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&output->events.destroy, listener);
}

// ----------------------------------------------------------------------
// xdg_toplevel
// ----------------------------------------------------------------------


struct wlr_surface *xdg_toplevel_get_surface(struct wlr_xdg_toplevel *toplevel)
{
    return toplevel->base->surface;
}

struct wlr_xdg_toplevel *xdg_surface_toplevel(struct wlr_xdg_surface *surface)
{
    return surface->toplevel;
}

// ----------------------------------------------------------------------
// keyboard
// ----------------------------------------------------------------------


void signal_add_keyboard_destroy(struct wl_listener *listener,
                                 struct wlr_input_device *device,
                                 void (*handler_func)(struct wl_listener*, void*))
{
    // FIXME: this is not like any of the others
    listener->notify = handler_func;
    wl_signal_add(&device->events.destroy, listener);
}


void signal_add_keyboard_key(struct wl_listener *listener,
                             struct wlr_keyboard *keyboard,
                             void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&keyboard->events.key, listener);
}

void signal_add_keyboard_modifiers(struct wl_listener *listener,
                                   struct wlr_keyboard *keyboard,
                                   void (*handler_func)(struct wl_listener*, void*))
{
    listener->notify = handler_func;
    wl_signal_add(&keyboard->events.modifiers, listener);
}

void seat_keyboard_notify_enter(struct wlr_seat *seat, struct wlr_xdg_toplevel *toplevel,
                                struct wlr_keyboard *keyboard)
{
    if (keyboard != NULL) {
        wlr_seat_keyboard_notify_enter(seat, toplevel->base->surface,
                                       keyboard->keycodes, keyboard->num_keycodes,
                                       &keyboard->modifiers);
    }
}

void seat_keyboard_notify_key(struct wlr_seat *seat, struct wlr_keyboard *keyboard,
                             struct wlr_keyboard_key_event *event)
{
    wlr_seat_set_keyboard(seat, keyboard);
    wlr_seat_keyboard_notify_key(seat, event->time_msec, event->keycode, event->state);
}

void seat_keyboard_notify_modifiers(struct wlr_seat *seat, struct wlr_keyboard *keyboard)
{
    wlr_seat_set_keyboard(seat, keyboard);
    wlr_seat_keyboard_notify_modifiers(seat, &keyboard->modifiers);
}

void setup_keyboard(struct wlr_keyboard *keyboard)
{
    struct xkb_context *context;
    struct xkb_keymap *keymap;

    struct xkb_rule_names xkb_rules = {
        /* can specify fields: rules, model, layout, variant, options */
        /* example:
        .options = "ctrl:nocaps",
        */
    };
    // TODO: make these configurable
    int repeat_rate = 25;
    int repeat_delay = 600;

    context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
    keymap = xkb_map_new_from_names(context, &xkb_rules, XKB_KEYMAP_COMPILE_NO_FLAGS);
    wlr_keyboard_set_keymap(keyboard, keymap);
    xkb_keymap_unref(keymap);
    xkb_context_unref(context);

    wlr_keyboard_set_repeat_info(keyboard, repeat_rate, repeat_delay);
}

int keyboard_key_state(struct wlr_keyboard_key_event *event)
{
    if (event->state == WL_KEYBOARD_KEY_STATE_PRESSED)
        return 0;
    if (event->state == WL_KEYBOARD_KEY_STATE_RELEASED)
        return 1;
    return 2;
}

// FIXME: this needs to be freed.
char *get_keyboard_modifiers(struct wlr_keyboard *keyboard)
{
    uint32_t mods = wlr_keyboard_get_modifiers(keyboard);
    char *mods_str = calloc(8, sizeof(*mods_str));
    int count = 0;

    mods_str[0] = '\0';
    // TODO: handle other modifiers and combinations of modifiers
    // FIXME: make modifiers order independent
    if ((mods & WLR_MODIFIER_CTRL) == WLR_MODIFIER_CTRL) {
        mods_str[count++] = 'C';
        mods_str[count++] = '-';
    }
    if ((mods & WLR_MODIFIER_ALT) == WLR_MODIFIER_ALT) {
        mods_str[count++] = 'M';
        mods_str[count++] = '-';
    }
    if ((mods & WLR_MODIFIER_SHIFT) == WLR_MODIFIER_SHIFT) {
        mods_str[count++] = 'S';
        mods_str[count++] = '-';
    }
    return mods_str;
}


// FIXME: this needs to be freed.
char *get_keyboard_keys(struct wlr_keyboard *keyboard, struct wlr_keyboard_key_event *event)
{
    uint32_t keycode;
    const xkb_keysym_t *syms;
    int nsyms;
    int len;
    int pos = 0;
    char key_str[100];
    char *keys = calloc(128, sizeof(*keys));

    /* Translate libinput keycode -> xkbcommon */
    keycode = event->keycode + 8;

    /* Get a list of keysyms based on the keymap for this keyboard */
    nsyms = xkb_state_key_get_syms(keyboard->xkb_state, keycode, &syms);

    /* Convert syms into key chars */
    for (int i = 0; i < nsyms; i++) {
        len = xkb_keysym_get_name(syms[i], key_str, sizeof(key_str));
        if (len == -1)
            ERROR("xkb_keysym_get_name failed.");
        for (int j=0; j<len; j++) {
            keys[pos++] = key_str[j];
        }
        if (nsyms > 1)
            keys[pos++] = ';';
    }
    return keys;
}


// ----------------------------------------------------------------------

int backend_start(struct wlr_backend *backend)
{
    if(!wlr_backend_start(backend))
        return 1;
    return 0;
}

// FIXME: Need a destroy_listener as well
struct wl_listener *make_listener()
{
    struct wl_listener *l = calloc(1, sizeof(*l));
    return l;
}

/* void setup_cursor(struct wlr_cursor *cursor, struct wlr_xcursor_manager *cursor_mgr) */
/* { */
/*     /\* XXX hack to get cursor to display in its initial location (100, 100) */
/*      * instead of (0, 0) and then jumping.  still may not be fully */
/*      * initialized, as the image/coordinates are not transformed for the */
/*      * output when displayed here *\/ */
/*     wlr_cursor_warp_closest(cursor, NULL, cursor->x, cursor->y); */
/*     wlr_xcursor_manager_set_cursor_image(cursor_mgr, "left_ptr", cursor); */
/* } */


struct timespec get_time()
{
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return now;
}
