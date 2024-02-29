CFLAGS ?= -g -Wall -Wextra -Wno-unused-parameter -Wno-sign-compare -Wno-error=unused-function
CFLAGS += -fPIC -I. -DWLR_USE_UNSTABLE -std=c99 -Werror=declaration-after-statement

WAYLAND_PROTOCOLS=$(shell pkg-config --variable=pkgdatadir wayland-protocols)
WAYLAND_SCANNER=$(shell pkg-config --variable=wayland_scanner wayland-scanner)

PKGS = wlroots wayland-server xkbcommon
CFLAGS += $(foreach p,$(PKGS),$(shell pkg-config --cflags $(p)))
LDLIBS += $(foreach p,$(PKGS),$(shell pkg-config --libs $(p)))
LDFLAGS = -shared

xdg-shell-protocol.h:
	$(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml xdg-shell-protocol.h

clwc.o: xdg-shell-protocol.h
	gcc $(CFLAGS) $(LDLIBS) -c clwc.c

clwc.so: clwc.o
	gcc $(LDFLAGS) $(LDLIBS) -o $@ $^

clean:
	rm -f *.o clwc.so xdg-shell-protocol.h xdg-shell-protocol.c

.DEFAULT_GOAL=clwc.so
.PHONY: clean
