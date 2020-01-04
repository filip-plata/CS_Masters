KDIR ?= /lib/modules/`uname -r`/build
COMMON := -std=gnu11 -DUSE_CMD_BUF
DEBUG_FLAGS += -g -DDEBUG -DHARDDOOM_DEBUG -O1 $(COMMON)
RELEASE_FLAGS += -O2 $(COMMON)

default:
	$(MAKE) release

release:
	$(MAKE)  -C $(KDIR) M=$$PWD EXTRA_CFLAGS="$(RELEASE_FLAGS)"

debug:
	$(MAKE)  -C $(KDIR) M=$$PWD EXTRA_CFLAGS="$(DEBUG_FLAGS)"

install:
	$(MAKE) -C $(KDIR) M=$$PWD modules_install

clean:
	$(MAKE) -C $(KDIR) M=$$PWD clean
