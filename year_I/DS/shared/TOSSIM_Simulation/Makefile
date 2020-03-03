COMPONENT=SimulationAppC
BUILD_EXTRA_DEPS = Radio.py
CFLAGS += -I$(TOSDIR)/lib/printf
CFLAGS += -DNEW_PRINTF_SEMANTICS=1
include $(MAKERULES)

Radio.py: Radio.h
	mig python -target=$(PLATFORM) $(CFLAGS) -python-classname=Radio Radio.h txt_message -o $@
