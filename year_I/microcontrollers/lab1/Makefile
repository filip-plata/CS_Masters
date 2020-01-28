ROOT 		 = /home/filip/Studia/mikrokontrolery/stm32
CC       = arm-none-eabi-gcc
OBJCOPY  = arm-none-eabi-objcopy
FLAGS    = -mthumb -mcpu=cortex-m4
CPPFLAGS = -DSTM32F411xE
CFLAGS   = $(FLAGS) -Wall -g \
					-O2 -ffunction-sections -fdata-sections \
					-I$(ROOT)/inc \
					-I$(ROOT)/CMSIS/Include \
					-I$(ROOT)/CMSIS/Device/ST/STM32F4xx/Include
LDFLAGS  = $(FLAGS) -Wl,--gc-sections -nostartfiles \
					-L$(ROOT)/lds -Tstm32f411re.lds

vpath %.c $(ROOT)/src

OBJECTS = leds_main.o startup_stm32.o delay.o gpio.o
TARGET  = leds

.SECONDARY: $(TARGET).elf $(OBJECTS)

all: $(TARGET).bin

%.elf : $(OBJECTS)
	$(CC) $(LDFLAGS) $^ -o $@

%.bin : %.elf
	$(OBJCOPY) $< $@ -O binary

clean :
	rm -f *.bin *.elf *.hex *.d *.o *.bak *~
