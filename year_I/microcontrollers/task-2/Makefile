ROOT 		 = /home/filip/Studia/mikrokontrolery/stm32
CC       = arm-none-eabi-gcc
OBJCOPY  = arm-none-eabi-objcopy
FLAGS    = -mthumb -mcpu=cortex-m4
CPPFLAGS = -DSTM32F411xE
CFLAGS   = $(FLAGS) -Wall -g -DEXPANDER \
					-O2 -ffunction-sections -fdata-sections \
					-I$(ROOT)/inc \
					-I$(ROOT)/CMSIS/Include \
					-I$(ROOT)/CMSIS/Device/ST/STM32F4xx/Include
LDFLAGS  = $(FLAGS) -Wl,--gc-sections -nostartfiles \
					-L$(ROOT)/lds -Tstm32f411re.lds

vpath %.c $(ROOT)/src

OBJECTS = uart_main.o handle_buttons.o uart.o dma_queue.o startup_stm32.o gpio.o
TARGET  = uart

.SECONDARY: $(TARGET).elf $(OBJECTS)

all: $(TARGET).bin

%.elf : $(OBJECTS)
	$(CC) $(LDFLAGS) $^ -o $@

%.bin : %.elf
	$(OBJCOPY) $< $@ -O binary

clean :
	rm -f *.bin *.elf *.hex *.d *.o *.bak *~
