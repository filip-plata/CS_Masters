import pygame, sys, serial, time

from pygame.locals import *
from random import *

pygame.init()

win_width = 480
win_height = 640
screen = pygame.display.set_mode((win_width, win_height))

pygame.display.set_caption('My First Game')

bg_col = (0, 0, 0)

ball_col = pygame.Color('#FFFF00')
ball_x = win_width/2
ball_y = win_height/2
ball_dir_x = randint(-1, 1)
ball_spd_x = 0.05
ball_spd_x_max = 0.1
ball_dir_y = 1
ball_spd_y = 1
ball_pos = (ball_x, ball_y)
ball_radius = 10;
ball_slowness = 3

plat_col = pygame.Color('#800080')
plat_width = win_width/3
plat_height = 20
plat_x = win_width/2 - plat_width/2
plat_y = win_height - plat_height
plat = (plat_x, plat_y, plat_width, plat_height)

myfont = pygame.font.SysFont("monospace", 30)
bigfont = pygame.font.SysFont("monospace", 60)

counter = 0
score = 0
high_score = 0

score_expiry = 0

dev = '/dev/rfcomm0'
#dev = '/dev/ttyACM0'
serial = serial.Serial(dev, 9600, timeout=0)

class EncoderMsgParser:
    ENCODER_MAX = 192

    def __init__(self):
        self.state = 0
        self.prev_pos = None
        self.num = None
        self.res = None

    def process_byte(self, byte):
        print(byte)
        if self.state == 0 and byte == b'P':
            self.state = 1
            self.num = 0
        elif self.state == 1 and byte != b'\r':
            try:
                digit = ord(byte.decode("ascii")) - ord('0')
            except (UnicodeDecodeError, ValueError):
                self.state = 0
                return None
            else:
                self.num *= 10
                self.num += digit
        elif self.state == 1 and byte == b'\r':
            self.state = 2
            if self.num >= self.ENCODER_MAX or self.num < 0:
                return None

            if self.prev_pos is not None:
                res = self.num - self.prev_pos

                if res > self.ENCODER_MAX / 2:
                    res -= self.ENCODER_MAX
                if res < -self.ENCODER_MAX / 2:
                    res += self.ENCODER_MAX
            else:
                res = None

            self.prev_pos = self.num
            self.res = res
        elif self.state == 2 and byte == b'\n':
            print("ACK cmd " + str(self.num))
            self.state = 0
            return self.res
        else:
            self.state = 0
            return None


encoder_msg_parser = EncoderMsgParser()


def new_paddle_position(encoder_read_sink, plat_x):
    global encoder_msg_parser

    while True:
        new_byte = encoder_read_sink.read()
        if not new_byte:
            break

        res = encoder_msg_parser.process_byte(new_byte)
        if res is not None:
            plat_x += res * 5

    return plat_x


def led_party(input_sink):
    for _ in range(16):
        for led in range(4):
            led_toggle = f"l{led}t".encode('utf-8')
            input_sink.write(led_toggle)
            time.sleep(0.05)


def clear_cmds(input_sink):
    while input_sink.read():
        pass


while True:
  for event in pygame.event.get():
    if event.type == QUIT:
      pygame.quit()
      sys.exit()

  keys = pygame.key.get_pressed()
  plat_x = new_paddle_position(serial, plat_x)

  plat_x = min(max(plat_x, 0), win_width - plat_width)

  # Update ball
  if counter % ball_slowness == 0:
    ball_left = ball_x - ball_radius
    ball_right = ball_x + ball_radius
    ball_top = ball_y - ball_radius
    ball_bot = ball_y + ball_radius
    plat_left = plat_x
    plat_center = plat_left + plat_width/2
    plat_right = plat_x + plat_width
    plat_top = plat_y
    plat_bot = plat_y + plat_height
    if ball_right >= plat_left and ball_left <= plat_right and ball_bot >= plat_y:
      ball_y = plat_y - 2 * ball_radius
      if (ball_x < plat_center):
        ball_dir_x = -1
      elif (ball_x > plat_center):
        ball_dir_x = 1
      ball_dir_y = -1
      ball_spd_x = abs(plat_center - ball_x / (plat_width/2))
      if (ball_spd_x > ball_spd_x_max):
        ball_spd_x = ball_spd_x_max
      ball_spd_y *= 1.05
      score += 1
    if ball_left <= 0:
      ball_x = 2 * ball_radius
      ball_dir_x = 1
    elif ball_right >= win_width:
      ball_x = win_width - 2 * ball_radius
      ball_dir_x = -1
    if ball_top <= 0:
      ball_dir_y = 1
    elif ball_top > win_height:
      ball_dir_x = randint(-1, 1)
      ball_x = win_width/2
      ball_y = win_height/2
      ball_spd_y = 1
      high_score = score
      score = 0
      score_expiry = 6000

      led_party(serial)
      clear_cmds(serial)

      encoder_msg_parser.prev_pos = None
      encoder_msg_parser.state = 0
      plat_x = win_width/2 - plat_width/2

    ball_x += ball_spd_x * ball_dir_x
    ball_y += ball_spd_y * ball_dir_y
    counter = 0

  # Remake tuples
  ball_pos = (int(ball_x), int(ball_y))
  plat = (int(plat_x), int(plat_y), int(plat_width), int(plat_height))

  # Clear screen
  screen.fill(bg_col)

  # Draw stuff
  pygame.draw.circle(screen, ball_col, ball_pos, ball_radius)
  pygame.draw.rect(screen, plat_col, plat)

  # Draw score
  label = myfont.render(str(score), 1, (255,255,255))
  screen.blit(label, (20, 20))

  if (score_expiry > 0):
    label2 = bigfont.render(str(high_score), 1, (255, 255, 255))
    screen.blit(label2, (50, 50))
    score_expiry -= 1

  # Update screen
  pygame.display.update()

  # Increment counter
  counter += 1
