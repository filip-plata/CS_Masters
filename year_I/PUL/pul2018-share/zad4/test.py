import adepttool
import adepttool.device
import usb1

from PIL import Image


class Driver:
    WIDTH = 320
    HEIGHT = 200

    def __init__(self):
        dev = adepttool.device.get_devices(usb1.USBContext())[0]
        dev.start()
        self.epp = dev.depp_ports[0]
        self.epp.enable()
        self.epp.set_timeout(1)

    def is_idle(self):
        return self.epp.get_reg(0xf, 1)[0] == 0

    def fill_rect(self, x, y, width, height, col = 1):
        self._set_num(0x0, x)
        self._set_num(0x2, y)
        self._set_num(0x8, width)
        self._set_num(0xa, height)
        self.epp.put_reg(0xd, chr(col).encode())
        while not self.is_idle():
            pass

    def copy_rect(self, t_x, t_y, s_x, s_y, width, height):
        self._set_num(0x0, t_x)
        self._set_num(0x2, t_y)
        self._set_num(0x4, s_x)
        self._set_num(0x6, s_y)
        self._set_num(0x8, width)
        self._set_num(0xa, height)
        self.epp.put_reg(0xc, b'\x00')
        while not self.is_idle():
            pass

    def dump_img_to_file(self, filename='/tmp/image.bmp'):
        def access_bit(data, num):
            base = int(num // 8)
            shift = int(num % 8)
            return (data[base] & (1<<shift)) >> shift

        self._set_num(0x0, 0)
        self._set_num(0x2, 0)

        data = []

        for _ in range(self.HEIGHT):
            data.append([])
            pixels = self.epp.get_reg(0xe, self.WIDTH // 8)
            data[-1].extend([access_bit(pixels,i) for i in range(len(pixels)*8)])

        img = Image.new('1', (self.WIDTH, self.HEIGHT))
        pixels = img.load()

        for i in range(img.size[0]):
            for j in range(img.size[1]):
                pixels[i, j] = data[j][i]

        img.save(filename)

    def _set_num(self, reg_num, num):
        self.epp.put_reg(reg_num, bytes([num & 0xFF]))
        self.epp.put_reg(reg_num + 1, bytes([(num & 0xFF00) >> 8]))


def test_copy(drv):
    drv.fill_rect(40, 10, 20, 10, 1)
    drv.copy_rect(40, 10, 40, 0, 20, 20)

drv = Driver()
