import adepttool
import adepttool.device
import usb1
import random
import time


class Driver:
    RSA_BITS = 2048

    def __init__(self):
        dev = adepttool.device.get_devices(usb1.USBContext())[0]
        dev.start()
        self.epp = dev.depp_ports[0]
        self.epp.enable()
        self.epp.set_timeout(500000)
        self.reset();

    def _set_num(self, reg_num, num, bytes):
        self.epp.put_reg(reg_num, num.to_bytes(bytes, byteorder='little'))

    def reset(self):
        self.epp.put_reg(0x0, b'\x00')

    def load_private_key(self, modulus, d_exp):
        if modulus <= 1:
            raise ValueError("Modulus must be larger than 1!")
        r = 4 ** self.RSA_BITS // modulus # special ingredient
        self._set_num(0x1, modulus, self.RSA_BITS // 8)
        self._set_num(0x1, d_exp, self.RSA_BITS // 8)
        self._set_num(0x1, r, self.RSA_BITS // 4)

    def load_message(self, msg):
        self._set_num(0x2, msg, self.RSA_BITS // 8)

    def read_result(self):
        return int.from_bytes(self.epp.get_reg(0x3, 256), "little")

    def encrypt(self):
        self.epp.put_reg(0x4, b'\x00')
        while self.device_is_busy():
            time.sleep(0.1)

    def debug(self):
        self.epp.put_reg(0x7, b'\x00')

    def device_is_busy(self):
        return self.epp.get_reg(0x5, 1)[0] == 1

    def draw_random(self, count = 1):
        return self.epp.get_reg(0x6, count)


def test_encryption(drv):
    a = random.randint(0, 2 ** 2048 - 1)
    b = random.randint(0, 2 ** 2048 - 1)
    c = random.randint(2, 2 ** 2048 - 1)

    drv.reset()
    drv.load_message(a)
    drv.load_private_key(c, b)
    drv.encrypt()

    assert drv.read_result() == pow(a, b, c)
    print("OK")


drv = Driver()
test_encryption()
