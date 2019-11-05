from machine import I2C, Pin



def __init__(self, scl, sda):
    self._i2c = I2C(scl=scl, sda=sda)
    self._addr = 0x18
    self._temp_reg = 0x05
    self._res_reg = 0x08
