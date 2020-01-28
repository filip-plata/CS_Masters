import bluetooth

bd_addr = '20:14:10:15:25:49'
port = 1

sock = BluetoothSocket(RFCOMM)
sock.connect((bd_addr,port))

sock.send("l0t".encode())
