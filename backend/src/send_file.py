#!/usr/bin/python

import socket
import time
import sys,os
import ntpath

def sendFile(path):
    filename = path_leaf(path)
    HOST = 'localhost'
    PORT = 18842
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((HOST, PORT))
    s.send("FFDT:FileName")
    time.sleep(0.1)
    s.sendall(filename)
    f = open(path,'rb')
    time.sleep(0.1)
    s.send("FFDT:FileLength")
    time.sleep(0.1)
    s.send(str(os.stat(path).st_size))
    time.sleep(0.1)
    s.send("FFDT:FileData")
    time.sleep(0.1)
    while True:
        data = f.read(4096)
        if not data:
            break;
        s.send(data)
        print data
    s.close()
    f.close()
    print 'Received'

def path_leaf(path):
    head,tail = ntpath.split(path)
    return tail or ntpath.basename(head)

sendFile("/tmp/test.tar")
