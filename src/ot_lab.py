#!/usr/bin/env python

import sys
from PyQt4 import QtGui, QtCore
from gui import MainWindow

app = QtGui.QApplication(sys.argv)

d = MainWindow()
d.show()

app.exec_()
