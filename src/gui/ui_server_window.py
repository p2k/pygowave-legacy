# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'src/gui/server_window.ui'
#
# Created: Tue Jul  7 20:50:22 2009
#      by: PyQt4 UI code generator 4.5.1
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_ServerWindow(object):
    def setupUi(self, ServerWindow):
        ServerWindow.setObjectName("ServerWindow")
        ServerWindow.resize(595, 392)
        self.verticalLayout = QtGui.QVBoxLayout(ServerWindow)
        self.verticalLayout.setObjectName("verticalLayout")
        self.splitter = QtGui.QSplitter(ServerWindow)
        self.splitter.setOrientation(QtCore.Qt.Vertical)
        self.splitter.setObjectName("splitter")
        self.groupBox = QtGui.QGroupBox(self.splitter)
        self.groupBox.setObjectName("groupBox")
        self.verticalLayout_3 = QtGui.QVBoxLayout(self.groupBox)
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.tblUsers = QtGui.QTableWidget(self.groupBox)
        self.tblUsers.setObjectName("tblUsers")
        self.tblUsers.setColumnCount(0)
        self.tblUsers.setRowCount(0)
        self.verticalLayout_3.addWidget(self.tblUsers)
        self.groupBox_2 = QtGui.QGroupBox(self.splitter)
        self.groupBox_2.setObjectName("groupBox_2")
        self.verticalLayout_2 = QtGui.QVBoxLayout(self.groupBox_2)
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.tblDeltas = QtGui.QTableView(self.groupBox_2)
        self.tblDeltas.setObjectName("tblDeltas")
        self.verticalLayout_2.addWidget(self.tblDeltas)
        self.verticalLayout.addWidget(self.splitter)

        self.retranslateUi(ServerWindow)
        QtCore.QMetaObject.connectSlotsByName(ServerWindow)

    def retranslateUi(self, ServerWindow):
        ServerWindow.setWindowTitle(QtGui.QApplication.translate("ServerWindow", "Server", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox.setTitle(QtGui.QApplication.translate("ServerWindow", "Users", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox_2.setTitle(QtGui.QApplication.translate("ServerWindow", "Deltas", None, QtGui.QApplication.UnicodeUTF8))

