
from PyQt4.QtCore import QAbstractTableModel, QModelIndex, Qt, QVariant, pyqtSignal, QString, QTimer
from PyQt4.QtGui import *

from core.operations import *
from core.diffop_generator import generateDiffOps
from ui_user_window import Ui_UserWindow
import simplejson

from op_text_edit import OpTextEdit

class OperationsModel(QAbstractTableModel):
	HEADER = ["Operation", "Blip", "Index", "Property"]
	
	def __init__(self, builder, parent = None):
		super(OperationsModel, self).__init__(parent)
		self.builder = builder
		self.builder.addEvent("beforeOperationsInserted", self.__beforeOperationsInserted)
		self.builder.addEvent("afterOperationsInserted", self.__afterOperationsInserted)
		self.builder.addEvent("beforeOperationsRemoved", self.__beforeOperationsRemoved)
		self.builder.addEvent("afterOperationsRemoved", self.__afterOperationsRemoved)
		self.builder.addEvent("operationChanged", self.__operationChanged)
	
	def __del__(self):
		self.builder.removeEvent("beforeOperationsInserted", self.__beforeOperationsInserted)
		self.builder.removeEvent("afterOperationsInserted", self.__afterOperationsInserted)
		self.builder.removeEvent("beforeOperationsRemoved", self.__beforeOperationsRemoved)
		self.builder.removeEvent("afterOperationsRemoved", self.__afterOperationsRemoved)
		self.builder.removeEvent("operationChanged", self.__operationChanged)
	
	def rowCount(self, parent = QModelIndex()):
		return len(self.builder.operations)
	
	def columnCount(self, parent = QModelIndex()):
		return 4
	
	def data(self, index, role = Qt.DisplayRole):
		if not role == Qt.DisplayRole:
			return QVariant()
		
		op = self.builder.operations[index.row()]
		
		if index.column() == 0:
			return QVariant(op.type.lower())
		elif index.column() == 1:
			return QVariant(op.blip_id)
		elif index.column() == 2:
			return QVariant(op.index)
		elif index.column() == 3:
			return QVariant(repr(op.property))
		
		return QVariant()

	def headerData(self, section, orientation, role = Qt.DisplayRole):
		if not role == Qt.DisplayRole:
			return QVariant()
		
		if orientation == Qt.Horizontal:
			return QVariant(self.HEADER[section])
		else:
			return QVariant(section)
	
	def __beforeOperationsInserted(self, start, end):
		self.beginInsertRows(QModelIndex(), start, end)
	
	def __afterOperationsInserted(self, start, end):
		self.endInsertRows()
	
	def __beforeOperationsRemoved(self, start, end):
		self.beginRemoveRows(QModelIndex(), start, end)
	
	def __afterOperationsRemoved(self, start, end):
		self.endRemoveRows()
	
	def __operationChanged(self, index):
		self.dataChanged.emit(
			self.index(index, 2, QModelIndex()),
			self.index(index, 3, QModelIndex())
		)

class UserWindow(QWidget, Ui_UserWindow):
	
	#                              Version, Operations
	processOperations = pyqtSignal(int,     QString)
	
	def __init__(self, name, parent = None):
		super(UserWindow, self).__init__(parent)
		self.setupUi(self)
		self.setUserName(name)
		self.rootBlip.setBlipId("root_blip")
		
		self.opsPending = OpManager("wave", "wavelet")
		self.tblPending.setModel(OperationsModel(self.opsPending, self))
		self.tblPending.horizontalHeader().resizeSection(0, 120)
		self.tblPending.horizontalHeader().setStretchLastSection(True)
		
		self.opsCache = OpManager("wave", "wavelet")
		self.tblCached.setModel(OperationsModel(self.opsCache, self))
		self.tblCached.horizontalHeader().resizeSection(0, 120)
		self.tblCached.horizontalHeader().setStretchLastSection(True)
		self.opsCache.addEvent("afterOperationsInserted", self.__afterOperationsInserted)
		
		self.__version = 0
		self.__applying = False
		self.__pendingMarkerTimer = QTimer(self)
		self.__pendingMarkerTimer.setInterval(10)
		self.__pendingMarkerTimer.setSingleShot(True)
		self.__pendingMarkerTimer.timeout.connect(self.__pendingMarkerCheck)
		self.__ackPending = False
	
	def on_rootBlip_documentInsert(self, blip_id, index, content):
		self.opsCache.documentInsert(str(blip_id), index, unicode(content))
	
	def on_rootBlip_documentDelete(self, blip_id, start, end):
		self.opsCache.documentDelete(str(blip_id), start, end)
	
	def __pendingMarkerCheck(self):
		if self.hasPendingOperations():
			self.rootBlip.setStyleSheet("background-color: #FFFFCC")
	
	def setUserName(self, name):
		self.setWindowTitle("User \"%s\"" % (name))

	def closeEvent(self, event):
		event.ignore()
		self.parent().setWindowState(Qt.WindowMinimized)
	
	def version(self):
		return self.__version

	def applyOperations(self, version, ops):
		self.__applying = True
		
		if version != self.__version + 1:
			QMessageBox.warning(self, "Error", "Version went out of sync (%d -> %d)!" % (self.__version, version))
		
		# Transform and apply
		for op in ops:
			for op in self.opsPending.transform(op):
				for op in self.opsCache.transform(op):
					if op.isNull(): continue
					if op.type == DOCUMENT_INSERT:
						cur = self.rootBlip.textCursor()
						cur.movePosition(QTextCursor.Start)
						cur.movePosition(QTextCursor.Right, QTextCursor.MoveAnchor, op.index)
						cur.insertText(op.property)
					elif op.type == DOCUMENT_DELETE:
						cur = self.rootBlip.textCursor()
						cur.movePosition(QTextCursor.Start)
						cur.movePosition(QTextCursor.Right, QTextCursor.MoveAnchor, op.index)
						cur.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor, op.property)
						cur.removeSelectedText()
		
		self.__applying = False
		self.__version = version
		self.__showVersion()

	def hasPendingOperations(self):
		return self.__ackPending

	def __showVersion(self):
		if not self.__ackPending:
			self.lblVersion.setText("v%d" % self.__version)
		else:
			self.lblVersion.setText("v%d - ack pending" % self.__version)

	def acknowledge(self, version):
		self.__version = version
		self.opsPending.fetch()
		self.__ackPending = False
		if not self.opsCache.isEmpty():
			self.opsPending.put(self.opsCache.fetch())
			self.__ackPending = True
			self.processOperations.emit(self.__version, simplejson.dumps(self.opsPending.serialize(False)))
		self.__showVersion()

	def blipText(self, id):
		if id == "root_blip":
			return self.rootBlip.toPlainText()
	
	def setBlipInconsistent(self, id, inconsistent):
		if id == "root_blip":
			if inconsistent:
				self.rootBlip.setStyleSheet("background-color: #FFCCCC")
			else:
				self.rootBlip.setStyleSheet("")

	def __afterOperationsInserted(self, start, end):
		if not self.hasPendingOperations():
			self.opsPending.put(self.opsCache.fetch())
			self.__ackPending = True
			self.processOperations.emit(self.__version, simplejson.dumps(self.opsPending.serialize(False)))
			self.__pendingMarkerTimer.start()
			self.__showVersion()
