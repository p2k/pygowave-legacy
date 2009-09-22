
from PyQt4.QtCore import Qt, QSignalMapper, pyqtSignal, QString, QAbstractTableModel, QModelIndex, QVariant, QTimer
from PyQt4.QtGui import *

from ui_server_window import Ui_ServerWindow
from core.operations import OpManager
import simplejson

class Delta(OpManager):
	"""
	A delta is basically a OpManager with an user_id and a version.
	
	"""
	def __init__(self, wave_id, wavelet_id, user_id, version):
		super(Delta, self).__init__(wave_id, wavelet_id)
		self.user_id = user_id
		self.version = version
	
	def fetch(self):
		return self.operations

class LagSettingDelegate(QStyledItemDelegate):
	
	def displayText(self, value, locale):
		return "%d ms" % (value.toInt()[0])
	
	def createEditor(self, parent, option, index):
		spb = QSpinBox(parent)
		spb.setSuffix(" ms")
		spb.setMaximum(10000)
		spb.setSingleStep(500)
		return spb

class DeltaModel(QAbstractTableModel):
	HEADER = ["User", "Base Version", "Operations"]
	
	def __init__(self, parent):
		super(DeltaModel, self).__init__(parent)
		self.srv = parent
		self.deltas = []
	
	def rowCount(self, parent = QModelIndex()):
		return len(self.deltas)
	
	def columnCount(self, parent = QModelIndex()):
		return 3
	
	def data(self, index, role = Qt.DisplayRole):
		if not role == Qt.DisplayRole:
			return QVariant()
		
		delta = self.deltas[index.row()]
		
		if index.column() == 0:
			return QVariant(self.srv.userName(delta.user_id))
		elif index.column() == 1:
			return QVariant(delta.version)
		elif index.column() == 2:
			return QVariant(repr(delta.operations))
		
		return QVariant()

	def headerData(self, section, orientation, role = Qt.DisplayRole):
		if not role == Qt.DisplayRole:
			return QVariant()
		
		if orientation == Qt.Horizontal:
			return QVariant(self.HEADER[section])
		else:
			return QVariant(section+1)
	
	def addDelta(self, delta):
		at = len(self.deltas)
		self.beginInsertRows(QModelIndex(), at, at)
		self.deltas.append(delta)
		self.endInsertRows()

class ServerWindow(QWidget, Ui_ServerWindow):
	
	#                           UserID, Name
	userNameChange = pyqtSignal(int,    QString)
	#                            UserID, Version, Operations
	applyOperations = pyqtSignal(int,    int,     QString)
	#                        UserID, Version
	acknowledge = pyqtSignal(int,    int)
	
	def __init__(self, parent = None):
		super(ServerWindow, self).__init__(parent)
		self.setupUi(self)
		self.tblUsers.setColumnCount(3)
		self.tblUsers.setHorizontalHeaderLabels(["User ID", "Lag", "Block"])
		self.delegate1 = LagSettingDelegate()
		self.tblUsers.setItemDelegateForColumn(1, self.delegate1)
		self.__blockMapper = QSignalMapper(self)
		self.__blockMapper.mapped[int].connect(self.__blockUnblockUser)
		self.tblUsers.cellChanged.connect(self.__cellChanged)
		self.model = DeltaModel(self)
		self.tblDeltas.setModel(self.model)
		self.tblDeltas.horizontalHeader().setStretchLastSection(True)
		
		self.__deferred = []
		self.__timers = []
		self.__deferredMapper = QSignalMapper(self)
		self.__deferredMapper.mapped[int].connect(self.__processDelta)
	
	def __cellChanged(self, row, column):
		if column == 0:
			name = self.userName(row)
			self.userNameChange.emit(row, name)
	
	def closeEvent(self, event):
		event.ignore()
		self.parent().setWindowState(Qt.WindowMinimized)
	
	def userLag(self, id):
		return self.tblUsers.item(id, 1).text().toInt()[0]
	
	def userBlocked(self, id):
		return self.tblUsers.cellWidget(id, 2).isChecked()
	
	def __blockUnblockUser(self, id):
		if not self.userBlocked(id):
			if self.__deferred[id] != None:
				self.__processDelta(id)
	
	def registerUser(self, name):
		rc = self.tblUsers.rowCount()
		self.tblUsers.setRowCount(rc + 1)
		item = QTableWidgetItem(name)
		self.tblUsers.setItem(rc, 0, item)
		self.tblUsers.setItem(rc, 1, QTableWidgetItem("0"))
		pb = QPushButton()
		pb.setText("Block")
		pb.setCheckable(True)
		pb.clicked[''].connect(self.__blockMapper.map)
		self.__blockMapper.setMapping(pb, rc)
		self.tblUsers.setCellWidget(rc, 2, pb)
		self.__deferred.append(None)
		tmr = QTimer(self)
		tmr.setSingleShot(True)
		tmr.timeout.connect(self.__deferredMapper.map)
		self.__deferredMapper.setMapping(tmr, rc)
		self.__timers.append(tmr)

	def userName(self, id):
		return self.tblUsers.item(id, 0).text()

	def processOperations(self, user_id, version, ops):
		delta = Delta(ops[0].wave_id, ops[0].wavelet_id, user_id, version)
		delta.operations = ops
		self.__deferred[user_id] = delta
		if not self.userBlocked(user_id):
			self.__timers[user_id].start(self.userLag(user_id))

	def __processDelta(self, user_id):
		if self.userBlocked(user_id):
			return
		delta = self.__deferred[user_id]
		self.__deferred[user_id] = None
		
		# Transform
		for i in xrange(delta.version, len(self.model.deltas)):
			for op in self.model.deltas[i].operations:
				delta.transform(op) # Trash results (an existing delta cannot be changed)
		self.model.addDelta(delta)
		self.tblDeltas.scrollToBottom()
		
		# Acknowledge users delta
		new_version = len(self.model.deltas)
		self.acknowledge.emit(user_id, new_version)
		
		# Send to others
		for other_id in xrange(self.tblUsers.rowCount()):
			if other_id == user_id: continue
			self.applyOperations.emit(other_id, new_version, simplejson.dumps(delta.serialize(False)))
