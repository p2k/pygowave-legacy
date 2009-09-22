
from PyQt4.QtCore import QSignalMapper, QSize, Qt
from PyQt4.QtGui import *
from ui_main_window import Ui_MainWindow

from server_window import ServerWindow
from user_window import UserWindow
import simplejson
from core.operations import Operation

class MainWindow(QMainWindow, Ui_MainWindow):
	USERNAMES = ["Alice", "Bob", "Carol", "Dave", "Eve", "Marvin", "Oscar", "Peggy", "Victor", "Ted"]
	
	def __init__(self):
		super(MainWindow, self).__init__()
		self.setupUi(self)
		self.windowMapper = QSignalMapper(self)
		self.windowMapper.mapped[QWidget].connect(self.setActiveSubWindow)
		self.menuWindow.aboutToShow.connect(self.updateWindowMenu)
		self.updateWindowMenu()
		self.actionAdd_user.triggered.connect(self.addUser)
		
		self.srv = ServerWindow()
		self.srv.userNameChange.connect(self.__userNameChange)
		self.srv.applyOperations.connect(self.__applyOperations)
		self.srv.acknowledge.connect(self.__acknowledge)
		self.addSubWindow(self.srv)
		
		self.users = []
		self.next_user_name = 0
		self.setWindowState(Qt.WindowMaximized)

	def __processOperations(self, version, ops):
		id = self.users.index(self.sender())
		ops = map(lambda o: Operation.unserialize(o), simplejson.loads(str(ops)))
		self.srv.processOperations(id, version, ops)
	
	def __userNameChange(self, id, name):
		self.users[id].setUserName(name)
	
	def __applyOperations(self, id, version, ops):
		ops = map(lambda o: Operation.unserialize(o), simplejson.loads(str(ops)))
		self.users[id].applyOperations(version, ops)
		# Consistency check
		clean_users = filter(lambda user: not user.hasPendingOperations(), self.users)
		for user in clean_users:
			bliptext = user.blipText("root_blip")
			good = True
			for other in clean_users:
				if other == user: continue
				if other.blipText("root_blip") != bliptext:
					good = False
					break
			user.setBlipInconsistent("root_blip", not good)
	
	def __acknowledge(self, id, version):
		self.users[id].acknowledge(version)
	
	def addUser(self):
		user_name = self.USERNAMES[self.next_user_name % 10]
		if self.next_user_name / 10 > 0:
			user_name += str(self.next_user_name / 10)
		user = UserWindow(user_name)
		self.next_user_name += 1
		self.addSubWindow(user).show()
		user.processOperations.connect(self.__processOperations)
		
		self.users.append(user)
		self.srv.registerUser(user_name)
	
	def addSubWindow(self, w, flags = None):
		sz = w.size() + QSize(8, 12)
		if flags == None:
			mdi = self.mdiArea.addSubWindow(w)
		else:
			mdi = self.mdiArea.addSubWindow(w, flags)
		mdi.resize(sz)
		return mdi
	
	def setActiveSubWindow(self, w):
		self.mdiArea.setActiveSubWindow(w)

	def updateWindowMenu(self):
		self.menuWindow.clear()
		self.menuWindow.addAction(self.actionClose)
		self.menuWindow.addAction(self.actionClose_all)
		self.menuWindow.addSeparator()
		self.menuWindow.addAction(self.actionTile)
		self.menuWindow.addAction(self.actionCascade)
		self.menuWindow.addSeparator()
		self.menuWindow.addAction(self.actionNext_window)
		self.menuWindow.addAction(self.actionPrevious_window)
		
		windows = self.mdiArea.subWindowList()
		if len(windows) > 0:
			self.menuWindow.addSeparator()
		
		for window in windows:
			action = self.menuWindow.addAction(window.windowTitle())
			action.setCheckable(True)
			action.setChecked(window == self.mdiArea.activeSubWindow())
			action.triggered[""].connect(self.windowMapper.map)
			self.windowMapper.setMapping(action, window)
