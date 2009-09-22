
from PyQt4.QtCore import pyqtSignal, QString, Qt
from PyQt4.QtGui import QTextEdit, QApplication, QTextCursor

class OpTextEdit(QTextEdit):
	"""
	This subclass of QTextEdit emits special signals for use with an OpManager.
	
	"""
	
	#                           blipId,  index, content
	documentInsert = pyqtSignal(QString, int,   QString)
	#                           blipId,  start, end
	documentDelete = pyqtSignal(QString, int,   int)
	
	def __init__(self, parent = None, blip_id = ""):
		super(OpTextEdit, self).__init__(parent)
		self.__blip_id = blip_id
		self.setAcceptDrops(False)
		self.setAcceptRichText(False)
	
	def setBlipId(self, blip_id):
		self.__blip_id = blip_id
	
	def blipId(self):
		return self.__blip_id
	
	def keyPressEvent(self, event):
		if event.key() == Qt.Key_Backspace or event.key() == Qt.Key_Delete:
			cur = self.textCursor()
			if not cur.hasSelection():
				if event.key() == Qt.Key_Backspace:
					cur.movePosition(QTextCursor.Left, QTextCursor.KeepAnchor)
				else:
					cur.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor)
			if cur.hasSelection():
				start = min(cur.position(), cur.anchor())
				end = max(cur.position(), cur.anchor())
				self.documentDelete.emit(self.__blip_id, start, end)
		elif event.key() == Qt.Key_Return or event.key() == Qt.Key_Enter:
			cur = self.textCursor()
			start = min(cur.position(), cur.anchor())
			if cur.hasSelection():
				end = max(cur.position(), cur.anchor())
				self.documentDelete.emit(self.__blip_id, start, end)
			self.documentInsert.emit(self.__blip_id, start, "\n")
		elif event.key() < Qt.Key_Escape and event.modifiers() & Qt.ControlModifier == Qt.NoModifier:
			cur = self.textCursor()
			start = min(cur.position(), cur.anchor())
			if cur.hasSelection():
				end = max(cur.position(), cur.anchor())
				self.documentDelete.emit(self.__blip_id, start, end)
			self.documentInsert.emit(self.__blip_id, start, event.text())
		elif event.modifiers() & Qt.ControlModifier == Qt.ControlModifier and event.key() == Qt.Key_X:
			cur = self.textCursor()
			if cur.hasSelection():
				start = min(cur.position(), cur.anchor())
				end = max(cur.position(), cur.anchor())
				self.documentDelete.emit(self.__blip_id, start, end)
		elif event.modifiers() & Qt.ControlModifier == Qt.ControlModifier and event.key() == Qt.Key_V:
			cur = self.textCursor()
			start = min(cur.position(), cur.anchor())
			if cur.hasSelection():
				end = max(cur.position(), cur.anchor())
				self.documentDelete.emit(self.__blip_id, start, end)
			cb = QApplication.clipboard()
			text = cb.text()
			if text != "":
				self.documentInsert.emit(self.__blip_id, start, text)
		super(OpTextEdit, self).keyPressEvent(event)
