
from operations import *
import difflib

def generateDiffOps(builder, blip_id, textA, textB):
	s = difflib.SequenceMatcher(None, textA, textB)
	for opcode in s.get_opcodes():
		type = None
		if opcode[0] == "equal":
			continue
		elif opcode[0] == "delete":
			builder.documentDelete(blip_id, opcode[1], opcode[2])
		elif opcode[0] == "insert":
			builder.documentInsert(blip_id, opcode[1], textB[opcode[3]:opcode[4]])
		elif opcode[0] == "replace":
			builder.documentDelete(blip_id, opcode[1], opcode[2])
			builder.documentInsert(blip_id, opcode[1], textB[opcode[3]:opcode[4]])
