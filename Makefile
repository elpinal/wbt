poly:
	poly --script wbt.sml

mlkit:
	mlkit wbt.mlb

mlton:
	mlton -default-ann 'warnUnused true' wbt.mlb

.PHONY: poly mlkit mlton
