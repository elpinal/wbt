poly:
	poly --script set.sml
	poly --script map.sml

mlkit:
	mlkit wbt.mlb

mlton:
	mlton -default-ann 'warnUnused true' wbt.mlb

.PHONY: poly mlkit mlton
