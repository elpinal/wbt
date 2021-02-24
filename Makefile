poly:
	poly --script set.sml
	poly --script map.sml

mlkit:
	mlkit wbt.mlb

mlton:
	mlton -default-ann 'warnUnused true' wbt.mlb

mosml:
	mosmlc -c -toplevel set.sml map.sml

.PHONY: poly mlkit mlton mosml
