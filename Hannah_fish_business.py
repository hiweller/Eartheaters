import csv

def for_hannah(filename):
	# filename is string denoting path to your .csv
	rd = {}
	with open(filename, 'rU') as f:
		reader = csv.DictReader(f)
		# reader is now an iterator of row dictionaries with keys as the column names
		for row in reader:
			# This way, you can have whatever order you want!
			rd[ row['Taxon'] ] = [ 
				row['Tissue sample'], row['Voucher'], 
				row['Collection locality'], row['ND4'], row['Cytochrome b'], 
				row['16S'], row['RAG2'], row['S7 NaN Intron 1']
				]

	# with this dictionary, put in the taxon as a key and get all the info out.
	return rd