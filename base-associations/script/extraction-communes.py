import sys
import csv

cityreader = csv.reader(sys.stdin, delimiter=';', quotechar='"')
for row in cityreader:
    # At this point, the row looks like this:
    # 0 EU_circo
    # 1 code_région
    # 2 nom_région
    # 3 chef-lieu_région
    # 4 numéro_département
    # 5 nom_département
    # 6 préfecture
    # 7 numéro_circonscription
    # 8 nom_commune
    # 9 codes_postaux
    # 10 code_insee
    # 11 latitude
    # 12 longitude
    # 13 éloignement

    row.pop(13)
    row.pop(7)
    row.pop(6)
    row.pop(5)
    row.pop(3)
    row.pop(2)
    row.pop(0)

    # Now the row looks like this:
    # 0 code_région
    # 1 numéro_département
    # 2 nom_commune
    # 3 codes_postaux
    # 4 code_insee
    # 5 latitude
    # 6 longitude

    for code_postal in row[3].split(' '):
        new_row = list(row)
        new_row[3] = code_postal
        print(';'.join(new_row))