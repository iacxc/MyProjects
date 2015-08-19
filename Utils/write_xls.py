#!/usr/bin/python -O
# -*- coding: utf-8 -*-

import sqlite3
from datetime import date, time, timedelta

conn = sqlite3.connect("workbook.db")
cursor = conn.cursor()

cursor.execute("select distinct card, workno, depart, name from sheet1")

ids = [row for row in cursor.fetchall()]

all_dates = []
start_date = date(2015, 6, 1)
while start_date <= date(2015, 8, 13):
    all_dates.append(start_date.strftime("%Y-%m-%d"))
    start_date += timedelta(days=1)

print all_dates

csv_file = file("x.csv", "w")

for row_id in ids:
    cursor.execute("""select date, time 
from sheet1 
where card=? and workno=? and depart=? and name=?
order by date, time""", row_id)

    values = {}

    for row in cursor.fetchall():
        if row[0].strip() in values:
            values[row[0].strip()] += "--" + row[1]
        else:
            values[row[0].strip()] = row[1]

    times = [row_id[0]]
    for d in all_dates:
        if d in values:
            times.append(values[d])
        else:
            times.append("")

    csv_file.write(",".join([row_id[0]] + all_dates) + "\n")
    csv_file.write(",".join(times) + "\n")
csv_file.close()



