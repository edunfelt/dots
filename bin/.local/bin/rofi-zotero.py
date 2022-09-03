#!/usr/bin/env python3

import argparse
import os
import re
import sqlite3
import subprocess
import shutil

HOME_DIR            = os.environ.get("HOME")
DEF_ZOTERO_DIR      = os.path.join(HOME_DIR, "Zotero")
ZOTERO_STORAGE_DIR  = "storage"
ZOTERO_SQLITE_FILE  = "zotero.sqlite"

rofi_theme = ""




# Query for getting the Authors
q_authors = """
select
  items.itemID, creators.creatorID, creators.Lastname, itemCreators.orderIndex
from
  (
    (creators JOIN itemCreators
    ON creators.creatorID = itemCreators.creatorID) as t1
    JOIN items
    ON t1.itemID = items.itemID
  )
"""

# Query for getting the titles
q_titles = """
    SELECT parentInfo.itemID, attachmentInfo.key,
           parentItemDataValues.value
    FROM itemAttachments
    INNER JOIN items AS attachmentInfo
        ON attachmentInfo.itemID = itemAttachments.itemID
    INNER JOIN itemData as attachmentItemData
        ON attachmentItemData.itemID = attachmentInfo.itemID
    INNER JOIN itemDataValues as attachmentItemDataValues
        ON attachmentItemData.valueID = attachmentItemDataValues.valueID
    INNER JOIN items AS parentInfo
        ON itemAttachments.parentItemID = parentInfo.itemID
    INNER JOIN itemData as parentItemData
        ON parentItemData.itemID = parentInfo.itemID
    INNER JOIN itemDataValues as parentItemDataValues
        ON parentItemDataValues.valueID = parentItemData.valueID
    WHERE
    attachmentItemData.fieldID = 1 AND parentItemData.fieldID = 1
      AND (itemAttachments.contentType LIKE '%pdf%'
          OR itemAttachments.contentType LIKE '%djvu%')
"""


# Query for getting the publishing dates
q_dates = """
    SELECT parentInfo.itemID, attachmentInfo.key,
           parentItemDataValues.value, parentItemData.fieldID
    FROM itemAttachments
    INNER JOIN items AS attachmentInfo
        ON attachmentInfo.itemID = itemAttachments.itemID
    INNER JOIN itemData as attachmentItemData
        ON attachmentItemData.itemID = attachmentInfo.itemID
    INNER JOIN itemDataValues as attachmentItemDataValues
        ON attachmentItemData.valueID = attachmentItemDataValues.valueID
    INNER JOIN items AS parentInfo
        ON itemAttachments.parentItemID = parentInfo.itemID
    INNER JOIN itemData as parentItemData
        ON parentItemData.itemID = parentInfo.itemID
    INNER JOIN itemDataValues as parentItemDataValues
        ON parentItemDataValues.valueID = parentItemData.valueID
    WHERE parentItemData.fieldID=6
"""

def getPDFSet(zotero_dir, PDFS_QUERY):
  conn = sqlite3.connect(target)

  pdfs = conn.execute(PDFS_QUERY)
  seen_pdfs = set()
  pdf_list = []

  for pdf in pdfs:
    # if pdf[1] not in seen_pdfs:
    pdf_list.append(pdf)
    seen_pdfs.add(pdf)

  conn.close()

  return pdf_list


VALID_EXTENSIONS = ["pdf", "djvu"]
INVALID_PATTERNS = ["sync-conflict", "zotero-pdf-state"]

def pick_file(files):
  valid_files = [f for f in files if \
    any((ext in f for ext in VALID_EXTENSIONS)) \
    and not any(invalid in f for invalid in INVALID_PATTERNS)]
  return valid_files[0]


parser = argparse.ArgumentParser( \
    description="Select a paper to open from Zotero using rofi")
parser.add_argument( \
    "-l", "--list", \
    action="store_true", \
    default=False,
    help="list PDFs instead of opening one")
parser.add_argument( \
    "-z", "--zotero", \
    action="store",
    default=DEF_ZOTERO_DIR,
    help="set the Zotero directory")
parser.add_argument( \
    "-t", "--theme", \
    action="store",
    default=rofi_theme,
    help="set the Zotero directory")

args = parser.parse_args()
# pdfs = getPDFSet(args.zotero)
pdfs_str = ""
# labels = list(range(len(pdfs)))

if args.theme:
  theming = ["-theme", args.theme]
else:
  theming = []

# make a copy of zotero database so it is available with zotero open
original = os.path.join(args.zotero, ZOTERO_SQLITE_FILE)
target = os.path.join("/tmp/", ZOTERO_SQLITE_FILE)

shutil.copyfile(original, target)

a_authors = getPDFSet(args.zotero, q_authors)
a_titles = getPDFSet(args.zotero, q_titles)
a_dates = getPDFSet(args.zotero, q_dates)

authors = {}
id = 1
author_list = []
for i in a_authors:
    if i[0] != id:
        authors[id] = author_list
        id = i[0]
        author_list = []
    author_list.append(i[2])
authors[id] = author_list


for i in authors:
    l = authors[i]
    if len(l) == 1:
        authors[i] = l[0].split(",")[0]
    elif len(l) == 2:
        authors[i] = l[0].split(",")[0] + " & " + l[1].split(",")[0]
    elif len(l) >= 3:
        authors[i] = l[0].split(",")[0] + " et al."

years = {}
for i in a_dates:
    if i[0] not in years:
        years[i[0]] = i[2].split("-")[0]

titles = {}
keys = {}
for i in a_titles:
    if i[0] not in titles:
        titles[i[0]] = i[2]
        keys[i[0]] = i[1]


pdf_list = []
for itemID in titles:
    if itemID in authors and itemID in years:
        string = authors[itemID] + " " + years[itemID] + " - " + titles[itemID]
        pdf_list.append((string, keys[itemID]))

labels = list(range(len(pdf_list)))

for i, pdf in enumerate(pdf_list):
  pdfs_str += "({}) {}\n".format(labels[i], pdf[0])

if args.list:
  print(pdfs_str)

else:
  rofi = subprocess.run(["rofi", "-threads", "0", "-dmenu", "-i", "-p", "paper"] + theming, \
      capture_output=True, text=True, input=pdfs_str)
  selected_pdf = rofi.stdout.strip()
  if len(selected_pdf) > 0:
    re_result = re.match(r"\((?P<index>\w+)\)", selected_pdf)
    if re_result is not None:
      index = re_result.group("index")
      key = pdf_list[int(index)][1]
      storage_dir = os.path.join(args.zotero, ZOTERO_STORAGE_DIR, key)
      file_to_open = pick_file(os.listdir(storage_dir))
      file_to_open_path = os.path.join(storage_dir, file_to_open)
      subprocess.Popen(["xdg-open", file_to_open_path])

