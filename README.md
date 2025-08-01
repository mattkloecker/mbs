# mbs
material block sync

The purpose of these files is to create a stable program to provide bidirectional sync between CSV files containing list of materials (LOM) and removals (LOR) and corresponding blocks in a DWG in AutoCAD 2023 (Base/vanilla).

The block attributes which correspond to columns in the CSV are as follows:
MATERIAL_ID (or REMOVAL_ID): A unique number provided for the singular purpose of associating a block (or blocks) with a row in the CSV list. 
DESCRIPTION: The description of the material. The string displayed in the block attribute may be shortened (depending on user settings) on a per-comma basis. 
ITEM NO (## Attribute): An number provided for formality to represent the material's row number on the CSV list. This number may have a prefix (auto-detected or manually entered) to represent the material type. The prefix detection is provided by keyword map which is hard-coded. The numerical portion of this number for each item should always be contigous list, starting from 1. When it comes to read/write operations, the value in the .CSV should always overwrite the value in the associated block. 

A high level function of the main routines is as follows. Any CHANGES to the CSV are intended to be performed transaction style; where we gather user input first on insertions, deletions, etc then provide preview of changes with Y or N option. Intent is to use BACKUP function to create backup of CSV prior to actually writing the changes. 

INSERT: Inserts block instance(s) for selected material in the list. User may choose material to insert via filter, keyword, or selection from list. Any row in the CSV lacking MATERIAL_ID (or REMOVAL_ID) should have one generated and inserted to both the block and CSV to ensure they are linked. 

INSERT (BATCH): Similar as INSERT, but with simpler workflow to quickly insert grid of blocks. 

UPDATE: This is intended to be a full scan and check of both the DWG and CSV. Several cases are expected: 
  Fully Orphaned Blocks: Block with DESCRIPTION and MATERIAL_ID that do not match any row in the .CSV. 
  Handling: Prompt user to (I)nsert to CSV, (D)elete Block, or (S)kip. (I)nsert to CSV should also generated and assign MATERIAL_ID to maintain link with block. 
  
  Semi Orphaned Blocks: Block with DESCRIPTION that has a fuzzy match to a row in the .CSV, but no (empty) MATERIAL_ID. 
  Handling: Prompt user (R)eassociate Block, (I)nsert to CSV, (D)elete Block, or (S)kip. (I)nsert to CSV should also generated and assign MATERIAL_ID to maintain link with block. 
  
  Orphaned Rows: CSV rows with no associated block in the DWG (by MATERIAL_ID or REMOVAL_ID). 
  Handling: Prompt user to (I)nsert Block, (D)elete Row, or (S)kip. This function should provide deletion using MATERIAL_ID as a reference, and DESCRIPTION as fallback. 

  All blocks and rows associated: UPDATE only - retrieve values from CSV and update associated blocks. 
