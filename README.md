# mbs
material block sync

The purpose of these files is to create a stable program to provide bidirectional sync between CSV files containing list of materials (LOM) and removals (LOR) and corresponding blocks in a DWG in AutoCAD 2023 (Base/vanilla).

The block attributes which correspond to columns in the CSV are as follows:
MATERIAL_ID (or REMOVAL_ID): A unique number provided for the singular purpose of associating a block (or blocks) with a row in the CSV list. 
DESCRIPTION: The description of the material. The string displayed in the block attribute may be shortened (depending on user settings) on a per-comma basis. 
ITEM NO (## Attribute): An number provided for formality to represent the material's row number on the CSV list. This number may have a prefix (auto-detected or manually entered) to represent the material type. The prefix detection is provided by keyword map which is hard-coded. The numerical portion of this number for each item should always be contigous list, starting from 1. 




