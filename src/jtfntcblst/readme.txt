README.TXT
=============================================================================
TFontListBox Component ver 1.01 and TFontComboBox Component ver 1.01
Last Updated 12-16-98 Created 06-29-1998 by Jimmy Theo

* TFontListBox is like standart Listbox with some standart property,
This component will list all the font installed on the system, 

* TFontComboBox is like standart Combobox with some standart property,
This component will list all the font installed on the system in Combobox.items. 

features :
- Truetype font will be displayed with a small symbol TT on the left side
- if TTOnly property is true it will display only Truetype font otherwise
  it will display all the font
- if UseItemFont property is true it will display every item in the list
  with different font according to they name.
  the item font displayed in the list has the same size and effect with
  the font property on the TFontListBox or TFontComboBox.
*New ver 1.01* 
- UpdateAllForms property : If it's set to TRUE, selecting a font will
                           automatically change the font property for all
                           forms in the application

These components are freeware !
The author is not liable for any kind of demage caused directly or
indirectly by this component. You use it on your own risk.

Theese are files you should find in archive:
  FontListBox.pas  -  Delphi unit for TFontListBox.
  FontComboBox.pas -  Delphi unit for TFontComboBox.
  readme.txt       -  The file you are reading just now.

To install TFontListBox, do following:
1) copy  FontListBox.Pas to delphi directory\lib and run delphi
2) In Delphi 3 or Delphi 4, from Component menu chose Install components
3) choose unit filename FontListBox.Pas
4) choose package file name dclusr30.dpk and click OK
5) FontListBox now installed on samples page
6) do the same with TFontComboBox

Bugs,improvements,comments all are welcome, just mail me :

Jimmy Theo
email : theo@elang.stts.ac.id 


Credits :
- This comp was based on fontcomb.pas by Hardy Yau, CIS 102144,712
- Added some features by Milos Dragovic <dragomil@EUnet.yu>

