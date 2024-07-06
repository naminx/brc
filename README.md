# brc

bulk renamer for CLI

Every options work only on basename (ignoring extension) by default.
However adding "-full" or "-ext" to their name will make them work
on a full file name (including extension) or only extension.

ex: brc --lowercase ABC.TXT -> abc.TXT
brc --lowercase-full ABC.TXT -> abc.txt
brc --lowercase-ext ABC.TXT -> ABC.txt

Available options:
--append-autonum START/INCREMENT/PAD
--prepend-autonum START/INCREMENT/PAD
--lowercase
--uppercase
--set VALUE
--regex /REGEX/REPLACEMENT/[g][i]
--replace /MATCH/REPLACEMENT/[g][i]
--subdir DIR
