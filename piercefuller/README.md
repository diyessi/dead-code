# Pierce Fuller Computer Collection Library
Tape dumps from [Pierce Fuller's Computer Collection](https://www.piercefuller.com/collect/index.html) in [the libray](https://www.piercefuller.com/library/index.html).

Each leaf directory comes from one tape dump. The tape dump is included, along with the contents of the dump and information about corrections to the dump. Leaf directories loosely organized into directories.

A `.json` file specifies the character set in use as well as any edits to the tape dump.

The file name is based on the title card in the dump. BCD decks are dumped in UTF-8, while binary decks are dumped in hex/octal pairs, where bit 2 of the hex digit is set to 1 if the parity was incorrect. Data is grouped into 36-bit words, eight per line. If there were parity errors in the line, the line is followed by `*`. The binary format is not yet understood and will change when it is better understood.
