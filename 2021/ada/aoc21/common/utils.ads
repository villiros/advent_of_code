with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

package Utils is
    -- Type def for reading strings from input files
    package InputStrPkg is new Ada.Strings.Bounded.Generic_Bounded_Length(16000);
    use InputStrPkg;
    subtype InputStr is InputStrPkg.Bounded_String;

    ReadFailed : exception;

    function PeekChar(File: File_Type; DoSkipWs : Boolean := False) return Character
        with Post => (if End_Of_File(File)
                      then PeekChar'Result = ASCII.NUL
                      else PeekChar'Result /= ASCII.NUL);
    -- Peek at next character. Returns nul if eof
    -- If SkipWs is true, first skips ws before peeking.

    procedure SkipWs(File : File_Type);
    -- Skip whitespace until newline, eof, or any other character

    procedure SkipAll(File : File_Type);
    -- Skip all characters until newline or eof.

    function AdvanceLine(File : File_Type) return Boolean
        with Post => AdvanceLine'Result = not End_Of_File(File);
    -- Advance to the next line, skipping any ws characters.
    -- Return True if more can be read from the file.
    -- Raises ReadFailed if next character is not a ws/nl. Note: advancing at eof simply returns False.

    procedure AdvanceLine(File : File_Type);
    -- Advance to the next line, skipping any ws characters.
    -- Raises ReadFailed if next character is not a ws/nl. Note: advancing at eof simply returns False.

    function GetAtom(File : File_Type; Delims : Character_Set := Null_Set) return InputStr
        with Post => InputStrPkg.Length(GetAtom'Result) > 0;
    -- Read a string from input, first skipping ws, then stopping before the first whitespace, newline, or character in delims
    -- Raises ReadFailed if a string cannot be read

    function GetInt(File : File_Type; Required : Boolean := True; Default : Integer := 0) return Integer;
    -- Read an integer from input, first skipping ws, then stopping before the first whitespace, newline, or non-string character
    -- If Required is True, throws ReadFailed, otherwise returns Default if there's nothing to read.

    function GetChar(File : File_Type) return Character;
    -- Read a single character and return it (after skipping ws).
    -- If eof or no more characters on the current line, raises ReadFailed
end Utils;