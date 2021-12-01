with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded;

package Utils is
    -- Type def for reading strings from input files
    package InputStrPkg is new Ada.Strings.Bounded.Generic_Bounded_Length(16000);
    subtype InputStr is InputStrPkg.Bounded_String;

    -- Skip whitespace until a newline or non-ws character is found. Stops there
    -- Returns True if more can be read from the file.
    function SkipWs(File : File_Type) return Boolean;

    -- Skip until past a newline (or end of file).
    -- Also skips any ws before the newline.
    -- Return true if a newline was indeed skipped
    function SkipNl(File : File_type) return Boolean;

    -- Read a string from input, first skipping ws, then stopping before the first whitespace, newline, or character in delims
    function GetAtom (File : File_type; Delims : String := "") return String;

    -- Read an integer from input, first skipping ws, then stopping before the first whitespace, newline, or non-string character
    -- Throws an exception if there is non-ws to read on the line, but it's not an integer.
    procedure GetInt(File : File_Type; Result : out Integer; DidRead : out Boolean);
end Utils;