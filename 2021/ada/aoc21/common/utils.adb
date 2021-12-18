with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Characters.Handling;
with Ada.Assertions;

with Harness;

package body Utils is
    subtype ResultType is Harness.ResultType;

    use InputStrPkg;
    procedure Assert(Check: Boolean; Message: String) renames Ada.Assertions.Assert;

    function PeekChar(File: File_Type; DoSkipWs : Boolean := False) return Character is
        c : Character;
        is_eol : Boolean;
    begin
        if DoSkipWs then
            SkipWs (File);
        end if;

        if End_Of_File(File) then
            return ASCII.NUL;
        else
            Look_Ahead(File, c, is_eol);
            if is_eol then
                return ASCII.LF;
            else
                return c;
            end if;
        end if;
    end PeekChar;

    -- Advance by one character
    procedure Advance(File : File_Type)
        with Pre => not End_Of_File(File)
        is
        c : Character;
        pragma Unreferenced(c);
    begin
        Get_Immediate(File, c);
    end Advance;

    -- Skip whitespace until newline, eof, or any other character
    procedure SkipWs(File : File_Type) is
    begin
        loop
            exit when (case PeekChar(File) is
                        when ' ' | ASCII.HT => False,
                        when others => True);
            Advance (File);
        end loop;
    end SkipWs;

    procedure SkipAll(File : File_Type) is
    begin
        loop
            exit when (case PeekChar(File) is
                       when ASCII.nul | ASCII.LF => True,
                       when others => False);
            Advance (File);
        end loop;
    end SkipAll;

    function AdvanceLine(File : File_Type) return Boolean
        is
    begin
        SkipWs(File);
        case PeekChar (File) is
            when ASCII.nul => return False;
            when ASCII.LF => Advance(File); return not end_of_file(File);
            when others => raise ReadFailed;
        end case;
    end AdvanceLine;

    procedure AdvanceLine(File : File_Type) is
        ingored : Boolean := AdvanceLine (File);
    begin
        null;
    end AdvanceLine;

    function GetAtom(File : File_Type; Delims : Character_Set := Null_Set) return InputStr is
        Result : InputStr := InputStrPkg.Null_Bounded_String;
        c : Character;
    begin
        SkipWs(File);

        loop
            c := PeekChar (File);
            exit when c = ASCII.NUL or c = ASCII.LF or c = ' ' or c = ASCII.HT;
            exit when Is_In (c, Delims);
            Result := Append (Result, "" & GetChar (File));
        end loop;

        if Length(Result) < 1 then
            raise ReadFailed;
        else
            return Result;
        end if;
    end GetAtom;

    function GetEnum(File : File_Type) return T is
        use Ada.Characters.Handling;
        InStr : String := To_Lower(To_String(GetAtom(File)));
    begin
        for i in T'first..T'last loop
            if InStr = To_Lower(i'image) then
                return i;
            end if;
        end loop;
        raise ReadFailed;
    end GetEnum;

    function GetInt(File : File_Type; Required : Boolean := True; Default : ResultType := 0)
        return ResultType is
        package Int_IO is new Integer_IO(ResultType);
        function Error return ResultType is
        begin
            if Required then
                raise ReadFailed;
            else
                return Default;
            end if;
        end Error;
    begin
        SkipWs (File);
        case PeekChar (File) is
            when '+' | '-' | '0'..'9' =>
                declare
                    Result : ResultType;
                begin
                    Int_IO.Get(File, Result);
                    return Result;
                end;
            when others => return Error;
        end case;
    end GetInt;

    function GetChar(File : File_Type; ShouldSkipWs : Boolean := True) return Character is
        c : Character;
    begin
        if ShouldSkipWs then
            SkipWs (File);
        end if;

        c := PeekChar (File);
        case c is
            when ASCII.LF | ASCII.NUL => raise ReadFailed;
            when others =>
                Advance (File);
                return c;
        end case;
    end GetChar;

    procedure SkipChar(File : File_Type; AssertExpected : Character) is
        c : Character := GetChar (File);
    begin
        Assert(c = AssertExpected, "Unexpected character " & c);
    end SkipChar;

    procedure SkipString(File : File_Type; AssertExpected : String) is
    begin
        SkipWs (File);
        for i in AssertExpected'Range loop
            declare
                actual: Character := GetChar (File, False);
            begin
                Assert(AssertExpected(i) = actual,
                    "Unexpected string at position " & i'Image & " expected char '" & AssertExpected(i) & "', got '" & actual & "'");                
            end;
        end loop;
    end SkipString;

end Utils;