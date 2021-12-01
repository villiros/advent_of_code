with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Utils is
    use InputStrPkg;

    function PeekChar(File: File_Type) return Character is
        c : Character;
        is_eol : Boolean;
    begin
        Look_Ahead(File, c, is_eol);
        if is_eol then
            return ASCII.LF;
        else
            return c;
        end if;
    end PeekChar;

    function SkipWs(File : File_Type) return Boolean is
        c : Character;
    begin
        while not End_Of_File(File) loop
            c := PeekChar (File);
            if c = ASCII.LF then
                return True;
            else
                case c is
                    when ' ' | ASCII.HT => Get_Immediate(File, c);
                    when others => return True;
                end case;
            end if;
        end loop;
        return False;
    end SkipWs;

    function SkipNl(File : File_type) return Boolean is
        NotAWs : exception;
        c : Character;
        is_out : Boolean;
    begin
        if not SkipWs(File) then
            -- End of file
            return False;
        else
            case PeekChar (File) is
                when ASCII.LF =>
                    Get_Immediate(File, c, is_out);
                    return True;
                when others =>
                    return False;
            end case;
        end if;
    end SkipNl;

    function GetAtom (File : File_type; Delims : String := "") return String is
        Result : InputStr := InputStrPkg.Null_Bounded_String;
        c : Character;
    begin
        if not SkipWs (File) then
            return "";
        else
            while not End_Of_File(File) loop
                c := PeekChar (File);
                if c = ASCII.LF then
                    return To_String (Result);
                else
                    if (c = ' ' or c = ASCII.HT or Index(Delims, "" & c) > 0) then
                        return To_String (Result);
                    else
                        Get_Immediate(File, c);
                        Result := Append (Result, "" & c);
                    end if;
                end if;
            end loop;
            return To_String (Result);
        end if;
    end GetAtom;

    procedure GetInt(File : File_Type; Result : out Integer; DidRead : out Boolean) is
        package Int_IO is new Integer_IO(Integer);
        NotAnInt : exception;
    begin
        if not SkipWs (File) then
            DidRead := False;
            return;
        else
            case PeekChar (File) is
                when ASCII.LF =>
                    DidRead := False;
                    return;
                when '+' | '-' | '0'..'9' =>
                    Int_IO.Get(File, Result);
                    DidRead := True;
                    return;
                when others =>
                    raise NotAnInt;
            end case;
        end if;
    end GetInt;
end Utils;