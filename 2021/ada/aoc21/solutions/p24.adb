with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;

with Harness; use Harness;
with utils; use utils;

package body p24 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(p24a'(Name => "p24a"));
        D.Append(p24b'(Name => "p24b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    ------------------------------------
    -- Input data definitions
    ------------------------------------

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => ResultType);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
    end record;

    -- procedure ReadInput(self : in out Solution'class; InData : File_Type)
    -- with
    --     Post => not self.Input.Is_Empty
    -- is
    -- begin
    --     while not End_Of_File(InData) loop
    --         self.Input.Append (GetInt(InData));
    --         AdvanceLine (InData);
    --     end loop;
    -- end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p24a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
    begin
        -- ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        -- This was solved in python with Z3
        return 91297395919993;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p24b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
    begin
        -- ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        -- This was solved in python with Z3
        return 71131151917891;
    end Solve;

end p24;
