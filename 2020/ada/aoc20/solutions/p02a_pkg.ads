with Ada.Text_IO;
with Ada.Containers.Vectors;
with Utils; use Utils;
with Harness; use Harness;

package p02a_pkg is
    type p02a is private;
    function Make return SolutionAcc;

    procedure Solve (Self : in out p02a;
                     InData : in Ada.Text_IO.File_type;
                     Result : out ResultType);

private
    type IEntry is record
        Lowest : Natural;
        Highest : Natural;
        Letter : String(1..1);
        Password : InputStr;
    end record;

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => IEntry);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type p02a is new Solution with record
        Input : InputType := InputPkg.Empty_Vector;
    end record;
end p02a_pkg;