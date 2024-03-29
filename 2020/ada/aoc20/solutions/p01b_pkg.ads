with Ada.Text_IO;
with Ada.Containers.Vectors;

with Harness; use Harness;

package p01b_pkg is
    type p01b is private;
    function Make return SolutionAcc;

    procedure Solve (Self : in out p01b;
                     InData : in Ada.Text_IO.File_type;
                     Result : out ResultType);

private

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Integer);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type p01b is new Solution with record
        Input : InputType := InputPkg.Empty_Vector;
    end record;

end p01b_pkg;
