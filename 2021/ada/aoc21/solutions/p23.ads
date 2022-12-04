with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Real_Time;

with Harness; use Harness;

package p23 is
    type p23a is new SolutionDispatcher with record
        null;
    end record;
    type p23b is new SolutionDispatcher with record
        null;
    end record;

    procedure GetDispatchers(D : in out Dispatchers);

    overriding
    function Solve (SDisp : p23a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType;
    overriding
    function Solve (SDisp : p23b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType;

end p23;
