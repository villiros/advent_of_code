with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Real_Time;

with Harness; use Harness;

package p12 is
    type p12a is new SolutionDispatcher with record
        null;
    end record;
    type p12b is new SolutionDispatcher with record
        null;
    end record;

    procedure GetDispatchers(D : in out Dispatchers);

    overriding
    function Solve (SDisp : p12a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType;
    overriding
    function Solve (SDisp : p12b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType;

end p12;
