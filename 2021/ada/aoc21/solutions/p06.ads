with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Real_Time;

with Harness; use Harness;

package p06 is
    type p06a is new SolutionDispatcher with record
        null;
    end record;
    type p06b is new SolutionDispatcher with record
        null;
    end record;

    procedure GetDispatchers(D : in out Dispatchers);

    overriding
    function Solve (SDisp : p06a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType;
    overriding
    function Solve (SDisp : p06b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType;

end p06;
