with Ada.Text_IO;
with Ada.Containers.Vectors;

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
                    InData : in Ada.Text_IO.File_type) return ResultType;
    overriding
    function Solve (SDisp : p06b;
                    InData : in Ada.Text_IO.File_type) return ResultType;

end p06;
